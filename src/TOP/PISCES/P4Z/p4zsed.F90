MODULE p4zsed
   !!======================================================================
   !!                         ***  MODULE p4sed  ***
   !! TOP :   PISCES Compute loss of organic matter in the sediments
   !!======================================================================
   !! History :   1.0  !  2004-03 (O. Aumont) Original code
   !!             2.0  !  2007-12 (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-06 (C. Ethe) USE of fldread
   !!             3.5  !  2012-07 (O. Aumont) improvment of river input of nutrients 
   !!----------------------------------------------------------------------
   !!   p4z_sed        :  Compute loss of organic matter in the sediments
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE p2zlim          !  Co-limitations of differents nutrients
   USE p4zlim          !  Co-limitations of differents nutrients
   USE p4zint          !  interpolation and computation of various fields
   USE p4zsink         !  Sinking fluxes
   USE sed             !  Sediment module
   USE iom             !  I/O manager
   USE prtctl          !  print control for debugging

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_sed  
   PUBLIC   p4z_sed_init
   PUBLIC   p4z_sed_alloc
 
   REAL(wp), PUBLIC ::   nitrfix      !: Nitrogen fixation rate
   REAL(wp), PUBLIC ::   diazolight   !: Nitrogen fixation sensitivty to light
   REAL(wp), PUBLIC ::   concfediaz   !: Fe half-saturation Cste for diazotrophs

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: nitrpot    !: Nitrogen fixation 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:  ) :: sdenit     !: Nitrate reduction in the sediments
   !
   REAL(wp), SAVE :: r1_rday          
   REAL(wp), SAVE :: sedsilfrac, sedcalfrac

   LOGICAL  :: l_dia_sdenit, l_dia_nfix, l_dia_sed

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p4zsed.F90 15287 2021-09-24 11:11:02Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_sed( kt, knt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_sed  ***
      !!
      !! ** Purpose :   Compute loss of organic matter in the sediments. This
      !!              is by no way a sediment model. The loss is simply 
      !!              computed to balance the inout from rivers and dust
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      !
      INTEGER, INTENT(in) ::   kt, knt ! ocean time step
      INTEGER, INTENT(in) ::   Kbb, Kmm, Krhs  ! time level indices
      INTEGER  ::  ji, jj, jk, ikt
      REAL(wp) ::  zbureff
      REAL(wp) ::  zlim, zfact, zfactcal
      REAL(wp) ::  zo2, zno3, zflx, zpdenit, z1pdenit, zolimit
      REAL(wp) ::  zsiloss, zsiloss2, zcaloss, zdep
      REAL(wp) ::  zwstpoc, zwstpon, zwstpop
      REAL(wp) ::  ztrfer, ztrpo4s, ztrdp, zwdust, zmudia, ztemp
      REAL(wp) ::  zsoufer, zlight, ztrpo4, ztrdop, zratpo4
      REAL(wp) ::  xdiano3, xdianh4
      !
      CHARACTER (len=25) :: charout
      REAL(wp), DIMENSION(A2D(0)) :: zdenit2d, zrivno3, zrivalk, zrivsil
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: zw3d
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )  CALL timing_start('p4z_sed')
      !
      IF( kt == nittrc000 )  THEN
         l_dia_nfix   = iom_use( "Nfix" ) 
         l_dia_sdenit = iom_use( "Sdenit" ) 
         l_dia_sed    = .NOT.lk_sed .AND. ( iom_use( "SedC" ) .OR. iom_use( "SedCal" ) .OR. iom_use( "SedSi" ) )
      ENDIF
      !
      zdenit2d(:,:) = 0.e0
      zrivno3 (:,:) = 0.e0
      zrivalk (:,:) = 0.e0
      !
      IF( .NOT.lk_sed ) THEN
         ! Computation of the sediment denitrification proportion: The metamodel from midlleburg (2006) is being used
         ! Computation of the fraction of organic matter that is permanently buried from Dunne's model
         ! -------------------------------------------------------
         DO_2D( 0, 0, 0, 0 )
           IF( tmask(ji,jj,1) == 1 ) THEN
              ikt = mbkt(ji,jj)
              zflx = sinkpocb(ji,jj) / xstep  * 1E3 * 1E6 / 1E4
              zflx  = LOG10( MAX( 1E-3, zflx ) )
              zo2   = LOG10( MAX( 10. , tr(ji,jj,ikt,jpoxy,Kbb) * 1E6 ) )
              zno3  = LOG10( MAX( 1.  , tr(ji,jj,ikt,jpno3,Kbb) * 1E6 * rno3 ) )
              zdep  = LOG10( gdepw(ji,jj,ikt+1,Kmm) )
              zdenit2d(ji,jj) = -2.2567 - 1.185 * zflx - 0.221 * zflx**2 - 0.3995 * zno3 * zo2 + 1.25 * zno3    &
                &                + 0.4721 * zo2 - 0.0996 * zdep + 0.4256 * zflx * zo2
              zdenit2d(ji,jj) = 10.0**( zdenit2d(ji,jj) )
                !
              zflx = sinkpocb(ji,jj) / xstep * 1E6 
              zbureff = 0.013 + 0.53 * zflx**2 / ( 7.0 + zflx )**2 * MIN(gdepw(ji,jj,ikt+1,Kmm) / 1000.00, 1.0)
              zrivno3(ji,jj) = 1. - zbureff
           ENDIF
         END_2D
         !
      ENDIF

      ! This loss is scaled at each bottom grid cell for equilibrating the total budget of silica in the ocean.
      ! Thus, the amount of silica lost in the sediments equal the supply at the surface (dust+rivers)
      ! ------------------------------------------------------
      !
      IF( .NOT.lk_sed ) THEN
         DO_2D( 0, 0, 0, 0 )
            ikt  = mbkt(ji,jj)
            zdep = 1._wp / e3t(ji,jj,ikt,Kmm)
            zcaloss = sinkcalb(ji,jj) * zdep
            !
            zfactcal = MAX(-0.1, MIN( excess(ji,jj,ikt), 0.2 ) )
            zfactcal = 0.3 + 0.7 * MIN( 1., (0.1 + zfactcal) / ( 0.5 - zfactcal ) )
            zrivalk(ji,jj) = sedcalfrac * zfactcal
            tr(ji,jj,ikt,jptal,Krhs) =  tr(ji,jj,ikt,jptal,Krhs) + zcaloss * zrivalk(ji,jj) * 2.0
            tr(ji,jj,ikt,jpdic,Krhs) =  tr(ji,jj,ikt,jpdic,Krhs) + zcaloss * zrivalk(ji,jj)
         END_2D

         IF( .NOT. ln_p2z ) THEN
            DO_2D( 0, 0, 0, 0 )
               ikt  = mbkt(ji,jj)
               zdep = 1._wp / e3t(ji,jj,ikt,Kmm)
               zsiloss = sinksilb(ji,jj) * zdep
               zsiloss2 = sinksilb(ji,jj) / xstep * 365.0 * 1E3 * 1E-4 * 1E6
               zrivsil(ji,jj) = 1.0 - sedsilfrac * zsiloss2 / ( 15.0 + zsiloss2 )
               tr(ji,jj,ikt,jpsil,Krhs) = tr(ji,jj,ikt,jpsil,Krhs) + zsiloss * zrivsil(ji,jj)
            END_2D
         ENDIF
      ENDIF
      !

      ! The 0.5 factor in zpdenit is to avoid negative NO3 concentration after
      ! denitrification in the sediments. Not very clever, but simpliest option.
      IF( .NOT.lk_sed ) THEN
         IF( ln_p2z ) THEN
            DO_2D( 0, 0, 0, 0 )
               ikt  = mbkt(ji,jj)
               zwstpoc = sinkpocb(ji,jj) / e3t(ji,jj,ikt,Kmm)
               zpdenit  = MIN( 0.5 * ( tr(ji,jj,ikt,jpno3,Kbb) - rtrn ) / rdenit, zdenit2d(ji,jj) * zwstpoc * zrivno3(ji,jj) )
               z1pdenit = zwstpoc * zrivno3(ji,jj) - zpdenit
               zolimit = MIN( ( tr(ji,jj,ikt,jpoxy,Kbb) - rtrn ) / (o2ut + o2nit), z1pdenit * ( 1.- nitrfac(ji,jj,ikt) ) )
               tr(ji,jj,ikt,jpdoc,Krhs) = tr(ji,jj,ikt,jpdoc,Krhs) + z1pdenit - zolimit
               tr(ji,jj,ikt,jpno3,Krhs) = tr(ji,jj,ikt,jpno3,Krhs) + zpdenit + zolimit - rdenit * zpdenit
               tr(ji,jj,ikt,jpoxy,Krhs) = tr(ji,jj,ikt,jpoxy,Krhs) - zolimit * (o2ut + o2nit)
               tr(ji,jj,ikt,jptal,Krhs) = tr(ji,jj,ikt,jptal,Krhs) - rno3 * (zolimit + (1.-rdenit) * zpdenit )
               tr(ji,jj,ikt,jpdic,Krhs) = tr(ji,jj,ikt,jpdic,Krhs) + zpdenit + zolimit
               sdenit(ji,jj) = rdenit * zpdenit * e3t(ji,jj,ikt,Kmm)
            END_2D
         ELSE
            DO_2D( 0, 0, 0, 0 )
               ikt  = mbkt(ji,jj)
               zwstpoc = sinkpocb(ji,jj) / e3t(ji,jj,ikt,Kmm)
               zpdenit  = MIN( 0.5 * ( tr(ji,jj,ikt,jpno3,Kbb) - rtrn ) / rdenit, zdenit2d(ji,jj) * zwstpoc * zrivno3(ji,jj) )
               z1pdenit = zwstpoc * zrivno3(ji,jj) - zpdenit
               zolimit = MIN( ( tr(ji,jj,ikt,jpoxy,Kbb) - rtrn ) / o2ut, z1pdenit * ( 1.- nitrfac(ji,jj,ikt) ) )
               tr(ji,jj,ikt,jpdoc,Krhs) = tr(ji,jj,ikt,jpdoc,Krhs) + z1pdenit - zolimit
               tr(ji,jj,ikt,jppo4,Krhs) = tr(ji,jj,ikt,jppo4,Krhs) + zpdenit + zolimit
               tr(ji,jj,ikt,jpnh4,Krhs) = tr(ji,jj,ikt,jpnh4,Krhs) + zpdenit + zolimit
               tr(ji,jj,ikt,jpno3,Krhs) = tr(ji,jj,ikt,jpno3,Krhs) - rdenit * zpdenit
               tr(ji,jj,ikt,jpoxy,Krhs) = tr(ji,jj,ikt,jpoxy,Krhs) - zolimit * o2ut
               tr(ji,jj,ikt,jptal,Krhs) = tr(ji,jj,ikt,jptal,Krhs) + rno3 * (zolimit + (1.+rdenit) * zpdenit )
               tr(ji,jj,ikt,jpdic,Krhs) = tr(ji,jj,ikt,jpdic,Krhs) + zpdenit + zolimit 
               sdenit(ji,jj) = rdenit * zpdenit * e3t(ji,jj,ikt,Kmm)
            END_2D
         ENDIF
         IF( ln_p5z ) THEN
            DO_2D( 0, 0, 0, 0 )
               ikt  = mbkt(ji,jj)
               zdep = 1._wp / e3t(ji,jj,ikt,Kmm)
               zwstpoc = sinkpocb(ji,jj) * zdep
               zwstpop = sinkpopb(ji,jj) * zdep
               zwstpon = sinkponb(ji,jj) * zdep
               zpdenit  = MIN( 0.5 * ( tr(ji,jj,ikt,jpno3,Kbb) - rtrn ) / rdenit, zdenit2d(ji,jj) * zwstpoc * zrivno3(ji,jj) )
               z1pdenit = zwstpoc * zrivno3(ji,jj) - zpdenit
               zolimit = MIN( ( tr(ji,jj,ikt,jpoxy,Kbb) - rtrn ) / o2ut, z1pdenit * ( 1.- nitrfac(ji,jj,ikt) ) )
               tr(ji,jj,ikt,jpdon,Krhs) = tr(ji,jj,ikt,jpdon,Krhs) + ( z1pdenit - zolimit ) * zwstpon / (zwstpoc + rtrn)
               tr(ji,jj,ikt,jpdop,Krhs) = tr(ji,jj,ikt,jpdop,Krhs) + ( z1pdenit - zolimit ) * zwstpop / (zwstpoc + rtrn)
            END_2D
         ENDIF
       ENDIF


      ! Nitrogen fixation process
      ! Small source iron from particulate inorganic iron
      !-----------------------------------
      !
      IF( ln_p2z ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            !                      ! Potential nitrogen fixation dependant on temperature and iron
            zlight  =  ( 1.- EXP( -etot_ndcy(ji,jj,jk) / diazolight ) ) * ( 1. - fr_i(ji,jj) )
            zsoufer = zlight * 2E-11 / ( 2E-11 + biron(ji,jj,jk) )
            !
            ztemp = ts(ji,jj,jk,jp_tem,Kmm)
            zmudia = MAX( 0.,-0.001096*ztemp**2 + 0.057*ztemp -0.637 ) / rno3
            !       Potential nitrogen fixation dependant on temperature and iron
            xdiano3 = tr(ji,jj,jk,jpno3,Kbb) / ( concnno3 + tr(ji,jj,jk,jpno3,Kbb) )
            zlim    = 1.- xdiano3 
            IF( zlim <= 0.1 )   zlim = 0.01
            zfact   = zlim * rfact2
            ztrfer  = biron(ji,jj,jk) / ( concfediaz + biron(ji,jj,jk) )
            nitrpot(ji,jj,jk) =  zmudia * r1_rday * zfact * ztrfer * zlight
            !
            ! Nitrogen change due to nitrogen fixation
            ! ----------------------------------------
            zfact = nitrpot(ji,jj,jk) * nitrfix
            tr(ji,jj,jk,jpno3,Krhs) = tr(ji,jj,jk,jpno3,Krhs) + zfact / 3.0
            tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) - rno3 * zfact / 3.0
            tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) - zfact * 2.0 / 3.0
            tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zfact * 1.0 / 3.0
            tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) + zfact * 1.0 / 3.0
            tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) - zfact * 1.0 / 3.0 * feratz
            tr(ji,jj,jk,jpoxy,Krhs) = tr(ji,jj,jk,jpoxy,Krhs) + ( o2ut + o2nit ) * zfact
            tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + 0.005 * 4E-10 * zsoufer * rfact2 / rday
         END_3D
      ELSE
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            !                      ! Potential nitrogen fixation dependant on temperature and iron
            zlight  =  ( 1.- EXP( -etot_ndcy(ji,jj,jk) / diazolight ) ) * ( 1. - fr_i(ji,jj) ) 
            zsoufer = zlight * 2E-11 / ( 2E-11 + biron(ji,jj,jk) )
            !
            ztemp = ts(ji,jj,jk,jp_tem,Kmm)
            zmudia = MAX( 0.,-0.001096*ztemp**2 + 0.057*ztemp -0.637 ) / rno3
            !       Potential nitrogen fixation dependant on temperature and iron
            xdianh4 = tr(ji,jj,jk,jpnh4,Kbb) / ( concnnh4 + tr(ji,jj,jk,jpnh4,Kbb) )
            xdiano3 = tr(ji,jj,jk,jpno3,Kbb) / ( concnno3 + tr(ji,jj,jk,jpno3,Kbb) ) * (1. - xdianh4)
            zlim    = ( 1.- xdiano3 - xdianh4 )
            IF( zlim <= 0.1 )   zlim = 0.01
            zfact   = zlim * rfact2
            ztrfer  = biron(ji,jj,jk) / ( concfediaz + biron(ji,jj,jk) )
            ztrpo4  = tr(ji,jj,jk,jppo4,Kbb) / ( 1E-6 + tr(ji,jj,jk,jppo4,Kbb) )
            IF (ln_p5z) THEN
               ztrdop  = tr(ji,jj,jk,jpdop,Kbb) / ( 10E-6 + tr(ji,jj,jk,jpdop,Kbb) ) * (1. - ztrpo4)
               ztrpo4  =  ztrpo4 + ztrdop + rtrn
               zratpo4 =  (ztrpo4 - ztrdop) / (ztrpo4 + rtrn)
            ENDIF
            nitrpot(ji,jj,jk) =  zmudia * r1_rday * zfact * MIN( ztrfer, ztrpo4 ) * zlight
            !
            ! Nitrogen change due to nitrogen fixation
            ! ----------------------------------------
            zfact = nitrpot(ji,jj,jk) * nitrfix
            tr(ji,jj,jk,jpnh4,Krhs) = tr(ji,jj,jk,jpnh4,Krhs) + zfact / 3.0
            tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) + rno3 * zfact / 3.0
            tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) - zfact * 2.0 / 3.0            
            tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zfact * 1.0 / 3.0
            tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) + zfact * 1.0 / 3.0 * 2.0 / 3.0
            tr(ji,jj,jk,jpgoc,Krhs) = tr(ji,jj,jk,jpgoc,Krhs) + zfact * 1.0 / 3.0 * 1.0 / 3.0
            tr(ji,jj,jk,jpoxy,Krhs) = tr(ji,jj,jk,jpoxy,Krhs) + ( o2ut + o2nit ) * zfact * 2.0 / 3.0 + o2nit * zfact / 3.0
            tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) - 30E-6 * zfact * 1.0 / 3.0
            tr(ji,jj,jk,jpsfe,Krhs) = tr(ji,jj,jk,jpsfe,Krhs) + 30E-6 * zfact * 1.0 / 3.0 * 2.0 / 3.0
            tr(ji,jj,jk,jpbfe,Krhs) = tr(ji,jj,jk,jpbfe,Krhs) + 30E-6 * zfact * 1.0 / 3.0 * 1.0 / 3.0
            tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + 0.003 * 4E-10 * zsoufer * rfact2 / rday
            IF ( ln_p4z) THEN
               tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) - zfact * 2.0 / 3.0
               tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) + concdnh4 / ( concdnh4 + tr(ji,jj,jk,jppo4,Kbb) ) &
               &                     * 0.001 * tr(ji,jj,jk,jpdoc,Kbb) * xstep
            ELSE
               tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) - 16.0 / 46.0 * zfact * 2.0 / 3.0  &
               &                     * zratpo4
               tr(ji,jj,jk,jpdon,Krhs) = tr(ji,jj,jk,jpdon,Krhs) + zfact * 1.0 / 3.0
               tr(ji,jj,jk,jpdop,Krhs) = tr(ji,jj,jk,jpdop,Krhs) + 16.0 / 46.0 * zfact / 3.0  &
               &                     - 16.0 / 46.0 * zfact * 2.0 / 3.0 * (1.0 - zratpo4)
               tr(ji,jj,jk,jppon,Krhs) = tr(ji,jj,jk,jppon,Krhs) + zfact * 1.0 / 3.0 * 2.0 /3.0
               tr(ji,jj,jk,jppop,Krhs) = tr(ji,jj,jk,jppop,Krhs) + 16.0 / 46.0 * zfact * 1.0 / 3.0 * 2.0 /3.0
               tr(ji,jj,jk,jpgon,Krhs) = tr(ji,jj,jk,jpgon,Krhs) + zfact * 1.0 / 3.0 * 1.0 /3.0
               tr(ji,jj,jk,jpgop,Krhs) = tr(ji,jj,jk,jpgop,Krhs) + 16.0 / 46.0 * zfact * 1.0 / 3.0 * 1.0 /3.0
            ENDIF
         END_3D
      ENDIF

      IF( lk_iomput .AND. knt == nrdttrc ) THEN
          !
          IF( l_dia_nfix ) THEN   ! nitrogen fixation
            zfact = rno3 * 1.e+3 * rfact2r !  conversion from molC/l/kt  to molN/m3/s
            ALLOCATE( zw3d(A2D(0),jpk) )  ;  zw3d(A2D(0),jpk) = 0._wp
            zw3d(A2D(0),1:jpkm1) = nitrpot(A2D(0),1:jpkm1) * nitrfix * zfact * tmask(A2D(0),1:jpkm1)
            CALL iom_put( "Nfix", zw3d )
            DEALLOCATE( zw3d )
          ENDIF
          !
          IF( l_dia_sed ) THEN
            zfact = 1.e+3 * rfact2r !  conversion from molC/l/kt  to molC/m3/s
            CALL iom_put( "SedCal", ( 1.0 - zrivalk(:,:) ) * sinkcalb(:,:) * zfact )
            CALL iom_put( "SedC"  , ( 1.0 - zrivno3(:,:) ) * sinkpocb(:,:) * zfact )
            IF( .NOT. ln_p2z ) THEN
               CALL iom_put( "SedSi" , ( 1.0 - zrivsil(:,:) ) * sinksilb(:,:) * zfact )
            ENDIF
          ENDIF
          !
          IF( l_dia_sdenit ) THEN
            zfact = rno3 * 1.e+3 * rfact2r !  conversion from molC/l/kt  to molN/m3/s
            CALL iom_put( "Sdenit", sdenit(:,:) * zfact )
          ENDIF
          !
      ENDIF
      !
      IF(sn_cfctl%l_prttrc) THEN  ! print mean trneds (USEd for debugging)
         WRITE(charout, fmt="('sed ')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )  CALL timing_stop('p4z_sed')
      !
   END SUBROUTINE p4z_sed

   SUBROUTINE p4z_sed_init
      !!----------------------------------------------------------------------
      !!                  ***  routine p4z_sed_init  ***
      !!
      !! ** purpose :   initialization of some parameters
      !!
      !!----------------------------------------------------------------------
      !!----------------------------------------------------------------------
      INTEGER  :: ji, jj, jk, jm
      INTEGER  :: ios                 ! Local integer output status for namelist read
      !
      !!
      NAMELIST/nampissed/ nitrfix, diazolight, concfediaz
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_sed_init : initialization of sediment mobilisation '
         WRITE(numout,*) '~~~~~~~~~~~~ '
      ENDIF
      !                            !* set file information
      READ_NML_REF(numnatp,nampissed)
      READ_NML_CFG(numnatp,nampissed)
      IF(lwm) WRITE ( numonp, nampissed )

      IF(lwp) THEN
         WRITE(numout,*) '   Namelist : nampissed '
         WRITE(numout,*) '      nitrogen fixation rate                       nitrfix = ', nitrfix
         WRITE(numout,*) '      nitrogen fixation sensitivty to light    diazolight  = ', diazolight
         IF( .NOT. ln_p2z ) THEN
            WRITE(numout,*) '      Fe half-saturation cste for diazotrophs  concfediaz  = ', concfediaz
         ENDIF
      ENDIF
      !
      r1_rday  = 1. / rday
      !
      sedsilfrac = 0.03     ! percentage of silica loss in the sediments
      sedcalfrac = 0.99     ! percentage of calcite loss in the sediments
      !
      lk_sed = ln_sediment .AND. ln_sed_2way 
      !
   END SUBROUTINE p4z_sed_init

   INTEGER FUNCTION p4z_sed_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_sed_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( nitrpot(A2D(0),jpk), sdenit(A2D(0)), STAT=p4z_sed_alloc )
      !
      IF( p4z_sed_alloc /= 0 )   CALL ctl_stop( 'STOP', 'p4z_sed_alloc: failed to allocate arrays' )
      !
   END FUNCTION p4z_sed_alloc

   !!======================================================================
END MODULE p4zsed
