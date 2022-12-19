MODULE p4zligand
   !!======================================================================
   !!                         ***  MODULE p4zligand  ***
   !! TOP :   PISCES Compute remineralization/dissolution of organic ligands
   !!=========================================================================
   !! History :   3.6  !  2016-03  (O. Aumont, A. Tagliabue) Quota model and reorganization
   !!----------------------------------------------------------------------
   !!   p4z_ligand     :  Compute remineralization/dissolution of organic ligands
   !!   p4z_ligand_init:  Initialisation of parameters for remineralisation
   !!----------------------------------------------------------------------
   USE oce_trc         ! shared variables between ocean and passive tracers
   USE trc             ! passive tracers common variables 
   USE sms_pisces      ! PISCES Source Minus Sink variables
   USE prtctl          ! print control for debugging
   USE iom             !  I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_ligand         ! called in p4zbio.F90
   PUBLIC   p4z_ligand_init    ! called in trcsms_pisces.F90

   REAL(wp), PUBLIC ::  rlgw     !: lifetime (years) of weak ligands
   REAL(wp), PUBLIC ::  rlgs     !: lifetime (years) of strong ligands
   REAL(wp), PUBLIC ::  rlig     !: Remin ligand production
   REAL(wp), PUBLIC ::  prlgw    !: Photochemical of weak ligand
   REAL(wp), PUBLIC ::  xklig    !: 1/2 saturation constant of photolysis

   LOGICAL  ::  l_dia_ligand

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p4zligand.F90 15459 2021-10-29 08:19:18Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_ligand( kt, knt, Kbb, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_ligand  ***
      !!
      !! ** Purpose :   Compute remineralization/scavenging of organic ligands
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, knt   ! ocean time step
      INTEGER, INTENT(in)  ::  Kbb, Krhs ! time level indices
      !
      INTEGER  :: ji, jj, jk
      REAL(wp) :: zlgwp, zlgwpr, zlgwr, zlablgw 
      REAL(wp) :: zlam1a, zlam1b, zaggliga, zligco
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: zligrem, zligpr, zligprod, zlcoll3d
      CHARACTER (len=25) ::   charout
      !!---------------------------------------------------------------------
      !
      IF( kt == nittrc000 )  &
          &  l_dia_ligand = iom_use( "LIGREM" ) .OR. iom_use( "LIGPR" ) &
          &            .OR. iom_use( "LPRODR" ) .OR. iom_use( "LGWCOLL" )

      IF( ln_timing )   CALL timing_start('p4z_ligand')
      !
      ! ------------------------------------------------------------------
      ! Remineralization of iron ligands
      ! ------------------------------------------------------------------

      ! production from remineralisation of organic matter
      IF( l_dia_ligand ) THEN
         ALLOCATE( zligprod(A2D(0),jpk) )   ;    zligprod(A2D(0),jpk) = 0._wp
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            zligprod(ji,jj,jk) = tr(ji,jj,jk,jplgw,Krhs)
         END_3D
      ENDIF
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         zlgwp = orem(ji,jj,jk) * rlig
         tr(ji,jj,jk,jplgw,Krhs) = tr(ji,jj,jk,jplgw,Krhs) + zlgwp 
         !
      END_3D
      !
      IF( l_dia_ligand .AND. ( lk_iomput .AND. knt == nrdttrc ) ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            zligprod(ji,jj,jk) = ( tr(ji,jj,jk,jplgw,Krhs) - zligprod(ji,jj,jk) )  &
                &               * 1e9 * 1.e+3 * rfact2r * tmask(ji,jj,jk)
         END_3D
         CALL iom_put( "LPRODR", zligprod )
         DEALLOCATE( zligprod ) 
      ENDIF

      ! Decay of weak ligand
      ! This is based on the idea that as LGW is lower
      ! there is a larger fraction of refractory OM
      IF( l_dia_ligand ) THEN
         ALLOCATE( zligrem(A2D(0),jpk) )   ;    zligrem(A2D(0),jpk) = 0._wp
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            zligrem(ji,jj,jk) = tr(ji,jj,jk,jplgw,Krhs)
         END_3D
      ENDIF
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         zlgwr = ( 1.0 / rlgs * MAX(0., tr(ji,jj,jk,jplgw,Kbb) - xfecolagg(ji,jj,jk) * 1.0E-9 )    &
         &       + 1.0 / rlgw * xfecolagg(ji,jj,jk) * 1.0E-9 ) / ( rtrn + tr(ji,jj,jk,jplgw,Kbb) )
         zlgwr = zlgwr * tgfunc(ji,jj,jk) * ( xstep / nyear_len(1) ) * blim(ji,jj,jk) * tr(ji,jj,jk,jplgw,Kbb)
         tr(ji,jj,jk,jplgw,Krhs) = tr(ji,jj,jk,jplgw,Krhs) - zlgwr 
      END_3D
      !
      IF( l_dia_ligand .AND. ( lk_iomput .AND. knt == nrdttrc ) ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            zligrem(ji,jj,jk) = - ( tr(ji,jj,jk,jplgw,Krhs) - zligrem(ji,jj,jk) )  &
                &               * 1e9 * 1.e+3 * rfact2r * tmask(ji,jj,jk)
         END_3D
         CALL iom_put( "LIGREM", zligrem )
         DEALLOCATE( zligrem ) 
      ENDIF

      ! photochem loss of weak ligand
      IF( l_dia_ligand ) THEN
         ALLOCATE( zligpr(A2D(0),jpk) )   ;    zligpr(A2D(0),jpk) = 0._wp
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            zligpr(ji,jj,jk) = tr(ji,jj,jk,jplgw,Krhs)
         END_3D
      ENDIF
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         zlgwpr = prlgw * xstep * etot(ji,jj,jk) * tr(ji,jj,jk,jplgw,Kbb)**3 * (1. - fr_i(ji,jj))   &
         &        / ( tr(ji,jj,jk,jplgw,Kbb)**2 + (xklig)**2)

         tr(ji,jj,jk,jplgw,Krhs) = tr(ji,jj,jk,jplgw,Krhs) - zlgwpr
      END_3D
      !
      IF( l_dia_ligand .AND. ( lk_iomput .AND. knt == nrdttrc ) ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            zligpr(ji,jj,jk) = - ( tr(ji,jj,jk,jplgw,Krhs) - zligpr(ji,jj,jk) )  &
               &               * 1e9 * 1.e+3 * rfact2r * tmask(ji,jj,jk)
         END_3D
         CALL iom_put( "LIGPR", zligpr )
         DEALLOCATE( zligpr ) 
      ENDIF

      ! Coagulation of ligands due to various processes (Brownian, shear, diff. sedimentation
      ! xcoagfe is computed in p4zfechem
      ! -------------------------------------------------------------------------------------
      ! 50% of the ligands are supposed to be in the colloidal size fraction as for FeL
      IF( l_dia_ligand ) THEN
         ALLOCATE( zlcoll3d(A2D(0),jpk) )   ;    zlcoll3d(A2D(0),jpk) = 0._wp
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            zlcoll3d(ji,jj,jk) = tr(ji,jj,jk,jplgw,Krhs)
         END_3D
      ENDIF
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         zligco   = 0.5 * MAX(0., tr(ji,jj,jk,jplgw,Kbb) - xfecolagg(ji,jj,jk) * 1.0E-9 )
         zaggliga = xcoagfe(ji,jj,jk) * xstep * zligco

         tr(ji,jj,jk,jplgw,Krhs) = tr(ji,jj,jk,jplgw,Krhs) - zaggliga
      END_3D
      !
      IF( l_dia_ligand .AND. ( lk_iomput .AND. knt == nrdttrc ) ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            zlcoll3d(ji,jj,jk) = - ( tr(ji,jj,jk,jplgw,Krhs) - zlcoll3d(ji,jj,jk) )  &
                &                 * 1e9 * 1.e+3 * rfact2r * tmask(ji,jj,jk)
         END_3D
         CALL iom_put( "LGWCOLL", zlcoll3d )
         DEALLOCATE( zlcoll3d ) 
      ENDIF
      !
      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('ligand1')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p4z_ligand')
      !
   END SUBROUTINE p4z_ligand


   SUBROUTINE p4z_ligand_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_ligand_init  ***
      !!
      !! ** Purpose :   Initialization of remineralization parameters
      !!
      !! ** Method  :   Read the nampislig namelist and check the parameters
      !!
      !! ** input   :   Namelist nampislig
      !!----------------------------------------------------------------------
      INTEGER ::   ios   ! Local integer 
      !
      NAMELIST/nampislig/ rlgw, prlgw, rlgs, rlig, xklig
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_ligand_init : remineralization/scavenging of organic ligands'
         WRITE(numout,*) '~~~~~~~~~~~~~~~'
      ENDIF
      READ  ( numnatp_ref, nampislig, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'nampislig in reference namelist' )
      READ  ( numnatp_cfg, nampislig, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'nampislig in configuration namelist' )
      IF(lwm) WRITE ( numonp, nampislig )
      !
      IF(lwp) THEN                         ! control print
         WRITE(numout,*) '   Namelist : nampislig'
         WRITE(numout,*) '      Lifetime (years) of weak ligands             rlgw  =', rlgw
         WRITE(numout,*) '      Remin ligand production per unit C           rlig  =', rlig
         WRITE(numout,*) '      Photolysis of weak ligand                    prlgw =', prlgw
         WRITE(numout,*) '      Lifetime (years) of strong ligands           rlgs  =', rlgs
         WRITE(numout,*) '      1/2 saturation for photolysis                xklig =', xklig
      ENDIF
      !
   END SUBROUTINE p4z_ligand_init

   !!======================================================================
END MODULE p4zligand
