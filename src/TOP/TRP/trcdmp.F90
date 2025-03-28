MODULE trcdmp
   !!======================================================================
   !!                       ***  MODULE  trcdmp  ***
   !! Ocean physics: internal restoring trend on passive tracers
   !!======================================================================
   !! History :  OPA  !  1991-03  (O. Marti, G. Madec)  Original code
   !!                 !  1996-01  (G. Madec) statement function for e3
   !!                 !  1997-05  (H. Loukos)  adapted for passive tracers
   !!    NEMO    9.0  !  2004-03  (C. Ethe)    free form + modules
   !!            3.2  !  2007-02  (C. Deltel)  Diagnose ML trends for passive tracers
   !!            3.3  !  2010-06  (C. Ethe, G. Madec) merge TRA-TRC 
   !!----------------------------------------------------------------------
#if  defined key_top 
   !!----------------------------------------------------------------------
   !!   trc_dmp      : update the tracer trend with the internal damping
   !!   trc_dmp_init : initialization, namlist read, parameters control
   !!----------------------------------------------------------------------
   USE par_trc        ! need jptra, number of passive tracers
   USE oce_trc         ! ocean dynamics and tracers variables
   USE trc             ! ocean passive tracers variables
   USE trcdta
   USE tradmp
   USE trdtra
   USE trd_oce
   !
   USE iom
   USE prtctl          ! Print control for debbuging

   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_dmp      
   PUBLIC trc_dmp_clo   
   PUBLIC trc_dmp_alloc  
   PUBLIC trc_dmp_ini    

   INTEGER            , PUBLIC ::   nn_zdmp_tr    !: = 0/1/2 flag for damping in the mixed layer
   CHARACTER(LEN=200) , PUBLIC ::   cn_resto_tr   !: File containing restoration coefficient

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   restotr   ! restoring coeff. on tracers (s-1)

   INTEGER, PARAMETER         ::   npncts = 8       ! number of closed sea
   INTEGER, DIMENSION(npncts) ::   nctsi1, nctsj1   ! south-west closed sea limits (i,j)
   INTEGER, DIMENSION(npncts) ::   nctsi2, nctsj2   ! north-east closed sea limits (i,j)

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION trc_dmp_alloc()
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_dmp_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( restotr(A2D(0),jpk) , STAT=trc_dmp_alloc )
      !
      IF( trc_dmp_alloc /= 0 )   CALL ctl_warn('trc_dmp_alloc: failed to allocate array')
      !
   END FUNCTION trc_dmp_alloc


   SUBROUTINE trc_dmp( kt, Kbb, Kmm, ptr, Krhs )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_dmp  ***
      !!                  
      !! ** Purpose :   Compute the passive tracer trend due to a newtonian damping
      !!      of the tracer field towards given data field and add it to the
      !!      general tracer trends.
      !!
      !! ** Method  :   Newtonian damping towards trdta computed 
      !!      and add to the general tracer trends:
      !!                     tr(Kmm) = tr(Krhs) + restotr * (trdta - tr(Kbb))
      !!         The trend is computed either throughout the water column
      !!      (nlmdmptr=0) or in area of weak vertical mixing (nlmdmptr=1) or
      !!      below the well mixed layer (nlmdmptr=2)
      !!
      !! ** Action  : - update the tracer trends tr(:,:,:,:,Krhs) with the newtonian 
      !!                damping trends.
      !!              - save the trends ('key_trdmxl_trc')
      !!----------------------------------------------------------------------
      INTEGER,                                    INTENT(in   ) :: kt              ! ocean time-step index
      INTEGER,                                    INTENT(in   ) :: Kbb, Kmm, Krhs  ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jptra,jpt), INTENT(inout) :: ptr             ! passive tracers and RHS of tracer equation
      !
      INTEGER ::   ji, jj, jk, jn   ! dummy loop indices
      CHARACTER (len=22) ::   charout
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   ztrtrd
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   ztrcdta   ! 3D  workspace
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_dmp')
      !
      IF( nb_trcdta > 0 ) THEN  ! Initialisation of tracer from a file that may also be used for damping
         !
         ALLOCATE( ztrcdta(T2D(nn_hls),jpk) )    ! Memory allocation
         !
         IF( l_trdtrc )   ALLOCATE( ztrtrd(T2D(0),jpk) )   ! temporary save of trends
         !                                                          ! ===========
         DO jn = 1, jptra                                           ! tracer loop
            !                                                       ! ===========
            IF( l_trdtrc ) THEN
               DO_3D( 0, 0, 0, 0, 1, jpk )
                  ztrtrd(ji,jj,jk) = ptr(ji,jj,jk,jn,Krhs)    ! save trends
               END_3D
            END IF
            !
            IF( ln_trc_ini(jn) ) THEN      ! update passive tracers arrays with input data read from file
               !
               CALL trc_dta( kt, jn, ztrcdta )   ! read tracer data at nit000
               !
               SELECT CASE ( nn_zdmp_tr )
               !
               CASE( 0 )                !==  newtonian damping throughout the water column  ==!
                  DO_3D( 0, 0, 0, 0, 1, jpkm1 )
                     ptr(ji,jj,jk,jn,Krhs) = ptr(ji,jj,jk,jn,Krhs) + restotr(ji,jj,jk) * ( ztrcdta(ji,jj,jk) - ptr(ji,jj,jk,jn,Kbb) )
                  END_3D
                  !
               CASE ( 1 )                !==  no damping in the turbocline (avt > 5 cm2/s)  ==!
                  DO_3D( 0, 0, 0, 0, 1, jpkm1 )
                     IF( avt(ji,jj,jk) <= avt_c )  THEN 
                        ptr(ji,jj,jk,jn,Krhs) = ptr(ji,jj,jk,jn,Krhs) + restotr(ji,jj,jk) * ( ztrcdta(ji,jj,jk) - ptr(ji,jj,jk,jn,Kbb) )
                     ENDIF
                  END_3D
                  !
               CASE ( 2 )               !==  no damping in the mixed layer   ==! 
                  DO_3D( 0, 0, 0, 0, 1, jpkm1 )
                     IF( gdept(ji,jj,jk,Kmm) >= hmlp (ji,jj) ) THEN
                        ptr(ji,jj,jk,jn,Krhs) = ptr(ji,jj,jk,jn,Krhs) + restotr(ji,jj,jk) * ( ztrcdta(ji,jj,jk) - ptr(ji,jj,jk,jn,Kbb) )
                     END IF
                  END_3D
                  !  
               END SELECT
               ! 
            ENDIF
            !
            IF( l_trdtrc ) THEN
               DO_3D( 0, 0, 0, 0, 1, jpk )
                  ztrtrd(ji,jj,jk) = ptr(ji,jj,jk,jn,Krhs) -  ztrtrd(ji,jj,jk)
               END_3D
               CALL trd_tra( kt, Kmm, Krhs, 'TRC', jn, jptra_dmp, ztrtrd )
            END IF
            !                                                       ! ===========
         END DO                                                     ! tracer loop
         !                                                          ! ===========
         DEALLOCATE( ztrcdta )
         !
         IF( l_trdtrc )  DEALLOCATE( ztrtrd )
      ENDIF
      !                                          ! print mean trends (used for debugging)
      IF( sn_cfctl%l_prttrc ) THEN
         WRITE(charout, FMT="('dmp ')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl( tab4d_1=ptr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm, clinfo3='trd' )
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('trc_dmp')
      !
   END SUBROUTINE trc_dmp


   SUBROUTINE trc_dmp_ini
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trc_dmp_ini  ***
      !! 
      !! ** Purpose :   Initialization for the newtonian damping 
      !!
      !! ** Method  :   read the nammbf namelist and check the parameters
      !!              called by trc_dmp at the first timestep (nittrc000)
      !!----------------------------------------------------------------------
      INTEGER ::   ios, imask  ! local integers
      !!
      NAMELIST/namtrc_dmp/ nn_zdmp_tr , cn_resto_tr
      !!----------------------------------------------------------------------
      !
      READ_NML_REF(numnat,namtrc_dmp)
      READ_NML_CFG(numnat,namtrc_dmp)
      IF(lwm) WRITE ( numont, namtrc_dmp )

      IF(lwp) THEN                       ! Namelist print
         WRITE(numout,*)
         WRITE(numout,*) 'trc_dmp : Passive tracers newtonian damping'
         WRITE(numout,*) '~~~~~~~'
         WRITE(numout,*) '   Namelist namtrc_dmp : set damping parameter'
         WRITE(numout,*) '      mixed layer damping option     nn_zdmp_tr  = ', nn_zdmp_tr, '(zoom: forced to 0)'
         WRITE(numout,*) '      Restoration coeff file         cn_resto_tr = ', cn_resto_tr
      ENDIF
      !                          ! Allocate arrays
      IF( trc_dmp_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'trc_dmp_ini: unable to allocate arrays' )
      !
      SELECT CASE ( nn_zdmp_tr )
      CASE ( 0 )   ;   IF(lwp) WRITE(numout,*) '      ===>>   tracer damping throughout the water column'
      CASE ( 1 )   ;   IF(lwp) WRITE(numout,*) '      ===>>   no tracer damping in the turbocline (avt > 5 cm2/s)'
      CASE ( 2 )   ;   IF(lwp) WRITE(numout,*) '      ===>>   no tracer damping in the mixed layer'
      CASE DEFAULT
         WRITE(ctmp1,*) 'bad flag value for nn_zdmp_tr = ', nn_zdmp_tr
         CALL ctl_stop(ctmp1)
      END SELECT

      IF( .NOT.ln_c1d ) THEN
         IF( .NOT.ln_tradmp )   &
            &   CALL ctl_stop( 'passive tracer damping need ln_tradmp to compute damping coef.' )
         !
         !                          ! Read damping coefficients from file
         !Read in mask from file
         CALL iom_open ( cn_resto_tr, imask)
         CALL iom_get  ( imask, jpdom_auto, 'resto', restotr)
         CALL iom_close( imask )
         !
      ENDIF
      !
   END SUBROUTINE trc_dmp_ini


   SUBROUTINE trc_dmp_clo( kt, Kbb, Kmm )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trc_dmp_clo  ***
      !!
      !! ** Purpose :   Closed sea domain initialization
      !!
      !! ** Method  :   if a closed sea is located only in a model grid point
      !!                we restore to initial data
      !!
      !! ** Action  :   nctsi1(), nctsj1() : south-west closed sea limits (i,j)
      !!                nctsi2(), nctsj2() : north-east Closed sea limits (i,j)
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt           ! ocean time-step index
      INTEGER, INTENT( in ) ::   Kbb, Kmm     ! time level indices
      !
      INTEGER :: ji , jj, jk, jn, jc                        ! dummy loop indicesa
      INTEGER :: isrow                                      ! local index
      REAL(wp), POINTER, DIMENSION(:,:,:) ::  ztrcdta       ! 3D  workspace
      !!----------------------------------------------------------------------

      IF( kt == nit000 ) THEN
         ! initial values
         nctsi1(:) = 1  ;  nctsi2(:) = 1
         nctsj1(:) = 1  ;  nctsj2(:) = 1

         ! set the closed seas (in data domain indices)
         ! -------------------

         IF( cn_cfg == "orca" .OR. cn_cfg == "ORCA") THEN
            !
            SELECT CASE ( nn_cfg )
            !                                           ! =======================
            CASE ( 1 )                                  ! eORCA_R1 configuration
               !                                        ! =======================
               !
               isrow = 332 - (Nj0glo + 1)   ! was 332 - jpjglo -> jpjglo_old_version = Nj0glo + 1
               !
               nctsi1(1)   = 333  ; nctsj1(1)   = 243 - isrow   ! Caspian Sea
               nctsi2(1)   = 342  ; nctsj2(1)   = 274 - isrow
               !                                        
               nctsi1(2)   = 198  ; nctsj1(2)   = 258 - isrow   ! Lake Superior
               nctsi2(2)   = 204  ; nctsj2(2)   = 262 - isrow
               !                                         
               nctsi1(3)   = 201  ; nctsj1(3)   = 250 - isrow   ! Lake Michigan
               nctsi2(3)   = 203  ; nctsj2(3)   = 256 - isrow
               !                                        
               nctsi1(4)   = 204  ; nctsj1(4)   = 252 - isrow   ! Lake Huron
               nctsi2(4)   = 209  ; nctsj2(4)   = 256 - isrow
               !                                        
               nctsi1(5)   = 206  ; nctsj1(5)   = 249 - isrow   ! Lake Erie
               nctsi2(5)   = 209  ; nctsj2(5)   = 251 - isrow
               !                                        
               nctsi1(6)   = 210  ; nctsj1(6)   = 252 - isrow   ! Lake Ontario
               nctsi2(6)   = 212  ; nctsj2(6)   = 252 - isrow
               !                                        
               nctsi1(7)   = 321  ; nctsj1(7)   = 180 - isrow   ! Victoria Lake
               nctsi2(7)   = 322  ; nctsj2(7)   = 189 - isrow
               !                                        
               nctsi1(8)   = 297  ; nctsj1(8)   = 270 - isrow   ! Baltic Sea
               nctsi2(8)   = 308  ; nctsj2(8)   = 293 - isrow
               !
               !                                        ! =======================
            CASE ( 2 )                                  !  ORCA_R2 configuration
               !                                        ! =======================
               !                                      
               nctsi1(1)   =  11  ;  nctsj1(1)   = 103       ! Caspian Sea
               nctsi2(1)   =  17  ;  nctsj2(1)   = 112
               !                                     
               nctsi1(2)   =  97  ;  nctsj1(2)   = 107       ! Great North American Lakes
               nctsi2(2)   = 103  ;  nctsj2(2)   = 111
               !                                     
               nctsi1(3)   = 174  ;  nctsj1(3)   = 107       ! Black Sea 1 : west part of the Black Sea
               nctsi2(3)   = 181  ;  nctsj2(3)   = 112
              !                                      
               nctsi1(4)   =   2  ;  nctsj1(4)   = 107       ! Black Sea 2 : est part of the Black Sea
               nctsi2(4)   =   6  ;  nctsj2(4)   = 112
               !                                     
               nctsi1(5)   =  145 ;  nctsj1(5)   = 116       ! Baltic Sea
               nctsi2(5)   =  150 ;  nctsj2(5)   = 126
               !
               !                                        ! =======================
            CASE ( 4 )                                  !  ORCA_R4 configuration
               !                                        ! =======================
               !                                   
               nctsi1(1)   =  4  ;  nctsj1(1)   = 53         ! Caspian Sea
               nctsi2(1)   =  4  ;  nctsj2(1)   = 56
               !                                   
               nctsi1(2)   = 49  ;  nctsj1(2)   = 55         ! Great North American Lakes
               nctsi2(2)   = 51  ;  nctsj2(2)   = 56
               !                                   
               nctsi1(3)   = 88  ;  nctsj1(3)   = 55         ! Black Sea
               nctsi2(3)   = 91  ;  nctsj2(3)   = 56
               !                                   
               nctsi1(4)   = 75  ;  nctsj1(4)   = 59         ! Baltic Sea
               nctsi2(4)   = 76  ;  nctsj2(4)   = 61
               !
               !                                        ! =======================
            CASE ( 025 )                                ! ORCA_R025 configuration
               !                                        ! =======================
               !                                   
               nctsi1(1)   = 1330 ; nctsj1(1)   = 645        ! Caspian + Aral sea
               nctsi2(1)   = 1400 ; nctsj2(1)   = 795
               !                                    
               nctsi1(2)   = 1284 ; nctsj1(2)   = 722        ! Azov Sea
               nctsi2(2)   = 1304 ; nctsj2(2)   = 747
               !
            END SELECT
            !
         ENDIF
         !
         nctsi1(:) = nctsi1(:) + nn_hls - 1   ;   nctsi2(:) = nctsi2(:) + nn_hls - 1   ! -1 as x-perio included in old input files
         nctsj1(:) = nctsj1(:) + nn_hls       ;   nctsj2(:) = nctsj2(:) + nn_hls
         !
         ! convert the position in local domain indices
         ! --------------------------------------------
         DO jc = 1, npncts
            nctsi1(jc)   = mi0( nctsi1(jc), nn_hls )
            nctsj1(jc)   = mj0( nctsj1(jc), nn_hls )
            !
            nctsi2(jc)   = mi1( nctsi2(jc), nn_hls )
            nctsj2(jc)   = mj1( nctsj2(jc), nn_hls )
         END DO
         !
      ENDIF

      ! Restore close seas values to initial data
      IF( ln_trcdta .AND. nb_trcdta > 0 )  THEN   ! Initialisation of tracer from a file that may also be used for damping
         !
         IF(lwp)  WRITE(numout,*)
         IF(lwp)  WRITE(numout,*) ' trc_dmp_clo : Restoring of nutrients on close seas at time-step kt = ', kt
         IF(lwp)  WRITE(numout,*)
         !
         ALLOCATE( ztrcdta(jpi,jpj,jpk) )   ! Memory allocation
         !
         DO jn = 1, jptra
            IF( ln_trc_ini(jn) ) THEN      ! update passive tracers arrays with input data read from file
                CALL trc_dta( kt, jn, ztrcdta )   ! read tracer data at nit000
                DO jc = 1, npncts
                   DO jk = 1, jpkm1
                      DO jj = nctsj1(jc), nctsj2(jc)
                         DO ji = nctsi1(jc), nctsi2(jc)
                            tr(ji,jj,jk,jn,Kmm) = ztrcdta(ji,jj,jk)
                            tr(ji,jj,jk,jn,Kbb) = tr(ji,jj,jk,jn,Kmm)
                         END DO
                      END DO
                   END DO
                END DO
             ENDIF
          END DO
          DEALLOCATE( ztrcdta )
      ENDIF
      !
   END SUBROUTINE trc_dmp_clo
 
#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
   IMPLICIT NONE
CONTAINS
   SUBROUTINE trc_dmp( kt )        ! Empty routine
      INTEGER, INTENT(in) :: kt
      WRITE(*,*) 'trc_dmp: You should not have seen this print! error?', kt
   END SUBROUTINE trc_dmp
#endif

   !!======================================================================
END MODULE trcdmp
