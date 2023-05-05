MODULE diaar5
   !!======================================================================
   !!                       ***  MODULE  diaar5  ***
   !! AR5 diagnostics
   !!======================================================================
   !! History :  3.2  !  2009-11  (S. Masson)  Original code
   !!            3.3  !  2010-10  (C. Ethe, G. Madec) reorganisation of initialisation phase + merge TRC-TRA
   !!----------------------------------------------------------------------
   !!   dia_ar5       : AR5 diagnostics
   !!   dia_ar5_init  : initialisation of AR5 diagnostics
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and active tracers
   USE dom_oce        ! ocean space and time domain
   USE domtile
   USE eosbn2         ! equation of state                (eos_bn2 routine)
   USE phycst         ! physical constant
   USE in_out_manager  ! I/O manager
   USE zdfddm
   USE zdf_oce
   !
   USE lib_mpp        ! distribued memory computing library
   USE iom            ! I/O manager library
   USE fldread        ! type FLD_N
   USE timing         ! preformance summary
   USE sbc_oce , ONLY : nn_ice
   USE sbc_ice , ONLY : snwice_mass, snwice_mass_b

   IMPLICIT NONE
   PRIVATE

   INTERFACE dia_ar5_hst
      MODULE PROCEDURE dia_ar5_hst_2d, dia_ar5_hst_3d
   END INTERFACE

   PUBLIC   dia_ar5        ! routine called in step.F90 module
   PUBLIC   dia_ar5_init
   PUBLIC   dia_ar5_alloc  ! routine called in nemogcm.F90 module
   PUBLIC   dia_ar5_hst    ! heat/salt transport

   REAL(wp)                         ::   vol0         ! ocean volume (interior domain)
   REAL(wp)                         ::   area_tot     ! total ocean surface (interior domain)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:  ) ::   thick0       ! ocean thickness (interior domain)
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   sn0          ! initial salinity

   LOGICAL  :: l_ar5

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: diaar5.F90 14834 2021-05-11 09:24:44Z hadcv $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   FUNCTION dia_ar5_alloc()
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dia_ar5_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER :: dia_ar5_alloc
      !!----------------------------------------------------------------------
      !
      ALLOCATE( thick0(A2D(0)) , sn0(A2D(0),jpk), STAT=dia_ar5_alloc )
      !
      CALL mpp_sum ( 'diaar5', dia_ar5_alloc )
      IF( dia_ar5_alloc /= 0 )   CALL ctl_stop( 'STOP', 'dia_ar5_alloc: failed to allocate arrays' )
      !
   END FUNCTION dia_ar5_alloc


   SUBROUTINE dia_ar5( kt, Kmm )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dia_ar5  ***
      !!
      !! ** Purpose :   compute and output some AR5 diagnostics
      !!----------------------------------------------------------------------
      !
      INTEGER, INTENT( in ) ::   kt   ! ocean time-step index
      INTEGER, INTENT( in ) ::   Kmm  ! ocean time level index
      !
      INTEGER  ::   ji, jj, jk, iks, ikb                      ! dummy loop arguments
      REAL(wp) ::   zvol, zztmp
      REAL(wp) ::   zaw, zbw, zrw, ztf
      REAL(wp)       ::                   zarho,           ztemp,           zsal
      REAL(wp), SAVE :: zsvolssh, zssst, zsarho, zsarho2, zstemp, zstemp2, zssal, zssshice    ! Working sums
      !
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)       :: z2d, zarea_ssh   ! 2D workspace
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:)     :: z3d, zrhd        ! 3D workspace
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: ztsn             ! 5D workspace
      !!--------------------------------------------------------------------
      IF( ln_timing )   CALL timing_start('dia_ar5')

      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN  ! Do only for the first tile
         zsvolssh = 0._wp
         zstemp = 0._wp
         zssal = 0._wp
         zstemp2 = 0._wp
         zssst = 0._wp
         zsarho = 0._wp
         zsarho2 = 0._wp
         zssshice = 0._wp
      ENDIF
      !
      CALL iom_put( 'e2u'      , e2u  (:,:) )
      CALL iom_put( 'e1v'      , e1v  (:,:) )
      CALL iom_put( 'areacello', e1e2t(:,:) )
      !
      ALLOCATE( zarea_ssh(T2D(0)), z2d(T2D(0)) )
      ALLOCATE( z3d(T2D(0),jpk) )
      ALLOCATE( ztsn(T2D(0),jpk,jpts,jpt) )
      !
      DO_2D( 0, 0, 0, 0 )
         zarea_ssh(ji,jj) = e1e2t(ji,jj) * ssh(ji,jj,Kmm)
      END_2D
      !
      IF( iom_use( 'volcello' ) .OR. iom_use( 'masscello' )  ) THEN
         z3d(:,:,jpk) = 0._wp        ! ocean volume ; rhd is used as workspace
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            z3d(ji,jj,jk) = e1e2t(ji,jj) * e3t(ji,jj,jk,Kmm) * tmask(ji,jj,jk)
         END_3D
         CALL iom_put( 'volcello'  , z3d(:,:,:) )  ! WARNING not consistent with CMIP DR where volcello is at ca. 2000
         DO_3D( 0, 0, 0, 0, 1, jpk )
            z3d(ji,jj,jk) =  rho0 * e3t(ji,jj,jk,Kmm) * tmask(ji,jj,jk)
         END_3D
         CALL iom_put( 'masscello' , z3d(:,:,:) )   ! ocean mass
      ENDIF
      !
      IF( iom_use( 'e3tb' ) )  THEN    ! bottom layer thickness
         DO_2D( 0, 0, 0, 0 )
            ikb = mbkt(ji,jj)
            z2d(ji,jj) = e3t(ji,jj,ikb,Kmm)
         END_2D
         CALL iom_put( 'e3tb', z2d(:,:) )
      ENDIF
      !
      IF( iom_use( 'masstot' ) .OR. iom_use( 'temptot' )  .OR. iom_use( 'saltot' ) .OR. &
         & iom_use( 'voltot' ) .OR. iom_use( 'sshtot' )  .OR. iom_use( 'sshdyn' )  ) THEN
         !                                               ! Sum over tiles (local_sum) and then MPI domains (glob_sum)
         zsvolssh = local_sum( zarea_ssh(:,:), zsvolssh )
         IF( .NOT. l_istiled .OR. ntile == nijtile )  THEN  ! Do only for the last tile
            !                                         ! total volume of liquid seawater
            zztmp = glob_sum( 'diaar5', zsvolssh )
            zvol    = vol0 + zztmp
            IF( iom_use( 'voltot' ) .OR. iom_use( 'sshtot' )  .OR. iom_use( 'sshdyn' )  ) THEN
               CALL iom_put( 'voltot', zvol               )
               CALL iom_put( 'sshtot', zztmp / area_tot )
               CALL iom_put( 'sshdyn', ssh(:,:,Kmm) - (zztmp / area_tot) )
            ENDIF
         ENDIF
      ENDIF

      IF( iom_use( 'sshice' ) ) THEN
         !                                         ! total volume of ice+snow 
         IF( nn_ice == 0 ) THEN
            zztmp = 0._wp
         ELSE
            ztf = REAL(MOD( kt-1, nn_fsbc ), wp) / REAL(nn_fsbc, wp)
            DO_2D( 0, 0, 0, 0 )
               z2d(ji,jj) = ztf * snwice_mass(ji,jj) + (1._wp - ztf) * snwice_mass_b(ji,jj)
            END_2D
            !                                                                 ! Sum over tiles (local_sum) and then MPI domains (glob_sum)
            zssshice = local_sum( e1e2t(T2D(0)) * z2d(:,:) * r1_rho0, zssshice )
         ENDIF

         IF( .NOT. l_istiled .OR. ntile == nijtile )  THEN  ! Do only for the last tile
            IF( nn_ice /= 0 )   zztmp = glob_sum( 'diaar5', zssshice )
            CALL iom_put( 'sshice', zztmp / area_tot )
         ENDIF
         !
      ENDIF

      IF( iom_use( 'sshthster' ) ) THEN
         ALLOCATE( zrhd(T2D(0),jpk) )
         !
         DO_3D( 0, 0, 0, 0, 1, jpk )
            ztsn(ji,jj,jk,jp_tem,Kmm) = ts(ji,jj,jk,jp_tem,Kmm)
            ztsn(ji,jj,jk,jp_sal,Kmm) = sn0(ji,jj,jk)
         END_3D
         CALL eos( ztsn, Kmm, zrhd, kbnd=0 )                           ! now in situ density using initial salinity
         !
         z2d(:,:) = 0._wp                        ! no atmospheric surface pressure, levitating sea-ice
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            z2d(ji,jj) = z2d(ji,jj) + e3t(ji,jj,jk,Kmm) * zrhd(ji,jj,jk)
         END_3D
         IF( ln_linssh ) THEN
            IF( ln_isfcav ) THEN
               DO_2D( 0, 0, 0, 0 )
                  iks = mikt(ji,jj)
                  z2d(ji,jj) = z2d(ji,jj) + ssh(ji,jj,Kmm) * zrhd(ji,jj,iks) + riceload(ji,jj)
               END_2D
            ELSE
               DO_2D( 0, 0, 0, 0 )
                  z2d(ji,jj) = z2d(ji,jj) + ssh(ji,jj,Kmm) * zrhd(ji,jj,1)
               END_2D
            END IF
!!gm
!!gm   riceload should be added in both ln_linssh=T or F, no?
!!gm
         END IF
         DEALLOCATE( zrhd )
         !                                                     ! Sum over tiles (local_sum) and then MPI domains (glob_sum)
         zsarho = local_sum( e1e2t(T2D(0)) * z2d(:,:), zsarho)
         IF( .NOT. l_istiled .OR. ntile == nijtile )  THEN  ! Do only for the last tile
            zarho = glob_sum( 'diaar5', zsarho )
            zztmp = - zarho / area_tot
            CALL iom_put( 'sshthster', zztmp )
         ENDIF

      ENDIF
         
      IF( iom_use( 'botpres' ) .OR. iom_use( 'sshsteric' ) .OR. iom_use( 'masstot' ) ) THEN
         !                                         ! steric sea surface height
         z2d(:,:) = 0._wp                          ! no atmospheric surface pressure, levitating sea-ice
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            z2d(ji,jj) = z2d(ji,jj) + e3t(ji,jj,jk,Kmm) * rhd(ji,jj,jk)
         END_3D
         IF( ln_linssh ) THEN
            IF ( ln_isfcav ) THEN
               DO_2D( 0, 0, 0, 0 )
                  iks = mikt(ji,jj)
                  z2d(ji,jj) = z2d(ji,jj) + ssh(ji,jj,Kmm) * rhd(ji,jj,iks) + riceload(ji,jj)
               END_2D
            ELSE
               DO_2D( 0, 0, 0, 0 )
                  z2d(ji,jj) = z2d(ji,jj) + ssh(ji,jj,Kmm) * rhd(ji,jj,1)
               END_2D
            END IF
         END IF
         !                                                        ! Sum over tiles (local_sum) and then MPI domains (glob_sum)
         zsarho2 = local_sum( e1e2t(T2D(0)) * z2d(:,:), zsarho2)
         IF( .NOT. l_istiled .OR. ntile == nijtile )  THEN  ! Do only for the last tile
            zarho = glob_sum( 'diaar5', zsarho2 )
            zztmp = - zarho / area_tot
            CALL iom_put( 'sshsteric', zztmp )
            !
            zztmp = rho0 * ( zarho + zvol )
            CALL iom_put( 'masstot', zztmp )
         END IF
         !                                            ! ocean bottom pressure
         zztmp = rho0 * grav * 1.e-4_wp               ! recover pressure from pressure anomaly and cover to dbar = 1.e4 Pa
         DO_2D( 0, 0, 0, 0 )
            z2d(ji,jj) = zztmp * ( z2d(ji,jj) + ssh(ji,jj,Kmm) + thick0(ji,jj) )
         END_2D
         CALL iom_put( 'botpres', z2d )
         !
      ENDIF

      IF( iom_use( 'temptot' )  .OR. iom_use( 'saltot' ) ) THEN
         !                                         ! Mean density anomalie, temperature and salinity
         ztsn(:,:,:,:,Kmm) = 0._wp                 ! ztsn(:,:,1,jp_tem/sal) is used here as 2D Workspace for temperature & salinity
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            zztmp = e1e2t(ji,jj) * e3t(ji,jj,jk,Kmm)
            ztsn(ji,jj,1,jp_tem,Kmm) = ztsn(ji,jj,1,jp_tem,Kmm) + zztmp * ts(ji,jj,jk,jp_tem,Kmm)
            ztsn(ji,jj,1,jp_sal,Kmm) = ztsn(ji,jj,1,jp_sal,Kmm) + zztmp * ts(ji,jj,jk,jp_sal,Kmm)
         END_3D

         IF( ln_linssh ) THEN
            IF( ln_isfcav ) THEN
               DO_2D( 0, 0, 0, 0 )
                  iks = mikt(ji,jj)
                  ztsn(ji,jj,1,jp_tem,Kmm) = ztsn(ji,jj,1,jp_tem,Kmm) + zarea_ssh(ji,jj) * ts(ji,jj,iks,jp_tem,Kmm)
                  ztsn(ji,jj,1,jp_sal,Kmm) = ztsn(ji,jj,1,jp_sal,Kmm) + zarea_ssh(ji,jj) * ts(ji,jj,iks,jp_sal,Kmm)
               END_2D
            ELSE
               DO_2D( 0, 0, 0, 0 )
                  ztsn(ji,jj,1,jp_tem,Kmm) = ztsn(ji,jj,1,jp_tem,Kmm) + zarea_ssh(ji,jj) * ts(ji,jj,1,jp_tem,Kmm)
                  ztsn(ji,jj,1,jp_sal,Kmm) = ztsn(ji,jj,1,jp_sal,Kmm) + zarea_ssh(ji,jj) * ts(ji,jj,1,jp_sal,Kmm)
               END_2D
            ENDIF
         ENDIF
         !                                                  ! Sum over tiles (local_sum) and then MPI domains (glob_sum)
         zstemp = local_sum( ztsn(:,:,1,jp_tem,Kmm), zstemp )
         zssal = local_sum( ztsn(:,:,1,jp_sal,Kmm), zssal )
         IF( .NOT. l_istiled .OR. ntile == nijtile )  THEN  ! Do only for the last tile
            ztemp = glob_sum( 'diaar5', zstemp )
            zsal  = glob_sum( 'diaar5', zssal )
            !
            CALL iom_put( 'temptot', ztemp / zvol )
            CALL iom_put( 'saltot' , zsal  / zvol )
         ENDIF
         !
      ENDIF

      IF( ln_teos10 ) THEN        ! ! potential temperature (TEOS-10 case)
         IF( iom_use( 'toce_pot') .OR. iom_use( 'temptot_pot' ) .OR. iom_use( 'sst_pot' )  &
            &                     .OR. iom_use( 'ssttot' ) .OR.  iom_use( 'tosmint_pot' ) ) THEN
            !
            z3d(:,:,jpk) = 0._wp
            DO jk = 1, jpkm1
               CALL eos_pt_from_ct( ts(T2D(0),jk,jp_tem,Kmm), ts(T2D(0),jk,jp_sal,Kmm), z3d(:,:,jk), kbnd=0 )
            END DO
            !
            CALL iom_put( 'toce_pot', z3d(:,:,:) )  ! potential temperature (TEOS-10 case)
            CALL iom_put( 'sst_pot' , z3d(:,:,1) )  ! surface temperature
            !
            IF( iom_use( 'temptot_pot' ) ) THEN   ! Output potential temperature in case we use TEOS-10
               z2d(:,:) = 0._wp
               DO_3D( 0, 0, 0, 0, 1, jpkm1 )
                  z2d(ji,jj) = z2d(ji,jj) + e1e2t(ji,jj) * e3t(ji,jj,jk,Kmm) * z3d(ji,jj,jk)
               END_3D
               !                                      ! Sum over tiles (local_sum) and then MPI domains (glob_sum)
               zstemp2 = local_sum( z2d(:,:), zstemp2 )
               IF( .NOT. l_istiled .OR. ntile == nijtile ) THEN   ! Do only for the last tile
                  ztemp = glob_sum( 'diaar5', zstemp2 )
                  CALL iom_put( 'temptot_pot', ztemp / zvol )
               ENDIF
            ENDIF
            !
            IF( iom_use( 'ssttot' ) ) THEN   ! Output potential temperature in case we use TEOS-10
               !                                                     ! Sum over tiles (local_sum) and then MPI domains (glob_sum)
               zssst = local_sum( e1e2t(T2D(0)) * z3d(:,:,1), zssst )
               IF( .NOT. l_istiled .OR. ntile == nijtile ) THEN   ! Do only for the last tile
                  zztmp = glob_sum( 'diaar5',  zssst )
                  CALL iom_put( 'ssttot', zztmp / area_tot )
               ENDIF
            ENDIF
            !
            ! Vertical integral of temperature
            IF( iom_use( 'tosmint_pot') ) THEN
               z2d(:,:) = 0._wp
               DO_3D( 0, 0, 0, 0, 1, jpkm1 )
                  z2d(ji,jj) = z2d(ji,jj) + rho0 * e3t(ji,jj,jk,Kmm) *  z3d(ji,jj,jk)
               END_3D
               CALL iom_put( 'tosmint_pot', z2d(:,:) )
            ENDIF
         ENDIF
      ELSE
         !                                                                 ! Sum over tiles (local_sum) and then MPI domains (glob_sum)
         zssst = local_sum( e1e2t(T2D(0)) * ts(T2D(0),1,jp_tem,Kmm), zssst )
         IF( .NOT. l_istiled .OR. ntile == nijtile ) THEN                       ! Do only on the last tile
            IF( iom_use('ssttot') ) THEN   ! Output sst in case we use EOS-80
               zztmp  = glob_sum( 'diaar5', zssst )
               CALL iom_put('ssttot', zztmp / area_tot )
            ENDIF
         ENDIF
      ENDIF

      IF( iom_use( 'tnpeo' )) THEN
         ! Work done against stratification by vertical mixing
         ! Exclude points where rn2 is negative as convection kicks in here and
         ! work is not being done against stratification
         z2d(:,:) = 0._wp
         IF( ln_zdfddm ) THEN
            DO_3D( 0, 0, 0, 0, 2, jpk )
               IF( rn2(ji,jj,jk) > 0._wp ) THEN
                  zrw = ( gdept(ji,jj,jk,Kmm) - gdepw(ji,jj,jk,Kmm) ) / e3w(ji,jj,jk,Kmm)
                  !
                  zaw = rab_n(ji,jj,jk,jp_tem) * (1. - zrw) + rab_n(ji,jj,jk-1,jp_tem)* zrw
                  zbw = rab_n(ji,jj,jk,jp_sal) * (1. - zrw) + rab_n(ji,jj,jk-1,jp_sal)* zrw
                  !
                  z2d(ji, jj) = z2d(ji,jj)   &
                     &        -  grav * (  avt(ji,jj,jk) * zaw * (ts(ji,jj,jk-1,jp_tem,Kmm) - ts(ji,jj,jk,jp_tem,Kmm) )  &
                     &                   - avs(ji,jj,jk) * zbw * (ts(ji,jj,jk-1,jp_sal,Kmm) - ts(ji,jj,jk,jp_sal,Kmm) ) )
               ENDIF
            END_3D
         ELSE
            DO_3D( 0, 0, 0, 0, 1, jpk )
               z2d(ji,jj) = z2d(ji,jj) + avt(ji,jj,jk) * MIN(0._wp,rn2(ji,jj,jk)) * rho0 * e3w(ji,jj,jk,Kmm)
            END_3D
         ENDIF
         CALL iom_put( 'tnpeo', z2d(:,:) )
      ENDIF

      DEALLOCATE( z2d, zarea_ssh )
      DEALLOCATE( z3d )
      DEALLOCATE( ztsn )
      !
      IF( ln_timing )   CALL timing_stop('dia_ar5')
      !
   END SUBROUTINE dia_ar5


   SUBROUTINE dia_ar5_hst_2d( ktra, cptr, puflx, pvflx, ldfin )
      !!
      INTEGER,                  INTENT(in)  :: ktra         ! tracer index
      CHARACTER(len=3),         INTENT(in)  :: cptr         ! transport type 'adv'/'ldf'
      REAL(wp), DIMENSION(:,:), INTENT(in)  :: puflx, pvflx ! 2D u/v-flux of advection/diffusion
      LOGICAL,                  INTENT(in)  :: ldfin        ! last call or not?
      !!
      CALL dia_ar5_hst_t( ktra, cptr, puflx2d=puflx, pvflx2d=pvflx, ktuvflx=lbnd_ij(puflx), ldfin=ldfin )
   END SUBROUTINE dia_ar5_hst_2d


   SUBROUTINE dia_ar5_hst_3d( ktra, cptr, puflx, pvflx )
      !!
      INTEGER,                    INTENT(in)  :: ktra         ! tracer index
      CHARACTER(len=3),           INTENT(in)  :: cptr         ! transport type 'adv'/'ldf'
      REAL(wp), DIMENSION(:,:,:), INTENT(in)  :: puflx, pvflx ! 3D u/v-flux of advection/diffusion
      !!
      CALL dia_ar5_hst_t( ktra, cptr, puflx3d=puflx, pvflx3d=pvflx, ktuvflx=lbnd_ij(puflx), ldfin=.TRUE. )
   END SUBROUTINE dia_ar5_hst_3d


   SUBROUTINE dia_ar5_hst_t( ktra, cptr, puflx2d, pvflx2d, puflx3d, pvflx3d, ktuvflx, ldfin )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dia_ar5_hst ***
      !!----------------------------------------------------------------------
      !! Wrapper for heat transport calculations
      !! Called from all advection and/or diffusion routines
      !!----------------------------------------------------------------------
      INTEGER,  DIMENSION(2),                 INTENT(in)            :: ktuvflx
      INTEGER,                                INTENT(in)            :: ktra              ! tracer index
      CHARACTER(len=3),                       INTENT(in)            :: cptr              ! transport type  'adv'/'ldf'
      LOGICAL,                                INTENT(in)            :: ldfin             ! are diagnostics ready for XIOS?
      REAL(wp), DIMENSION(AB2D(ktuvflx)),     INTENT(in), OPTIONAL  :: puflx2d, pvflx2d  ! 2D u/v-flux of advection/diffusion
      REAL(wp), DIMENSION(AB2D(ktuvflx),JPK), INTENT(in), OPTIONAL  :: puflx3d, pvflx3d  ! 3D "  "
      !
      INTEGER ::  ji, jj, jk
      REAL(wp), DIMENSION(:,:), ALLOCATABLE, SAVE  :: zuflx, zvflx
      !!----------------------------------------------------------------------

      ! Flux in i-direction
      IF( .NOT. ALLOCATED(zuflx) ) THEN
         ALLOCATE( zuflx(T2D(0)) )
         zuflx(:,:) = 0._wp
      ENDIF

      IF( PRESENT(puflx2d) ) THEN
         DO_2D( 0, 0, 0, 0 )
            zuflx(ji,jj) = zuflx(ji,jj) + puflx2d(ji,jj)
         END_2D
      ELSE IF( PRESENT(puflx3d) ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            zuflx(ji,jj) = zuflx(ji,jj) + puflx3d(ji,jj,jk)
         END_3D
      ENDIF

      IF( ldfin ) THEN
         IF( cptr == 'adv' ) THEN
            IF( ktra == jp_tem ) CALL iom_put( 'uadv_heattr'  , rho0_rcp * zuflx(:,:) ) ! advective heat transport
            IF( ktra == jp_sal ) CALL iom_put( 'uadv_salttr'  , rho0     * zuflx(:,:) ) ! advective salt transport
         ELSE IF( cptr == 'ldf' ) THEN
            IF( ktra == jp_tem ) CALL iom_put( 'udiff_heattr' , rho0_rcp * zuflx(:,:) ) ! diffusive heat transport
            IF( ktra == jp_sal ) CALL iom_put( 'udiff_salttr' , rho0     * zuflx(:,:) ) ! diffusive salt transport
         ENDIF

         DEALLOCATE( zuflx )
      ENDIF

      ! Flux in j-direction
      IF( .NOT. ALLOCATED(zvflx) ) THEN
         ALLOCATE( zvflx(T2D(0)) )
         zvflx(:,:) = 0._wp
      ENDIF

      IF( PRESENT(pvflx2d) ) THEN
         DO_2D( 0, 0, 0, 0 )
            zvflx(ji,jj) = zvflx(ji,jj) + pvflx2d(ji,jj)
         END_2D
      ELSE IF( PRESENT(pvflx3d) ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            zvflx(ji,jj) = zvflx(ji,jj) + pvflx3d(ji,jj,jk)
         END_3D
      ENDIF

      IF( ldfin ) THEN
         IF( cptr == 'adv' ) THEN
            IF( ktra == jp_tem ) CALL iom_put( 'vadv_heattr'  , rho0_rcp * zvflx(:,:) ) ! advective heat transport
            IF( ktra == jp_sal ) CALL iom_put( 'vadv_salttr'  , rho0     * zvflx(:,:) ) ! advective salt transport
         ELSE IF( cptr == 'ldf' ) THEN
            IF( ktra == jp_tem ) CALL iom_put( 'vdiff_heattr' , rho0_rcp * zvflx(:,:) ) ! diffusive heat transport
            IF( ktra == jp_sal ) CALL iom_put( 'vdiff_salttr' , rho0     * zvflx(:,:) ) ! diffusive salt transport
         ENDIF

         DEALLOCATE( zvflx )
      ENDIF

   END SUBROUTINE dia_ar5_hst_t


   SUBROUTINE dia_ar5_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dia_ar5_init  ***
      !!
      !! ** Purpose :   initialization for AR5 diagnostic computation
      !!----------------------------------------------------------------------
      INTEGER  ::   inum
      INTEGER  ::   ik
      INTEGER  ::   ji, jj, jk  ! dummy loop indices
      REAL(wp) ::   zztmp
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:,:) ::   zsaldta   ! Jan/Dec levitus salinity
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)     ::   zvol0
      !
      !!----------------------------------------------------------------------
      !
      l_ar5 = .FALSE.
      IF(   iom_use( 'voltot'  ) .OR. iom_use( 'sshtot'    )  .OR. iom_use( 'sshdyn' )  .OR.  &
         &  iom_use( 'masstot' ) .OR. iom_use( 'temptot'   )  .OR. iom_use( 'saltot' ) .OR.  &
         &  iom_use( 'botpres' ) .OR. iom_use( 'sshthster' )  .OR. iom_use( 'sshsteric' ) .OR. &
         &  iom_use( 'uadv_heattr' ) .OR. iom_use( 'udiff_heattr' ) .OR. &
         &  iom_use( 'uadv_salttr' ) .OR. iom_use( 'udiff_salttr' ) .OR. &
         &  iom_use( 'vadv_heattr' ) .OR. iom_use( 'vdiff_heattr' ) .OR. &
         &  iom_use( 'vadv_salttr' ) .OR. iom_use( 'vdiff_salttr' ) .OR. &
         &  iom_use( 'rhop' ) .OR. iom_use( 'sshice' )  ) L_ar5 = .TRUE.

      IF( l_ar5 ) THEN
         !
         !                                      ! allocate dia_ar5 arrays
         IF( dia_ar5_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'dia_ar5_init : unable to allocate arrays' )

         area_tot  = glob_sum( 'diaar5', e1e2t(:,:) )

         ALLOCATE( zvol0(A2D(0)) )
         zvol0 (:,:) = 0._wp
         thick0(:,:) = 0._wp
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )   ! interpolation of salinity at the last ocean level (i.e. the partial step)
            zztmp = tmask(ji,jj,jk) * e3t_0(ji,jj,jk)
            zvol0 (ji,jj) = zvol0 (ji,jj) + zztmp * e1e2t(ji,jj)
            thick0(ji,jj) = thick0(ji,jj) + zztmp
         END_3D
         vol0 = glob_sum( 'diaar5', zvol0 )
         DEALLOCATE( zvol0 )

         IF( iom_use( 'sshthster' ) ) THEN
            ALLOCATE( zsaldta(A2D(0),jpk,jpts) )
            CALL iom_open ( 'sali_ref_clim_monthly', inum )
            CALL iom_get  ( inum, jpdom_global, 'vosaline' , zsaldta(:,:,:,1), 1  )
            CALL iom_get  ( inum, jpdom_global, 'vosaline' , zsaldta(:,:,:,2), 12 )
            CALL iom_close( inum )

            sn0(:,:,:) = 0.5_wp * ( zsaldta(:,:,:,1) + zsaldta(:,:,:,2) )
            sn0(:,:,:) = sn0(:,:,:) * tmask(A2D(0),:)
            IF( l_zps ) THEN                ! z-coord. partial steps
               DO_2D( 0, 0, 0, 0 )          ! interpolation of salinity at the last ocean level (i.e. the partial step)
                  ik = mbkt(ji,jj)
                  IF( ik > 1 ) THEN
                     zztmp = ( gdept_1d(ik) - gdept_0(ji,jj,ik) ) / ( gdept_1d(ik) - gdept_1d(ik-1) )
                     sn0(ji,jj,ik) = ( 1._wp - zztmp ) * sn0(ji,jj,ik) + zztmp * sn0(ji,jj,ik-1)
                  ENDIF
               END_2D
            ENDIF
            !
            DEALLOCATE( zsaldta )
         ENDIF
         !
      ENDIF
      !
   END SUBROUTINE dia_ar5_init

   !!======================================================================
END MODULE diaar5
