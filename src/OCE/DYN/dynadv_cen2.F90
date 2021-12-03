MODULE dynadv_cen2
   !!======================================================================
   !!                       ***  MODULE  dynadv  ***
   !! Ocean dynamics: Update the momentum trend with the flux form advection
   !!                 using a 2nd order centred scheme
   !!======================================================================
   !! History :  2.0  ! 2006-08  (G. Madec, S. Theetten)  Original code
   !!            3.2  ! 2009-07  (R. Benshila)  Suppression of rigid-lid option
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dyn_adv_cen2  : flux form momentum advection (ln_dynadv_cen2=T) using a 2nd order centred scheme  
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE trd_oce        ! trends: ocean variables
   USE trddyn         ! trend manager: dynamics
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE prtctl         ! Print control

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dyn_adv_cen2   ! routine called by step.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: dynadv_cen2.F90 14419 2021-02-09 12:22:16Z techene $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dyn_adv_cen2( kt, Kmm, puu, pvv, Krhs, pau, pav, paw, no_zad )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_adv_cen2  ***
      !!
      !! ** Purpose :   Compute the momentum advection trend in flux form
      !!              and the general trend of the momentum equation.
      !!
      !! ** Method  :   Trend evaluated with a 2nd order centered scheme 
      !!              using fields at Kmm time-level.
      !!                In RK3 time stepping case, the optional arguments (pau,pav,paw) 
      !!              are present. They are used as advective velocity while 
      !!              the advected velocity remains (puu,pvv). 
      !!
      !! ** Action  :   (puu,pvv)(:,:,:,Krhs)   updated with the advective trend
      !!----------------------------------------------------------------------
      INTEGER                                     , INTENT(in   ) ::   kt , Kmm, Krhs   ! ocean time-step and level indices
      INTEGER                   , OPTIONAL        , INTENT(in   ) ::   no_zad                ! no vertical advection computation
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpt), TARGET, INTENT(inout) ::   puu, pvv         ! ocean velocities and RHS of momentum equation
      REAL(wp), DIMENSION(:,:,:), OPTIONAL, TARGET, INTENT(in   ) ::   pau, pav, paw    ! advective velocity
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zzu, zzv     ! local scalars
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) ::   zfu_t, zfu_f, zfu_uw, zfu
      REAL(wp), DIMENSION(A2D(nn_hls),jpk) ::   zfv_t, zfv_f, zfv_vw, zfv, zfw
      REAL(wp), DIMENSION(:,:,:) , POINTER ::   zpt_u, zpt_v, zpt_w
      !!----------------------------------------------------------------------
      !
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == nit000 .AND. lwp ) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'dyn_adv_cen2 : 2nd order flux form momentum advection'
            WRITE(numout,*) '~~~~~~~~~~~~'
         ENDIF
      ENDIF
      !
      IF( l_trddyn ) THEN           ! trends: store the input trends
         zfu_uw(:,:,:) = puu(:,:,:,Krhs)
         zfv_vw(:,:,:) = pvv(:,:,:,Krhs)
      ENDIF
      !
      IF( PRESENT( pau ) ) THEN     ! RK3: advective velocity (pau,pav,paw) /= advected velocity (puu,pvv,ww)
         zpt_u => pau(:,:,:)
         zpt_v => pav(:,:,:)
         zpt_w => paw(:,:,:)
      ELSE                          ! MLF: advective velocity = (puu,pvv,ww)
         zpt_u => puu(:,:,:,Kmm)
         zpt_v => pvv(:,:,:,Kmm)
         zpt_w => ww (:,:,:    )
      ENDIF
      !
      !                             !==  Horizontal advection  ==!
      !
      DO jk = 1, jpkm1                    ! horizontal transport
         DO_2D( 1, 1, 1, 1 )
            zfu(ji,jj,jk) = 0.25_wp * e2u(ji,jj) * e3u(ji,jj,jk,Kmm) * zpt_u(ji,jj,jk)
            zfv(ji,jj,jk) = 0.25_wp * e1v(ji,jj) * e3v(ji,jj,jk,Kmm) * zpt_v(ji,jj,jk)
         END_2D
         DO_2D( 1, 0, 1, 0 )              ! horizontal momentum fluxes (at T- and F-point)
            zfu_t(ji+1,jj  ,jk) = ( zfu(ji,jj,jk) + zfu(ji+1,jj,jk) ) * ( puu(ji,jj,jk,Kmm) + puu(ji+1,jj  ,jk,Kmm) )
            zfv_f(ji  ,jj  ,jk) = ( zfv(ji,jj,jk) + zfv(ji+1,jj,jk) ) * ( puu(ji,jj,jk,Kmm) + puu(ji  ,jj+1,jk,Kmm) )
            zfu_f(ji  ,jj  ,jk) = ( zfu(ji,jj,jk) + zfu(ji,jj+1,jk) ) * ( pvv(ji,jj,jk,Kmm) + pvv(ji+1,jj  ,jk,Kmm) )
            zfv_t(ji  ,jj+1,jk) = ( zfv(ji,jj,jk) + zfv(ji,jj+1,jk) ) * ( pvv(ji,jj,jk,Kmm) + pvv(ji  ,jj+1,jk,Kmm) )
         END_2D
         DO_2D( 0, 0, 0, 0 )              ! divergence of horizontal momentum fluxes
            puu(ji,jj,jk,Krhs) = puu(ji,jj,jk,Krhs) - (  zfu_t(ji+1,jj,jk) - zfu_t(ji,jj  ,jk)    &
               &                                       + zfv_f(ji  ,jj,jk) - zfv_f(ji,jj-1,jk)  ) * r1_e1e2u(ji,jj)   &
               &                                    / e3u(ji,jj,jk,Kmm)
            pvv(ji,jj,jk,Krhs) = pvv(ji,jj,jk,Krhs) - (  zfu_f(ji,jj  ,jk) - zfu_f(ji-1,jj,jk)    &
               &                                       + zfv_t(ji,jj+1,jk) - zfv_t(ji  ,jj,jk)  ) * r1_e1e2v(ji,jj)   &
               &                                    / e3v(ji,jj,jk,Kmm)
         END_2D
      END DO
      !
      IF( l_trddyn ) THEN           ! trends: send trend to trddyn for diagnostic
         zfu_uw(:,:,:) = puu(:,:,:,Krhs) - zfu_uw(:,:,:)
         zfv_vw(:,:,:) = pvv(:,:,:,Krhs) - zfv_vw(:,:,:)
         CALL trd_dyn( zfu_uw, zfv_vw, jpdyn_keg, kt, Kmm )
         zfu_t(:,:,:) = puu(:,:,:,Krhs)
         zfv_t(:,:,:) = pvv(:,:,:,Krhs)
      ENDIF
      !
      IF( PRESENT( no_zad ) ) THEN  !==  No vertical advection  ==!   (except if linear free surface)
         !                               ==
         IF( ln_linssh ) THEN                ! linear free surface: advection through the surface z=0
            DO_2D( 0, 0, 0, 0 )
               zzu = 0.5_wp * ( e1e2t(ji,jj) * zpt_w(ji,jj,1) + e1e2t(ji+1,jj) * zpt_w(ji+1,jj,1) ) * puu(ji,jj,1,Kmm)
               zzv = 0.5_wp * ( e1e2t(ji,jj) * zpt_w(ji,jj,1) + e1e2t(ji,jj+1) * zpt_w(ji,jj+1,1) ) * pvv(ji,jj,1,Kmm)
               puu(ji,jj,1,Krhs) = puu(ji,jj,1,Krhs) - zzu * r1_e1e2u(ji,jj)   &
                  &                                        / e3u(ji,jj,1,Kmm)
               pvv(ji,jj,1,Krhs) = pvv(ji,jj,1,Krhs) - zzv * r1_e1e2v(ji,jj)   &
                  &                                        / e3v(ji,jj,1,Kmm)
            END_2D
         ENDIF
         !
      ELSE                          !==  Vertical advection  ==!
         !
         DO_2D( 0, 0, 0, 0 )                 ! surface/bottom advective fluxes set to zero
            zfu_uw(ji,jj,jpk) = 0._wp   ;   zfv_vw(ji,jj,jpk) = 0._wp
            zfu_uw(ji,jj, 1 ) = 0._wp   ;   zfv_vw(ji,jj, 1 ) = 0._wp
         END_2D
         IF( ln_linssh ) THEN                ! linear free surface: advection through the surface z=0
            DO_2D( 0, 0, 0, 0 )
               zfu_uw(ji,jj,1) = 0.5_wp * ( e1e2t(ji,jj) * zpt_w(ji,jj,1) + e1e2t(ji+1,jj) * zpt_w(ji+1,jj,1) ) * puu(ji,jj,1,Kmm)
               zfv_vw(ji,jj,1) = 0.5_wp * ( e1e2t(ji,jj) * zpt_w(ji,jj,1) + e1e2t(ji,jj+1) * zpt_w(ji,jj+1,1) ) * pvv(ji,jj,1,Kmm)
            END_2D
         ENDIF
         DO jk = 2, jpkm1                    ! interior advective fluxes
            DO_2D( 0, 1, 0, 1 )                  ! 1/4 * Vertical transport
               zfw(ji,jj,jk) = 0.25_wp * e1e2t(ji,jj) * zpt_w(ji,jj,jk)
            END_2D
            DO_2D( 0, 0, 0, 0 )
               zfu_uw(ji,jj,jk) = ( zfw(ji,jj,jk) + zfw(ji+1,jj  ,jk) ) * ( puu(ji,jj,jk,Kmm) + puu(ji,jj,jk-1,Kmm) )
               zfv_vw(ji,jj,jk) = ( zfw(ji,jj,jk) + zfw(ji  ,jj+1,jk) ) * ( pvv(ji,jj,jk,Kmm) + pvv(ji,jj,jk-1,Kmm) )
            END_2D
         END DO
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )       ! divergence of vertical momentum flux divergence
            puu(ji,jj,jk,Krhs) = puu(ji,jj,jk,Krhs) - ( zfu_uw(ji,jj,jk) - zfu_uw(ji,jj,jk+1) ) * r1_e1e2u(ji,jj)   &
               &                                      / e3u(ji,jj,jk,Kmm)
            pvv(ji,jj,jk,Krhs) = pvv(ji,jj,jk,Krhs) - ( zfv_vw(ji,jj,jk) - zfv_vw(ji,jj,jk+1) ) * r1_e1e2v(ji,jj)   &
               &                                      / e3v(ji,jj,jk,Kmm)
         END_3D
         !
         IF( l_trddyn ) THEN                 ! trends: send trend to trddyn for diagnostic
            zfu_t(:,:,:) = puu(:,:,:,Krhs) - zfu_t(:,:,:)
            zfv_t(:,:,:) = pvv(:,:,:,Krhs) - zfv_t(:,:,:)
            CALL trd_dyn( zfu_t, zfv_t, jpdyn_zad, kt, Kmm )
         ENDIF
         !                                   ! Control print
         IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=puu(:,:,:,Krhs), clinfo1=' cen2 adv - Ua: ', mask1=umask,   &
            &                                  tab3d_2=pvv(:,:,:,Krhs), clinfo2=           ' Va: ', mask2=vmask, clinfo3='dyn' )
         !
      ENDIF
      !
   END SUBROUTINE dyn_adv_cen2

   !!==============================================================================
END MODULE dynadv_cen2
