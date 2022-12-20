MODULE dynadv_cen2
   !!======================================================================
   !!                       ***  MODULE  dynadv  ***
   !! Ocean dynamics: Update the momentum trend with the flux form advection
   !!                 using a 2nd order centred scheme
   !!======================================================================
   !! History :  2.0  ! 2006-08  (G. Madec, S. Theetten)  Original code
   !!            3.2  ! 2009-07  (R. Benshila)  Suppression of rigid-lid option
   !!            4.5  ! 2022-06  (S. Techene, G, Madec) refactorization to reduce local memory usage
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

   SUBROUTINE dyn_adv_cen2( kt, Kmm, puu, pvv, Krhs, pau, pav, paw )
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
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpt), TARGET, INTENT(inout) ::   puu, pvv         ! ocean velocities and RHS of momentum equation
      REAL(wp), DIMENSION(:,:,:), OPTIONAL, TARGET, INTENT(in   ) ::   pau, pav, paw    ! advective velocity
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zzu, zzfu_kp1     ! local scalars
      REAL(wp) ::   zzv, zzfv_kp1     !   -      -
      REAL(wp), DIMENSION(T2D(1)) ::   zfu_t, zfu_f, zfu
      REAL(wp), DIMENSION(T2D(1)) ::   zfv_t, zfv_f, zfv
      REAL(wp), DIMENSION(T2D(1)) ::   zfu_uw, zfv_vw, zfw
      REAL(wp), DIMENSION(:,:,:) , POINTER ::   zpt_u, zpt_v, zpt_w
      REAL(wp), DIMENSION(:,:,:) , ALLOCATABLE ::   zu_trd, zv_trd
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
         ALLOCATE( zu_trd(A2D(0),jpkm1), zv_trd(A2D(0),jpkm1) )
         zu_trd(:,:,:) = puu(A2D(0),:,Krhs)
         zv_trd(:,:,:) = pvv(A2D(0),:,Krhs)
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
            zfu(ji,jj) = 0.25_wp * e2u(ji,jj) * e3u(ji,jj,jk,Kmm) * zpt_u(ji,jj,jk)
            zfv(ji,jj) = 0.25_wp * e1v(ji,jj) * e3v(ji,jj,jk,Kmm) * zpt_v(ji,jj,jk)
         END_2D
         DO_2D( 1, 0, 1, 0 )              ! horizontal momentum fluxes (at T- and F-point)
            zfu_t(ji+1,jj  ) = ( zfu(ji,jj) + zfu(ji+1,jj) ) * ( puu(ji,jj,jk,Kmm) + puu(ji+1,jj  ,jk,Kmm) )
            zfv_f(ji  ,jj  ) = ( zfv(ji,jj) + zfv(ji+1,jj) ) * ( puu(ji,jj,jk,Kmm) + puu(ji  ,jj+1,jk,Kmm) )
            zfu_f(ji  ,jj  ) = ( zfu(ji,jj) + zfu(ji,jj+1) ) * ( pvv(ji,jj,jk,Kmm) + pvv(ji+1,jj  ,jk,Kmm) )
            zfv_t(ji  ,jj+1) = ( zfv(ji,jj) + zfv(ji,jj+1) ) * ( pvv(ji,jj,jk,Kmm) + pvv(ji  ,jj+1,jk,Kmm) )
         END_2D
         DO_2D( 0, 0, 0, 0 )              ! divergence of horizontal momentum fluxes
            puu(ji,jj,jk,Krhs) = puu(ji,jj,jk,Krhs) - (  ( zfu_t(ji+1,jj) - zfu_t(ji,jj  ) )    &   ! add () for NP repro
               &                                       + ( zfv_f(ji  ,jj) - zfv_f(ji,jj-1) )  ) * r1_e1e2u(ji,jj)   &
               &                                    / e3u(ji,jj,jk,Kmm)
            pvv(ji,jj,jk,Krhs) = pvv(ji,jj,jk,Krhs) - (  ( zfu_f(ji,jj  ) - zfu_f(ji-1,jj) )    &   ! add () for NP repro
               &                                       + ( zfv_t(ji,jj+1) - zfv_t(ji  ,jj) )  ) * r1_e1e2v(ji,jj)   &
               &                                    / e3v(ji,jj,jk,Kmm)
         END_2D
      END DO
      !
      IF( l_trddyn ) THEN           ! trends: send trend to trddyn for diagnostic
         zu_trd(:,:,:) = puu(A2D(0),:,Krhs) - zu_trd(:,:,:)
         zv_trd(:,:,:) = pvv(A2D(0),:,Krhs) - zv_trd(:,:,:)
         CALL trd_dyn( zu_trd, zv_trd, jpdyn_keg, kt, Kmm )
         zu_trd(:,:,:) = puu(A2D(0),:,Krhs)
         zv_trd(:,:,:) = pvv(A2D(0),:,Krhs)
      ENDIF
      !
      !                          !==  Vertical advection  ==!
      !
      !                              ! surface vertical fluxes
      !
      IF( ln_linssh ) THEN                ! linear free surface: advection through the surface z=0
         DO_2D( 0, 0, 0, 0 )
            zfu_uw(ji,jj) = 0.5_wp * ( e1e2t(ji,jj) * zpt_w(ji,jj,1) + e1e2t(ji+1,jj) * zpt_w(ji+1,jj,1) ) * puu(ji,jj,1,Kmm)
            zfv_vw(ji,jj) = 0.5_wp * ( e1e2t(ji,jj) * zpt_w(ji,jj,1) + e1e2t(ji,jj+1) * zpt_w(ji,jj+1,1) ) * pvv(ji,jj,1,Kmm)
         END_2D
      ELSE                                ! non linear free: surface advective fluxes set to zero
         DO_2D( 0, 0, 0, 0 )
            zfu_uw(ji,jj) = 0._wp
            zfv_vw(ji,jj) = 0._wp
         END_2D
      ENDIF
      !
      DO jk = 1, jpk-2               !  divergence of advective fluxes
         !
         DO_2D( 0, 1, 0, 1 )                  ! 1/4 * Vertical transport at level k+1
            zfw(ji,jj) = 0.25_wp * e1e2t(ji,jj) * zpt_w(ji,jj,jk+1)
         END_2D
         DO_2D( 0, 0, 0, 0 )
            !                                       ! vertical flux at level k+1
            zzfu_kp1 = ( zfw(ji,jj) + zfw(ji+1,jj  ) ) * ( puu(ji,jj,jk+1,Kmm) + puu(ji,jj,jk,Kmm) )
            zzfv_kp1 = ( zfw(ji,jj) + zfw(ji  ,jj+1) ) * ( pvv(ji,jj,jk+1,Kmm) + pvv(ji,jj,jk,Kmm) )
            !                                       ! divergence of vertical momentum flux
            puu(ji,jj,jk,Krhs) = puu(ji,jj,jk,Krhs) - ( zfu_uw(ji,jj) - zzfu_kp1 ) * r1_e1e2u(ji,jj)   &
               &                                      / e3u(ji,jj,jk,Kmm)
            pvv(ji,jj,jk,Krhs) = pvv(ji,jj,jk,Krhs) - ( zfv_vw(ji,jj) - zzfv_kp1 ) * r1_e1e2v(ji,jj)   &
               &                                      / e3v(ji,jj,jk,Kmm)
            !                                       ! store vertical flux for next level calculation
            zfu_uw(ji,jj) = zzfu_kp1
            zfv_vw(ji,jj) = zzfv_kp1
         END_2D
      END DO
      !
      jk = jpkm1
      DO_2D( 0, 0, 0, 0 )
         puu(ji,jj,jk,Krhs) = puu(ji,jj,jk,Krhs) - zfu_uw(ji,jj) * r1_e1e2u(ji,jj)   &
            &                                      / e3u(ji,jj,jk,Kmm)
         pvv(ji,jj,jk,Krhs) = pvv(ji,jj,jk,Krhs) - zfv_vw(ji,jj) * r1_e1e2v(ji,jj)   &
            &                                      / e3v(ji,jj,jk,Kmm)
      END_2D
      !
      IF( l_trddyn ) THEN                 ! trends: send trend to trddyn for diagnostic
         zu_trd(:,:,:) = puu(A2D(0),:,Krhs) - zu_trd(:,:,:)
         zv_trd(:,:,:) = pvv(A2D(0),:,Krhs) - zv_trd(:,:,:)
         CALL trd_dyn( zu_trd, zv_trd, jpdyn_zad, kt, Kmm )
         DEALLOCATE( zu_trd, zv_trd )
      ENDIF
      !                                   ! Control print
      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=puu(:,:,:,Krhs), clinfo1=' cen2 adv - Ua: ', mask1=umask,   &
         &                                  tab3d_2=pvv(:,:,:,Krhs), clinfo2=           ' Va: ', mask2=vmask, clinfo3='dyn' )
      !
         !
   END SUBROUTINE dyn_adv_cen2

   !!==============================================================================
END MODULE dynadv_cen2
