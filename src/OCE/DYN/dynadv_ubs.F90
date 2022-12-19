MODULE dynadv_ubs
   !!======================================================================
   !!                       ***  MODULE  dynadv_ubs  ***
   !! Ocean dynamics: Update the momentum trend with the flux form advection
   !!                 trend using a 3rd order upstream biased scheme
   !!======================================================================
   !! History :  2.0  ! 2006-08  (R. Benshila, L. Debreu)  Original code
   !!            3.2  ! 2009-07  (R. Benshila)  Suppression of rigid-lid option
   !!            4.5  ! 2022-06  (S. Techene, G, Madec) refactorization to reduce local memory usage
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dyn_adv_ubs   : flux form momentum advection using    (ln_dynadv=T)
   !!                   an 3rd order Upstream Biased Scheme or Quick scheme
   !!                   combined with 2nd or 4th order finite differences 
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE trd_oce        ! trends: ocean variables
   USE trddyn         ! trend manager: dynamics
   !
   USE in_out_manager ! I/O manager
   USE prtctl         ! Print control
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   REAL(wp), PARAMETER :: gamma1 = 1._wp/3._wp  ! =1/4 quick      ; =1/3  3rd order UBS
   REAL(wp), PARAMETER :: gamma2 = 1._wp/32._wp ! =0   2nd order  ; =1/32 4th order centred

   PUBLIC   dyn_adv_ubs        ! routine called by dynadv.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: dynadv_ubs.F90 14419 2021-02-09 12:22:16Z techene $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dyn_adv_ubs( kt, Kbb, Kmm, puu, pvv, Krhs, pau, pav, paw )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_adv_ubs  ***
      !!
      !! ** Purpose :   Compute the now momentum advection trend in flux form
      !!              and the general trend of the momentum equation.
      !!
      !! ** Method  :   The scheme is the one implemeted in ROMS. It depends 
      !!      on two parameter gamma1 and gamma2. The former control the 
      !!      upstream baised part of the scheme and the later the centred 
      !!      part:     gamma1 = 0    pure centered  (no diffusive part)
      !!                       = 1/4  Quick scheme
      !!                       = 1/3  3rd order Upstream biased scheme
      !!                gamma2 = 0    2nd order finite differencing 
      !!                       = 1/32 4th order finite differencing
      !!      For stability reasons, the first term of the fluxes which cor-
      !!      responds to a second order centered scheme is evaluated using  
      !!      the now velocity (centered in time) while the second term which  
      !!      is the diffusive part of the scheme, is evaluated using the 
      !!      before velocity (forward in time). 
      !!      Default value (hard coded in the begining of the module) are 
      !!      gamma1=1/3 and gamma2=1/32.
      !!
      !!                In RK3 time stepping case, the optional arguments 
      !!      (pau,pav,paw) are present. They are used as advective velocity  
      !!      while the advected velocity remains (puu,pvv). 
      !!
      !! ** Action  :   (puu,pvv)(:,:,:,Krhs)   updated with the advective trend
      !!
      !! Reference : Shchepetkin & McWilliams, 2005, Ocean Modelling. 
      !!----------------------------------------------------------------------
      INTEGER                                     , INTENT(in   ) ::   kt , Kbb, Kmm, Krhs   ! ocean time-step and level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpt), TARGET, INTENT(inout) ::   puu, pvv              ! ocean velocities and RHS of momentum equation
      REAL(wp), DIMENSION(:,:,:), OPTIONAL, TARGET, INTENT(in   ) ::   pau, pav, paw         ! advective velocity
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zzu, zui, zfuj, zl_u, zzfu_kp1     ! local scalars
      REAL(wp) ::   zzv, zvj, zfvi, zl_v, zzfv_kp1     !   -      -
      REAL(wp), DIMENSION(T2D(2))   ::   zfu_t, zfu_f, zfu
      REAL(wp), DIMENSION(T2D(2))   ::   zfv_t, zfv_f, zfv
      REAL(wp), DIMENSION(T2D(2),2) ::   zlu_uu, zlu_uv
      REAL(wp), DIMENSION(T2D(2),2) ::   zlv_vv, zlv_vu
      REAL(wp), DIMENSION(:,:,:) , POINTER ::   zpt_u, zpt_v, zpt_w
      REAL(wp), DIMENSION(:,:,:) , ALLOCATABLE ::   zu_trd, zv_trd
      !!----------------------------------------------------------------------
      !
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == nit000 ) THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'dyn_adv_ubs : UBS flux form momentum advection'
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
         ENDIF
      ENDIF
      !
      IF( l_trddyn ) THEN           ! trends: send trend to trddyn for diagnostic  
         ALLOCATE( zu_trd(A2D(0),jpkm1), zv_trd(A2D(0),jpkm1) )
         zu_trd(A2D(0),:) = puu(A2D(0),:,Krhs)
         zv_trd(A2D(0),:) = pvv(A2D(0),:,Krhs)
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
      !                                      ! =========================== !
      DO jk = 1, jpkm1                       !  Laplacian of the velocity  !
         !                                   ! =========================== !
         !                                         ! horizontal volume fluxes
         DO_2D( 2, 2, 2, 2 )
            zfu(ji,jj) = e2u(ji,jj) * e3u(ji,jj,jk,Kmm) * zpt_u(ji,jj,jk)
            zfv(ji,jj) = e1v(ji,jj) * e3v(ji,jj,jk,Kmm) * zpt_v(ji,jj,jk)
         END_2D
         !            
         DO_2D( 1, 1, 1, 1 )                       ! laplacian
            zlu_uu(ji,jj,1) = ( ( puu (ji+1,jj  ,jk,Kbb) - puu (ji  ,jj  ,jk,Kbb) ) &                      ! add () for NP repro
               &              + ( puu (ji-1,jj  ,jk,Kbb) - puu (ji  ,jj  ,jk,Kbb) ) ) * umask(ji  ,jj  ,jk)
            zlv_vv(ji,jj,1) = ( ( pvv (ji  ,jj+1,jk,Kbb) - pvv (ji  ,jj  ,jk,Kbb) ) &                      ! add () for NP repro
               &              + ( pvv (ji  ,jj-1,jk,Kbb) - pvv (ji  ,jj  ,jk,Kbb) ) ) * vmask(ji  ,jj  ,jk)
            zlu_uv(ji,jj,1) = (    puu(ji  ,jj+1,jk,Kbb) - puu(ji  ,jj  ,jk,Kbb)    ) * fmask(ji  ,jj  ,jk)   &
               &            - (    puu(ji  ,jj  ,jk,Kbb) - puu(ji  ,jj-1,jk,Kbb)    ) * fmask(ji  ,jj-1,jk)
            zlv_vu(ji,jj,1) = (    pvv(ji+1,jj  ,jk,Kbb) - pvv(ji  ,jj  ,jk,Kbb)    ) * fmask(ji  ,jj  ,jk)   &
               &            - (    pvv(ji  ,jj  ,jk,Kbb) - pvv(ji-1,jj  ,jk,Kbb)    ) * fmask(ji-1,jj  ,jk)
            !
            zlu_uu(ji,jj,2) = ( ( zfu(ji+1,jj  ) - zfu(ji  ,jj  ) ) &                      ! add () for NP repro
               &              + ( zfu(ji-1,jj  ) - zfu(ji  ,jj  ) ) ) * umask(ji  ,jj  ,jk)
            zlv_vv(ji,jj,2) = ( ( zfv(ji  ,jj+1) - zfv(ji  ,jj  ) ) &                      ! add () for NP repro
               &              + ( zfv(ji  ,jj-1) - zfv(ji  ,jj  ) ) ) * vmask(ji  ,jj  ,jk)
            zlu_uv(ji,jj,2) = (   zfu(ji  ,jj+1) - zfu(ji  ,jj  )   ) * fmask(ji  ,jj  ,jk)             &
               &            - (   zfu(ji  ,jj  ) - zfu(ji  ,jj-1)   ) * fmask(ji  ,jj-1,jk)
            zlv_vu(ji,jj,2) = (   zfv(ji+1,jj  ) - zfv(ji  ,jj  )   ) * fmask(ji  ,jj  ,jk)             &
               &            - (   zfv(ji  ,jj  ) - zfv(ji-1,jj  )   ) * fmask(ji-1,jj  ,jk)
         END_2D
         !
         !                                      ! ====================== !
         !                                      !  Horizontal advection  !
         !                                      ! ====================== !
         !                                         ! horizontal volume fluxes
         DO_2D( 1, 1, 1, 1 )
            zfu(ji,jj) = 0.25_wp * zfu(ji,jj)
            zfv(ji,jj) = 0.25_wp * zfv(ji,jj)
         END_2D
         !
         DO_2D( 1, 0, 1, 0 )                       ! horizontal momentum fluxes at T- and F-point
            zui = ( puu(ji,jj,jk,Kmm) + puu(ji+1,jj  ,jk,Kmm) )
            zvj = ( pvv(ji,jj,jk,Kmm) + pvv(ji  ,jj+1,jk,Kmm) )
            !
            IF( zui > 0 ) THEN   ;   zl_u = zlu_uu(ji  ,jj,1)
            ELSE                 ;   zl_u = zlu_uu(ji+1,jj,1)
            ENDIF
            IF( zvj > 0 ) THEN   ;   zl_v = zlv_vv(ji,jj  ,1)
            ELSE                 ;   zl_v = zlv_vv(ji,jj+1,1)
            ENDIF
            !
            zfu_t(ji+1,jj  ) = (            (    zfu(ji,jj  ) +    zfu(ji+1,jj    ) )      &   ! add () for NP repro
               &                 - gamma2 * ( zlu_uu(ji,jj,2) + zlu_uu(ji+1,jj,  2) )  ) * ( zui - gamma1 * zl_u )
            zfv_t(ji  ,jj+1) = (            (    zfv(ji,jj  ) +    zfv(ji  ,jj+1  ) )      &   ! add () for NP repro
               &                 - gamma2 * ( zlv_vv(ji,jj,2) + zlv_vv(ji  ,jj+1,2) )  ) * ( zvj - gamma1 * zl_v )
            !
            zfuj = ( zfu(ji,jj) + zfu(ji  ,jj+1) )
            zfvi = ( zfv(ji,jj) + zfv(ji+1,jj  ) )
            IF( zfuj > 0 ) THEN   ;    zl_v = zlv_vu(ji  ,jj,1)
            ELSE                  ;    zl_v = zlv_vu(ji+1,jj,1)
            ENDIF
            IF( zfvi > 0 ) THEN   ;    zl_u = zlu_uv(ji,jj  ,1)
            ELSE                  ;    zl_u = zlu_uv(ji,jj+1,1)
            ENDIF
            !
            zfv_f(ji  ,jj  ) = ( zfvi - gamma2 * ( zlv_vu(ji,jj,2) + zlv_vu(ji+1,jj  ,2) )  )   &
               &             * ( ( puu(ji,jj,jk,Kmm) + puu(ji  ,jj+1,jk,Kmm) ) - gamma1 * zl_u )   ! add () for NP repro
            zfu_f(ji  ,jj  ) = ( zfuj - gamma2 * ( zlu_uv(ji,jj,2) + zlu_uv(ji  ,jj+1,2) )  )   &
               &             * ( ( pvv(ji,jj,jk,Kmm) + pvv(ji+1,jj  ,jk,Kmm) ) - gamma1 * zl_v )   ! add () for NP repro
         END_2D
         DO_2D( 0, 0, 0, 0 )                       ! divergence of horizontal momentum fluxes
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
         zu_trd(A2D(0),:) = puu(A2D(0),:,Krhs) - zu_trd(A2D(0),:)
         zv_trd(A2D(0),:) = pvv(A2D(0),:,Krhs) - zv_trd(A2D(0),:)
         CALL trd_dyn( zu_trd, zv_trd, jpdyn_keg, kt, Kmm )
         zu_trd(A2D(0),:) = puu(A2D(0),:,Krhs)
         zv_trd(A2D(0),:) = pvv(A2D(0),:,Krhs)
      ENDIF
      !                                      ! ==================== !
      !                                      !  Vertical advection  !
      !                                      ! ==================== !
      !
#define zfu_uw   zfu_t
#define zfv_vw   zfv_t
#define zfw      zfu
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
            !                                 ! vertical flux at level k+1
            zzfu_kp1 = ( zfw(ji,jj) + zfw(ji+1,jj  ) ) * ( puu(ji,jj,jk+1,Kmm) + puu(ji,jj,jk,Kmm) )
            zzfv_kp1 = ( zfw(ji,jj) + zfw(ji  ,jj+1) ) * ( pvv(ji,jj,jk+1,Kmm) + pvv(ji,jj,jk,Kmm) )
            !                                 ! divergence of vertical momentum flux
            puu(ji,jj,jk,Krhs) = puu(ji,jj,jk,Krhs) - ( zfu_uw(ji,jj) - zzfu_kp1 ) * r1_e1e2u(ji,jj) / e3u(ji,jj,jk,Kmm)
            pvv(ji,jj,jk,Krhs) = pvv(ji,jj,jk,Krhs) - ( zfv_vw(ji,jj) - zzfv_kp1 ) * r1_e1e2v(ji,jj) / e3v(ji,jj,jk,Kmm) 
            !                                 ! store vertical flux for next level calculation
            zfu_uw(ji,jj) = zzfu_kp1
            zfv_vw(ji,jj) = zzfv_kp1
         END_2D
      END DO
      !
      jk = jpkm1                              ! compute last level (zzfu_kp1 = 0)
      DO_2D( 0, 0, 0, 0 )
         puu(ji,jj,jk,Krhs) = puu(ji,jj,jk,Krhs) - zfu_uw(ji,jj) * r1_e1e2u(ji,jj) / e3u(ji,jj,jk,Kmm)
         pvv(ji,jj,jk,Krhs) = pvv(ji,jj,jk,Krhs) - zfv_vw(ji,jj) * r1_e1e2v(ji,jj) / e3v(ji,jj,jk,Kmm) 
      END_2D
      !
      IF( l_trddyn ) THEN                     ! trends: send trend to trddyn for diagnostic
         zu_trd(A2D(0),:) = puu(A2D(0),:,Krhs) - zu_trd(A2D(0),:)
         zv_trd(A2D(0),:) = pvv(A2D(0),:,Krhs) - zv_trd(A2D(0),:)
         CALL trd_dyn( zu_trd, zv_trd, jpdyn_zad, kt, Kmm )
         DEALLOCATE( zu_trd, zv_trd )
      ENDIF
      !
#undef zfu_uw
#undef zfv_vw
#undef zfw
      !                                         ! Control print
      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=puu(:,:,:,Krhs), clinfo1=' ubs2 adv - Ua: ', mask1=umask,   &
         &                                  tab3d_2=pvv(:,:,:,Krhs), clinfo2=           ' Va: ', mask2=vmask, clinfo3='dyn' )
      !
   END SUBROUTINE dyn_adv_ubs

   !!==============================================================================
END MODULE dynadv_ubs
