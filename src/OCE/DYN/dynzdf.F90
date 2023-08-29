MODULE dynzdf
   !!==============================================================================
   !!                 ***  MODULE  dynzdf  ***
   !! Ocean dynamics :  vertical component of the momentum mixing trend
   !!==============================================================================
   !! History :  1.0  !  2005-11  (G. Madec)  Original code
   !!            3.3  !  2010-10  (C. Ethe, G. Madec) reorganisation of initialisation phase
   !!            4.0  !  2017-06  (G. Madec) remove the explicit time-stepping option + avm at t-point
   !!            4.5  !  2022-06  (S. Techene, G, Madec) refactorization to reduce local memory usage
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dyn_zdf       : compute the after velocity through implicit calculation of vertical mixing
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers variables
   USE phycst         ! physical constants
   USE dom_oce        ! ocean space and time domain variables 
   USE sbc_oce        ! surface boundary condition: ocean
   USE zdf_oce        ! ocean vertical physics variables
   USE zdfdrg         ! vertical physics: top/bottom drag coef.
   USE dynadv    ,ONLY: ln_dynadv_vec    ! dynamics: advection form
   USE dynldf_iso,ONLY: akzu, akzv       ! dynamics: vertical component of rotated lateral mixing 
   USE ldfdyn         ! lateral diffusion: eddy viscosity coef. and type of operator
   USE trd_oce        ! trends: ocean variables
   USE trddyn         ! trend manager: dynamics
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE prtctl         ! Print control
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dyn_zdf   !  routine called by step.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: dynzdf.F90 14547 2021-02-25 17:07:15Z techene $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
   
   SUBROUTINE dyn_zdf( kt, Kbb, Kmm, Krhs, puu, pvv, Kaa )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_zdf  ***
      !!
      !! ** Purpose :   compute the trend due to the vert. momentum diffusion
      !!              together with the Leap-Frog time stepping using an 
      !!              implicit scheme.
      !!
      !! ** Method  :  - Leap-Frog time stepping on all trends but the vertical mixing
      !!         u(after) =         u(before) + 2*dt *       u(rhs)                vector form or linear free surf.
      !!         u(after) = ( e3u_b*u(before) + 2*dt * e3u_n*u(rhs) ) / e3u_after   otherwise
      !!               - update the after velocity with the implicit vertical mixing.
      !!      This requires to solver the following system: 
      !!         u(after) = u(after) + 1/e3u_after  dk+1[ mi(avm) / e3uw_after dk[ua] ]
      !!      with the following surface/top/bottom boundary condition:
      !!      surface: wind stress input (averaged over kt-1/2 & kt+1/2)
      !!      top & bottom : top stress (iceshelf-ocean) & bottom stress (cf zdfdrg.F90)
      !!
      !! ** Action :   (puu(:,:,:,Kaa),pvv(:,:,:,Kaa))   after velocity 
      !!---------------------------------------------------------------------
      INTEGER                             , INTENT( in )  ::  kt                  ! ocean time-step index
      INTEGER                             , INTENT( in )  ::  Kbb, Kmm, Krhs, Kaa ! ocean time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpt), INTENT(inout) ::  puu, pvv            ! ocean velocities and RHS of momentum equation
      !
      INTEGER  ::   ji, jj, jk           ! dummy loop indices
      INTEGER  ::   iku, ikv             ! local integers
      REAL(wp) ::   zzwi, ze3ua, zDt_2   ! local scalars
      REAL(wp) ::   zzws, ze3va          !   -      -
      REAL(wp) ::   z1_e3ua, z1_e3va     !   -      -
      REAL(wp) ::   zWu , zWv            !   -      -
      REAL(wp) ::   zWui, zWvi           !   -      -
      REAL(wp) ::   zWus, zWvs           !   -      -
      REAL(wp), DIMENSION(T1Di(0),jpk)        ::   zwi, zwd, zws  ! 2D workspace
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   ztrdu, ztrdv   !  -      -
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('dyn_zdf')
      !
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == nit000 ) THEN       !* initialization
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'dyn_zdf_imp : vertical momentum diffusion implicit operator'
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~~ '
         ENDIF
      ENDIF
      !
      zDt_2 = rDt * 0.5_wp
      !
      !                             !* explicit top/bottom drag case
      IF( .NOT.ln_drgimp )   CALL zdf_drg_exp( kt, Kmm, puu(:,:,:,Kbb), pvv(:,:,:,Kbb), puu(:,:,:,Krhs), pvv(:,:,:,Krhs) )  ! add top/bottom friction trend to (puu(Kaa),pvv(Kaa))
      !
      !
      IF( l_trddyn )   THEN         !* temporary save of ta and sa trends
         ALLOCATE( ztrdu(jpi,jpj,jpk), ztrdv(jpi,jpj,jpk) ) 
         ztrdu(:,:,:) = puu(:,:,:,Krhs)
         ztrdv(:,:,:) = pvv(:,:,:,Krhs)
      ENDIF
      !
      !                                               ! ================= !
      DO_1Dj( 0, 0 )                                  !  i-k slices loop  !
         !                                            ! ================= !
         !
         !              !==  RHS: Leap-Frog time stepping on all trends but the vertical mixing  ==!   (put in puu(:,:,:,Kaa),pvv(:,:,:,Kaa))
         !
         !                    ! time stepping except vertical diffusion
         IF( ln_dynadv_vec .OR. ln_linssh ) THEN   ! applied on velocity
            DO_2Dik( 0, 0,    1, jpkm1, 1 )
               puu(ji,jj,jk,Kaa) = ( puu(ji,jj,jk,Kbb) + rDt * puu(ji,jj,jk,Krhs) ) * umask(ji,jj,jk)
               pvv(ji,jj,jk,Kaa) = ( pvv(ji,jj,jk,Kbb) + rDt * pvv(ji,jj,jk,Krhs) ) * vmask(ji,jj,jk)
            END_2D
         ELSE                                      ! applied on thickness weighted velocity
            DO_2Dik( 0, 0,    1, jpkm1, 1 )
               puu(ji,jj,jk,Kaa) = (         e3u(ji,jj,jk,Kbb) * puu(ji,jj,jk,Kbb )  &
                  &                  + rDt * e3u(ji,jj,jk,Kmm) * puu(ji,jj,jk,Krhs)  ) &
                  &                        / e3u(ji,jj,jk,Kaa) * umask(ji,jj,jk)
               pvv(ji,jj,jk,Kaa) = (         e3v(ji,jj,jk,Kbb) * pvv(ji,jj,jk,Kbb )  &
                  &                  + rDt * e3v(ji,jj,jk,Kmm) * pvv(ji,jj,jk,Krhs)  ) &
                  &                        / e3v(ji,jj,jk,Kaa) * vmask(ji,jj,jk)
            END_2D
         ENDIF
         !                    ! add top/bottom friction
         !     With split-explicit free surface, barotropic stress is treated explicitly Update velocities at the bottom.
         !     J. Chanut: The bottom stress is computed considering after barotropic velocities, which does
         !                not lead to the effective stress seen over the whole barotropic loop.
         !     G. Madec : in linear free surface, e3u(:,:,:,Kaa) = e3u(:,:,:,Kmm) = e3u_0, so systematic use of e3u(:,:,:,Kaa)
         IF( ln_drgimp .AND. ln_dynspg_ts ) THEN
            DO_2Dik( 0, 0,     1, jpkm1, 1 )      ! remove barotropic velocities
               puu(ji,jj,jk,Kaa) = ( puu(ji,jj,jk,Kaa) - uu_b(ji,jj,Kaa) ) * umask(ji,jj,jk)
               pvv(ji,jj,jk,Kaa) = ( pvv(ji,jj,jk,Kaa) - vv_b(ji,jj,Kaa) ) * vmask(ji,jj,jk)
            END_2D
            DO_1Di( 0, 0 )      ! Add bottom/top stress due to barotropic component only
               iku = mbku(ji,jj)         ! ocean bottom level at u- and v-points
               ikv = mbkv(ji,jj)         ! (deepest ocean u- and v-points)
               puu(ji,jj,iku,Kaa) = puu(ji,jj,iku,Kaa) + zDt_2 * ( rCdU_bot(ji+1,jj)+rCdU_bot(ji,jj) ) * uu_b(ji,jj,Kaa)   &
                  &                                            / e3u(ji,jj,iku,Kaa)
               pvv(ji,jj,ikv,Kaa) = pvv(ji,jj,ikv,Kaa) + zDt_2 * ( rCdU_bot(ji,jj+1)+rCdU_bot(ji,jj) ) * vv_b(ji,jj,Kaa)   &
                  &                                            / e3v(ji,jj,ikv,Kaa)
            END_1D
            IF( ln_isfcav.OR.ln_drgice_imp ) THEN    ! Ocean cavities (ISF)
               DO_1Di( 0, 0 )
                  iku = miku(ji,jj)         ! top ocean level at u- and v-points
                  ikv = mikv(ji,jj)         ! (first wet ocean u- and v-points)
                  puu(ji,jj,iku,Kaa) = puu(ji,jj,iku,Kaa) + zDt_2 * ( rCdU_top(ji+1,jj)+rCdU_top(ji,jj) ) * uu_b(ji,jj,Kaa)   &
                     &                                            / e3u(ji,jj,iku,Kaa)
                  pvv(ji,jj,ikv,Kaa) = pvv(ji,jj,ikv,Kaa) + zDt_2 * ( rCdU_top(ji,jj+1)+rCdU_top(ji,jj) ) * vv_b(ji,jj,Kaa)   &
                     &                                            / e3v(ji,jj,ikv,Kaa)
               END_1D
            END IF
         ENDIF
         !
         !              !==  Vertical diffusion on u  ==!
         !
         !
         !                    !* Matrix construction
         IF( ln_zad_Aimp ) THEN      !- including terms associated with partly implicit vertical advection
            SELECT CASE( nldf_dyn )
            CASE( np_lap_i )           ! rotated lateral mixing: add its vertical mixing (akzu)
               DO_2Dik( 0, 0,   1, jpkm1, 1 )
                  zzwi = - zDt_2 * ( ( avm(ji+1,jj,jk  ) + avm(ji,jj,jk  ) ) + akzu(ji,jj,jk  ) )   &   ! add () for NP repro
                     &           / ( e3u(ji,jj,jk,Kaa) * e3uw(ji,jj,jk  ,Kmm) ) * wumask(ji,jj,jk  )
                  zzws = - zDt_2 * ( ( avm(ji+1,jj,jk+1) + avm(ji,jj,jk+1) ) + akzu(ji,jj,jk+1) )   &   ! add () for NP repro
                     &           / ( e3u(ji,jj,jk,Kaa) * e3uw(ji,jj,jk+1,Kmm) ) * wumask(ji,jj,jk+1)
                  z1_e3ua =  1._wp  / e3u(ji,jj,jk,Kaa)   ! after scale factor at U-point
                  zWui = ( wi(ji,jj,jk  ) + wi(ji+1,jj,jk  ) ) * z1_e3ua
                  zWus = ( wi(ji,jj,jk+1) + wi(ji+1,jj,jk+1) ) * z1_e3ua
                  zwi(ji,jk) = zzwi + zDt_2 * MIN( zWui, 0._wp )
                  zws(ji,jk) = zzws - zDt_2 * MAX( zWus, 0._wp )
                  zwd(ji,jk) = 1._wp - zzwi - zzws + zDt_2 * ( MAX( zWui, 0._wp ) - MIN( zWus, 0._wp ) )
               END_2D
            CASE DEFAULT               ! iso-level lateral mixing
               DO_2Dik( 0, 0,   1, jpkm1, 1 )
                  zzwi = - zDt_2 * ( avm(ji+1,jj,jk  ) + avm(ji,jj,jk  ) )   &
                     &           / ( e3u(ji,jj,jk,Kaa) * e3uw(ji,jj,jk  ,Kmm) ) * wumask(ji,jj,jk  )
                  zzws = - zDt_2 * ( avm(ji+1,jj,jk+1) + avm(ji,jj,jk+1) )   &
                     &           / ( e3u(ji,jj,jk,Kaa) * e3uw(ji,jj,jk+1,Kmm) ) * wumask(ji,jj,jk+1)
                  z1_e3ua =  1._wp  / e3u(ji,jj,jk,Kaa)   ! after scale factor at U-point
                  zWui = ( wi(ji,jj,jk  ) + wi(ji+1,jj,jk  ) ) * z1_e3ua
                  zWus = ( wi(ji,jj,jk+1) + wi(ji+1,jj,jk+1) ) * z1_e3ua
                  zwi(ji,jk) = zzwi + zDt_2 * MIN( zWui, 0._wp )
                  zws(ji,jk) = zzws - zDt_2 * MAX( zWus, 0._wp )
                  zwd(ji,jk) = 1._wp - zzwi - zzws + zDt_2 * ( MAX( zWui, 0._wp ) - MIN( zWus, 0._wp ) )
               END_2D
            END SELECT
            !
            zwi(:,1) = 0._wp
            DO_1Di( 0, 0 )     !* Surface boundary conditions
               zwi(ji,1) = 0._wp
               zzws = - zDt_2 * ( avm(ji+1,jj,2) + avm(ji  ,jj,2) )   &
                  &           / ( e3u(ji,jj,1,Kaa) * e3uw(ji,jj,2,Kmm) ) * wumask(ji,jj,2)
               zWus = ( wi(ji  ,jj,2) +  wi(ji+1,jj,2) ) / e3u(ji,jj,1,Kaa)
               zws(ji,1) = zzws - zDt_2 * MAX( zWus, 0._wp )
               zwd(ji,1) = 1._wp - zzws - zDt_2 * ( MIN( zWus, 0._wp ) )
            END_1D
         ELSE                       !- only vertical diffusive terms
            SELECT CASE( nldf_dyn )
            CASE( np_lap_i )           ! rotated lateral mixing: add its vertical mixing (akzu)
               DO_2Dik( 0, 0,   1, jpkm1, 1 )
                  zzwi = - zDt_2 * ( ( avm(ji+1,jj,jk  ) + avm(ji,jj,jk  ) ) + akzu(ji,jj,jk  ) )   &   ! add () for NP repro
                     &           / ( e3u(ji,jj,jk,Kaa) * e3uw(ji,jj,jk  ,Kmm) ) * wumask(ji,jj,jk  )
                  zzws = - zDt_2 * ( ( avm(ji+1,jj,jk+1) + avm(ji,jj,jk+1) ) + akzu(ji,jj,jk+1) )   &   ! add () for NP repro
                     &           / ( e3u(ji,jj,jk,Kaa) * e3uw(ji,jj,jk+1,Kmm) ) * wumask(ji,jj,jk+1)
                  zwi(ji,jk) = zzwi
                  zws(ji,jk) = zzws
                  zwd(ji,jk) = 1._wp - zzwi - zzws
               END_2D
            CASE DEFAULT               ! iso-level lateral mixing
               DO_2Dik( 0, 0,   1, jpkm1, 1 )
                  zzwi = - zDt_2 * ( avm(ji+1,jj,jk  ) + avm(ji,jj,jk  ) )    &
                     &           / ( e3u(ji,jj,jk,Kaa) * e3uw(ji,jj,jk  ,Kmm) ) * wumask(ji,jj,jk  )
                  zzws = - zDt_2 * ( avm(ji+1,jj,jk+1) + avm(ji,jj,jk+1) )    &
                     &           / ( e3u(ji,jj,jk,Kaa) * e3uw(ji,jj,jk+1,Kmm) ) * wumask(ji,jj,jk+1)
                  zwi(ji,jk) = zzwi
                  zws(ji,jk) = zzws
                  zwd(ji,jk) = 1._wp - zzwi - zzws
               END_2D
            END SELECT
            !
            zwi(:,1) = 0._wp
            DO_1Di( 0, 0 )     !* Surface boundary conditions
               zwd(ji,1) = 1._wp - zws(ji,1)
            END_1D
         ENDIF
         !
         !
         !              !==  Apply semi-implicit bottom friction  ==!
         !
         !     Only needed for semi-implicit bottom friction setup. The explicit
         !     bottom friction has been included in "u(v)a" which act as the R.H.S
         !     column vector of the tri-diagonal matrix equation
         !
         IF ( ln_drgimp ) THEN      ! implicit bottom friction
            DO_1Di( 0, 0 )
               iku = mbku(ji,jj)       ! ocean bottom level at u- and v-points
               zwd(ji,iku) = zwd(ji,iku) - zDt_2 *( rCdU_bot(ji+1,jj)+rCdU_bot(ji,jj) ) / e3u(ji,jj,iku,Kaa)
            END_1D
            IF ( ln_isfcav.OR.ln_drgice_imp ) THEN   ! top friction (always implicit)
               DO_1Di( 0, 0 )
               !!gm   top Cd is masked (=0 outside cavities) no need of test on mik>=2  ==>> it has been suppressed
                  iku = miku(ji,jj)       ! ocean top level at u- and v-points
                  zwd(ji,iku) = zwd(ji,iku) - zDt_2 *( rCdU_top(ji+1,jj)+rCdU_top(ji,jj) ) / e3u(ji,jj,iku,Kaa)
               END_1D
            ENDIF
         ENDIF
         !
         ! Matrix inversion starting from the first level
         !-----------------------------------------------------------------------
         !   solve m.x = y  where m is a tri diagonal matrix ( jpk*jpk )
         !
         !        ( zwd1 zws1   0    0    0  )( zwx1 ) ( zwy1 )
         !        ( zwi2 zwd2 zws2   0    0  )( zwx2 ) ( zwy2 )
         !        (  0   zwi3 zwd3 zws3   0  )( zwx3 )=( zwy3 )
         !        (        ...               )( ...  ) ( ...  )
         !        (  0    0    0   zwik zwdk )( zwxk ) ( zwyk )
         !
         !   m is decomposed in the product of an upper and a lower triangular matrix
         !   The 3 diagonal terms are in 2d arrays: zwd, zws, zwi
         !   The solution (the after velocity) is in puu(:,:,:,Kaa)
         !-----------------------------------------------------------------------
         !
         DO_2Dik( 0, 0,    2, jpkm1, 1 )   !==  First recurrence : Dk = Dk - Lk * Uk-1 / Dk-1   (increasing k)  ==
            zwd(ji,jk) = zwd(ji,jk) - zwi(ji,jk) * zws(ji,jk-1) / zwd(ji,jk-1)
         END_2D
         !
         DO_1Di( 0, 0 )                    !==  second recurrence:    SOLk = RHSk - Lk / Dk-1  Lk-1  ==!
#if defined key_RK3
            !                                  ! RK3: use only utau (not utau_b)
            puu(ji,jj,1,Kaa) = puu(ji,jj,1,Kaa) + rDt * utauU(ji,jj)   &
                 &                                    / ( e3u(ji,jj,1,Kaa) * rho0 ) * umask(ji,jj,1)
#else
            !                                  ! MLF: average of utau and utau_b
            puu(ji,jj,1,Kaa) = puu(ji,jj,1,Kaa) + zDt_2 * ( utau_b(ji,jj) + utauU(ji,jj) )   &
                 &                                      / ( e3u(ji,jj,1,Kaa) * rho0 ) * umask(ji,jj,1)
#endif
         END_1D
         DO_2Dik( 0, 0,     2, jpkm1, 1 )
            puu(ji,jj,jk,Kaa) = puu(ji,jj,jk,Kaa) - zwi(ji,jk) / zwd(ji,jk-1) * puu(ji,jj,jk-1,Kaa)
         END_2D
         !
         DO_1Di( 0, 0 )                    !==  third recurrence : SOLk = ( Lk - Uk * Ek+1 ) / Dk  ==!
            puu(ji,jj,jpkm1,Kaa) = puu(ji,jj,jpkm1,Kaa) / zwd(ji,jpkm1)
         END_1D
         DO_2Dik( 0, 0,    jpk-2, 1, -1 )
            puu(ji,jj,jk,Kaa) = ( puu(ji,jj,jk,Kaa) - zws(ji,jk) * puu(ji,jj,jk+1,Kaa) ) / zwd(ji,jk)
         END_2D
         !
         !
         !              !==  Vertical diffusion on v  ==!
         !
         !                       !* Matrix construction
         IF( ln_zad_Aimp ) THEN   !!
            SELECT CASE( nldf_dyn )
            CASE( np_lap_i )           ! rotated lateral mixing: add its vertical mixing (akzv)
               DO_2Dik( 0, 0,    1, jpkm1, 1 )
                  zzwi = - zDt_2 * ( ( avm(ji,jj+1,jk  ) + avm(ji,jj,jk  ) ) + akzv(ji,jj,jk  ) )   &   ! add () for NP repro
                     &           / ( e3v(ji,jj,jk,Kaa) * e3vw(ji,jj,jk  ,Kmm) ) * wvmask(ji,jj,jk  )
                  zzws = - zDt_2 * ( ( avm(ji,jj+1,jk+1) + avm(ji,jj,jk+1) ) + akzv(ji,jj,jk+1) )   &   ! add () for NP repro
                     &           / ( e3v(ji,jj,jk,Kaa) * e3vw(ji,jj,jk+1,Kmm) ) * wvmask(ji,jj,jk+1)
                  z1_e3va = 1._wp / e3v(ji,jj,jk,Kaa)   ! after scale factor at V-point
                  zWvi = ( wi(ji,jj,jk  ) + wi(ji,jj+1,jk  ) ) * z1_e3va
                  zWvs = ( wi(ji,jj,jk+1) + wi(ji,jj+1,jk+1) ) * z1_e3va
                  zwi(ji,jk) = zzwi + zDt_2 * MIN( zWvi, 0._wp )
                  zws(ji,jk) = zzws - zDt_2 * MAX( zWvs, 0._wp )
                  zwd(ji,jk) = 1._wp - zzwi - zzws - zDt_2 * ( - MAX( zWvi, 0._wp ) + MIN( zWvs, 0._wp ) )
               END_2D
            CASE DEFAULT               ! iso-level lateral mixing
               DO_2Dik( 0, 0,    1, jpkm1, 1 )
                  zzwi = - zDt_2 * ( avm(ji,jj+1,jk  ) + avm(ji,jj,jk  ) )    &
                     &           / ( e3v(ji,jj,jk,Kaa) * e3vw(ji,jj,jk  ,Kmm) ) * wvmask(ji,jj,jk  )
                  zzws = - zDt_2 * ( avm(ji,jj+1,jk+1) + avm(ji,jj,jk+1) )    &
                     &           / ( e3v(ji,jj,jk,Kaa) * e3vw(ji,jj,jk+1,Kmm) ) * wvmask(ji,jj,jk+1)
                  z1_e3va = 1._wp / e3v(ji,jj,jk,Kaa)   ! after scale factor at V-point
                  zWvi = ( wi(ji,jj,jk  ) + wi(ji,jj+1,jk  ) ) * z1_e3va
                  zWvs = ( wi(ji,jj,jk+1) + wi(ji,jj+1,jk+1) ) * z1_e3va
                  zwi(ji,jk) = zzwi  + zDt_2 * MIN( zWvi, 0._wp )
                  zws(ji,jk) = zzws  - zDt_2 * MAX( zWvs, 0._wp )
                  zwd(ji,jk) = 1._wp - zzwi - zzws - zDt_2 * ( - MAX( zWvi, 0._wp ) + MIN( zWvs, 0._wp ) )
               END_2D
            END SELECT
            DO_1Di( 0, 0 )   !* Surface boundary conditions
               zwi(ji,1) = 0._wp
               zzws = - zDt_2 * ( avm(ji,jj+1,2) + avm(ji,jj,2) )    &
                  &           / ( e3v(ji,jj,1,Kaa) * e3vw(ji,jj,2,Kmm) ) * wvmask(ji,jj,2)
               zWvs = ( wi(ji,jj  ,2) +  wi(ji,jj+1,2) ) / e3v(ji,jj,1,Kaa)
               zws(ji,1 ) = zzws - zDt_2 * MAX( zWvs, 0._wp )
               zwd(ji,1 ) = 1._wp - zzws - zDt_2 * ( MIN( zWvs, 0._wp ) )
            END_1D
         ELSE
            SELECT CASE( nldf_dyn )
            CASE( np_lap_i )           ! rotated lateral mixing: add its vertical mixing (akzu)
               DO_2Dik( 0, 0,    1, jpkm1, 1 )
                  zzwi = - zDt_2 * ( ( avm(ji,jj+1,jk  ) + avm(ji,jj,jk  ) ) + akzv(ji,jj,jk  ) )   &   ! add () for NP repro
                     &           / ( e3v(ji,jj,jk,Kaa) * e3vw(ji,jj,jk  ,Kmm) ) * wvmask(ji,jj,jk  )
                  zzws = - zDt_2 * ( ( avm(ji,jj+1,jk+1) + avm(ji,jj,jk+1) ) + akzv(ji,jj,jk+1) )   &   ! add () for NP repro
                     &           / ( e3v(ji,jj,jk,Kaa) * e3vw(ji,jj,jk+1,Kmm) ) * wvmask(ji,jj,jk+1)
                  zwi(ji,jk) = zzwi
                  zws(ji,jk) = zzws
                  zwd(ji,jk) = 1._wp - zzwi - zzws
               END_2D
            CASE DEFAULT               ! iso-level lateral mixing
               DO_2Dik( 0, 0,    1, jpkm1, 1 )
                  zzwi = - zDt_2 * ( avm(ji,jj+1,jk  ) + avm(ji,jj,jk  ) )    &
                     &           / ( e3v(ji,jj,jk,Kaa) * e3vw(ji,jj,jk  ,Kmm) ) * wvmask(ji,jj,jk  )
                  zzws = - zDt_2 * ( avm(ji,jj+1,jk+1) + avm(ji,jj,jk+1) )    &
                     &           / ( e3v(ji,jj,jk,Kaa) * e3vw(ji,jj,jk+1,Kmm) ) * wvmask(ji,jj,jk+1)
                  zwi(ji,jk) = zzwi
                  zws(ji,jk) = zzws
                  zwd(ji,jk) = 1._wp - zzwi - zzws
               END_2D
            END SELECT
            DO_1Di( 0, 0 )        !* Surface boundary conditions
               zwi(ji,1) = 0._wp
               zwd(ji,1) = 1._wp - zws(ji,1)
            END_1D
         ENDIF
         !
         !              !==  Apply semi-implicit top/bottom friction  ==!
         !
         !     Only needed for semi-implicit bottom friction setup. The explicit
         !     bottom friction has been included in "u(v)a" which act as the R.H.S
         !     column vector of the tri-diagonal matrix equation
         !
         IF( ln_drgimp ) THEN
            DO_1Di( 0, 0 )
               ikv = mbkv(ji,jj)       ! (deepest ocean u- and v-points)
               zwd(ji,ikv) = zwd(ji,ikv) - zDt_2*( rCdU_bot(ji,jj+1)+rCdU_bot(ji,jj) )   &
                  &                                   / e3v(ji,jj,ikv,Kaa)
            END_1D
            IF ( ln_isfcav.OR.ln_drgice_imp ) THEN
               DO_1Di( 0, 0 )
                  ikv = mikv(ji,jj)       ! (first wet ocean u- and v-points)
                  zwd(ji,ikv) = zwd(ji,ikv) - zDt_2*( rCdU_top(ji,jj+1)+rCdU_top(ji,jj) )   &
                     &                                   / e3v(ji,jj,ikv,Kaa)
               END_1D
            ENDIF
         ENDIF

         ! Matrix inversion
         !-----------------------------------------------------------------------
         !   solve m.x = y  where m is a tri diagonal matrix ( jpk*jpk )
         !
         !        ( zwd1 zws1   0    0    0  )( zwx1 ) ( zwy1 )
         !        ( zwi2 zwd2 zws2   0    0  )( zwx2 ) ( zwy2 )
         !        (  0   zwi3 zwd3 zws3   0  )( zwx3 )=( zwy3 )
         !        (        ...               )( ...  ) ( ...  )
         !        (  0    0    0   zwik zwdk )( zwxk ) ( zwyk )
         !
         !   m is decomposed in the product of an upper and lower triangular matrix
         !   The 3 diagonal terms are in 2d arrays: zwd, zws, zwi
         !   The solution (after velocity) is in 2d array va
         !-----------------------------------------------------------------------
         !
         DO_2Dik( 0, 0,    2, jpkm1, 1 )   !==  First recurrence : Dk = Dk - Lk * Uk-1 / Dk-1   (increasing k)  ==
            zwd(ji,jk) = zwd(ji,jk) - zwi(ji,jk) * zws(ji,jk-1) / zwd(ji,jk-1)
         END_2D
         !
         DO_1Di( 0, 0 )                    !==  second recurrence:    SOLk = RHSk - Lk / Dk-1  Lk-1  ==!
#if defined key_RK3
            !                                  ! RK3: use only vtau (not vtau_b)
            pvv(ji,jj,1,Kaa) = pvv(ji,jj,1,Kaa) + rDt * vtauV(ji,jj)   &
               &                                   / ( e3v(ji,jj,1,Kaa) * rho0 ) * vmask(ji,jj,1)
#else
            !                                  ! MLF: average of vtau and vtau_b
            pvv(ji,jj,1,Kaa) = pvv(ji,jj,1,Kaa) + zDt_2*( vtau_b(ji,jj) + vtauV(ji,jj) )   &
                 &                                 / ( e3v(ji,jj,1,Kaa) * rho0 ) * vmask(ji,jj,1)
#endif
         END_1D
         DO_2Dik( 0, 0,    2, jpkm1, 1 )
            pvv(ji,jj,jk,Kaa) = pvv(ji,jj,jk,Kaa) - zwi(ji,jk) / zwd(ji,jk-1) * pvv(ji,jj,jk-1,Kaa)
         END_2D
         !
         DO_1Di( 0, 0 )                    !==  third recurrence : SOLk = ( Lk - Uk * SOLk+1 ) / Dk  ==!
            pvv(ji,jj,jpkm1,Kaa) = pvv(ji,jj,jpkm1,Kaa) / zwd(ji,jpkm1)
         END_1D
         DO_2Dik( 0, 0,   jpk-2, 1, -1 )
            pvv(ji,jj,jk,Kaa) = ( pvv(ji,jj,jk,Kaa) - zws(ji,jk) * pvv(ji,jj,jk+1,Kaa) ) / zwd(ji,jk)
         END_2D
         !                                            ! ================= !
      END_1D                                          !  i-k slices loop  !
      !                                               ! ================= !
      !
      IF( l_trddyn )   THEN                      ! save the vertical diffusive trends for further diagnostics
         ztrdu(:,:,:) = ( puu(:,:,:,Kaa) - puu(:,:,:,Kbb) )*r1_Dt - ztrdu(:,:,:)
         ztrdv(:,:,:) = ( pvv(:,:,:,Kaa) - pvv(:,:,:,Kbb) )*r1_Dt - ztrdv(:,:,:)
         CALL trd_dyn( ztrdu, ztrdv, jpdyn_zdf, kt, Kmm )
         DEALLOCATE( ztrdu, ztrdv ) 
      ENDIF
      !                                          ! print mean trends (used for debugging)
      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=puu(:,:,:,Kaa), clinfo1=' zdf  - Ua: ', mask1=umask,               &
         &                                  tab3d_2=pvv(:,:,:,Kaa), clinfo2=       ' Va: ', mask2=vmask, clinfo3='dyn' )
         !
      IF( ln_timing )   CALL timing_stop('dyn_zdf')
      !
   END SUBROUTINE dyn_zdf

   !!==============================================================================
END MODULE dynzdf
