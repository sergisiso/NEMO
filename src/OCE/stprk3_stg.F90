MODULE stprk3_stg
   !!======================================================================
   !!                       ***  MODULE stprk3_stg  ***
   !! Time-stepping   : manager of the ocean, tracer and ice time stepping
   !!                   using a 3rd order Runge-Kutta  with fixed or quasi-eulerian coordinate
   !!======================================================================
   !! History :  4.5  !  2021-01  (S. Techene, G. Madec, N. Ducousso, F. Lemarie)  Original code
   !!   NEMO
   !!----------------------------------------------------------------------
#if defined key_qco   ||   defined key_linssh
   !!----------------------------------------------------------------------
   !!   'key_qco'                        Quasi-Eulerian vertical coordinate
   !!                          OR
   !!   'key_linssh                       Fixed in time vertical coordinate
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!    stp_RK3_stg  : NEMO 3rd order Runge-Kutta stage with qco or linssh
   !!----------------------------------------------------------------------
   USE step_oce       ! time stepping used modules
   USE domqco         ! quasi-eulerian coordinate      (dom_qco_r3c routine)
   USE bdydyn         ! ocean open boundary conditions (define bdy_dyn)
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
# if defined key_top
   USE trc            ! ocean passive tracers variables
   USE trcadv         ! passive tracers advection      (trc_adv routine)
   USE trcsms         ! passive tracers source and sink
   USE trctrp         ! passive tracers transport
   USE trcsbc         ! passive tracers surface boundary condition !!st WARNING USELESS TO BE REMOVED
   USE trcbdy         ! passive tracers transport open boundary
   USE trcstp_rk3
# endif
# if defined key_agrif
   USE agrif_oce_interp
# endif
   !
   USE prtctl         ! print control

   IMPLICIT NONE
   PRIVATE

   PUBLIC   stp_RK3_stg   ! called by nemogcm.F90

   REAL(wp) ::   r1_2 = 1._wp / 2._wp
   REAL(wp) ::   r1_3 = 1._wp / 3._wp
   REAL(wp) ::   r2_3 = 2._wp / 3._wp

   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ssha         ! sea-surface height  at N+1
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ua_b, va_b   ! barotropic velocity at N+1
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   r3ta, r3ua, r3va   ! ssh/h_0 ratio at t,u,v-column at N+1
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:) ::   r3fb, r3fa   ! ssh/h_0 ratio at f-column at N and N+1

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: step.F90 12377 2020-02-12 14:39:06Z acc $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE stp_RK3_stg( kstg, kstp, Kbb, Kmm, Krhs, Kaa )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE stp_RK3_stg  ***
      !!
      !! ** Purpose : - stage of RK3 time stepping of OCE and TOP
      !!
      !! ** Method  :   input: computed in dynspg_ts
      !!              ssh             shea surface height at N+1           (oce.F90)
      !!              (uu_b,vv_b)     barotropic velocity at N, N+1        (oce.F90)
      !!              (un_adv,vn_adv) barotropic transport from N to N+1   (dynspg_ts.F90)
      !!              ,
      !!              -1- set ssh(Naa) (Naa=N+1/3, N+1/2, or N)
      !!              -2- set the advective velocity (zadU,zaV)
      !!              -4- Compute the after (Naa) T-S
      !!              -5- Update now
      !!              -6- Update the horizontal velocity
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kstg                        ! RK3 stage
      INTEGER, INTENT(in) ::   kstp, Kbb, Kmm, Krhs, Kaa   ! ocean time-step and time-level indices
      !
      INTEGER  ::   ji, jj, jk, jn, jtile                  ! dummy loop indices
      REAL(wp) ::   ze3Tb, ze3Sb, z1_e3t     ! local scalars
      REAL(wp) ::   ze3Tr, ze3Sr             !   -      -
      REAL(wp), DIMENSION(jpi,jpj,jpk) ::   zaU, zaV       ! advective horizontal velocity
      REAL(wp), DIMENSION(jpi,jpj)     ::   zub, zvb       ! advective transport
      !! ---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('stp_RK3_stg')
      !
      IF( kstp == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'stp_RK3_stg : Runge Kutta 3rd order at stage ', kstg
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
      ENDIF
      !
      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      !  ssh, uu_b, vv_b, and  ssh/h0 at Kaa
      !  3D advective velocity at Kmm
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !
      SELECT CASE( kstg )
      !                    !---------------!
      CASE ( 1 )           !==  Stage 1  ==!   Kbb = Kmm = N  ;  Kaa = N+1/3
         !                 !---------------!
         !
         ALLOCATE( ssha(jpi,jpj) , ua_b(jpi,jpj) , va_b(jpi,jpj) )
         !
         rDt = r1_3 * rn_Dt            ! set time-step : rn_Dt/3
         r1_Dt = 1._wp / rDt
         !
         ssha(:,:) = ssh (:,:,Kaa)     ! save ssh, uu_b, vv_b at N+1  (computed in dynspg_ts)
         ua_b(:,:) = uu_b(:,:,Kaa)
         va_b(:,:) = vv_b(:,:,Kaa)
         !                             ! interpolated ssh and (uu_b,vv_b) at Kaa (N+1/3)
         ssh (:,:,Kaa) = r2_3 * ssh (:,:,Kbb) + r1_3 * ssha(:,:)
         uu_b(:,:,Kaa) = r2_3 * uu_b(:,:,Kbb) + r1_3 * ua_b(:,:)
         vv_b(:,:,Kaa) = r2_3 * vv_b(:,:,Kbb) + r1_3 * va_b(:,:)
         !
         !
         !                     !==  ssh/h0 ratio at Kaa  ==!
         !
         IF( .NOT.lk_linssh ) THEN     ! "after" ssh/h_0 ratio at t,u,v-column computed at N+1 stored in r3.a
            !
            ALLOCATE( r3ta(jpi,jpj) , r3ua(jpi,jpj) , r3va(jpi,jpj) , r3fa(jpi,jpj) , r3fb(jpi,jpj) )
            !
            r3fb(:,:) = r3f(:,:)
            CALL dom_qco_r3c_RK3( ssha, r3ta, r3ua, r3va, r3fa )
            !
            CALL lbc_lnk( 'stprk3_stg', r3ua, 'U', 1._wp, r3va, 'V', 1._wp, r3fa, 'F', 1._wp )
            !                          !
            r3t(:,:,Kaa) = r2_3 * r3t(:,:,Kbb) + r1_3 * r3ta(:,:)   ! at N+1/3 (Kaa)
            r3u(:,:,Kaa) = r2_3 * r3u(:,:,Kbb) + r1_3 * r3ua(:,:)
            r3v(:,:,Kaa) = r2_3 * r3v(:,:,Kbb) + r1_3 * r3va(:,:)
            ! r3f already properly set up                           ! at N     (Kmm)
         ENDIF
         !
         !                 !---------------!
      CASE ( 2 )           !==  Stage 2  ==!   Kbb = N   ;   Kmm = N+1/3   ;   Kaa = N+1/2
         !                 !---------------!
         !
         rDt = r1_2 * rn_Dt            ! set time-step : rn_Dt/2
         r1_Dt = 1._wp / rDt
         !
         !                             ! set ssh and (uu_b,vv_b) at N+1/2  (Kaa)
         ssh (:,:,Kaa) = r1_2 * ( ssh (:,:,Kbb) + ssha(:,:) )
         uu_b(:,:,Kaa) = r1_2 * ( uu_b(:,:,Kbb) + ua_b(:,:) )
         vv_b(:,:,Kaa) = r1_2 * ( vv_b(:,:,Kbb) + va_b(:,:) )
         !
         IF( .NOT.lk_linssh ) THEN
            r3t(:,:,Kaa) = r1_2 * ( r3t(:,:,Kbb) + r3ta(:,:) )   ! at N+1/2 (Kaa)
            r3u(:,:,Kaa) = r1_2 * ( r3u(:,:,Kbb) + r3ua(:,:) )
            r3v(:,:,Kaa) = r1_2 * ( r3v(:,:,Kbb) + r3va(:,:) )
            r3f(:,:)     = r2_3 * r3fb(:,:) + r1_3 * r3fa(:,:)   ! at N+1/3 (Kmm)
         ENDIF
         !
         !                 !---------------!
      CASE ( 3 )           !==  Stage 3  ==!   Kbb = N   ;   Kmm = N+1/2   ;   Kaa = N+1
         !                 !---------------!
         !
         rDt = rn_Dt                   ! set time-step : rn_Dt
         r1_Dt = 1._wp / rDt
         !
         ssh (:,:,Kaa) = ssha(:,:)     ! recover ssh and (uu_b,vv_b) at N + 1
         uu_b(:,:,Kaa) = ua_b(:,:)
         vv_b(:,:,Kaa) = va_b(:,:)
         !
         DEALLOCATE( ssha , ua_b , va_b )
         !
         IF( .NOT.lk_linssh ) THEN
            r3t(:,:,Kaa) = r3ta(:,:)                          ! at N+1   (Kaa)
            r3u(:,:,Kaa) = r3ua(:,:)
            r3v(:,:,Kaa) = r3va(:,:)
            r3f(:,:    ) = r1_2 * ( r3fb(:,:) + r3fa(:,:) )   ! at N+1/2 (Kmm)
            DEALLOCATE( r3ta, r3ua, r3va, r3fb )              ! deallocate all r3. except r3fa which will be
            !                                                 ! saved in r3f at the end of the time integration and then deallocated
            !
         ENDIF
         !
      END SELECT
      !
      !                     !==  advective velocity at Kmm  ==!
      !
      !                                            !- horizontal components -!   (zaU,zaV)
      DO_2D_OVR( nn_hls, nn_hls, nn_hls, nn_hls )
         zub(ji,jj) = un_adv(ji,jj)*r1_hu(ji,jj,Kmm) - uu_b(ji,jj,Kmm)    ! barotropic velocity correction
         zvb(ji,jj) = vn_adv(ji,jj)*r1_hv(ji,jj,Kmm) - vv_b(ji,jj,Kmm)
      END_2D
      DO_3D_OVR( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )               ! horizontal advective velocity
         zaU(ji,jj,jk) = uu(ji,jj,jk,Kmm) + zub(ji,jj)*umask(ji,jj,jk)
         zaV(ji,jj,jk) = vv(ji,jj,jk,Kmm) + zvb(ji,jj)*vmask(ji,jj,jk)
      END_3D
      !                                            !- vertical components -!   ww
      !
      ! Always need:                               ! ww cross-level velocity consistent with zaU/zaV
      !
      CALL wzv  ( kstp, Kbb, Kmm, Kaa, zaU, zaV, ww_T )
      !
      !                                            ! Adaptive-implicit vertical advection partitioning
      IF( ln_zad_Aimp )  wi_T(:,:,:) = 0.0_wp                                                    ! ensure it is zero for stages 1 and 2
      IF( ln_zad_Aimp .AND. kstg == 3 )  CALL wAimp( kstp, Kmm, zaU, zaV, ww_T, wi_T, kstg )     ! Partition at stage 3 only
      !
      IF( ln_dynadv_vec ) THEN
         ! Also need:                              ! ww cross-level velocity consistent with uu/vv at Kmm
         CALL wzv  ( kstp, Kbb, Kmm, Kaa, uu(:,:,:,Kmm), vv(:,:,:,Kmm), ww_U )
         !
         !                                         ! Adaptive-implicit vertical advection partitioning
         IF( ln_zad_Aimp )  wi_U(:,:,:) = 0.0_wp                                                                      ! ensure it is zero for stages 1 and 2
         IF( ln_zad_Aimp .AND. kstg == 3 )  CALL wAimp( kstp, Kmm, uu(:,:,:,Kmm), vv(:,:,:,Kmm), ww_U, wi_U, kstg )   ! partition at stage 3 only
         !
         !                                         ! use ww cross-level velocity consistent with uu/vv at Kmm
         IF( ln_zad_Aimp ) wi => wi_U
         ww => ww_U
      ELSE                                         ! use ww cross-level velocity consistent with zaU/zaV for momentum
         IF( ln_zad_Aimp ) wi => wi_T
         ww => ww_T
      ENDIF
      !

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      !  RHS of ocean dynamics : ADV + VOR/COR + HPG (+ ASM )      <<<===  Question:  Stokes drift ?
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !
      uu(:,:,:,Krhs) = 0._wp           ! set dynamics trends to zero
      vv(:,:,:,Krhs) = 0._wp
      !
!===>>>>>> Modify dyn_adv_... dyn_keg routines so that Krhs to zero useless
      !                                         ! advection (VF or FF)	==> RHS
      IF( ln_dynadv_vec ) THEN                                            ! uu and vv used for momentum advection
         CALL dyn_adv( kstp, Kmm, Kmm      , uu, vv, Krhs)
      ELSE                                                                ! advective velocity used for momentum advection
         CALL dyn_adv( kstp, Kmm, Kmm      , uu, vv, Krhs, zaU, zaV, ww ) !!st !!! TO DO !!! METTRE Kmm partout et faire le test !!!
      ENDIF
      !                                         ! Coriolis / vorticity  ==> RHS
      CALL dyn_vor( kstp,      Kmm      , uu, vv, Krhs )
      !

!===>>>>>> Modify dyn_hpg & dyn_hpg_...  routines : rhd computed in dyn_hpg and pass in argument to dyn_hpg_...

      CALL eos    ( ts, Kmm, rhd )              ! Kmm in situ density anomaly for hpg computation

!!gm end
      CALL dyn_hpg( kstp,      Kmm      , uu, vv, Krhs )
      !
!!gm ===>>>>>> Probably useless since uu_b(Kaa) will be imposed at the end of stage 1 and 2
!                   but may be necessary in stage 3 due to implicite in dynzdf.
!                   except if my idea for the matrice construction is OK !
!      !                                         ! grad_h of ps          ==> RHS
!      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
!         uu(ji,jj,jk,Krhs) = uu(ji,jj,jk,Krhs) - grav * ( ssh(ji+1,jj  ,Kmm) - ssh(ji,jj,Kmm) )
!         vv(ji,jj,jk,Krhs) = vv(ji,jj,jk,Krhs) - grav * ( ssh(ji  ,jj+1,Kmm) - ssh(ji,jj,Kmm) )
!      END_3D
!!gm

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! RHS of tracers : ADV only using (zaU,zaV,ww)
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !
      !                                            ! Advective velocity needed for tracers advection - already in use if ln_dynadv_vec=F
      IF( ln_dynadv_vec )  THEN
         ww => ww_T
         IF( ln_zad_Aimp ) wi => wi_T
      ENDIF
      !
      !                                            ! BBL coefficients required for both passive- and active-tracer transport within
      !                                            ! the BBL (stage 3 only, requires uu, vv, gdept at Kmm)
      IF( ( kstg == 3 ) .AND. ln_trabbl ) CALL bbl( kstp, nit000, Kbb, Kmm )
      !
# if defined key_top
      !                       !==  Passive Tracer  ==!
      !
      SELECT CASE( kstg )
      !                    !-------------------!
      CASE ( 1 , 2 )       !==  Stage 1 & 2  ==!   stg1:  Kbb = N  ;  Kaa = N+1/3
         !                 !-------------------!   stg2:  Kbb = N  ;  Kmm = N+1/3  ;  Kaa = N+1/2
         !
         IF( kstg == 1 ) THEN
            CALL trc_stp_start( kstp, Kbb, Kmm, Krhs, Kaa )
         ENDIF
         !
         IF(.NOT. ln_trcadv_mus ) THEN
            !
            DO jn = 1, jptra
               tr(:,:,:,jn,Krhs) = 0._wp                              ! set tracer trends to zero !!st ::: required because of tra_adv new loops
            END DO
            !                                      !==  advection of passive tracers  ==!
            rDt_trc = rDt
            !
            CALL trc_sbc_RK3( kstp,      Kmm, tr, Krhs, kstg )              ! surface boundary condition
            !
            CALL trc_adv    ( kstp, Kbb, Kmm, Kaa, tr, Krhs, zaU, zaV, ww ) ! horizontal & vertical advection
            !
            !                                      !==  time integration  ==!   ∆t = rn_Dt/3 (stg1) or rn_Dt/2 (stg2)
            DO jn = 1, jptra
               DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               ze3Tb = e3t(ji,jj,jk,Kbb) * tr(ji,jj,jk,jn,Kbb )
               ze3Tr = e3t(ji,jj,jk,Kmm) * tr(ji,jj,jk,jn,Krhs)
               z1_e3t= 1._wp / e3t(ji,jj,jk, Kaa)
               tr(ji,jj,jk,jn,Kaa) = ( ze3Tb + rDt * ze3Tr*tmask(ji,jj,jk) ) * z1_e3t
               END_3D
            END DO
            !
!!st need a lnc lkn at stage 1 & 2 otherwise tr@Kmm will not be usable in trc_adv
            CALL lbc_lnk( 'stprk3_stg', tr(:,:,:,:,Kaa), 'T', 1._wp )

         ENDIF
         !                 !---------------!
      CASE ( 3 )           !==  Stage 3  ==!   add all RHS terms but advection (=> Kbb only)
         !                 !---------------!
         !
         DO jn = 1, jptra
            tr(:,:,:,jn,Krhs) = 0._wp
         END DO
         !                                         !==  advection of passive tracers  ==!
         rDt_trc = rDt
         !
         CALL trc_sbc_RK3( kstp,      Kmm, tr, Krhs, kstg )              ! surface boundary condition
         !
         CALL trc_adv    ( kstp, Kbb, Kmm, Kaa, tr, Krhs, zaU, zaV, ww ) ! horizontal & vertical advection
         !
         CALL trc_sms    ( kstp, Kbb, Kbb, Krhs      )       ! tracers: sinks and sources
         CALL trc_trp    ( kstp, Kbb, Kmm, Krhs, Kaa )       ! transport of passive tracers (without advection)
         !
         !
         CALL trc_stp_end( kstp, Kbb, Kmm,       Kaa )
         !
      END SELECT
# endif

      !                       !==  T-S Tracers  ==!

!===>>>>>> Modify tra_adv_...  routines so that Krhs to zero useless
      DO jn = 1, jpts
         ts(:,:,:,jn,Krhs) = 0._wp                                   ! set tracer trends to zero (:,:,:) needed otherwise it does not work (?)
      END DO

      CALL eos( ts, Kmm, rhd, rhop ) ! now in potential density for tra_mle computation
!===>>> CAUTION here may be without GM velocity but stokes drift required ! 0 barotropic divergence for GM  != 0 barotropic divergence for SD 
!!st consistence 2D / 3D - flux de masse 
      CALL tra_adv( kstp, Kbb, Kmm, Kaa, ts, Krhs, zaU, zaV, ww, kstg )       ! hor. + vert. advection	==> RHS

!===>>>>>> stg1&2:  Verify the necessity of these trends (we may need it as there are in the RHS of dynspg_ts ?)
!!gm ====>>>>   needed for heat and salt fluxes associated with mass/volume flux
                        CALL tra_sbc_RK3( kstp,      Kmm, ts, Krhs, kstg )   ! surface boundary condition

      IF( ln_isf )      CALL tra_isf    ( kstp,      Kmm, ts, Krhs )   ! ice shelf heat flux
      IF( ln_traqsr )   CALL tra_qsr    ( kstp,      Kmm, ts, Krhs )   ! penetrative solar radiation qsr
!!gm

      !
!!gm ===>>>>>>  Verify the necessity of these trends  at stages 1 and 2
!           (we may need it as they are in the RHS of dynspg_ts ?)
!      IF(  lk_asminc .AND. ln_asmiau ) THEN               ! apply assimilation increment
!         IF( ln_dyninc )   CALL dyn_asm_inc( kstp, Kbb, Kmm, uu, vv, Krhs )   ! dynamics   ==> RHS
!         IF( ln_trainc )   CALL tra_asm_inc( kstp, Kbb, Kmm, ts    , Krhs )   ! tracers    ==> RHS
!      ENDIF
!!gm  end Verif

      !
      SELECT CASE( kstg )
      !                    !-------------------!
      CASE ( 1 , 2 )       !==  Stage 1 & 2  ==!   stg1:  Kbb = N  ;  Kaa = N+1/3
         !                 !-------------------!   stg2:  Kbb = N  ;  Kmm = N+1/3  ;  Kaa = N+1/2
         !
         !                                      !==  time integration  ==!   ∆t = rn_Dt/3 (stg1) or rn_Dt/2 (stg2)
         IF( ln_dynadv_vec .OR. ln_linssh ) THEN   ! applied on velocity
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               uu(ji,jj,jk,Kaa) = ( uu(ji,jj,jk,Kbb) + rDt * uu(ji,jj,jk,Krhs) ) * umask(ji,jj,jk)
               vv(ji,jj,jk,Kaa) = ( vv(ji,jj,jk,Kbb) + rDt * vv(ji,jj,jk,Krhs) ) * vmask(ji,jj,jk)
            END_3D
         ELSE                                      ! applied on thickness weighted velocity
            DO_3D( 0, 0, 0, 0, 1, jpkm1 )
               uu(ji,jj,jk,Kaa) = (         e3u(ji,jj,jk,Kbb) * uu(ji,jj,jk,Kbb )  &
                  &                 + rDt * e3u(ji,jj,jk,Kmm) * uu(ji,jj,jk,Krhs)  ) &
                  &                       / e3u(ji,jj,jk,Kaa) * umask(ji,jj,jk)
               vv(ji,jj,jk,Kaa) = (         e3v(ji,jj,jk,Kbb) * vv(ji,jj,jk,Kbb )  &
                  &                 + rDt * e3v(ji,jj,jk,Kmm) * vv(ji,jj,jk,Krhs)  ) &
                  &                       / e3v(ji,jj,jk,Kaa) * vmask(ji,jj,jk)
            END_3D
         ENDIF
         !
         DO_3D( 0, 0, 0, 0, 1, jpkm1 )
            ze3Tb = e3t(ji,jj,jk,Kbb) * ts(ji,jj,jk,jp_tem,Kbb )
            ze3Sb = e3t(ji,jj,jk,Kbb) * ts(ji,jj,jk,jp_sal,Kbb )
            ze3Tr = e3t(ji,jj,jk,Kmm) * ts(ji,jj,jk,jp_tem,Krhs)
            ze3Sr = e3t(ji,jj,jk,Kmm) * ts(ji,jj,jk,jp_sal,Krhs)
            z1_e3t= 1._wp / e3t(ji,jj,jk, Kaa)
            ts(ji,jj,jk,jp_tem,Kaa) = ( ze3Tb + rDt * ze3Tr*tmask(ji,jj,jk) ) * z1_e3t
            ts(ji,jj,jk,jp_sal,Kaa) = ( ze3Sb + rDt * ze3Sr*tmask(ji,jj,jk) ) * z1_e3t
         END_3D
         !
         !
         IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=uu(:,:,:,Kaa), clinfo1='stp stg   - Ua: ', mask1=umask,   &
            &                                  tab3d_2=vv(:,:,:,Kaa), clinfo2=           ' Va: ', mask2=vmask, clinfo3='dyn' )
         !
         IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=ts(:,:,:,jp_tem,Kaa), clinfo1='stp stg   - Ta: ', mask1=tmask,   &
            &                                  tab3d_2=ts(:,:,:,jp_sal,Kaa), clinfo2=           ' Sa: ', mask2=tmask, clinfo3='tra' )
         !
         !                 !---------------!
      CASE ( 3 )           !==  Stage 3  ==!   add all remaining RHS terms
         !                 !---------------!
         !
         !                                      !==  complete the momentum RHS ==!   except ZDF (implicit)
         !                                                   ! lateral mixing                    ==> RHS
                            CALL dyn_ldf( kstp, Kbb, Kmm, uu, vv, Krhs )
         !                                                   ! OSMOSIS non-local velocity fluxes ==> RHS
         IF( ln_zdfosm  )   CALL dyn_osm( kstp,      Kmm, uu, vv, Krhs )
         !
         IF( ln_bdy     ) THEN                               ! bdy damping trends     ==> RHS
                            CALL bdy_dyn3d_dmp ( kstp, Kbb, uu, vv, Krhs )
                            CALL bdy_tra_dmp   ( kstp, Kbb, ts    , Krhs )
         ENDIF

# if defined key_agrif
         IF(.NOT. Agrif_Root() ) THEN                        ! AGRIF:   sponge ==> momentum and tracer RHS
            CALL Agrif_Sponge_dyn
            CALL Agrif_Sponge_tra
         ENDIF
# endif
         !                                      !==  complete the tracers RHS  ==!   except ZDF (implicit)
         !                                            !*  T-S Tracer  *!
         !
                            CALL tra_ldf( kstp, Kbb, Kmm, ts, Krhs )  ! lateral mixing
         IF( ln_trabbc  )   CALL tra_bbc( kstp,      Kmm, ts, Krhs )  ! bottom heat flux
         IF( ln_trabbl  )   CALL tra_bbl( kstp, Kbb, Kmm, ts, Krhs )  ! advective (and/or diffusive) bottom boundary layer scheme
         IF( ln_tradmp  )   CALL tra_dmp( kstp, Kbb, Kmm, ts, Krhs )  ! internal damping trends

         IF( ln_zdfmfc  )   CALL tra_mfc( kstp, Kbb,      ts, Krhs )  ! Mass Flux Convection
         IF( ln_zdfosm  ) THEN
                            CALL tra_osm( kstp,      Kmm, ts, Krhs )  ! OSMOSIS non-local tracer fluxes ==> RHS
            IF( lrst_oce )  CALL osm_rst( kstp,      Kmm, 'WRITE'  )  ! write OSMOSIS outputs + ww (so must do here) to restarts
         ENDIF
         !
         !                                      !==  DYN & TRA time integration + ZDF  ==!   ∆t = rDt
         !
         IF( ln_dynadv_vec ) THEN 
                             ww => ww_U
                             IF( ln_zad_Aimp ) wi => wi_U
         ENDIF 
                            CALL dyn_zdf( kstp, Kbb, Kmm, Krhs, uu, vv, Kaa  )  ! vertical diffusion and time integration
         IF( ln_dynadv_vec .AND. ln_zad_Aimp )  wi => wi_T
                            CALL tra_zdf( kstp, Kbb, Kmm, Krhs, ts    , Kaa  )  ! vertical mixing and after tracer fields
         IF( ln_zdfnpc  )   CALL tra_npc( kstp,      Kmm, Krhs, ts    , Kaa  )  ! update after fields by non-penetrative convection
         !
         IF( .NOT.lk_linssh ) THEN
            r3f(:,:) = r3fa(:,:)                                         ! save r3fa in r3f before deallocation
            DEALLOCATE( r3fa )                                           ! (r3f = r3f(Kbb) of the next time step)
         ENDIF
         !
      END SELECT
      !                                         !==  correction of the barotropic (all stages)  ==!    at Kaa = N+1/3, N+1/2 or N+1
      !                                                           ! barotropic velocity correction
      DO_2D( 0, 0, 0, 0 )
         zub(ji,jj) = uu_b(ji,jj,Kaa) - SUM( e3u_0(ji,jj,:)*uu(ji,jj,:,Kaa) ) * r1_hu_0(ji,jj)
         zvb(ji,jj) = vv_b(ji,jj,Kaa) - SUM( e3v_0(ji,jj,:)*vv(ji,jj,:,Kaa) ) * r1_hv_0(ji,jj)
      END_2D
      !
      DO jk = 1, jpkm1                                            ! corrected horizontal velocity
         uu(T2D(0),jk,Kaa) = uu(T2D(0),jk,Kaa) + zub(T2D(0))*umask(T2D(0),jk)
         vv(T2D(0),jk,Kaa) = vv(T2D(0),jk,Kaa) + zvb(T2D(0))*vmask(T2D(0),jk)
      END DO

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ! Set boundary conditions
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !
# if defined key_agrif
            CALL Agrif_tra( kstp, kstg )             !* AGRIF zoom boundaries
            CALL Agrif_dyn( kstp, kstg )
# endif
      !                                              !* local domain boundaries  (T-point, unchanged sign)
      CALL lbc_lnk( 'stp_RK3_stg', uu(:,:,:,       Kaa), 'U', -1._wp, vv(:,:,:       ,Kaa), 'V', -1._wp   &
         &                       , ts(:,:,:,jp_tem,Kaa), 'T',  1._wp, ts(:,:,:,jp_sal,Kaa), 'T',  1._wp )
      !
      IF( l_zdfsh2 ) CALL lbc_lnk( 'stp_RK3_stg', avm_k, 'W', 1.0_wp )   !  lbc_lnk needed for zdf_sh2, moved here to allow tiling in zdf_phy
      !
      !                                              !* BDY open boundaries
      IF( ln_bdy )   THEN
                               CALL bdy_tra( kstp, Kbb, ts,     Kaa )
         IF( ln_dynspg_exp )   CALL bdy_dyn( kstp, Kbb, uu, vv, Kaa )
         IF( ln_dynspg_ts  )   CALL bdy_dyn( kstp, Kbb, uu, vv, Kaa, dyn3d_only=.true. )
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('stp_RK3_stg')
      !
   END SUBROUTINE stp_RK3_stg

#else
   !!----------------------------------------------------------------------
   !!   default option             EMPTY MODULE           qco not activated
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE stprk3_stg
