MODULE stp2d
   !!======================================================================
   !!                       ***  MODULE stp2d  ***
   !! Time-stepping   : manager of the ocean, tracer and ice time stepping
   !!                   using a 3rd order Rung Kuta  with fixed or quasi-eulerian coordinate
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
   !!    stp_2D   : RK3 case
   !!----------------------------------------------------------------------
   USE step_oce       ! time stepping used modules
   USE domqco         ! quasi-eulerian coordinate      (dom_qco_r3c routine)
   USE dynspg_ts      ! 2D mode integration
   USE sshwzv         ! vertical speed
   USE sbc_ice , ONLY : snwice_mass, snwice_mass_b
   USE sbcapr         ! surface boundary condition: atmospheric pressure
   USE sbcwave,  ONLY : bhd_wave
#if defined key_agrif
   USE agrif_oce_interp
   USE agrif_oce_sponge
#endif

   PRIVATE

   PUBLIC   stp_2D   ! called by nemogcm.F90
   REAL (wp) :: r1_2 = 0.5_wp 

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: step.F90 12377 2020-02-12 14:39:06Z acc $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE stp_2D( kt, Kbb, Kmm, Kaa, Krhs )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE stp_2D  ***
      !!
      !! ** Purpose : - Compute sea-surface height and barotropic velocity at Kaa
      !!                in single 1st RK3.
      !!
      !! ** Method  : -1- Compute the 3D to 2D forcing
      !!                 * Momentum (Ue,Ve)_rhs :
      !!                      3D to 2D dynamics, i.e. the vertical sum of :
      !!                        - Hor. adv. : KEG   + RVO in vector form
      !!                                    : ADV_h + MET in flux   form
      !!                        - LDF Lateral mixing 
      !!                        - HPG Hor. pressure gradient
      !!                      External forcings
      !!                        - baroclinic drag
      !!                        - wind 
      !!                        - atmospheric pressure
      !!                        - snow+ice load
      !!                        - surface wave load
      !!                 * ssh (sshe_rhs) :
      !!                      Net column average freshwater flux
      !!
      !!              -2- Solve the external mode Eqs. using sub-time step
      !!                  by a call to dyn_spg_ts (will be renamed dyn_2D or stp_2D)
      !!
      !! ** action  :   ssh            : N+1 sea surface height (Kaa=N+1)
      !!                (uu_b,vv_b)    : N+1 barotropic velocity 
      !!                (un_adv,vn_adv): barotropic transport from N to N+1 
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, Kbb, Kmm, Kaa, Krhs    ! ocean time-step and time-level indices
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp) ::   zg_2, zintp, zgrho0r, zld, zztmp     ! local scalars
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   zpice   ! 2D workspace
      !! ---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('stp_2D')
      !
      IF( kt == nit000 ) THEN
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'stp_2D : barotropic field in single first '
         IF(lwp) WRITE(numout,*) '~~~~~~'
      ENDIF
      !
      IF( ln_linssh ) THEN    !==  Compute ww(:,:,1)  ==!   (needed for momentum advection)
!!gm  only in Flux Form, Vector Form  dzU_z=0 assumed to be zero 
!!gm  ww(k=1) = div_h(uu_b) ==> modif dans dynadv                        <<<=== TO BE DONE
      ENDIF

      ALLOCATE( sshe_rhs(jpi,jpj) , Ue_rhs(jpi,jpj) , Ve_rhs(jpi,jpj) , CdU_u(jpi,jpj) , CdU_v(jpi,jpj) )

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      !   RHS of barotropic momentum  Eq.
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      !                       !======================================!
      !                       !==  Dynamics 2D RHS from 3D trends  ==!   (HADV + LDF + HPG) (No Coriolis trend)
      !                       !======================================!

      uu(:,:,:,Krhs) = 0._wp        ! set dynamics trends to zero
      vv(:,:,:,Krhs) = 0._wp
      !
      !                             !*  compute advection + coriolis *!
      !
      CALL ssh_nxt( kt, Kbb, Kbb, ssh, Kaa )
      !
      IF( .NOT.lk_linssh ) THEN
         DO_2D_OVR( 1, nn_hls, 1, nn_hls )      ! loop bounds limited by ssh definition in ssh_nxt
            r3t(ji,jj,Kaa) =  ssh(ji,jj,Kaa) * r1_ht_0(ji,jj)               ! "after" ssh/h_0 ratio guess at t-column at Kaa (n+1)
         END_2D
      ENDIF
      !
      CALL wzv    ( kt, Kbb, Kbb, Kaa , uu(:,:,:,Kbb), vv(:,:,:,Kbb), ww )  ! ww guess at Kbb (n)
      !
      CALL dyn_adv( kt, Kbb, Kbb      , uu, vv, Krhs)       !- vector form KEG+ZAD 
      !                                                     !- flux   form ADV
      CALL dyn_vor( kt,            Kbb, uu, vv, Krhs )      !- vector form COR+RVO
      !                                                     !- flux   form COR+MET
      !
      !                             !*  lateral viscosity  *!
      CALL dyn_ldf( kt,   Kbb, Kbb, uu, vv, Krhs )
#if defined key_agrif
      IF(.NOT. Agrif_Root() ) THEN  !*  AGRIF: sponge *!
         CALL Agrif_Sponge_dyn
      ENDIF
#endif
      !
      !                             !*  hydrostatic pressure gradient  *!  
      CALL eos    ( ts , Kbb, rhd )                          ! in situ density anomaly at Kbb
      CALL dyn_hpg( kt  , Kbb     , uu, vv, Krhs )           ! horizontal gradient of Hydrostatic pressure
      !
      !                             !*  vertical averaging  *!
      Ue_rhs(:,:) = SUM( e3u_0(:,:,:) * uu(:,:,:,Krhs) * umask(:,:,:), DIM=3 ) * r1_hu_0(:,:)
      Ve_rhs(:,:) = SUM( e3v_0(:,:,:) * vv(:,:,:,Krhs) * vmask(:,:,:), DIM=3 ) * r1_hv_0(:,:)

      !                       !===========================!
      !                       !==  external 2D forcing  ==!
      !                       !===========================!
      !
      ! 			    !* baroclinic drag forcing *!   (also provide the barotropic drag coeff.)
      !
      CALL dyn_drg_init( Kbb, Kbb, uu, vv, uu_b, vv_b, Ue_rhs, Ve_rhs, CdU_u, CdU_v )
      !
      !                             !* wind forcing *!
      IF( ln_bt_fw ) THEN
         DO_2D( 0, 0, 0, 0 )
            Ue_rhs(ji,jj) =  Ue_rhs(ji,jj) + r1_rho0 * utau(ji,jj) * r1_hu(ji,jj,Kbb)
            Ve_rhs(ji,jj) =  Ve_rhs(ji,jj) + r1_rho0 * vtau(ji,jj) * r1_hv(ji,jj,Kbb)
         END_2D
      ELSE
         zztmp = r1_rho0 * r1_2
         DO_2D( 0, 0, 0, 0 )
            Ue_rhs(ji,jj) =  Ue_rhs(ji,jj) + zztmp * ( utau_b(ji,jj) + utau(ji,jj) ) * r1_hu(ji,jj,Kbb)
            Ve_rhs(ji,jj) =  Ve_rhs(ji,jj) + zztmp * ( vtau_b(ji,jj) + vtau(ji,jj) ) * r1_hv(ji,jj,Kbb)
         END_2D
      ENDIF
      !
      !                             !* atmospheric pressure forcing *!
      IF( ln_apr_dyn ) THEN
         IF( ln_bt_fw ) THEN                          ! FORWARD integration: use kt+1/2 pressure (NOW+1/2)
            DO_2D( 0, 0, 0, 0 )
               Ue_rhs(ji,jj) = Ue_rhs(ji,jj) + grav * (  ssh_ib (ji+1,jj  ) - ssh_ib (ji,jj) ) * r1_e1u(ji,jj)
               Ve_rhs(ji,jj) = Ve_rhs(ji,jj) + grav * (  ssh_ib (ji  ,jj+1) - ssh_ib (ji,jj) ) * r1_e2v(ji,jj)
            END_2D
         ELSE                                         ! CENTRED integration: use kt-1/2 + kt+1/2 pressure (NOW)
            zztmp = grav * r1_2
            DO_2D( 0, 0, 0, 0 )
               Ue_rhs(ji,jj) = Ue_rhs(ji,jj) + zztmp * (  ssh_ib (ji+1,jj  ) - ssh_ib (ji,jj)  &
                    &                                   + ssh_ibb(ji+1,jj  ) - ssh_ibb(ji,jj)  ) * r1_e1u(ji,jj)
               Ve_rhs(ji,jj) = Ve_rhs(ji,jj) + zztmp * (  ssh_ib (ji  ,jj+1) - ssh_ib (ji,jj)  &
                    &                                   + ssh_ibb(ji  ,jj+1) - ssh_ibb(ji,jj)  ) * r1_e2v(ji,jj)
            END_2D
         ENDIF
      ENDIF
      !
      !                             !* snow+ice load *!   (embedded sea ice)
      IF( ln_ice_embd ) THEN
         ALLOCATE( zpice(jpi,jpj) )
         zintp = REAL( MOD( kt-1, nn_fsbc ) ) / REAL( nn_fsbc )
         zgrho0r     = - grav * r1_rho0
         zpice(:,:) = (  zintp * snwice_mass(:,:) + ( 1.- zintp ) * snwice_mass_b(:,:)  ) * zgrho0r
         DO_2D( 0, 0, 0, 0 )
            Ue_rhs(ji,jj) = Ue_rhs(ji,jj) + ( zpice(ji+1,jj) - zpice(ji,jj) ) * r1_e1u(ji,jj)
            Ve_rhs(ji,jj) = Ve_rhs(ji,jj) + ( zpice(ji,jj+1) - zpice(ji,jj) ) * r1_e2v(ji,jj)
         END_2D
         DEALLOCATE( zpice )
      ENDIF
      !
      !                             !* surface wave load *!   (Bernoulli head)
      !
      IF( ln_wave .AND. ln_bern_srfc ) THEN
         DO_2D( 0, 0, 0, 0 )
            Ue_rhs(ji,jj) = Ue_rhs(ji,jj) + ( bhd_wave(ji+1,jj) - bhd_wave(ji,jj) ) * r1_e1u(ji,jj)   !++ bhd_wave from wave model in m2/s2 [BHD parameters in WW3]
            Ve_rhs(ji,jj) = Ve_rhs(ji,jj) + ( bhd_wave(ji,jj+1) - bhd_wave(ji,jj) ) * r1_e1u(ji,jj)
         END_2D
      ENDIF

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      !   RHS of see surface height  Eq.
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !
      ! 		!==  Net water flux forcing  ==!  (applied to a water column)
      !
      IF (ln_bt_fw) THEN                          ! FORWARD integration: use kt+1/2 fluxes (NOW+1/2)
         sshe_rhs(:,:) = r1_rho0 * ( emp(:,:) - rnf(:,:) - fwfisf_cav(:,:) - fwfisf_par(:,:) )
      ELSE                                        ! CENTRED integration: use kt-1/2 + kt+1/2 fluxes (NOW)
         zztmp = r1_rho0 * r1_2
         sshe_rhs(:,:) = zztmp * (   emp(:,:)        + emp_b(:,:)          &
            &                      - rnf(:,:)        - rnf_b(:,:)          &
            &                      - fwfisf_cav(:,:) - fwfisf_cav_b(:,:)   &
            &                      - fwfisf_par(:,:) - fwfisf_par_b(:,:)   )
      ENDIF
      !
      ! 		!==  Stokes drift divergence  ==!   (if exist)
      !
      IF( ln_sdw )    sshe_rhs(:,:) = sshe_rhs(:,:) + div_sd(:,:)
      !
      !
      ! 		!==  ice sheet coupling  ==!
      !
      IF( ln_isf .AND. ln_isfcpl ) THEN
         IF( ln_rstart .AND. kt == nit000 )   sshe_rhs(:,:) = sshe_rhs(:,:) + risfcpl_ssh(:,:)
         IF( ln_isfcpl_cons               )   sshe_rhs(:,:) = sshe_rhs(:,:) + risfcpl_cons_ssh(:,:)
      ENDIF
      !
#if defined key_asminc
      !                 !==  Add the IAU weighted SSH increment  ==!
      !
      IF( lk_asminc .AND. ln_sshinc .AND. ln_asmiau )   sshe_rhs(:,:) = sshe_rhs(:,:) - ssh_iau(:,:)
#endif
      !
#if defined key_agrif
      !                 !==  AGRIF : fill boundary data arrays (on both )
         IF( .NOT.Agrif_Root() )   CALL agrif_dta_ts( kt )
#endif

      !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      !             Compute ssh and (uu_b,vv_b)  at N+1  (Kaa)
      !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      !    using a split-explicit time integration in forward mode 
      !    ( ABM3-AM4 time-integration Shchepetkin et al. OM2005) with temporal diffusion (Demange et al. JCP2019) )

      CALL dyn_spg_ts( kt, Kbb, Kbb, Krhs, uu, vv, ssh, uu_b, vv_b, Kaa ) ! time-splitting
                   

      DEALLOCATE( sshe_rhs , Ue_rhs , Ve_rhs , CdU_u , CdU_v )

!!gm  this is useless I guess : RK3,  done in each stages
!
!      IF( ln_dynspg_ts ) THEN      ! With split-explicit free surface, since now transports have been updated and ssh(:,:,Krhs)
!                                   ! as well as vertical scale factors and vertical velocity need to be updated
!                            CALL div_hor    ( kstp, Kbb, Kmm )                ! Horizontal divergence  (2nd call in time-split case)
!         IF(.NOT.lk_linssh) CALL dom_qco_r3c( ssh(:,:,Kaa), r3t(:,:,Kaa), r3u(:,:,Kaa), r3v(:,:,Kaa), r3f(:,:) )   ! update ssh/h_0 ratio at t,u,v,f pts 
!      ENDIF
!!gm   
      !
      IF( ln_timing )   CALL timing_stop('stp_2D')
      !
   END SUBROUTINE stp_2D


#else
   !!----------------------------------------------------------------------
   !!   default option             EMPTY MODULE           qco not activated
   !!----------------------------------------------------------------------
#endif
   
   !!======================================================================
END MODULE stp2d
