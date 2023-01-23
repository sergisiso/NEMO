MODULE icethd_sal
   !!======================================================================
   !!                       ***  MODULE icethd_sal ***
   !!   sea-ice : computation of salinity variations in the ice
   !!======================================================================
   !! History :   -   !  2003-05  (M. Vancoppenolle) original code 1-D
   !!            3.0  !  2005-12  (M. Vancoppenolle) adapted to the 3-D version
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!---------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_thd_sal      : salinity variations in the ice
   !!   ice_thd_sal_init : initialization
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain
   USE phycst         ! physical constants
   USE ice            ! sea-ice: variables
   USE ice1D          ! sea-ice: thermodynamics variables
   USE icevar         ! sea-ice: operations
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE lib_fortran    ! fortran utilities (glob_sum + no signed zero)
   USE iom            ! I/O manager library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_thd_sal        ! called by icethd
   PUBLIC   ice_thd_sal_init   ! called by ice_init
   
   ! ** namelist (namthd_sal) **
   REAL(wp) ::   rn_sal_gd     ! restoring salinity for gravity drainage [PSU]
   REAL(wp) ::   rn_time_gd    ! restoring time constant for gravity drainage (= 20 days) [s]
   REAL(wp) ::   rn_sal_fl     ! restoring salinity for flushing [PSU]
   REAL(wp) ::   rn_time_fl    ! restoring time constant for gravity drainage (= 10 days) [s]
   INTEGER  ::   nn_sal_scheme ! convection scheme
   LOGICAL  ::   ln_flushing   ! activate flushing
   LOGICAL  ::   ln_drainage   ! activate gravity drainage
   INTEGER  ::   nn_drainage   ! number of subcycles for gravity drainage
   INTEGER  ::   nn_flushing   ! number of subcycles for flushing
   REAL(wp) ::   rn_flushrate  ! rate of flushing (fraction of melt water used for flushing)
   REAL(wp) ::   rn_alpha_CW   ! Brine flow for CW1988
   REAL(wp) ::   rn_alpha_RJW  ! Brine flow for RJW2014
   REAL(wp) ::   rn_alpha_GN   ! Brine flow for GN2013 (kg/m3/s)
   REAL(wp) ::   rn_Rc_RJW     ! critical Rayleigh number for RJW
   REAL(wp) ::   rn_Rc_GN      !                          for GN 
   REAL(wp) ::   rn_sal_himin  ! min ice thickness for gravity drainage and flushing calculation
   REAL(wp) ::   rn_vbrc       ! critical brines volume above which flushing can occur
   
   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: icethd_sal.F90 13472 2020-09-16 13:05:19Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_thd_sal
      !!-------------------------------------------------------------------
      !!                ***  ROUTINE ice_thd_sal  ***    
      !!   
      !! ** Purpose :   computes new salinities in the ice
      !!
      !! ** Method  :  4 possibilities
      !!               -> nn_icesal = 1 -> Sice = cst    [ice salinity constant in both time & space] 
      !!               -> nn_icesal = 2 -> Sice = S(z,t) [Vancoppenolle et al. 2005]
      !!               -> nn_icesal = 3 -> Sice = S(z)   [multiyear ice]
      !!               -> nn_icesal = 4 -> Sice = S(z,t) [Gravity Drainage and Flushing parameterizations]
      !!
      !! ** Case 4 details :
      !!
      !!    For both gravity drainage and flushing, brines are calculated depending on ice temperature (liquidus formulation):
      !!       Sbr = - T / mu                                [linear liquidus]   ( nn_liquidus == 1 )
      !!       Sbr = -18.7 * T - 0.519 * T2 - 0.00535 * T3   [VC2019]            ( nn_liquidus == 2 )
      !!       Sbr = -17.6 * T - 0.389 * T2 - 0.00362 * T3   [Weast]             ( nn_liquidus == 3 )
      !!
      !! ****************
      !! Gravity Drainage
      !! ****************
      !!
      !! we want to solve this equation:
      !! ==============================
      !!    dS/dt = -w dSbr/dz
      !!
      !!    with S   = sea ice salinity
      !!         Sbr = brine salinity
      !!         w   = upwelling Darcy velocity of the return flow (i.e. vertical velocity of the brines, positive downward => >0)
      !!
      !!    discrete form is solved using upward scheme (such as in CICE):
      !!    (S(t+dt)-S(t))/dt = -w(k) * (Sbr(k+1)-Sbr(k))/dz
      !!
      !! 3 schemes are proposed based on the paper from Thomas et al. (2020):
      !! ======================
      !!   0 |  ----------------------------------- surface
      !!     |                           
      !!     |  ----------------------------------- zc
      !!   z |
      !!     |      Ra > Rac => brine convection
      !!     |                          
      !!   h |  ------------------------------------ bottom
      !!     v
      !!
      !!     Ra = cp_br * g * beta * (Sbr(z) - Sw) * perm * (h-z) / (cnd_br*visc)   [RWJ2014 formulation]
      !!
      !!        with Ra     : Rayleigh number
      !!             cp_br  : brine heat capacity         (J/m3/K)
      !!             g      : gravity                     (m/s2)
      !!             beta   : saline density coefficient  (g/kg)-1
      !!             cnd_br : brine thermal conductivity  (W/m/K)
      !!             visc   : brine kinematic viscosity   (m2/s)
      !!             Sw     : ocean salinity              (g/kg)
      !!             zc     : critical depth below which convection occurs (m)
      !!             h      : total ice thickness         (m)
      !!             perm   : effective permeability      (m2)
      !!                      = 3.e-8    * (S/Sbr)^3      [SI3]     ( np_perm_for == 0 )
      !!                      = 1.995e-8 * (S/Sbr)^3.1    [Freitag] ( np_perm_for == 1 )
      !!                      = 1.0e-8   * (S/Sbr)^3.     [RJW2014] ( np_perm_for == 2 )
      !!
      !!    1) === Reese Jones & Worster 2014 (refer to as RJW2014) ===
      !!
      !!        w(z) = - alpha_rjw * cnd_br / cp_br * max(Ra(z)-Rac) * (z-zc)/(h-zc)^2
      !!           with alpha_rjw : intensity parameter
      !!
      !!    2) === Griewank & Notz 2013 (refer to as GN2013) ===
      !!
      !!        w(k) = - alpha_gn/rho * sum( (Ra(kk)-Rac) * dz(kk), [from kk=1 to k] )
      !!           with rho      : brine density       (kg/m3)
      !!                alpha_gn : intensity parameter (kg/m3/s)
      !!
      !!    3) === Cox and Weeks 1988 (refer to as CW1988) ===
      !!
      !!        w(k) = - alpha_cw * 0.0589_wp * MAX( 0._wp, zv_br(z)/rn_vbrc - 1._wp )
      !!           with alpha_cw : intensity parameter
      !!                rn_vbrc  : critical brines volume (for permeability)
      !!
      !! ********
      !! Flushing
      !! ********
      !!
      !! we want to solve this equation:
      !! ==============================
      !!    dS/dt = -w dSbr/dz
      !!
      !!    with Sbr = brine salinity
      !!         w   = upwelling velocity (i.e. vertical velocity of the brines, negative upward => < 0)
      !!
      !!         w   = Fmass / rhob            if v_br > v_brc (= 5%)
      !!             = 0                       otherwise
      !!
      !!           with Fmass = -Flush * rhoi * dh / dt : mass flux (kg/m2/s, >0 since dh<0)
      !!                rhob = rhow * ( 1 + c*Sbr )     : brine density
      !!                v_br = S / Sbr                  : brine volume fraction
      !!                rhoi                            : ice density
      !!                rhow                            : fresh water density (kg/m3)
      !!                c                               : empirical coef (0.8e-3 ‰-1)
      !!           tuning parameters:
      !!                Flush                           : fraction of melt water allowed to percolate thru the ice (30%)
      !!                v_brc                           : critical brine volume above which there is flushing (5%)
      !!
      !!    discrete form is solved using upward scheme (such as in CICE):
      !!    (S(t+dt)-S(t))/dt = -w(k) * (Sbr(k-1)-Sbr(k))/dz
      !!
      !!
      !! ** References
      !! Thomas, M., Vancoppenolle, M., France, J. L., Sturges, W. T., Bakker, D. C. E., Kaiser, J., & von Glasow, R. (2020).
      !!             Tracer measurements in growing sea ice support convective gravity drainage parameterizations.
      !              Journal of Geophysical Research: Oceans, 125, e2019JC015791. https://doi.org/10. 1029/2019JC015791
      !!---------------------------------------------------------------------
      INTEGER  ::   ji, jk, jk1, jk2            ! dummy loop indices 
      REAL(wp) ::   z1_time_gd, z1_time_fl
      !
      ! for gravity drainage and flushing
      INTEGER  ::   iter
      REAL(wp) ::   zh_i, z1_h_i, zhmelt, zc, zcfl, zperm, ztmp, zdt
      REAL(wp), DIMENSION(nlay_i)     ::   z_mid
      REAL(wp), DIMENSION(nlay_i+1)   ::   z_edge 
      REAL(wp), DIMENSION(nlay_i)     ::   zds, zperm_eff, zv_br, zRa, zRae, zw_br 
      REAL(wp), DIMENSION(0:nlay_i+1) ::   zs_br 
      !
      ! permeability
      REAL(wp), PARAMETER ::   np_perm_eff = 2     ! 1 = vertical minimum
      !                                              2 = harmonic mean
      REAL(wp), PARAMETER ::   np_perm_for = 1     ! 0 = SI3 
      !                                              1 = Freitag 99
      !                                              2 = RJW 2014
      ! Rayleigh
      REAL(wp), PARAMETER ::   zcp_br    = 4.e6    ! heat capacity of brine (J/m3)
      REAL(wp), PARAMETER ::   zbeta     = 7.5e-4  ! saline density coefficient (g/kg)-1
      REAL(wp), PARAMETER ::   zcnd_br   = 0.523   ! thermal conductivity of brine W/m/K
      REAL(wp), PARAMETER ::   zvisc     = 1.8e-6  ! Kinematic viscosity of brine
      ! GN scheme constant
      REAL(wp), PARAMETER ::   zrhob_GN  = 1020.   ! Brine density (kg/m3)
      !
      ! for sanity checks
      REAL(wp) ::   zmiss, zs_min, zds_max, zcfl_max
      !!---------------------------------------------------------------------
      !
      ! sanity check
      IF( ln_sal_chk)   CALL iom_miss_val( 'icetemp', zmiss )   ! get missing value from xml
      !
      SELECT CASE ( nn_icesal )
      !
      !               !---------------------------------------------!
      CASE( 2 )       !  time varying salinity with linear profile  !
         !            !---------------------------------------------!
         IF( ln_drainage ) THEN   ;   z1_time_gd = rDt_ice / rn_time_gd
         ELSE                     ;   z1_time_gd = 0._wp
         ENDIF
         IF( ln_flushing ) THEN   ;   z1_time_fl = rDt_ice / rn_time_fl
         ELSE                     ;   z1_time_fl = 0._wp
         ENDIF
         !         
         DO ji = 1, npti
            !
            IF( h_i_1d(ji) > rn_sal_himin ) THEN
               !
               ! --- Update ice salinity from brine drainage and flushing --- !
               IF( t_su_1d(ji) >= rt0 ) THEN             ! flushing (summer time)
                  zds(1) = - MAX( s_i_1d(ji) - rn_sal_fl , 0._wp ) * z1_time_fl
               ELSEIF( t_su_1d(ji) <= t_bo_1d(ji) ) THEN ! gravity drainage
                  zds(1) = - MAX( s_i_1d(ji) - rn_sal_gd , 0._wp ) * z1_time_gd
               ELSE
                  zds(1) = 0._wp
               ENDIF
               ! update salinity
               s_i_1d(ji) = s_i_1d(ji) + zds(1)
               ! salt flux
               sfx_bri_1d(ji) = sfx_bri_1d(ji) - rhoi * a_i_1d(ji) * h_i_1d(ji) * zds(1) * r1_Dt_ice
               !
               ! --- salinity must stay inbounds --- !
               IF( ln_drainage .OR. ln_flushing ) THEN
                  zds(1) =          MAX( 0._wp, rn_simin            - s_i_1d(ji) ) ! > 0 if s_i < simin
                  zds(1) = zds(1) + MIN( 0._wp, rn_sinew*sss_1d(ji) - s_i_1d(ji) ) ! < 0 if s_i > simax
                  ! update salinity
                  s_i_1d(ji) = s_i_1d(ji) + zds(1)
                  ! salt flux
                  sfx_res_1d(ji) = sfx_res_1d(ji) - rhoi * a_i_1d(ji) * h_i_1d(ji) * zds(1) * r1_Dt_ice
               ENDIF
               !
            ENDIF
            !
         END DO
         !
         ! Salinity profile (gives sz_i)
         CALL ice_var_salprof1d
         !
         !             !----------------------------------------!
      CASE( 3 )        ! constant salinity with a fixed profile ! (Schwarzacher (1959) multiyear salinity profile (mean = 2.30)
         !             !----------------------------------------!
         CALL ice_var_salprof1d
         !
         !             !--------------------------------!
      CASE( 4 )        ! Gravity Drainage and Flushing  !
         !             !--------------------------------!
                        
         ! Initialization
         ! ==============         
         DO jk = 1, nlay_i
            z_mid(jk) = ( REAL( jk ) - 0.5_wp ) * r1_nlay_i
         END DO
         DO jk = 1, nlay_i+1
            z_edge(jk) = ( REAL( jk ) - 1._wp ) * r1_nlay_i
         END DO

         ! Gravity Drainage
         ! ================
         IF( ln_drainage ) THEN
            !
            zdt   = rDt_ice / REAL( nn_drainage )
            !
            DO ji = 1, npti
               ! ice thickness ( we do not want to do anything for salt when ice is thinner than the minimum allowed )
               IF( h_i_1d(ji) >= rn_sal_himin ) THEN   ;   z1_h_i = 1._wp / ( h_i_1d(ji) * r1_nlay_i )
               ELSE                                    ;   z1_h_i = 0._wp
               ENDIF
               !
               ! surface melting (m)
               zhmelt = dh_s_sum(ji) + dh_i_sum(ji) ! =0 if no melt, <0 otherwise 
               !
               ! iteration to converge (usually 10 is ok)
               zcfl_max = 0._wp
               zds_max  = 0._wp
               zs_min   = 0._wp
               DO iter = 1, nn_drainage
                  !
                  CALL ice_brine( sss_1d(ji), t_i_1d(ji,:), sz_i_1d(ji,:), zs_br, zv_br )

                  IF( h_i_1d(ji) >= rn_sal_himin .AND. MAXVAL( zs_br(:) ) > sss_1d(ji) .AND. zhmelt >= 0._wp ) THEN
                     !                                                                       during melting season, salt flux can turn upward with these schemes
                     ! Effective permeability
                     ! ----------------------
                     IF( np_perm_eff == 1 ) THEN ! Minimum

                        DO jk = 1, nlay_i
                           IF    ( np_perm_for == 0 ) THEN ; zperm_eff(jk) = 3.e-8_wp    * ( MINVAL( zv_br(jk:nlay_i) ) )**3.  ! SI3
                           ELSEIF( np_perm_for == 1 ) THEN ; zperm_eff(jk) = 1.995e-8_wp * ( MINVAL( zv_br(jk:nlay_i) ) )**3.1 ! Freitag
                           ELSEIF( np_perm_for == 2 ) THEN ; zperm_eff(jk) = 1.e-8_wp    * ( MINVAL( zv_br(jk:nlay_i) ) )**3.  ! Rees Jones and Worster
                           END IF                                                                                           !    -> this case leads to bizarre results
                        END DO

                     ELSEIF( np_perm_eff == 2 ) THEN ! Harmonic Mean

                        DO jk1 = 1, nlay_i
                           ztmp = 0._wp
                           DO jk2 = jk1, nlay_i
                              IF    ( np_perm_for == 0 ) THEN ; zperm = 3.e-8_wp    * zv_br(jk2)**3.  ! SI3
                              ELSEIF( np_perm_for == 1 ) THEN ; zperm = 1.995e-8_wp * zv_br(jk2)**3.1 ! Freitag
                              ELSEIF( np_perm_for == 2 ) THEN ; zperm = 1.e-8_wp    * zv_br(jk2)**3.  ! Rees Jones and Worster
                              END IF
                              ztmp = ztmp + 1._wp / zperm
                           END DO
                           zperm_eff(jk1) = REAL( nlay_i - jk1 + 1 ) / ztmp
                        END DO

                     END IF

                     ! Rayleigh number
                     ! ---------------
                     ! Ra = cp_br * g * beta * (Sbr(z) - Sw) * perm * (h-z) / (cnd_br*visc)   [RWJ2014 formulation]
                     DO jk = 1, nlay_i
                        zRa(jk) = zcp_br * grav * zbeta * MAX( 0., zs_br(jk) - sss_1d(ji)) * zperm_eff(jk) &
                           &                            *  h_i_1d(ji) * ( 1._wp - z_mid(jk) ) / ( zcnd_br * zvisc )
                     END DO

                     ! Vertical velocity
                     ! -----------------
                     IF ( nn_sal_scheme == 1 ) THEN     ! *** RJW 2014 ***
                        !
                        ! if Ra is everywhere < Rc : no convection => Rae =0
                        ! else                     : convection until zc => Rae /= 0
                        zc = 0._wp
                        ztmp = 0._wp
                        DO jk = nlay_i,1,-1
                           IF ( zRa(jk) >= rn_Rc_RJW ) THEN
                              ztmp = MAX( ztmp , zRa(jk) - rn_Rc_RJW )
                              zc = z_edge(jk)
                           END IF
                           zRae(jk) = ztmp 
                        END DO

                        DO jk = 1, nlay_i
                           zw_br(jk) = - rn_alpha_RJW * zRae(jk) * ( zcnd_br / zcp_br ) * &
                              &                                  ( z_mid(jk) - zc ) / ( h_i_1d(ji) * ( 1 - zc )**2 )
                        END DO

                     ELSEIF ( nn_sal_scheme == 2 ) THEN    ! *** GN 2013 ***
                        DO jk = 1, nlay_i
                           zw_br(jk) = - rn_alpha_GN / zrhob_GN * SUM ( ( zRa(1:jk) - rn_Rc_GN ) * h_i_1d(ji) * r1_nlay_i )
                        END DO

                     ELSEIF ( nn_sal_scheme == 3 ) THEN    ! *** CW 1988 ***
                        DO jk = 1, nlay_i
                           zw_br(jk) = - rn_alpha_CW  * 0.0589_wp * MAX( 0._wp, zv_br(jk)/rn_vbrc - 1._wp )
                        END DO

                     END IF

                     ! Salinity
                     ! --------
                     ! upstream scheme as in CICE: ds = -w(k)*dt/dz * ( s_br(k+1) - s_br(k) ), w > 0 or < 0
                     DO jk = 1, nlay_i
                        !
                        zcfl = zw_br(jk) * zdt * z1_h_i
                        zds(jk) = - zcfl * ( zs_br(jk+1) - zs_br(jk) )
                        !
                        zcfl_max = MAX( zcfl_max, ABS(zcfl) )
                        zs_min   = MIN( zs_min , sz_i_1d(ji,jk) + zds(jk) ) ! record what salinity would be without the trick below
                     ENDDO
                     IF( ln_sal_chk)   zds_max = MAX( zds_max, MAX( 0._wp, SUM( zds(:) ) * r1_nlay_i )*REAL(nn_drainage) )

                     DO jk = 1, nlay_i
                        !
                        !!clem trick
                        zds(jk) = MAX( zds(jk), -sz_i_1d(ji,jk)+rn_simin )
                        !
                        ! new salinity
                        sz_i_1d(ji,jk) = sz_i_1d(ji,jk) + zds(jk)
                        !
                        ! salt flux
                        sfx_bri_1d(ji) = sfx_bri_1d(ji) - rhoi * a_i_1d(ji) * h_i_1d(ji) * r1_nlay_i * zds(jk) * r1_Dt_ice ! r1_Dt_ice is ok
                        !
                     END DO

                  ENDIF

               END DO

               ! sanity check
               IF( ln_sal_chk) THEN
                  cfl_drain_1d(ji) = zcfl_max 
                  IF( zds_max > 0._wp ) THEN   ;   s_drain_dserr_1d(ji) = zds_max ; t_drain_dserr_1d(ji,:) = t_i_1d(ji,:)-rt0
                  ELSE                         ;   s_drain_dserr_1d(ji) = zmiss   ; t_drain_dserr_1d(ji,:) = zmiss
                  ENDIF
                  IF( zs_min < 0._wp )  THEN   ;   s_drain_serr_1d(ji) = zs_min ; t_drain_serr_1d(ji,:) = t_i_1d(ji,:)-rt0
                  ELSE                         ;   s_drain_serr_1d(ji) = zmiss  ; t_drain_serr_1d(ji,:) = zmiss
                  ENDIF
               ENDIF

            ENDDO
         ENDIF

         ! Flushing
         ! ========
         IF( ln_flushing ) THEN

            zdt   = rDt_ice / REAL( nn_flushing )
            !
            DO ji = 1, npti
               ! ice thickness ( we do not want to do anything for salt when ice is thinner than the minimum allowed )
               IF( h_i_1d(ji) >= rn_sal_himin ) THEN   ;   z1_h_i = 1._wp / ( h_i_1d(ji) * r1_nlay_i )
               ELSE                                    ;   z1_h_i = 0._wp
               ENDIF
               !
               ! surface melting (m)
               zhmelt = dh_s_sum(ji) + dh_i_sum(ji) ! =0 if no melt, <0 otherwise 
               !
               ! iteration to converge (usually 1 is ok)
               zcfl_max = 0._wp
               zds_max  = 0._wp
               zs_min   = 0._wp
               DO iter = 1, nn_flushing
                  !
                  CALL ice_brine( sss_1d(ji), t_i_1d(ji,:), sz_i_1d(ji,:), zs_br, zv_br )
                  !
                  IF(        zhmelt < 0._wp                    &      ! Flushing if  surface melting
                                !& .AND. t_i_1d(ji,1) >= t_i_1d(ji,nlay_i) &      !          and surface temperature is warmer than bottom temperature
                     & .AND. MINVAL(zv_br(:)) >= rn_vbrc       ) THEN !          and brine volume fraction exceeds a certain treshold
                     !
                     ! Vertical velocity
                     ! -----------------
                     DO jk = 1, nlay_i
                        zw_br(jk) = -rn_flushrate * ( dh_i_sum(ji)*rhoi + dh_s_sum(ji)*rhos )  &
                           &                 / ( rhow * ( 1._wp + 0.8e-3_wp * zs_br(jk) ) ) * r1_Dt_ice ! r1_Dt_ice is ok
                        !                        can be replaced by rhow but in theory rhow should be rho_br = (rho0*(1+c*S_br)), with c = 0.8e-3
                     ENDDO

                     ! Salinity
                     ! --------
                     DO jk = 1, nlay_i
                        ! upstream scheme as in CICE: ds = -w*dt/dz * ( s_br(k) - s_br(k-1) ), w > 0
                        ! zcfl = w*dt/dz                                 
                        zcfl = zw_br(jk) * zdt * z1_h_i
                        !
                        zcfl_max = MAX( zcfl_max, ABS(zcfl) )
                        !
                        zds(jk) = - zcfl * ( zs_br(jk) - zs_br(jk-1) )
                        !
                        zs_min  = MIN( zs_min , sz_i_1d(ji,jk) + zds(jk) )   ! record what salinity would be without the trick below
                        !
                     ENDDO
                     IF( ln_sal_chk)   zds_max = MAX( zds_max, MAX( 0._wp, SUM( zds(:) ) * r1_nlay_i )*REAL(nn_flushing) )

                     DO jk = 1, nlay_i
!!$                           zds(jk) = MIN( 0._wp, zds(jk) ) ! min to block flushing when temperature profile is not ok
                        !
                        ! new salinity
                        sz_i_1d(ji,jk) = sz_i_1d(ji,jk) + zds(jk)
                        !
                        ! salt flux
                        sfx_bri_1d(ji) = sfx_bri_1d(ji) - rhoi * a_i_1d(ji) * h_i_1d(ji) * r1_nlay_i * zds(jk) * r1_Dt_ice ! r1_Dt_ice is ok

                     ENDDO
                  ENDIF

               END DO

               ! sanity check
               IF( ln_sal_chk) THEN
                  cfl_flush_1d(ji) = zcfl_max 
                  IF( zds_max > 0._wp ) THEN   ;   s_flush_dserr_1d(ji) = zds_max ; t_flush_dserr_1d(ji,:) = t_i_1d(ji,:)-rt0
                  ELSE                         ;   s_flush_dserr_1d(ji) = zmiss   ; t_flush_dserr_1d(ji,:) = zmiss
                  ENDIF
                  IF( zs_min < 0._wp ) THEN    ;   s_flush_serr_1d(ji) = zs_min ; t_flush_serr_1d(ji,:) = t_i_1d(ji,:)-rt0
                  ELSE                         ;   s_flush_serr_1d(ji) = zmiss  ; t_flush_serr_1d(ji,:) = zmiss
                  ENDIF
               ENDIF

            ENDDO
         ENDIF

         ! --- salinity must stay inbounds --- !
         IF( ln_drainage .OR. ln_flushing ) THEN
            DO ji = 1, npti
               DO jk = 1, nlay_i
                  zds(jk) =           MAX( 0._wp, rn_simin            - sz_i_1d(ji,jk) ) ! > 0 if s_i < simin
                  zds(jk) = zds(jk) + MIN( 0._wp, rn_sinew*sss_1d(ji) - sz_i_1d(ji,jk) ) ! < 0 if s_i > simax
                  ! update salinity
                  sz_i_1d(ji,jk) = sz_i_1d(ji,jk) + zds(jk)
                  ! salt flux
                  sfx_res_1d(ji) = sfx_res_1d(ji) - rhoi * a_i_1d(ji) * h_i_1d(ji) * r1_nlay_i * zds(jk) * r1_Dt_ice
               END DO
            ENDDO
         ENDIF
                
      END SELECT
      !
   END SUBROUTINE ice_thd_sal


   SUBROUTINE ice_brine( zsss, pt_i, ps_i, ps_br, pv_br )
      !!-------------------------------------------------------------------
      !!                ***  ROUTINE ice_brine ***
      !!
      !! ** Purpose :   computes brine volume fraction (%)
      !!                         and salinity of the brine in sea ice
      !!
      !!-------------------------------------------------------------------
      INTEGER  ::   ji, jk   ! dummy loop indices
      REAL(wp) ::   zt1, zt2, zt3
      REAL(wp),                        INTENT(in   ) ::   zsss
      REAL(wp), DIMENSION(1:nlay_i)  , INTENT(in   ) ::   pt_i, ps_i
      REAL(wp), DIMENSION(0:nlay_i+1), INTENT(  out) ::   ps_br
      REAL(wp), DIMENSION(1:nlay_i)  , INTENT(  out) ::   pv_br
      !!-------------------------------------------------------------------
      !
      ! brines
      DO jk = 1, nlay_i
         ! brine salinity
         zt1 = pt_i(jk) - rt0
         zt2 = zt1 * zt1
         zt3 = zt2 * zt1
         IF    ( nn_liquidus == 1 ) THEN ; ps_br(jk) = - zt1 / rTmlt                                      ! --- Linear liquidus
         ELSEIF( nn_liquidus == 2 ) THEN ; ps_br(jk) = -18.7_wp * zt1 - 0.519_wp * zt2 - 0.00535_wp * zt3 ! --- 3rd order liquidus, VC19
         ELSEIF( nn_liquidus == 3 ) THEN ; ps_br(jk) = -17.6_wp * zt1 - 0.389_wp * zt2 - 0.00362_wp * zt3 ! --- Weast 71 liquidus in RJW14
         ENDIF
         ! brine volume fraction
         IF( zt1 < - epsi06 ) THEN
            pv_br(jk) = ps_i(jk) / ps_br(jk)
         ELSE
            pv_br(jk) = 0._wp
         ENDIF
      ENDDO
      ! brine salinity at the interfaces
      ps_br(0) = 0._wp
      ps_br(nlay_i+1) = zsss
      !
   END SUBROUTINE ice_brine

   
   SUBROUTINE ice_thd_sal_init
      !!-------------------------------------------------------------------
      !!                  ***  ROUTINE ice_thd_sal_init  ***
      !!
      !! ** Purpose :   initialization of ice salinity parameters
      !!
      !! ** Method  :   Read the namthd_sal namelist and check the parameter
      !!                values called at the first timestep (nit000)
      !!
      !! ** input   :   Namelist namthd_sal
      !!-------------------------------------------------------------------
      INTEGER  ::   ios   ! Local integer
      !!
      NAMELIST/namthd_sal/ nn_icesal, ln_flushing, ln_drainage, rn_sinew, rn_simin, &
         &                 rn_icesal, rn_sal_gd, rn_time_gd, rn_sal_fl, rn_time_fl, &
         &                 rn_sal_himin, nn_liquidus, nn_drainage, nn_flushing, rn_flushrate, rn_vbrc, &
         &                 nn_sal_scheme, rn_alpha_RJW, rn_Rc_RJW, rn_alpha_GN, rn_Rc_GN, rn_alpha_CW, ln_sal_chk
      !!-------------------------------------------------------------------
      !
      READ  ( numnam_ice_ref, namthd_sal, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namthd_sal in reference namelist' )
      READ  ( numnam_ice_cfg, namthd_sal, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namthd_sal in configuration namelist' )
      IF(lwm) WRITE ( numoni, namthd_sal )
      !
      IF(lwp) THEN                           ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'ice_thd_sal_init : Ice parameters for salinity '
         WRITE(numout,*) '~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namthd_sal:'
         WRITE(numout,*) '      switch for salinity                                     nn_icesal     = ', nn_icesal
         WRITE(numout,*) '      activate flushing                                       ln_flushing   = ', ln_flushing
         WRITE(numout,*) '      activate gravity drainage                               ln_drainage   = ', ln_drainage
         WRITE(numout,*) '      New ice salinity (fraction of sss)                      rn_sinew      = ', rn_sinew
         WRITE(numout,*) '      Minimum tolerated ice salinity                          rn_simin      = ', rn_simin
                   ! -- nn_icesal=1 -- !
         WRITE(numout,*) '      bulk salinity value if nn_icesal = 1                    rn_icesal     = ', rn_icesal
                  ! -- nn_icesal=2 -- !
         WRITE(numout,*) '      restoring salinity for gravity drainage                 rn_sal_gd     = ', rn_sal_gd
         WRITE(numout,*) '      restoring time for for gravity drainage                 rn_time_gd    = ', rn_time_gd
         WRITE(numout,*) '      restoring salinity for flushing                         rn_sal_fl     = ', rn_sal_fl
         WRITE(numout,*) '      restoring time for flushing                             rn_time_fl    = ', rn_time_fl
                  ! -- nn_icesal=4 -- !
         WRITE(numout,*) '      min ice thickness for drainage and flushing             rn_sal_himin  = ', rn_sal_himin
         WRITE(numout,*) '      liquidous formulation (1=linear, 2=VC2019, 3=Weast71)   nn_liquidus   = ', nn_liquidus
         WRITE(numout,*) '      number of subcycles for gravity drainage                nn_drainage   = ', nn_drainage
         WRITE(numout,*) '      number of subcycles for flushing                        nn_flushing   = ', nn_flushing
         WRITE(numout,*) '      fraction of melt water used for flushing                rn_flushrate  = ', rn_flushrate
         WRITE(numout,*) '      critical brines volume above which flushing can occur   rn_vbrc       = ', rn_vbrc
         WRITE(numout,*) '      convection scheme (1=RJW2014, 2=GN2013, 3=CW88)         nn_sal_scheme = ', nn_sal_scheme
         WRITE(numout,*) '      brine flow for RJW2014 scheme                           rn_alpha_RJW  = ', rn_alpha_RJW
         WRITE(numout,*) '      critical Rayleigh number for RJW2014 scheme             rn_Rc_RJW     = ', rn_Rc_RJW
         WRITE(numout,*) '      brine flow for GN2013 scheme (kg/m3/s)                  rn_alpha_GN   = ', rn_alpha_GN
         WRITE(numout,*) '      critical Rayleigh number for GN2013 scheme              rn_Rc_GN      = ', rn_Rc_GN
         WRITE(numout,*) '      brine flow for CW1988 scheme                            rn_alpha_CW   = ', rn_alpha_CW
         WRITE(numout,*) '      sanity checks (output diags)                            ln_sal_chk    = ', ln_sal_chk
      ENDIF
      !
      IF( nn_icesal /= 4 )   ln_sal_chk=.FALSE. ! option only valid for nn_icesal = 4
      !
   END SUBROUTINE ice_thd_sal_init

#else
   !!----------------------------------------------------------------------
   !!   Default option         Dummy Module           No SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE icethd_sal
