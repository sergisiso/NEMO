MODULE sbcfwb
   !!======================================================================
   !!                       ***  MODULE  sbcfwb  ***
   !! Ocean fluxes   : domain averaged freshwater budget
   !!======================================================================
   !! History :  OPA  ! 2001-02  (E. Durand)  Original code
   !!   NEMO     1.0  ! 2002-06  (G. Madec)  F90: Free form and module
   !!            3.0  ! 2006-08  (G. Madec)  Surface module
   !!            3.2  ! 2009-07  (C. Talandier) emp mean s spread over erp area 
   !!            3.6  ! 2014-11  (P. Mathiot  ) add ice shelf melting
   !!            4.2  ! 2022-12  (J. Chanut   ) Compatibility with AGRIF
   !!                                            + analytical cycle 
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sbc_fwb       : freshwater budget for global ocean configurations (free surface & forced mode)
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE sbc_oce        ! surface ocean boundary condition
   USE isf_oce , ONLY : fwfisf_cav, fwfisf_par                    ! ice shelf melting contribution
   USE sbc_ice , ONLY : snwice_mass_b, snwice_fmass
   USE phycst         ! physical constants
   USE sbcrnf         ! ocean runoffs
   USE sbcssr         ! Sea-Surface damping terms
#if defined key_agrif
   USE agrif_oce , ONLY: Kmm_a
#endif
   !
   USE in_out_manager ! I/O manager
   USE iom            ! IOM
   USE lib_mpp        ! distribued memory computing library
   USE timing         ! Timing
   USE lbclnk         ! ocean lateral boundary conditions
   USE lib_fortran    ! 

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc_fwb    ! routine called by step

   REAL(wp) ::   rn_fwb0     ! initial freshwater adjustment flux [kg/m2/s] (nn_fwb = 2 only)
   INTEGER  ::   nn_fwb_voltype
   LOGICAL  ::   ln_hvolg_var ! Prescribed a time varying volume
   REAL(wp) ::   rn_hvolg_amp ! Global seasonal volume height amplitude (m)
   REAL(wp) ::   rn_hvolg_trd ! Global seasonal volume height trend (m/s)
   INTEGER  ::   nn_hvolg_mth ! Month when global volume height starts to rise
   REAL(wp) ::   a_fwb     ! annual domain averaged freshwater budget from the previous year
   REAL(wp) ::   a_fwb_b   ! annual domain averaged freshwater budget from the year before or at initial state
   REAL(wp) ::   area      ! global mean ocean surface (interior domain)
   REAL(wp) ::   emp_corr  ! current, globally constant, emp correction
   REAL(wp) ::   emp_ext   ! prescribed emp flux
   REAL(wp) ::   hvolg_n, hvolg_a  ! Now and future equivalent height to prescribe 
#if defined key_agrif
!$AGRIF_DO_NOT_TREAT
   REAL(wp), ALLOCATABLE, DIMENSION(:) :: agrif_tmp ! temporary array holding values for each grid
!$AGRIF_END_DO_NOT_TREAT
#endif

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: sbcfwb.F90 15439 2021-10-22 17:53:09Z clem $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE sbc_fwb( kt, kn_fwb, kn_fsbc, Kmm )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE sbc_fwb  ***
      !!
      !! ** Purpose :   Control the mean sea surface drift
      !!
      !! ** Method  :   several ways  depending on kn_fwb
      !!                =0 no control 
      !!                =1 global mean of emp set to zero at each nn_fsbc time step
      !!                =2 annual global mean corrected from previous year
      !!                =3 global mean of emp set to zero at each nn_fsbc time step
      !!                   & spread out over erp area depending its sign
      !! Note: if sea ice is embedded it is taken into account when computing the budget 
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt       ! ocean time-step index
      INTEGER, INTENT( in ) ::   kn_fsbc  ! 
      INTEGER, INTENT( in ) ::   kn_fwb   ! ocean time-step index
      INTEGER, INTENT( in ) ::   Kmm      ! ocean time level index
      !
      INTEGER  ::   ios, inum, ikty, igrid       ! local integers
      INTEGER  ::   ji, jj, istart, iend, jstart, jend
      REAL(wp) ::   z_fwf, z_fwf_nsrf, zsum_fwf, zsum_erp         ! local scalars
      REAL(wp) ::   zsurf_neg, zsurf_pos, zsurf_tospread          !   -      -
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   ztmsk_neg, ztmsk_pos, z_wgt ! 2D workspaces
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   ztmsk_tospread, zerp_cor    !   -      -
      REAL(wp)   ,DIMENSION(1) ::   z_fwfprv  
      COMPLEX(dp),DIMENSION(1) ::   y_fwfnow  
      !
      NAMELIST/namsbc_fwb/rn_fwb0, nn_fwb_voltype, ln_hvolg_var, rn_hvolg_amp, rn_hvolg_trd, nn_hvolg_mth
      !!----------------------------------------------------------------------
      !
      IF( kt == nit000 ) THEN
         READ( numnam_ref, namsbc_fwb, IOSTAT = ios, ERR = 901 )
901      IF( ios /= 0 ) CALL ctl_nam( ios, 'namsbc_fwb in reference namelist'     )
         READ( numnam_cfg, namsbc_fwb, IOSTAT = ios, ERR = 902 )
902      IF( ios >  0 ) CALL ctl_nam( ios, 'namsbc_fwb in configuration namelist' )
         IF(lwm) WRITE( numond, namsbc_fwb )
#if defined key_agrif
         IF ( .NOT.Agrif_Root() ) THEN ! Copy namelist values from parent (for print)
            nn_fwb_voltype = Agrif_parent(nn_fwb_voltype)   
            rn_fwb0        = Agrif_parent(rn_fwb0)
            ln_hvolg_var   = Agrif_parent(ln_hvolg_var)
            rn_hvolg_amp   = Agrif_parent(rn_hvolg_amp)
            rn_hvolg_trd   = Agrif_parent(rn_hvolg_trd)
            nn_hvolg_mth   = Agrif_parent(nn_hvolg_mth)
         ENDIF
#endif

         IF(lwp) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'sbc_fwb : FreshWater Budget correction'
            WRITE(numout,*) '~~~~~~~'
            SELECT CASE ( kn_fwb )
            CASE ( 1 )
               WRITE(numout,*) '          nn_fwb = 1: Volume set to zero at each time step'
               WRITE(numout,*) '          => uniform correction to emp'
            CASE(  2 )
               WRITE(numout,*) '          nn_fwb = 2: Volume adjusted from previous year budget'
               WRITE(numout,*) '          => uniform correction to emp'
               WRITE(numout,*) '   Namelist namsbc_fwb'
               WRITE(numout,*) '      Initial freshwater adjustment flux [kg/m2/s] = ', rn_fwb0
            CASE( 3 )
               WRITE(numout,*) '          nn_fwb = 3: Volume set to zero at each time step'
               WRITE(numout,*) '          => non-uniform correction proportional to erp'
            CASE DEFAULT
               CALL ctl_stop( 'sbc_fwb : wrong nn_fwb value for the FreshWater Budget correction, choose either 1, 2 or 3' )
            END SELECT
            !
            SELECT CASE ( nn_fwb_voltype )
            CASE( 1 )
               WRITE(numout,*) ' '
               WRITE(numout,*) '          nn_fwb_voltype = 1: Control ICE + OCEAN volume'
               WRITE(numout,*) ' '
            CASE( 2 )
               WRITE(numout,*) ' '
               WRITE(numout,*) '          nn_fwb_voltype = 2: Control OCEAN volume'
               WRITE(numout,*) ' '
            CASE DEFAULT
               CALL ctl_stop( 'sbc_fwb : wrong nn_fwb_voltype value for the FreshWater Budget correction, choose either 1 or 2' )
            END SELECT
            !
            IF (ln_hvolg_var) THEN
               WRITE(numout,*) ' '
               WRITE(numout,*) '          ln_hvolg_var = T: specify the global volume variation'
               WRITE(numout,*) '          Seasonnal height amplitude [m]:              ', rn_hvolg_amp
               WRITE(numout,*) '          Volume anomaly crosses zero at month (1:12): ', nn_hvolg_mth
               WRITE(numout,*) '          Trend [m/s]:                                 ', rn_hvolg_trd
               WRITE(numout,*) ' '
            ELSE
               WRITE(numout,*) ' '
               WRITE(numout,*) '          ln_hvolg_var = F: no specification of the volume variation '
               WRITE(numout,*) ' '
            ENDIF   
         ENDIF
         !
         IF( kn_fwb == 3 .AND. nn_sssr /= 2 )   CALL ctl_stop( 'sbc_fwb: nn_fwb = 3 requires nn_sssr = 2, we stop ' )
         IF( kn_fwb == 3 .AND. ln_isfcav    )   CALL ctl_stop( 'sbc_fwb: nn_fwb = 3 with ln_isfcav = .TRUE. not working, we stop ' )
#if defined key_agrif
         IF((kn_fwb == 3).AND.(Agrif_maxlevel()/=0)) CALL ctl_stop( 'sbc_fwb: nn_fwb = 3 not yet implemented with AGRIF zooms ' ) 
#endif
         !
         IF ( Agrif_Root() ) THEN
#if defined key_agrif
            ALLOCATE(agrif_tmp(Agrif_nb_fine_grids()+1))
            agrif_tmp(:) = 1.e+40_dp                     ! Initialize to a big value
            agrif_tmp(1) = glob_sum( 'sbcfwb', e1e2t(:,:) * tmask_agrif(:,:)) ! Coarse grid value
            CALL Agrif_step_child_adj(glob_sum_area_agrif)                    ! Get value over child grids
            CALL mpp_min('sbcfwb', agrif_tmp(:)) ! Required with // sisters to populate the value of each grid on each processor
            area = SUM(agrif_tmp)                ! Sum over all grids
            IF (lwp) WRITE(numout,*) 'Domain area for each agrif grid (km**2):'
            DO igrid = 1, Agrif_nb_fine_grids() + 1
               IF (lwp) WRITE(numout,*) '                                        ', igrid, agrif_tmp(igrid)/1000._wp/1000._wp
            END DO
#else
            area = glob_sum( 'sbcfwb', e1e2t(:,:) * tmask(:,:,1))         ! interior global domain surface
#endif
            IF (lwp) WRITE(numout,*) 'Total Domain area (km**2):', area/1000._wp/1000._wp
            !
            IF ( ln_hvolg_var ) THEN
               ! get global ssh at "now" time step:
               CALL set_hglo_ana(kt,  0, rn_hvolg_amp, nn_hvolg_mth, rn_hvolg_trd, hvolg_a)
            ELSE
               hvolg_n  = 0._wp
               hvolg_a  = 0._wp
               emp_ext  = 0._wp
            ENDIF   
         ENDIF
         ! isf cavities are excluded because it can feedback to the melting with generation of inhibition of plumes
         ! and in case of no melt, it can generate HSSW.
         !
         IF( nn_ice == 0 ) THEN
            snwice_mass_b(:,:) = 0.e0               ! no sea-ice model is being used : no snow+ice mass
            snwice_fmass (:,:) = 0.e0
         ENDIF
         !
      ENDIF


         ! If needed, define the volume change to prescribe:
#if defined key_agrif
      IF ( Agrif_Root() ) THEN
#endif
         IF ( ln_hvolg_var ) THEN
            IF( MOD( kt-1, kn_fsbc ) == 0 ) THEN
               hvolg_n  = hvolg_a
               CALL set_hglo_ana(kt,  kn_fsbc, rn_hvolg_amp, nn_hvolg_mth, rn_hvolg_trd, hvolg_a)
               ! prescribed volume change leads to the following freshwater flux:
               emp_ext = -rho0 * (hvolg_a - hvolg_n) / ( rn_Dt * REAL(kn_fsbc, wp) ) 
            ENDIF   
         ENDIF
#if defined key_agrif
      ENDIF
#endif

      SELECT CASE ( kn_fwb )
      !
      CASE ( 1 )                             !==  set volume at each time step  ==!
         !
#if defined key_agrif                                            
         IF ( Agrif_Root() ) THEN
            IF ( Agrif_maxlevel()==0 ) THEN
               ! No child grid, correct "now" fluxes (i.e. as in the "no agrif" case)
#endif
               IF( MOD( kt-1, kn_fsbc ) == 0 ) THEN
                  SELECT CASE (nn_fwb_voltype)
                  CASE( 1 )
                     z_fwfprv(1) = glob_sum( 'sbcfwb', e1e2t(:,:) * (  emp(:,:) - rnf(:,:)                & 
                               &                                      - fwfisf_cav(:,:) - fwfisf_par(:,:) & 
                               &                                      - snwice_fmass(:,:) ) )
                     !y_fwfnow(1) = local_sum( e1e2t(:,:) * ( emp(:,:) - rnf(:,:) - fwfisf_cav(:,:) - fwfisf_par(:,:) - snwice_fmass(:,:) ) )
                     !CALL mpp_delay_sum( 'sbcfwb', 'fwb', y_fwfnow(:), z_fwfprv(:), kt == nitend - nn_fsbc + 1 )
                  CASE( 2 )
                     z_fwfprv(1)  = glob_sum( 'sbcfwb', e1e2t(:,:) * (  emp(:,:) - rnf(:,:)                & 
                                &                                      - fwfisf_cav(:,:) - fwfisf_par(:,:) ))
                     !y_fwfnow(1) = local_sum( e1e2t(:,:) * ( emp(:,:) - rnf(:,:) - fwfisf_cav(:,:) - fwfisf_par(:,:)  ) )
                     !CALL mpp_delay_sum( 'sbcfwb', 'fwb', y_fwfnow(:), z_fwfprv(:), kt == nitend - nn_fsbc + 1 )
                  END SELECT
              ENDIF
              emp_corr = emp_ext - z_fwfprv(1) / area
#if defined key_agrif
            ELSE
               !
               ! Volume is here corrected according to the budget computed in the past, e.g. between
               ! the last two consecutive calls to the surface module. Hence, the volume is allowed to drift slightly during
               ! the current time step. 
               !
               IF( kt == nit000 ) THEN                                                               ! initialisation
                  !                                                                                  ! 
                  IF ( ln_rstart .AND. iom_varid( numror, 'a_fwb',      ldstop = .FALSE. ) > 0     & ! read from restart file
                     &           .AND. iom_varid( numror, 'emp_corr',   ldstop = .FALSE. ) > 0 ) THEN
                     IF(lwp)   WRITE(numout,*) 'sbc_fwb : reading freshwater-budget from restart file'
                     CALL iom_get( numror, 'a_fwb'     , a_fwb    )
                     CALL iom_get( numror, 'emp_corr'  , emp_corr )
                     !                                                                                 
                  ELSE                                                                               
                     emp_corr = 0._wp
                     a_fwb    = 999._wp
                     a_fwb_b  = 999._wp   
                  END IF
                  !
                  IF(lwp)   WRITE(numout,*)
                  IF(lwp)   WRITE(numout,*)'sbc_fwb : initial freshwater correction flux = ', emp_corr , 'kg/m2/s'
                  !
               ENDIF
               !
               IF( MOD( kt-1, kn_fsbc ) == 0 ) THEN
                  a_fwb_b   = a_fwb                            ! time swap
                  agrif_tmp(:) = 1.e+40_dp                     ! Initialize to a big value
                  SELECT CASE (nn_fwb_voltype)
                  CASE( 1 )
                     agrif_tmp(1) = glob_sum( 'sbcfwb', e1e2t(:,:) * tmask_agrif(:,:) * ( ssh(:,:,Kmm) + snwice_mass_b(:,:) * r1_rho0 ))
                  CASE( 2 )
                     agrif_tmp(1) = glob_sum( 'sbcfwb', e1e2t(:,:) * tmask_agrif(:,:) * ssh(:,:,Kmm))
                  END SELECT
                  CALL Agrif_step_child_adj(glob_sum_volume_agrif) ! Get value over child grids
                  CALL mpp_min('sbcfwb', agrif_tmp(:)) ! Required with // sisters to populate the value of each grid on each processor
                  a_fwb = SUM(agrif_tmp) * rho0 / area ! Sum over all grids
                  IF ( a_fwb_b == 999._wp ) a_fwb_b = a_fwb
                  emp_corr = (a_fwb - a_fwb_b) / ( rn_Dt * REAL(kn_fsbc, wp) ) + emp_corr + emp_ext
!!                  IF (lwp) WRITE(numout,*) 'Averaged liquid height (m) and flux correction (kg/m2/s):', kt, a_fwb * r1_rho0, emp_corr
               ENDIF    
               !   
            ENDIF 
         ELSE ! child grid if any
            IF( MOD( kt-1, kn_fsbc ) == 0 ) THEN
                emp_corr  =  Agrif_parent(emp_corr) 
            ENDIF  
         ENDIF
#endif
         !
         IF( MOD( kt-1, kn_fsbc ) == 0 ) THEN        ! correct the freshwater fluxes on all grids
            emp(:,:) = emp(:,:) + emp_corr * tmask(:,:,1)
            qns(:,:) = qns(:,:) - emp_corr * rcp * sst_m(:,:) * tmask(:,:,1) ! account for change to the heat budget due to fw correction
         ENDIF

         IF ( Agrif_Root() ) THEN
            ! Output restart information (root grid only)
            IF( lrst_oce ) THEN
               IF(lwp) WRITE(numout,*)
               IF(lwp) WRITE(numout,*) 'sbc_fwb : writing FW-budget adjustment to ocean restart file at it = ', kt
               IF(lwp) WRITE(numout,*) '~~~~'
               CALL iom_rstput( kt, nitrst, numrow, 'a_fwb'  ,   a_fwb   )
               CALL iom_rstput( kt, nitrst, numrow, 'emp_corr', emp_corr )
            END IF
            !
            IF( kt == nitend .AND. lwp ) THEN
               WRITE(numout,*) 'sbc_fwb : freshwater-budget at the end of simulation (year now) = ', emp_corr  , 'kg/m2/s'
            END IF
         END IF

         ! outputs
         IF( MOD( kt-1, kn_fsbc ) == 0 ) THEN
            IF( iom_use('hflx_fwb_cea') )  CALL iom_put( 'hflx_fwb_cea', -emp_corr * rcp * sst_m(:,:) * tmask(:,:,1) )
            IF( iom_use('vflx_fwb_cea') )  CALL iom_put( 'vflx_fwb_cea', -emp_corr * tmask(:,:,1) )
         ENDIF   
         !
      CASE ( 2 )                             !==  set volume annual trend  ==!
         ! 
         IF ( Agrif_Root() ) THEN
            IF( kt == nit000 ) THEN                                                                 ! initialisation
               !                                                                                    ! 
               IF ( ln_rstart .AND. iom_varid( numror, 'a_fwb',      ldstop = .FALSE. ) > 0     &   ! read from restart file
                  &           .AND. iom_varid( numror, 'a_fwb_b',    ldstop = .FALSE. ) > 0     & 
                  &           .AND. iom_varid( numror, 'emp_corr',   ldstop = .FALSE. ) > 0 ) THEN
                  IF(lwp)   WRITE(numout,*) 'sbc_fwb : reading freshwater-budget from restart file'
                  CALL iom_get( numror, 'a_fwb'     , a_fwb    )
                  CALL iom_get( numror, 'a_fwb_b'   , a_fwb_b  )
                  CALL iom_get( numror, 'emp_corr'  , emp_corr )
               ELSE                                                                                 !    as specified in namelist
                  IF(lwp)   WRITE(numout,*) 'sbc_fwb : setting freshwater-budget from namelist rn_fwb0'
                  emp_corr = rn_fwb0
                  a_fwb    = 999._wp
                  a_fwb_b  = 999._wp    
               END IF
               !
               IF(lwp)   WRITE(numout,*)
               IF(lwp)   WRITE(numout,*)'sbc_fwb : initial freshwater correction flux = ', emp_corr , 'kg/m2/s'
               !
            ENDIF
            !
            ! at the end of year n:
            ikty = nyear_len(1) * rday / NINT(rn_Dt)
            IF( MOD( kt-1, ikty ) == 0 ) THEN   ! Update a_fwb at the last time step of a year
               a_fwb_b = a_fwb
               ! mean sea level taking into account ice+snow
#if defined key_agrif
               agrif_tmp(:) = 1.e+40_dp                     ! Initialize to a big value
               SELECT CASE (nn_fwb_voltype)
               CASE( 1 )
                  agrif_tmp(1) = glob_sum( 'sbcfwb', e1e2t(:,:) * tmask_agrif(:,:) * ( ssh(:,:,Kmm) + snwice_mass_b(:,:) * r1_rho0 ))
               CASE( 2 )
                  agrif_tmp(1) = glob_sum( 'sbcfwb', e1e2t(:,:) * tmask_agrif(:,:) * ssh(:,:,Kmm) )
               END SELECT
               CALL Agrif_step_child_adj(glob_sum_volume_agrif) ! Get value over child grids
               CALL mpp_min('sbcfwb', agrif_tmp(:)) ! Required with // sisters to populate the value of each grid on each processor
               a_fwb = SUM(agrif_tmp) ! Sum over all grids
#else          
               SELECT CASE (nn_fwb_voltype)
               CASE( 1 )     
                  a_fwb   = glob_sum( 'sbcfwb', e1e2t(:,:) * ( ssh(:,:,Kmm) + snwice_mass_b(:,:) * r1_rho0 ) )
               CASE( 2 )
                  a_fwb   = glob_sum( 'sbcfwb', e1e2t(:,:) * ssh(:,:,Kmm)  )
               END SELECT
#endif
               a_fwb = a_fwb * rho0 / area - hvolg_n * rho0
               !
               ! Special case if less than a year has been performed:
               ! hence namelist rn_fwb0 still rules
               IF ( a_fwb_b == 999._wp ) a_fwb_b = a_fwb
               !
               emp_corr = ( a_fwb - a_fwb_b ) / ( rday * REAL(nyear_len(0), wp) ) + emp_corr
               IF(lwp)   WRITE(numout,*)
               IF(lwp)   WRITE(numout,*)'sbc_fwb : Compute new global mass at step = ', kt
               IF(lwp)   WRITE(numout,*)'sbc_fwb : New      averaged liquid height (ocean + snow + ice) = ',    a_fwb * r1_rho0, 'm'
               IF(lwp)   WRITE(numout,*)'sbc_fwb : Previous averaged liquid height (ocean + snow + ice) = ',  a_fwb_b * r1_rho0, 'm'
               IF(lwp)   WRITE(numout,*)'sbc_fwb : Implied freshwater-budget correction flux = ', emp_corr , 'kg/m2/s'
            ENDIF
#if defined key_agrif
         ELSE ! child grid if any
            IF( MOD( kt-1, kn_fsbc ) == 0 ) THEN
                emp_corr  =  Agrif_parent(emp_corr) 
            ENDIF
#endif
         ENDIF
         !
         IF( MOD( kt-1, kn_fsbc ) == 0 ) THEN         ! correct the freshwater fluxes
            emp(:,:) = emp(:,:) + emp_corr * tmask(:,:,1)
            qns(:,:) = qns(:,:) - emp_corr * rcp * sst_m(:,:) * tmask(:,:,1) ! account for change to the heat budget due to fw correction
            ! outputs
            IF( iom_use('hflx_fwb_cea') )  CALL iom_put( 'hflx_fwb_cea', -emp_corr * rcp * sst_m(:,:) * tmask(:,:,1) )
            IF( iom_use('vflx_fwb_cea') )  CALL iom_put( 'vflx_fwb_cea', -emp_corr * tmask(:,:,1) )
         ENDIF

         IF ( Agrif_Root() ) THEN
            ! Output restart information (root grid only)
            IF( lrst_oce ) THEN
               IF(lwp) WRITE(numout,*)
               IF(lwp) WRITE(numout,*) 'sbc_fwb : writing FW-budget adjustment to ocean restart file at it = ', kt
               IF(lwp) WRITE(numout,*) '~~~~'
               CALL iom_rstput( kt, nitrst, numrow, 'a_fwb',   a_fwb   )
               CALL iom_rstput( kt, nitrst, numrow, 'a_fwb_b', a_fwb_b )
               CALL iom_rstput( kt, nitrst, numrow, 'emp_corr',emp_corr)
            END IF
            !
            IF( kt == nitend .AND. lwp ) THEN
               IF(lwp)   WRITE(numout,*)'sbc_fwb : Previous          year averaged liquid height (ocean + snow + ice) = ',    a_fwb * r1_rho0, 'm'
               IF(lwp)   WRITE(numout,*)'sbc_fwb : Previous previous year averaged liquid height (ocean + snow + ice) = ',  a_fwb_b * r1_rho0, 'm'
               IF(lwp)   WRITE(numout,*)'sbc_fwb : freshwater-budget correction flux = ', emp_corr , 'kg/m2/s'
            END IF
         END IF 
         !
      CASE ( 3 )                   !==  set volume at each time step and spread out the correction over erp area  ==!
         !
         ALLOCATE( ztmsk_neg(jpi,jpj) , ztmsk_pos(jpi,jpj) , ztmsk_tospread(jpi,jpj) , z_wgt(jpi,jpj) , zerp_cor(jpi,jpj) )
         !
         IF( MOD( kt-1, kn_fsbc ) == 0 ) THEN
            ztmsk_pos(:,:) = tmask_i(:,:)                      ! Select <0 and >0 area of erp
            WHERE( erp < 0._wp )   ztmsk_pos = 0._wp
            ztmsk_neg(:,:) = tmask_i(:,:) - ztmsk_pos(:,:)
            !                                                  ! fwf global mean (excluding ocean to ice/snow exchanges)
            SELECT CASE (nn_fwb_voltype)
            CASE( 1 )  
               z_fwf     = -emp_ext + glob_sum( 'sbcfwb', e1e2t(:,:) * ( emp(:,:) - rnf(:,:) - fwfisf_cav(:,:) - fwfisf_par(:,:) - snwice_fmass(:,:) ) ) / area
            CASE( 2 )
               z_fwf     = -emp_ext + glob_sum( 'sbcfwb', e1e2t(:,:) * ( emp(:,:) - rnf(:,:) - fwfisf_cav(:,:) - fwfisf_par(:,:) ) ) / area
            END SELECT
            !            
            IF( z_fwf < 0._wp ) THEN         ! spread out over >0 erp area to increase evaporation
               zsurf_pos = glob_sum( 'sbcfwb', e1e2t(:,:)*ztmsk_pos(:,:) )
               zsurf_tospread      = zsurf_pos
               ztmsk_tospread(:,:) = ztmsk_pos(:,:)
            ELSE                             ! spread out over <0 erp area to increase precipitation
               zsurf_neg = glob_sum( 'sbcfwb', e1e2t(:,:)*ztmsk_neg(:,:) )  ! Area filled by <0 and >0 erp 
               zsurf_tospread      = zsurf_neg
               ztmsk_tospread(:,:) = ztmsk_neg(:,:)
            ENDIF
            !
            zsum_fwf   = glob_sum( 'sbcfwb', e1e2t(:,:) * z_fwf )         ! fwf global mean over <0 or >0 erp area
!!gm :  zsum_fwf   = z_fwf * area   ???  it is right?  I think so....
            z_fwf_nsrf =  zsum_fwf / ( zsurf_tospread + rsmall )
            !                                                  ! weight to respect erp field 2D structure 
            zsum_erp   = glob_sum( 'sbcfwb', ztmsk_tospread(:,:) * erp(:,:) * e1e2t(:,:) )
            z_wgt(:,:) = ztmsk_tospread(:,:) * erp(:,:) / ( zsum_erp + rsmall )
            !                                                  ! final correction term to apply
            zerp_cor(:,:) = -1. * z_fwf_nsrf * zsurf_tospread * z_wgt(:,:)
            !
!!gm   ===>>>>  lbc_lnk should be useless as all the computation is done over the whole domain !
            CALL lbc_lnk( 'sbcfwb', zerp_cor, 'T', 1.0_wp )
            !
            emp(:,:) = emp(:,:) + zerp_cor(:,:)
            qns(:,:) = qns(:,:) - zerp_cor(:,:) * rcp * sst_m(:,:)  ! account for change to the heat budget due to fw correction
            erp(:,:) = erp(:,:) + zerp_cor(:,:)
            ! outputs
            IF( iom_use('hflx_fwb_cea') )  CALL iom_put( 'hflx_fwb_cea', -zerp_cor(:,:) * rcp * sst_m(:,:) )
            IF( iom_use('vflx_fwb_cea') )  CALL iom_put( 'vflx_fwb_cea', -zerp_cor(:,:) )
            !
            IF( lwp ) THEN                   ! control print
               IF( z_fwf < 0._wp ) THEN
                  WRITE(numout,*)'   z_fwf < 0'
                  WRITE(numout,*)'   SUM(erp+)     = ', SUM( ztmsk_tospread(:,:)*erp(:,:)*e1e2t(:,:) )*1.e-9,' Sv'
               ELSE
                  WRITE(numout,*)'   z_fwf >= 0'
                  WRITE(numout,*)'   SUM(erp-)     = ', SUM( ztmsk_tospread(:,:)*erp(:,:)*e1e2t(:,:) )*1.e-9,' Sv'
               ENDIF
               WRITE(numout,*)'   SUM(empG)     = ', SUM( z_fwf*e1e2t(:,:) )*1.e-9,' Sv'
               WRITE(numout,*)'   z_fwf         = ', z_fwf      ,' Kg/m2/s'
               WRITE(numout,*)'   z_fwf_nsrf    = ', z_fwf_nsrf ,' Kg/m2/s'
               WRITE(numout,*)'   MIN(zerp_cor) = ', MINVAL(zerp_cor) 
               WRITE(numout,*)'   MAX(zerp_cor) = ', MAXVAL(zerp_cor) 
            ENDIF
         ENDIF
         DEALLOCATE( ztmsk_neg , ztmsk_pos , ztmsk_tospread , z_wgt , zerp_cor )
         !
      END SELECT
      !
   END SUBROUTINE sbc_fwb


   SUBROUTINE set_hglo_ana(kt, koffset, rn_sshm_amp, nn_sshm_mth, rn_sshm_trd, phglo)
      !!---------------------------------------------------------------------
      !!                   ***  Set the global volume  ***
      !!  
      !!    Define the globally averaged equivalent volume height analytically 
      !!----------------------------------------------------------------------
      INTEGER,  INTENT(in)  :: kt, koffset
      REAL(wp), INTENT(in)  :: rn_sshm_amp, rn_sshm_trd
      INTEGER,  INTENT(in)  :: nn_sshm_mth
      REAL(wp), INTENT(out) :: phglo
      !
      REAL(wp)              :: zt, zf0, zr_nsy
      !!----------------------------------------------------------------------    
      !
      IF ( nleapy==1 ) THEN 
         zr_nsy = 1._wp / (    365.25_wp * rday )
      ELSE 
         zr_nsy = 1._wp / ( nyear_len(1) * rday )
      ENDIF
      !
      ! Time at "Now" time step (i.e. 0.5*dt seconds in the past compared to what nsec_year gives)
      zt  = REAL(nsec1jan000 + nsec_year, wp) + (REAL(koffset,wp) - 0.5_wp) * rn_Dt
      zf0 = 2._wp * rpi * REAL(nn_sshm_mth-1, wp) / 12._wp  ! Phase lag
      ! 
      phglo  = 0.5_wp * rn_sshm_amp * SIN(2._wp * rpi * zr_nsy * zt - zf0)  & 
             &        + rn_sshm_trd * (kt + koffset - 1) * rn_Dt    

   END SUBROUTINE set_hglo_ana 


#if defined key_agrif
   SUBROUTINE glob_sum_area_agrif()
      !!---------------------------------------------------------------------
      !!           ***  compute area with embedded zooms ***
      !!----------------------------------------------------------------------
      INTEGER :: igrid

      IF (Agrif_root()) RETURN

      igrid = agrif_fixed() + 1
      agrif_tmp(igrid) = glob_sum( 'sbcfwb', e1e2t(:,:) * tmask_agrif(:,:))

   END SUBROUTINE glob_sum_area_agrif   

   SUBROUTINE glob_sum_volume_agrif()
      !!---------------------------------------------------------------------
      !!           ***  Compute volume with embedded zooms  ***
      !!----------------------------------------------------------------------
      INTEGER :: igrid
      !
      IF (Agrif_root()) RETURN

      igrid = agrif_fixed() + 1
      IF (( nn_ice==2 ).OR.(nn_fwb_voltype==1)) THEN
         ! NB: we use "now" value for snwice_mass over child grids since it has not been updated yet at the time
         !     this call is made (e.g. when starting a new step over the parent grid)
         agrif_tmp(igrid) = glob_sum( 'sbcfwb', e1e2t(:,:) * tmask_agrif(:,:) * ( ssh(:,:,Kmm_a) + snwice_mass(:,:) * r1_rho0 ))
      ELSE
         agrif_tmp(igrid) = glob_sum( 'sbcfwb', e1e2t(:,:) * tmask_agrif(:,:) * ssh(:,:,Kmm_a) )
      ENDIF

   END SUBROUTINE glob_sum_volume_agrif 



#endif

   !!======================================================================
END MODULE sbcfwb
