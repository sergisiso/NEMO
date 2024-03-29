MODULE ice
   !!======================================================================
   !!                        ***  MODULE  ice  ***
   !!   sea-ice:  ice variables defined in memory
   !!======================================================================
   !! History :  3.0  !  2008-03  (M. Vancoppenolle) Original code
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_alloc   ! called by icestp.F90
   PUBLIC   ice_dealloc ! called by ?

   !!======================================================================
   !!                                                                     |
   !!              I C E   S T A T E   V A R I A B L E S                  |
   !!                                                                     |
   !! Introduction :                                                      |
   !! --------------                                                      |
   !! Every ice-covered grid cell is characterized by a series of state   |
   !! variables. To account for unresolved spatial variability in ice     |
   !! thickness, the ice cover in divided in ice thickness categories.    |
   !!                                                                     |
   !! Sea ice state variables depend on the ice thickness category        |
   !!                                                                     |
   !! Those variables are divided into two groups                         |
   !! * Extensive (or global) variables.                                  |
   !!   These are the variables that are transported by all means         |
   !! * Intensive (or equivalent) variables.                              |
   !!   These are the variables that are either physically more           |
   !!   meaningful and/or used in ice thermodynamics                      |
   !!                                                                     |
   !! List of ice state variables :                                       |
   !! -----------------------------                                       |
   !!                                                                     |
   !!-------------|-------------|---------------------------------|-------|
   !!   name in   |   name in   |              meaning            | units |
   !! 2D routines | 1D routines |                                 |       |
   !!-------------|-------------|---------------------------------|-------|
   !!                                                                     |
   !! ******************************************************************* |
   !! ***         Dynamical variables (prognostic)                    *** |
   !! ******************************************************************* |
   !!                                                                     |
   !! u_ice       |      -      |    ice velocity in i-direction  | m/s   |
   !! v_ice       |      -      |    ice velocity in j-direction  | m/s   |
   !!                                                                     |
   !! ******************************************************************* |
   !! ***         Category dependent state variables (prognostic)     *** |
   !! ******************************************************************* |
   !!                                                                     |
   !! ** Global variables                                                 |
   !!-------------|-------------|---------------------------------|-------|
   !! a_i         |   a_i_1d    |    Ice concentration            |       |
   !! v_i         |      -      |    Ice volume per unit area     | m     |
   !! v_s         |      -      |    Snow volume per unit area    | m     |
   !! sv_i        |      -      |    Sea ice salt content (3D)    | g/kg.m|
   !! szv_i       |      -      |    Sea ice salt content (4D)    | g/kg.m|
   !! oa_i        |      -      |    Sea ice areal age content    | s     |
   !! e_i         |             |    Ice enthalpy                 | J/m2  |
   !!             |    e_i_1d   |    Ice enthalpy per unit vol.   | J/m3  |
   !! e_s         |             |    Snow enthalpy                | J/m2  |
   !!             |    e_s_1d   |    Snow enthalpy per unit vol.  | J/m3  |
   !! a_ip        |      -      |    Ice pond concentration       |       |
   !! v_ip        |      -      |    Ice pond volume per unit area| m     |
   !! v_il        |    v_il_1d  |    Ice pond lid volume per area | m     |
   !!                                                                     |
   !!-------------|-------------|---------------------------------|-------|
   !!                                                                     |
   !! ** Equivalent variables                                             |
   !!-------------|-------------|---------------------------------|-------|
   !!                                                                     |
   !! h_i         | h_i_1d      |    Ice thickness                | m     |
   !! h_s         ! h_s_1d      |    Snow depth                   | m     |
   !! s_i         ! s_i_1d      |    Sea ice bulk salinity        | g/kg  |
   !! sz_i        ! sz_i_1d     |    Sea ice salinity profile     | g/kg  |
   !! o_i         !      -      |    Sea ice Age                  | s     |
   !! t_i         ! t_i_1d      |    Sea ice temperature          | K     |
   !! t_s         ! t_s_1d      |    Snow temperature             | K     |
   !! t_su        ! t_su_1d     |    Sea ice surface temperature  | K     |
   !! h_ip        | h_ip_1d     |    Ice pond thickness           | m     |
   !! h_il        | h_il_1d     |    Ice pond lid thickness       | m     |
   !!                                                                     |
   !! notes: the ice model only sees a bulk (i.e., vertically averaged)   |
   !!        salinity, except in thermodynamic computations, for which    |
   !!        the salinity profile is computed as a function of bulk       |
   !!        salinity                                                     |
   !!                                                                     |
   !!        the sea ice surface temperature is not associated to any     |
   !!        heat content. Therefore, it is not a state variable and      |
   !!        does not have to be advected. Nevertheless, it has to be     |
   !!        computed to determine whether the ice is melting or not      |
   !!                                                                     |
   !! ******************************************************************* |
   !! ***         Category-summed state variables (diagnostic)        *** |
   !! ******************************************************************* |
   !! at_i        | at_i_1d     |    Total ice concentration      |       |
   !! vt_i        |      -      |    Total ice vol. per unit area | m     |
   !! vt_s        |      -      |    Total snow vol. per unit ar. | m     |
   !! st_i        |      -      |    Total Sea ice salt content   | g/kg.m|
   !! sm_i        |      -      |    Mean sea ice salinity        | g/kg  |
   !! tm_i        |      -      |    Mean sea ice temperature     | K     |
   !! tm_s        |      -      |    Mean snow    temperature     | K     |
   !! et_i        |      -      |    Total ice enthalpy           | J/m2  |
   !! et_s        |      -      |    Total snow enthalpy          | J/m2  |
   !! v_ibr       |      -      |    relative brine volume        | ???   |
   !! at_ip       |      -      |    Total ice pond concentration |       |
   !! at_ip_eff   !      -      !    Effective pond concentration |       |
   !! hm_ip       |      -      |    Mean ice pond depth          | m     |
   !! vt_ip       |      -      |    Total ice pond vol. per unit area| m |
   !! hm_il       |      -      |    Mean ice pond lid depth      | m     |
   !! vt_il       |      -      |    Total ice pond lid vol. per area | m |
   !!=====================================================================

   !!----------------------------------------------------------------------
   !! * Share Module variables
   !!----------------------------------------------------------------------
   !                                     !!** ice-generic parameters namelist (nampar) **
   INTEGER           , PUBLIC ::   jpl              !: number of ice  categories
   INTEGER           , PUBLIC ::   nlay_i           !: number of ice  layers
   INTEGER           , PUBLIC ::   nlay_s           !: number of snow layers
   LOGICAL           , PUBLIC ::   ln_virtual_itd   !: virtual ITD mono-category parameterization (T) or not (F)
   LOGICAL           , PUBLIC ::   ln_icedyn        !: flag for ice dynamics (T) or not (F)
   LOGICAL           , PUBLIC ::   ln_icethd        !: flag for ice thermo   (T) or not (F)
   REAL(wp)          , PUBLIC ::   rn_amax_n        !: maximum ice concentration Northern hemisphere
   REAL(wp)          , PUBLIC ::   rn_amax_s        !: maximum ice concentration Southern hemisphere
   CHARACTER(len=256), PUBLIC ::   cn_icerst_in     !: suffix of ice restart name (input)
   CHARACTER(len=256), PUBLIC ::   cn_icerst_out    !: suffix of ice restart name (output)
   CHARACTER(len=256), PUBLIC ::   cn_icerst_indir  !: ice restart input directory
   CHARACTER(len=256), PUBLIC ::   cn_icerst_outdir !: ice restart output directory

   !                                     !!** ice-itd namelist (namitd) **
   REAL(wp), PUBLIC ::   rn_himin         !: minimum ice thickness

   !                                     !!** ice-dynamics namelist (namdyn) **
   REAL(wp), PUBLIC ::   rn_ishlat        !: lateral boundary condition for sea-ice
   LOGICAL , PUBLIC ::   ln_landfast_L16  !: landfast ice parameterization from lemieux2016
   REAL(wp), PUBLIC ::   rn_lf_depfra     !:    fraction of ocean depth that ice must reach to initiate landfast ice
   REAL(wp), PUBLIC ::   rn_lf_bfr        !:    maximum bottom stress per unit area of contact (lemieux2016) or per unit volume (home)
   REAL(wp), PUBLIC ::   rn_lf_relax      !:    relaxation time scale (s-1) to reach static friction
   REAL(wp), PUBLIC ::   rn_lf_tensile    !:    isotropic tensile strength
   !
   !                                     !!** ice-ridging/rafting namelist (namdyn_rdgrft) **
   LOGICAL,  PUBLIC ::   ln_str_H79       !: ice strength parameterization (Hibler79) (may be used in rheology)
   REAL(wp), PUBLIC ::   rn_crhg          !: determines changes in ice strength (also used for landfast param)
   REAL(wp), PUBLIC ::   rn_pstar         !: determines ice strength, Hibler JPO79 (may be used in rheology)
   !
   !                                     !!** ice-rheology namelist (namdyn_rhg) **
   ! -- evp
   LOGICAL , PUBLIC ::   ln_rhg_EVP       ! EVP rheology switch, used for rdgrft and rheology
   LOGICAL , PUBLIC ::   ln_rhg_EAP       ! EAP rheology switch, used for rdgrft and rheology
   LOGICAL , PUBLIC ::   ln_aEVP          !: using adaptive EVP (T or F)
   REAL(wp), PUBLIC ::   rn_creepl        !: creep limit (has to be low enough, circa 10-9 m/s, depending on rheology)
   REAL(wp), PUBLIC ::   rn_ecc           !: eccentricity of the elliptical yield curve
   INTEGER , PUBLIC ::   nn_nevp          !: number of iterations for subcycling
   REAL(wp), PUBLIC ::   rn_relast        !: ratio => telast/rDt_ice (1/3 or 1/9 depending on nb of subcycling nevp)
   INTEGER , PUBLIC ::   nn_rhg_chkcvg    !: check ice rheology convergence
   ! -- vp
   LOGICAL , PUBLIC ::   ln_rhg_VP        !: VP rheology
   INTEGER , PUBLIC ::   nn_vp_nout       !: Number of outer iterations
   INTEGER , PUBLIC ::   nn_vp_ninn       !: Number of inner iterations (linear system solver)
   INTEGER , PUBLIC ::   nn_vp_chkcvg     !: Number of iterations every each convergence is checked
   !
   !                                     !!** ice-advection namelist (namdyn_adv) **
   LOGICAL , PUBLIC ::   ln_adv_Pra       !: Prather        advection scheme
   LOGICAL , PUBLIC ::   ln_adv_UMx       !: Ultimate-Macho advection scheme
   !
   !                                     !!** ice-surface boundary conditions namelist (namsbc) **
                                          ! -- icethd_dh -- !
   REAL(wp), PUBLIC ::   rn_snwblow       !: coef. for partitioning of snowfall between leads and sea ice
                                          ! -- icethd_zdf and icealb -- !
   INTEGER , PUBLIC ::   nn_snwfra        !: calculate the fraction of ice covered by snow
   !                                      !   = 0  fraction = 1 (if snow) or 0 (if no snow)
   !                                      !   = 1  fraction = 1-exp(-0.2*rhos*hsnw) [MetO formulation]
   !                                      !   = 2  fraction = hsnw / (hsnw+0.02)    [CICE formulation]
                                          ! -- icethd -- !
   REAL(wp), PUBLIC ::   rn_Cd_io         !: drag coefficient for oceanic stress
   INTEGER , PUBLIC ::   nn_flxdist       !: Redistribute heat flux over ice categories
   !                                      !   =-1  Do nothing (needs N(cat) fluxes)
   !                                      !   = 0  Average N(cat) fluxes then apply the average over the N(cat) ice
   !                                      !   = 1  Average N(cat) fluxes then redistribute over the N(cat) ice using T-ice and albedo sensitivity
   !                                      !   = 2  Redistribute a single flux over categories
                                          ! -- icethd_zdf -- !
   LOGICAL , PUBLIC ::   ln_cndflx        !: use conduction flux as surface boundary condition (instead of qsr and qns)
   LOGICAL , PUBLIC ::   ln_cndemulate    !: emulate conduction flux (if not provided)
   !                                      ! Conduction flux as surface forcing or not
   INTEGER, PUBLIC, PARAMETER ::   np_cnd_OFF = 0  !: no forcing from conduction flux (ice thermodynamics forced via qsr and qns)
   INTEGER, PUBLIC, PARAMETER ::   np_cnd_ON  = 1  !: forcing from conduction flux (SM0L) (compute qcn and qsr_tr via sbcblk.F90 or sbccpl.F90)
   INTEGER, PUBLIC, PARAMETER ::   np_cnd_EMU = 2  !: emulate conduction flux via icethd_zdf.F90 (BL99) (1st round compute qcn and qsr_tr, 2nd round use it)
   INTEGER, PUBLIC ::   nn_qtrice         !: Solar flux transmitted thru the surface scattering layer:
   !                                      !   = 0  Grenfell and Maykut 1977 (depends on cloudiness and is 0 when there is snow)
   !                                      !   = 1  Lebrun 2019 (equals 0.3 anytime with different melting/dry snw conductivities)

   !                                     !!** namelist (namthd) **
   LOGICAL , PUBLIC ::   ln_icedH         ! activate ice thickness change from growing/melting (T) or not (F)
   LOGICAL , PUBLIC ::   ln_icedA         ! activate lateral melting param. (T) or not (F)
   LOGICAL , PUBLIC ::   ln_icedO         ! activate ice growth in open-water (T) or not (F)
   LOGICAL , PUBLIC ::   ln_leadhfx       ! heat in the leads is used to melt sea-ice before warming the ocean
   !
   !                                     !!** namelist (namthd_do) **
   REAL(wp), PUBLIC ::   rn_hinew         ! thickness for new ice formation (m)
   LOGICAL , PUBLIC ::   ln_frazil        ! use of frazil ice collection as function of wind (T) or not (F)
   REAL(wp), PUBLIC ::   rn_maxfraz       ! maximum portion of frazil ice collecting at the ice bottom
   REAL(wp), PUBLIC ::   rn_vfraz         ! threshold drift speed for collection of bottom frazil ice
   REAL(wp), PUBLIC ::   rn_Cfraz         ! squeezing coefficient for collection of bottom frazil ice
   !
   !                                     !!** ice-vertical diffusion namelist (namthd_zdf) **
   LOGICAL , PUBLIC ::   ln_cndi_U64      !: thermal conductivity: Untersteiner (1964)
   LOGICAL , PUBLIC ::   ln_cndi_P07      !: thermal conductivity: Pringle et al (2007)
   REAL(wp), PUBLIC ::   rn_cnd_s         !: thermal conductivity of the snow [W/m/K]
   REAL(wp), PUBLIC ::   rn_kappa_i       !: coef. for the extinction of radiation in sea ice, Grenfell et al. (2006) [1/m]
   REAL(wp), PUBLIC ::   rn_kappa_s       !: coef. for the extinction of radiation in snw (nn_qtrice=0) [1/m]
   REAL(wp), PUBLIC ::   rn_kappa_smlt    !: coef. for the extinction of radiation in melt snw (nn_qtrice=1) [1/m]
   REAL(wp), PUBLIC ::   rn_kappa_sdry    !: coef. for the extinction of radiation in dry  snw (nn_qtrice=1) [1/m]
   LOGICAL , PUBLIC ::   ln_zdf_chkcvg    !: check convergence of heat diffusion scheme

   !                                     !!** ice-salinity namelist (namthd_sal) **
   INTEGER , PUBLIC ::   nn_icesal        !: salinity configuration used in the model
   !                                      ! 1 - constant salinity in both space and time
   !                                      ! 2 - prognostic salinity (s(z,t))
   !                                      ! 3 - salinity profile, constant in time
   REAL(wp), PUBLIC ::   rn_icesal        !: bulk salinity (ppt) in case of constant salinity
   REAL(wp), PUBLIC ::   rn_sinew         !: fraction of sss that is kept in new ice
   REAL(wp), PUBLIC ::   rn_simin         !: minimum ice salinity [PSU]
   LOGICAL , PUBLIC ::   ln_sal_chk       !: sanity checks for salt drainage and flushing
   INTEGER , PUBLIC ::   nn_liquidus      !: formulation of liquidus
   !                                        1 = linear liquidus
   !                                        2 = Vancopenolle et al (2019) formulation
   !                                        3 = Weast formulation (used in RJW2014)

   !                                     !!** ice-ponds namelist (namthd_pnd)
   LOGICAL , PUBLIC ::   ln_pnd           !: Melt ponds (T) or not (F)
   LOGICAL , PUBLIC ::   ln_pnd_TOPO      !: Topographic Melt ponds scheme (Flocco et al 2007, 2010)
   LOGICAL , PUBLIC ::   ln_pnd_LEV       !: Simple melt pond scheme
   REAL(wp), PUBLIC ::   rn_apnd_min      !: Minimum fraction of melt water contributing to ponds
   REAL(wp), PUBLIC ::   rn_apnd_max      !: Maximum fraction of melt water contributing to ponds
   REAL(wp), PUBLIC ::   rn_pnd_flush     !: Pond flushing efficiency (tuning parameter)
   LOGICAL , PUBLIC ::   ln_pnd_CST       !: Melt ponds scheme with constant fraction and depth
   REAL(wp), PUBLIC ::   rn_apnd          !: prescribed pond fraction (0<rn_apnd<1)
   REAL(wp), PUBLIC ::   rn_hpnd          !: prescribed pond depth (0<rn_hpnd<1)
   LOGICAL,  PUBLIC ::   ln_pnd_lids      !: Allow ponds to have frozen lids
   LOGICAL,  PUBLIC ::   ln_pnd_rain      !: rain added to melt ponds
   LOGICAL , PUBLIC ::   ln_pnd_alb       !: melt ponds affect albedo
   REAL(wp), PUBLIC ::   rn_pnd_hl_min    !: pond lid thickness below which full pond area used in albedo calculation
   REAL(wp), PUBLIC ::   rn_pnd_hl_max    !: pond lid thickness above which ponds disappear from albedo calculation

   !                                     !!** ice-diagnostics namelist (namdia) **
   LOGICAL , PUBLIC ::   ln_icediachk     !: flag for ice diag (T) or not (F)
   REAL(wp), PUBLIC ::   rn_icechk_cel    !: rate of ice spuriously gained/lost (at any gridcell)
   REAL(wp), PUBLIC ::   rn_icechk_glo    !: rate of ice spuriously gained/lost (globally)
   LOGICAL , PUBLIC ::   ln_icediahsb     !: flag for ice diag (T) or not (F)
   LOGICAL , PUBLIC ::   ln_icectl        !: flag for sea-ice points output (T) or not (F)
   INTEGER , PUBLIC ::   iiceprt          !: debug i-point
   INTEGER , PUBLIC ::   jiceprt          !: debug j-point

   !                                     !!** some other parameters
   INTEGER , PUBLIC ::   kt_ice           !: iteration number
   REAL(wp), PUBLIC ::   rDt_ice          !: ice time step
   REAL(wp), PUBLIC ::   r1_Dt_ice        !: = 1. / rDt_ice
   REAL(wp), PUBLIC ::   r1_nlay_i        !: 1 / nlay_i
   REAL(wp), PUBLIC ::   r1_nlay_s        !: 1 / nlay_s
   REAL(wp), PUBLIC ::   rdiag_v, rdiag_s, rdiag_t, rdiag_fv, rdiag_fs, rdiag_ft   !: conservation diagnostics
   REAL(wp), PUBLIC, PARAMETER ::   ppepsi02 = 1.e-02_wp   !: Small number
   REAL(wp), PUBLIC, PARAMETER ::   epsi06 = 1.e-06_wp  !: small number
   REAL(wp), PUBLIC, PARAMETER ::   epsi10 = 1.e-10_wp  !: small number
   REAL(wp), PUBLIC, PARAMETER ::   epsi20 = 1.e-20_wp  !: small number

   !                                     !!** define arrays
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   u_oce,v_oce     !: surface ocean velocity used in ice dynamics
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   ht_i_new        !: ice collection thickness accreted in leads
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   fraz_frac       !: fraction of frazil ice accreted at the ice bottom
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   strength        !: ice strength
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   stress1_i, stress2_i, stress12_i   !: 1st, 2nd & diagonal stress tensor element
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   delta_i         !: ice rheology elta factor (Flato & Hibler 95) [s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   divu_i          !: Divergence of the velocity field             [s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   shear_i         !: Shear of the velocity field                  [s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   aniso_11, aniso_12   !: structure tensor elements
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   rdg_conv
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   t_bo            !: Sea-Ice bottom temperature [Kelvin]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   qlead           !: heat balance of the lead (or of the open ocean)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   qsb_ice_bot     !: net downward heat flux from the ice to the ocean
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   fhld            !: heat flux from the lead used for bottom melting

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_snw         !: mass flux from snow-ocean mass exchange             [kg.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_snw_sni     !: mass flux from snow ice growth component of wfx_snw [kg.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_snw_sum     !: mass flux from surface melt component of wfx_snw    [kg.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_pnd         !: mass flux from melt pond-ocean mass exchange        [kg.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_spr         !: mass flux from snow precipitation on ice            [kg.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_sub         !: mass flux from sublimation of snow/ice              [kg.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_snw_sub     !: mass flux from snow sublimation                     [kg.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_ice_sub     !: mass flux from ice sublimation                      [kg.m-2.s-1]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_snw_dyn     !: mass flux from dynamical component of wfx_snw       [kg.m-2.s-1]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_ice         !: mass flux from ice-ocean mass exchange                   [kg.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_sni         !: mass flux from snow ice growth component of wfx_ice      [kg.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_opw         !: mass flux from lateral ice growth component of wfx_ice   [kg.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_bog         !: mass flux from bottom ice growth component of wfx_ice    [kg.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_dyn         !: mass flux from dynamical ice growth component of wfx_ice [kg.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_bom         !: mass flux from bottom melt component of wfx_ice          [kg.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_sum         !: mass flux from surface melt component of wfx_ice         [kg.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_lam         !: mass flux from lateral melt component of wfx_ice         [kg.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_res         !: mass flux from residual component of wfx_ice             [kg.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   wfx_err_sub     !: mass flux error after sublimation                        [kg.m-2.s-1]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sfx_bog         !: salt flux due to ice bottom growth                   [g/kg.kg.m-2.s-1 => g.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sfx_bom         !: salt flux due to ice bottom melt                     [g/kg.kg.m-2.s-1 => g.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sfx_lam         !: salt flux due to ice lateral melt                    [g/kg.kg.m-2.s-1 => g.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sfx_sum         !: salt flux due to ice surface melt                    [g/kg.kg.m-2.s-1 => g.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sfx_sni         !: salt flux due to snow-ice growth                     [g/kg.kg.m-2.s-1 => g.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sfx_opw         !: salt flux due to growth in open water                [g/kg.kg.m-2.s-1 => g.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sfx_bri         !: salt flux due to brine rejection                     [g/kg.kg.m-2.s-1 => g.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sfx_dyn         !: salt flux due to porous ridged ice formation         [g/kg.kg.m-2.s-1 => g.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sfx_res         !: salt flux due to correction on ice thick. (residual) [g/kg.kg.m-2.s-1 => g.m-2.s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   sfx_sub         !: salt flux due to ice sublimation                     [g/kg.kg.m-2.s-1 => g.m-2.s-1]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hfx_bog         !: total heat flux causing bottom ice growth           [W.m-2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hfx_bom         !: total heat flux causing bottom ice melt             [W.m-2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hfx_sum         !: total heat flux causing surface ice melt            [W.m-2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hfx_opw         !: total heat flux causing open water ice formation    [W.m-2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hfx_dif         !: total heat flux causing Temp change in the ice      [W.m-2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hfx_snw         !: heat flux for snow melt                             [W.m-2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hfx_err_dif     !: heat flux remaining due to change in non-solar flux [W.m-2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   qt_atm_oi       !: heat flux at the interface atm-[oce+ice]            [W.m-2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   qt_oce_ai       !: heat flux at the interface oce-[atm+ice]            [W.m-2]

   ! heat flux associated with ice-atmosphere mass exchange
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hfx_sub         !: heat flux for sublimation            [W.m-2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hfx_spr         !: heat flux of the snow precipitation  [W.m-2]

   ! heat flux associated with ice-ocean mass exchange
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hfx_thd         !: ice-ocean heat flux from thermo processes (icethd_dh) [W.m-2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hfx_dyn         !: ice-ocean heat flux from ridging                      [W.m-2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   hfx_res         !: heat flux due to correction on ice thick. (residual)  [W.m-2]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   rn_amax_2d      !: maximum ice concentration 2d array
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qtr_ice_bot     !: transmitted solar radiation under ice
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   t1_ice          !: temperature of the first layer          (ln_cndflx=T) [K]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   cnd_ice         !: effective conductivity of the 1st layer (ln_cndflx=T) [W.m-2.K-1]

   ! sea ice drag coefficients
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   drag_io         !: ice-ocean drag coefficient
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   drag_ia         !: ice-atmosphere drag coefficient

   !!----------------------------------------------------------------------
   !! * Ice global state variables
   !!----------------------------------------------------------------------
   !! Variables defined for each ice category
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   h_i           !: Ice thickness                           (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   a_i           !: Ice fractional areas (concentration)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   v_i           !: Ice volume per unit area                (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   v_s           !: Snow volume per unit area               (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   h_s           !: Snow thickness                          (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   t_su          !: Sea-Ice Surface Temperature             (K)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   s_i           !: Sea-Ice Bulk salinity                   (g/kg)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   sv_i          !: Sea-Ice Bulk salinity * volume per area (g/kg.m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   o_i           !: Sea-Ice Age                             (s)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   oa_i          !: Sea-Ice Age times ice area              (s)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   v_ibr         !: brine volume

   !! Variables summed over all categories, or associated to all the ice in a single grid cell
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   u_ice, v_ice  !: components of the ice velocity                          (m/s)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   vt_i , vt_s   !: ice and snow total volume per unit area                 (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   st_i          !: Total ice salinity content                              (g/kg.m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   at_i          !: ice total fractional area (ice concentration)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   ato_i         !: =1-at_i ; total open water fractional area
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   et_i , et_s   !: ice and snow total heat content                         (J/m2)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   tm_i          !: mean ice temperature over all categories                (K)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   tm_s          !: mean snw temperature over all categories                (K)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   vm_ibr        !: brine volume averaged over all categories
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   sm_i          !: mean sea ice salinity averaged over all categories      (g/kg)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   tm_su         !: mean surface temperature over all categories            (K)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   hm_i          !: mean ice  thickness over all categories                 (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   hm_s          !: mean snow thickness over all categories                 (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   om_i          !: mean ice age over all categories                        (s)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   tau_icebfr    !: ice friction on ocean bottom (landfast param activated)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   icb_tmask , icb_umask , icb_vmask  !: mask of grounded icebergs if landfast [0-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   fast_tmask, fast_umask, fast_vmask !: mask of landfast ice [0-1]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   t_s           !: Snow temperatures     [K]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   e_s           !: Snow enthalpy         [J/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   t_i           !: ice temperatures      [K]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   e_i           !: ice enthalpy          [J/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   sz_i          !: ice salinity          [g/kg]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   szv_i         !: ice salinity content  [g/kg]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   a_ip          !: melt pond concentration
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   v_ip          !: melt pond volume per grid cell area      [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   a_ip_frac     !: melt pond fraction (a_ip/a_i)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   a_ip_eff      !: melt pond effective fraction (not covered up by lid) (a_ip/a_i)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   h_ip          !: melt pond depth                          [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   v_il          !: melt pond lid volume                     [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   h_il          !: melt pond lid thickness                  [m]

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   at_ip         !: total melt pond concentration
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   at_ip_eff     !: effective melt pond concentration
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   hm_ip         !: mean melt pond depth                     [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   vt_ip         !: total melt pond volume per gridcell area [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   hm_il         !: mean melt pond lid depth                     [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   vt_il         !: total melt pond lid volume per gridcell area [m]

   ! meltwater arrays to save for melt ponds (mv - could be grouped in a single meltwater volume array)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   dh_i_sum_2d   !: surface melt (2d arrays for ponds)       [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   dh_s_sum_2d   !: snow surf melt (2d arrays for ponds)     [m]

   !!----------------------------------------------------------------------
   !! * Global variables at before time step
   !!----------------------------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   v_s_b, v_i_b, h_s_b, h_i_b !: snow and ice volumes/thickness
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   v_ip_b, v_il_b             !: ponds and lids volumes
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::   a_i_b, sv_i_b              !:
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   e_s_b                      !: snow heat content
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) ::   e_i_b, szv_i_b             !: ice temperatures and salt
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   u_ice_b, v_ice_b           !: ice velocity
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)     ::   at_i_b                     !: ice concentration (total)

   !!----------------------------------------------------------------------
   !! * Ice thickness distribution variables
   !!----------------------------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)   ::   hi_max            !: Boundary of ice thickness categories in thickness space
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)   ::   hi_mean           !: Mean ice thickness in catgories
   !
   !!----------------------------------------------------------------------
   !! * Ice diagnostics
   !!----------------------------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_trp_vi       !: transport of ice volume
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_trp_vs       !: transport of snw volume
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_trp_ei       !: transport of ice enthalpy [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_trp_es       !: transport of snw enthalpy [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_trp_sv       !: transport of salt content
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_heat         !: snw/ice heat content variation   [W/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_sice         !: ice salt content variation   []
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_vice         !: ice volume variation   [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_vsnw         !: snw volume variation   [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_aice         !: ice conc.  variation   [s-1]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_vpnd         !: pond volume variation  [m/s]
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_adv_mass     !: advection of mass (kg/m2/s)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_adv_salt     !: advection of salt (g/m2/s)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_adv_heat     !: advection of heat (W/m2)
   !
   !!----------------------------------------------------------------------
   !! * Ice conservation
   !!----------------------------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_v            !: conservation of ice volume
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_s            !: conservation of ice salt
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_t            !: conservation of ice heat
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_fv           !: conservation of ice volume
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_fs           !: conservation of ice salt
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   diag_ft           !: conservation of ice heat
   !
   !!----------------------------------------------------------------------
   !! * SIMIP extra diagnostics
   !!----------------------------------------------------------------------
   ! Extra sea ice diagnostics to address the data request
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   t_si            !: Temperature at Snow-ice interface (K)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)   ::   tm_si           !: mean temperature at the snow-ice interface (K)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qcn_ice_bot     !: Bottom  conduction flux (W/m2)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   qcn_ice_top     !: Surface conduction flux (W/m2)
   !
   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/ICE 4.0 , NEMO Consortium (2018)
   !! $Id: ice.F90 15388 2021-10-17 11:33:47Z clem $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   FUNCTION ice_alloc()
      !!-----------------------------------------------------------------
      !!               *** Routine ice_alloc ***
      !!-----------------------------------------------------------------
      INTEGER :: ice_alloc
      !
      INTEGER :: ierr(21), ii
      !!-----------------------------------------------------------------
      ierr(:) = 0
      ii = 0
      ! ----------------- !
      ! == FULL ARRAYS == !
      ! ----------------- !
      
      ! * Ice global state variables
      ii = ii + 1
      ALLOCATE( u_ice(jpi,jpj) , v_ice(jpi,jpj) , STAT=ierr(ii) )

      ii = ii + 1
      ALLOCATE( h_i  (jpi,jpj,jpl) , a_i (jpi,jpj,jpl) , v_i (jpi,jpj,jpl) , v_s(jpi,jpj,jpl) , &
         &      h_s  (jpi,jpj,jpl) , s_i (jpi,jpj,jpl) , sv_i(jpi,jpj,jpl) , o_i(jpi,jpj,jpl) , oa_i(jpi,jpj,jpl) , &
         &      h_ip (jpi,jpj,jpl) , a_ip(jpi,jpj,jpl) , v_ip(jpi,jpj,jpl) , &
         &      h_il (jpi,jpj,jpl) , v_il(jpi,jpj,jpl) , t_su(jpi,jpj,jpl) , &
         &      t_s  (jpi,jpj,nlay_s,jpl) , t_i(jpi,jpj,nlay_i,jpl) , sz_i(jpi,jpj,nlay_i,jpl) , &
         &      ato_i(jpi,jpj)     , STAT = ierr(ii) )

      ii = ii + 1
      ALLOCATE( e_s(jpi,jpj,nlay_s,jpl) , e_i(jpi,jpj,nlay_i,jpl) , szv_i(jpi,jpj,nlay_i,jpl) , STAT=ierr(ii) )

      ! * Before values of global variables
      ii = ii + 1
      ALLOCATE( u_ice_b(jpi,jpj) , v_ice_b(jpi,jpj) , STAT=ierr(ii) )

      ! * ice rheology
      ii = ii+1
      ALLOCATE( u_oce     (jpi,jpj) , v_oce     (jpi,jpj) ,  &
         &      strength  (jpi,jpj) , stress1_i (jpi,jpj) , stress2_i (jpi,jpj) , stress12_i(jpi,jpj) ,  &
         &      aniso_11  (jpi,jpj) , aniso_12  (jpi,jpj) , rdg_conv  (jpi,jpj) , &
         &      icb_tmask (jpi,jpj) , icb_umask (jpi,jpj) , icb_vmask (jpi,jpj) , &
         &      fast_tmask(jpi,jpj) , fast_umask(jpi,jpj) , fast_vmask(jpi,jpj) , STAT=ierr(ii) )

      ! * Form Drag
      ii = ii + 1
      ALLOCATE( drag_io(jpi,jpj) , drag_ia(jpi,jpj), STAT=ierr(ii) )

      ! * mean and total
      ii = ii + 1
      ALLOCATE( vt_i (jpi,jpj) , vt_s (jpi,jpj) , at_i (jpi,jpj) , & ! full arrays since they are used in rheology
         &      vt_ip(jpi,jpj) , vt_il(jpi,jpj) , at_ip(jpi,jpj) , STAT=ierr(ii) )
      
      ! * others
      ii = ii + 1
      ALLOCATE( rn_amax_2d(jpi,jpj) , STAT=ierr(ii) )

      ! -------------------- !
      ! == REDUCED ARRAYS == !
      ! -------------------- !
      ! * Ice global state variables
      ii = ii + 1
      ALLOCATE( v_ibr(A2D(0),jpl) , a_ip_frac(A2D(0),jpl) , a_ip_eff(A2D(0),jpl) , STAT=ierr(ii) )

      ! * Before values of global variables
      ii = ii + 1
      ALLOCATE( at_i_b(A2D(0))     , h_i_b (A2D(0),jpl) , a_i_b(A2D(0),jpl) , v_i_b(A2D(0),jpl) ,  &
         &      v_s_b (A2D(0),jpl) , h_s_b (A2D(0),jpl) ,                                          &
         &      v_ip_b(A2D(0),jpl) , v_il_b(A2D(0),jpl) , sv_i_b(A2D(0),jpl) ,                     &
         &      e_i_b (A2D(0),nlay_i,jpl) , e_s_b(A2D(0),nlay_s,jpl) , szv_i_b (A2D(0),nlay_i,jpl) , STAT=ierr(ii) )

      ! * fluxes
      ii = ii + 1
      ALLOCATE( qsb_ice_bot(A2D(0)) , qlead      (A2D(0)) , qt_atm_oi  (A2D(0)) , qt_oce_ai  (A2D(0)) , fhld       (A2D(0)) , &
         &      wfx_snw_sni(A2D(0)) , wfx_snw    (A2D(0)) , wfx_snw_dyn(A2D(0)) , wfx_snw_sum(A2D(0)) , wfx_snw_sub(A2D(0)) , &
         &      wfx_ice    (A2D(0)) , wfx_sub    (A2D(0)) , wfx_ice_sub(A2D(0)) , wfx_lam    (A2D(0)) ,                       &
         &      wfx_pnd    (A2D(0)) , wfx_bog    (A2D(0)) , wfx_dyn    (A2D(0)) , wfx_bom    (A2D(0)) , wfx_sum    (A2D(0)) , &
         &      wfx_sni    (A2D(0)) , wfx_opw    (A2D(0)) , wfx_spr(A2D(0)) ,                                     &
         &      sfx_bri    (A2D(0)) , sfx_dyn    (A2D(0)) , sfx_sub(A2D(0)) , sfx_lam(A2D(0)) ,                   &
         &      sfx_bog    (A2D(0)) , sfx_bom    (A2D(0)) , sfx_sum(A2D(0)) , sfx_sni(A2D(0)) , sfx_opw(A2D(0)) , &
         &      hfx_snw    (A2D(0)) , hfx_sub    (A2D(0)) ,                                                       &
         &      hfx_sum    (A2D(0)) , hfx_bom    (A2D(0)) , hfx_bog(A2D(0)) , hfx_dif(A2D(0)) ,                   &
         &      hfx_opw    (A2D(0)) , hfx_thd    (A2D(0)) , hfx_dyn(A2D(0)) , hfx_spr(A2D(0)) ,                   &
         &      hfx_err_dif(A2D(0)) , wfx_err_sub(A2D(0))                   , STAT=ierr(ii) )
      ii = ii + 1
      ALLOCATE( wfx_res(A2D(0)) , sfx_res(A2D(0)) , hfx_res(A2D(0)) , STAT=ierr(ii) )
      ii = ii + 1
      ALLOCATE( qtr_ice_bot(A2D(0),jpl) , cnd_ice(A2D(0),jpl) , t1_ice(A2D(0),jpl) , STAT=ierr(ii) )

      ! * ice rheology
      ii = ii+1
      ALLOCATE( delta_i(A2D(0)) , divu_i(A2D(0)) , shear_i(A2D(0)) , STAT=ierr(ii) )

      ! * mean and total
      ii = ii + 1
      ALLOCATE( t_bo (A2D(0)) , st_i (A2D(0)) , et_i(A2D(0)) , et_s  (A2D(0)) , hm_i (A2D(0)) ,  &
         &      hm_ip(A2D(0)) , hm_il(A2D(0)) , tm_i(A2D(0)) , tm_s  (A2D(0)) ,  &
         &      sm_i (A2D(0)) , hm_s (A2D(0)) , om_i(A2D(0)) , vm_ibr(A2D(0)) ,  &
         &      tm_su(A2D(0)) , at_ip_eff(A2D(0)) , STAT=ierr(ii) )

      ! * others
      ii = ii + 1
      ALLOCATE( tau_icebfr(A2D(0)) , dh_i_sum_2d(A2D(0),jpl) , dh_s_sum_2d(A2D(0),jpl) ,  STAT=ierr(ii) )
      ii = 1
      ALLOCATE( ht_i_new (A2D(0)) , fraz_frac (A2D(0)) , STAT=ierr(ii) )

      ! * Ice thickness distribution variables
      ii = ii + 1
      ALLOCATE( hi_max(0:jpl), hi_mean(jpl),  STAT=ierr(ii) )

      ! * Ice diagnostics
      ii = ii + 1
      ALLOCATE( diag_trp_vi  (A2D(0)) , diag_trp_vs  (A2D(0)) , diag_trp_ei  (A2D(0)) ,                                         &
         &      diag_trp_es  (A2D(0)) , diag_trp_sv  (A2D(0)) , diag_heat    (A2D(0)) ,                                         &
         &      diag_sice    (A2D(0)) , diag_vice    (A2D(0)) , diag_vsnw    (A2D(0)) , diag_aice(A2D(0)) , diag_vpnd(A2D(0)) , &
         &      diag_adv_mass(A2D(0)) , diag_adv_salt(A2D(0)) , diag_adv_heat(A2D(0)) , STAT=ierr(ii) )

      ! * Ice conservation
      ii = ii + 1
      ALLOCATE( diag_v (A2D(0)) , diag_s (A2D(0)) , diag_t (A2D(0)),   &
         &      diag_fv(A2D(0)) , diag_fs(A2D(0)) , diag_ft(A2D(0)), STAT=ierr(ii) )

      ! * SIMIP diagnostics
      ii = ii + 1
      ALLOCATE( t_si(A2D(0),jpl) , tm_si(A2D(0)) , qcn_ice_bot(A2D(0),jpl) , qcn_ice_top(A2D(0),jpl) , STAT = ierr(ii) )

      ice_alloc = MAXVAL( ierr(:) )
      IF( ice_alloc /= 0 )   CALL ctl_stop( 'STOP', 'ice_alloc: failed to allocate arrays.' )
      !

   END FUNCTION ice_alloc


   SUBROUTINE ice_dealloc()
      IF( .NOT. ALLOCATED(u_ice) ) RETURN
      DEALLOCATE( u_ice , v_ice )
      DEALLOCATE( h_i   , a_i  , v_i  , v_s , &
         &      h_s   , s_i  , sv_i , o_i , oa_i , &
         &      h_ip  , a_ip , v_ip , &
         &      h_il  , v_il , t_su , &
         &      t_s   , t_i  , sz_i , &
         &      ato_i )
      DEALLOCATE( e_s , e_i , szv_i )
      DEALLOCATE( u_ice_b , v_ice_b )
      DEALLOCATE( u_oce      , v_oce      ,  &
         &      strength   , stress1_i  , stress2_i  , stress12_i ,  &
         &      aniso_11   , aniso_12   , rdg_conv   , &
         &      icb_tmask  , icb_umask  , icb_vmask  , &
         &      fast_tmask , fast_umask , fast_vmask )
      DEALLOCATE( vt_i  , vt_s  , at_i  , vt_ip , vt_il , at_ip )
      DEALLOCATE( rn_amax_2d )
      DEALLOCATE( v_ibr , a_ip_frac , a_ip_eff )
      DEALLOCATE( at_i_b     , h_i_b  , a_i_b , v_i_b ,  &
         &      v_s_b  , h_s_b  ,                                          &
         &      v_ip_b , v_il_b , sv_i_b ,                     &
         &      e_i_b  , e_s_b , szv_i_b  )
      DEALLOCATE( qsb_ice_bot , qlead       , qt_atm_oi   , qt_oce_ai   , fhld        , &
         &      wfx_snw_sni , wfx_snw     , wfx_snw_dyn , wfx_snw_sum , wfx_snw_sub , &
         &      wfx_ice     , wfx_sub     , wfx_ice_sub , wfx_lam     ,                       &
         &      wfx_pnd     , wfx_bog     , wfx_dyn     , wfx_bom     , wfx_sum     , &
         &      wfx_sni     , wfx_opw     , wfx_spr ,                                     &
         &      sfx_bri     , sfx_dyn     , sfx_sub , sfx_lam ,                   &
         &      sfx_bog     , sfx_bom     , sfx_sum , sfx_sni , sfx_opw , &
         &      hfx_snw     , hfx_sub     ,                                                       &
         &      hfx_sum     , hfx_bom     , hfx_bog , hfx_dif ,                   &
         &      hfx_opw     , hfx_thd     , hfx_dyn , hfx_spr ,                   &
         &      hfx_err_dif , wfx_err_sub  )
      DEALLOCATE( wfx_res , sfx_res , hfx_res )
      DEALLOCATE( qtr_ice_bot , cnd_ice , t1_ice )
      DEALLOCATE( drag_io, drag_ia )
      DEALLOCATE( delta_i , divu_i , shear_i ) 
      DEALLOCATE( t_bo  , st_i  , et_i , et_s   , hm_i  ,  &
         &      hm_ip , hm_il , tm_i , tm_s   ,  &
         &      sm_i  , hm_s  , om_i , vm_ibr ,  &
         &      tm_su )
      DEALLOCATE( tau_icebfr , dh_i_sum_2d , dh_s_sum_2d )
      DEALLOCATE( ht_i_new  , fraz_frac )
      DEALLOCATE( hi_max, hi_mean )
      DEALLOCATE( diag_trp_vi   , diag_trp_vs   , diag_trp_ei   ,                                         &
         &      diag_trp_es   , diag_trp_sv   , diag_heat     ,                                         &
         &      diag_sice     , diag_vice     , diag_vsnw     , diag_aice , diag_vpnd , &
         &      diag_adv_mass , diag_adv_salt , diag_adv_heat )
      DEALLOCATE( diag_v  , diag_s  , diag_t ,   &
         &      diag_fv , diag_fs , diag_ft )
      DEALLOCATE( t_si , tm_si , qcn_ice_bot , qcn_ice_top )      
   END SUBROUTINE ice_dealloc
   
#else
   !!----------------------------------------------------------------------
   !!   Default option         Empty module           NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE ice
