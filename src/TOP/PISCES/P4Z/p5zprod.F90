MODULE p5zprod
   !!======================================================================
   !!                         ***  MODULE p5zprod  ***
   !! TOP :  Growth Rate of the three phytoplanktons groups 
   !!        PISCES-QUOTA version of the module
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-05  (O. Aumont, C. Ethe) New parameterization of light limitation
   !!             3.6  !  2015-05  (O. Aumont) PISCES quota
   !!----------------------------------------------------------------------
   !!   p5z_prod       :   Compute the growth Rate of the two phytoplanktons groups
   !!   p5z_prod_init  :   Initialization of the parameters for growth
   !!   p5z_prod_alloc :   Allocate variables for growth
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE p2zlim
   USE p4zlim
   USE p5zlim          !  Co-limitations of differents nutrients
   USE prtctl          !  print control for debugging
   USE iom             !  I/O manager

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p5z_prod         ! called in p5zbio.F90
   PUBLIC   p5z_prod_init    ! called in trcsms_pisces.F90

   !! * Shared module variables
   REAL(wp), PUBLIC ::  pislopen        !: P-I slope of nanophytoplankton
   REAL(wp), PUBLIC ::  pislopep        !: P-I slope of picophytoplankton
   REAL(wp), PUBLIC ::  pisloped        !: P-I slope of diatoms
   REAL(wp), PUBLIC ::  excretn         !: Excretion ratio of nanophyto
   REAL(wp), PUBLIC ::  excretp         !: Excretion ratio of picophyto
   REAL(wp), PUBLIC ::  excretd         !: Excretion ratio of diatoms
   REAL(wp), PUBLIC ::  bresp           !: Basal respiration rate
   REAL(wp), PUBLIC ::  chlcmin         !: Minimum Chl/C ratio of phytoplankton
   REAL(wp), PUBLIC ::  grosip          !: Mean Si/C ratio of diatoms

   REAL(wp) :: r1_rday     !: 1 / rday
   REAL(wp) :: texcretn    !: 1 - excretn 
   REAL(wp) :: texcretp    !: 1 - excretp 
   REAL(wp) :: texcretd    !: 1 - excretd        
   REAL(wp) :: xq10_n      !: q10 coef for nano =  1. + xpsino3 * qnnmax
   REAL(wp) :: xq10_p      !: q10 coef for pico =  1. + xpsino3 * qnpmax
   REAL(wp) :: xq10_d      !: q10 coef for diat =  1. + xpsino3 * qndmax

   LOGICAL  :: l_dia_ppphy, l_dia_gpphy, l_dia_ppnew, l_dia_ppbfe, l_dia_ppbsi
   LOGICAL  :: l_dia_mu, l_dia_light, l_dia_lprod

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p5zprod.F90 15459 2021-10-29 08:19:18Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p5z_prod( kt , knt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_prod  ***
      !!
      !! ** Purpose :   Compute the phytoplankton production depending on
      !!              light, temperature and nutrient availability
      !!              Computes also the uptake of nutrients. PISCES-quota
      !!              relies on a full quota formalism
      !!---------------------------------------------------------------------
      !
      INTEGER, INTENT(in) :: kt, knt
      INTEGER, INTENT(in) :: Kbb, Kmm, Krhs      ! time level indices
      !
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zsilfac, znanotot, zpicotot, zdiattot
      REAL(wp) ::   zration, zratiop, zratiof, zmax
      REAL(wp) ::   zprofmax, zratio
      REAL(wp) ::   zpronewn, zpronewp, zpronewd
      REAL(wp) ::   zproregn, zproregp, zproregd
      REAL(wp) ::   zpropo4n, zpropo4p, zpropo4d
      REAL(wp) ::   zprodopn, zprodopp, zprodopd
      REAL(wp) ::   zlim, zsiborn, zprod, zprod1, zprontot, zproptot, zprodtot
      REAL(wp) ::   zproddoc, zproddon, zproddop, zprodsil, zprodfer, zprodlig
      REAL(wp) ::   zprnutmax, zprochln, zprochld, zprochlp
      REAL(wp) ::   zpislopen, zpislopep, zpisloped
      REAL(wp) ::   zval, zpo4tot, zpptot, zpnewtot, zpregtot
      REAL(wp) ::   zmxl_chl, zmxl_fac
      REAL(wp) ::   zqfpmax, zqfnmax, zqfdmax
      REAL(wp) ::   zfact, zrfact2, zmaxsi, zratiosi, zsizetmp, zlimfac, zsilim
      CHARACTER (len=25) :: charout
      REAL(wp), DIMENSION(A2D(0),jpk) :: zprorcan, zprorcap, zprorcad
      REAL(wp), DIMENSION(A2D(0),jpk) :: zpislopeadn, zpislopeadp, zpislopeadd
      REAL(wp), DIMENSION(A2D(0),jpk) :: zprnut, zprbio, zprpic, zprdia, zysopt
      REAL(wp), DIMENSION(A2D(0),jpk) :: zprchln, zprchlp, zprchld
      REAL(wp), DIMENSION(A2D(0),jpk) :: zprofed, zprofep, zprofen
      REAL(wp), DIMENSION(A2D(0),jpk) :: zpronmaxn, zpronmaxp,zpronmaxd
      REAL(wp), DIMENSION(A2D(0),jpk) :: zpropmaxn, zpropmaxp,zpropmaxd
      REAL(wp), DIMENSION(A2D(0),jpk) :: zprmaxn, zprmaxd, zprmaxp, zmxl
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: zw3d
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p5z_prod')
      !
      IF( kt == nittrc000 ) THEN
         l_dia_ppphy = iom_use( "PPPHYN" ) .OR. iom_use( "PPPHYD" ) .OR. iom_use( "PPPHYP" ) .OR. iom_use( "TPP"  )
         l_dia_gpphy = iom_use( "GPPHYN" ) .OR. iom_use( "GPPHYD" ) .OR. iom_use( "GPPHYP" )
         l_dia_ppnew = iom_use( "PPNEWN" ) .OR. iom_use( "PPNEWD" ) .OR. iom_use( "PPNEWP" ) .OR. iom_use( "TPNEW")
         l_dia_ppbfe = iom_use( "PFeN"   ) .OR. iom_use( "PFeD"   ) .OR. iom_use( "PFeP"   ) .OR. iom_use( "TPBFE")
         l_dia_ppbsi = iom_use( "PBSi"   )
         l_dia_mu    = iom_use( "Mumax"  ) .OR. iom_use( "MuN"    ) .OR. iom_use( "MuD") .OR. iom_use( "MuP")
         l_dia_light = iom_use( "LNlight") .OR. iom_use( "LDlight") .OR. iom_use( "LPlight")
         l_dia_lprod = ln_ligand .AND. ( iom_use( "LPRODP") .OR. iom_use( "LDETP") )
      ENDIF

      ! Initialize the local arrays
      zprorcan (:,:,:) = 0._wp ; zprorcap (:,:,:) = 0._wp ; zprorcad (:,:,:) = 0._wp
      zprofen  (:,:,:) = 0._wp ; zprofep  (:,:,:) = 0._wp ; zprofed  (:,:,:) = 0._wp
      zprbio   (:,:,:) = 0._wp ; zprpic   (:,:,:) = 0._wp ; zprdia   (:,:,:) = 0._wp
      zpronmaxn(:,:,:) = 0._wp ; zpronmaxp(:,:,:) = 0._wp ; zpronmaxd(:,:,:) = 0._wp
      zpropmaxn(:,:,:) = 0._wp ; zpropmaxp(:,:,:) = 0._wp ; zpropmaxd(:,:,:) = 0._wp
      zmxl     (:,:,:) = 0._wp ; zysopt   (:,:,:) = 0._wp 
      
      ! Computation of the optimal production rates and nutrient uptake
      ! rates. Based on a Q10 description of the thermal dependency.
       zprnut (:,:,:) =  0.65_wp          * r1_rday * tgfunc(:,:,:)
       zprmaxn(:,:,:)  = 0.65_wp * xq10_n * r1_rday * tgfunc(:,:,:)
       zprmaxd(:,:,:)  = 0.65_wp * xq10_d * r1_rday * tgfunc(:,:,:)
       zprmaxp(:,:,:)  = 0.50_wp * xq10_p * r1_rday * tgfunc(:,:,:)

      ! Impact of the day duration and light intermittency on phytoplankton growth
      ! Intermittency is supposed to have a similar effect on production as 
      ! day length (Shatwell et al., 2012). The correcting factor is zmxl_fac. 
      ! zmxl_chl is the fractional day length and is used to compute the mean
      ! PAR during daytime. The effect of mixing is computed using the 
      ! absolute light level definition of the euphotic zone
      ! ------------------------------------------------------------------------- 

      IF ( ln_p4z_dcyc ) THEN    ! Diurnal cycle in PISCES
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
               zval = 24.0
               IF( gdepw(ji,jj,jk+1,Kmm) <= hmld(ji,jj) ) THEN
                  zval = zval * MIN(1., heup_01(ji,jj) / ( hmld(ji,jj) + rtrn ))
               ENDIF
               zmxl(ji,jj,jk) = zval 
            ENDIF
         END_3D
      ELSE ! No diurnal cycle in PISCES
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
               zval = MAX( 1., strn(ji,jj) )
               IF( gdepw(ji,jj,jk+1,Kmm) <= hmld(ji,jj) ) THEN
                  zval = zval * MIN(1., heup_01(ji,jj) / ( hmld(ji,jj) + rtrn ))
               ENDIF
               zmxl(ji,jj,jk) = zval 
            ENDIF
         END_3D

      ENDIF

      ! Computation of the P-I slope for nanos, picos and diatoms
      ! The formulation proposed by Geider et al. (1997) has been used.
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            zmxl_fac = 1.0 - EXP( -0.26 * zmxl(ji,jj,jk) )
            zmxl_chl = zmxl(ji,jj,jk) / 24.
            zprbio(ji,jj,jk) = zprmaxn(ji,jj,jk) * zmxl_fac
            zprdia(ji,jj,jk) = zprmaxd(ji,jj,jk) * zmxl_fac
            zprpic(ji,jj,jk) = zprmaxp(ji,jj,jk) * zmxl_fac
            zprnut(ji,jj,jk) = zprnut (ji,jj,jk) * zmxl_fac

            ! Computation of the P-I slope for nanos and diatoms
            ! Nanophytoplankton
            zpislopeadn(ji,jj,jk) = pislopen * tr(ji,jj,jk,jpnch,Kbb)    &
            &                       /( tr(ji,jj,jk,jpphy,Kbb) * 12. + rtrn)

            ! Picophytoplankton
            zpislopeadp(ji,jj,jk) = pislopep * tr(ji,jj,jk,jppch,Kbb)    &
            &                       /( tr(ji,jj,jk,jppic,Kbb) * 12. + rtrn)

            ! Diatoms
            zpislopeadd(ji,jj,jk) = pisloped * tr(ji,jj,jk,jpdch,Kbb)    &
               &                    /( tr(ji,jj,jk,jpdia,Kbb) * 12. + rtrn)
            !
            zpislopen = zpislopeadn(ji,jj,jk) / ( zprbio(ji,jj,jk) * rday * xlimphy(ji,jj,jk) + rtrn )
            zpislopep = zpislopeadp(ji,jj,jk) / ( zprpic(ji,jj,jk) * rday * xlimpic(ji,jj,jk) + rtrn )
            zpisloped = zpislopeadd(ji,jj,jk) / ( zprdia(ji,jj,jk) * rday * xlimdia(ji,jj,jk) + rtrn )

            ! Computation of production function for Carbon
            ! Actual light levels are used here 
            !  ---------------------------------------------
            zprbio(ji,jj,jk) = zprbio(ji,jj,jk) * ( 1.- EXP( -zpislopen * enano(ji,jj,jk) )  )
            zprpic(ji,jj,jk) = zprpic(ji,jj,jk) * ( 1.- EXP( -zpislopep * epico(ji,jj,jk) )  )
            zprdia(ji,jj,jk) = zprdia(ji,jj,jk) * ( 1.- EXP( -zpisloped * ediat(ji,jj,jk) )  )

            !  Computation of production function for Chlorophyll
            !  Mean light level in the mixed layer (when appropriate)
            !  is used here (acclimation is in general slower than 
            !  the characteristic time scales of vertical mixing)
            !  ------------------------------------------------------
            zpislopen = zpislopen * zmxl_fac / ( zmxl_chl + rtrn )
            zpisloped = zpisloped * zmxl_fac / ( zmxl_chl + rtrn )
            zpislopep = zpislopep * zmxl_fac / ( zmxl_chl + rtrn )
            !
            zprchln(ji,jj,jk) = ( 1.- EXP( -zpislopen * enanom(ji,jj,jk) )  )
            zprchlp(ji,jj,jk) = ( 1.- EXP( -zpislopep * epicom(ji,jj,jk) )  )
            zprchld(ji,jj,jk) = ( 1.- EXP( -zpisloped * ediatm(ji,jj,jk) )  )
         ENDIF
      END_3D

      DO_3D( 0, 0, 0, 0, 1, jpkm1)
          IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            !
            ! Si/C of diatoms
            ! ------------------------
            ! Si/C increases with iron stress and silicate availability (zsilfac)
            ! Si/C is arbitrariliy increased for very high Si concentrations
            ! to mimic the very high ratios observed in the Southern Ocean (zsilfac)
            ! A parameterization derived from Flynn (2003) is used for the control
            ! when Si is not limiting which is similar to the parameterisation
            ! proposed by Gurney and Davidson (1999).
            ! -----------------------------------------------------------------------
            zlim  = tr(ji,jj,jk,jpsil,Kbb) / ( tr(ji,jj,jk,jpsil,Kbb) + xksi1 )
            zsilim = MIN(1.0, xlimdia(ji,jj,jk) * zprdia(ji,jj,jk) / ( zprnut(ji,jj,jk) + rtrn ) )
            zsiborn = tr(ji,jj,jk,jpsil,Kbb) * tr(ji,jj,jk,jpsil,Kbb) * tr(ji,jj,jk,jpsil,Kbb)
            IF (gphit(ji,jj) < -30 ) THEN
              zsilfac = 1. + 2. * zsiborn / ( zsiborn + xksi2**3 )
            ELSE
              zsilfac = 1. + 1. * zsiborn / ( zsiborn + xksi2**3 )
            ENDIF
            zratiosi = 1.0 - tr(ji,jj,jk,jpdsi,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) * zsilfac * grosip * 3.0 + rtrn )
            zratiosi = MAX(0., MIN(1.0, zratiosi) )
            zmaxsi  = (1.0 + 0.1**4) * zratiosi**4 / ( zratiosi**4 + 0.1**4 )
            IF ( xlimsi(ji,jj,jk) /= xlimdia(ji,jj,jk) ) THEN
               zysopt(ji,jj,jk) = zlim * zsilfac * grosip * 1.0 * zmaxsi
            ELSE
               zysopt(ji,jj,jk) = zlim * zsilfac * grosip * 1.0 * zsilim**0.7 * zmaxsi
            ENDIF
         ENDIF
      END_3D

      ! Sea-ice effect on production
      ! No production is assumed below sea ice
      ! --------------------------------------
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            zprbio(ji,jj,jk) = zprbio(ji,jj,jk) * (1.0 - fr_i(ji,jj) )
            zprdia(ji,jj,jk) = zprdia(ji,jj,jk) * (1.0 - fr_i(ji,jj) )
            zprpic(ji,jj,jk) = zprpic(ji,jj,jk) * (1.0 - fr_i(ji,jj) )
            zprnut(ji,jj,jk) = zprnut(ji,jj,jk) * (1.0 - fr_i(ji,jj) )
         ENDIF
      END_3D


      ! Computation of the various production and uptake terms of nanophytoplankton 
      ! Interactions between N and P are modeled according to the Chain Model 
      ! of Pahlow et al. (2009). Iron uptake is modeled following traditional
      ! Droop kinetics. When the quota is approaching the maximum achievable
      ! quota, uptake is downregulated according to a sigmoidal function 
      ! (power 2), as proposed by Flynn (2003)
      ! ---------------------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            !  production terms for nanophyto.
            zprorcan(ji,jj,jk) = zprbio(ji,jj,jk)  * xlimphy(ji,jj,jk) * tr(ji,jj,jk,jpphy,Kbb) * rfact2

            ! Size computation
            ! Size is made a function of the limitation of of phytoplankton growth
            ! Strongly limited cells are supposed to be smaller. sizena is the 
            ! size at time step t+1 and is thus updated at the end of the 
            ! current time step
            ! --------------------------------------------------------------------
            zlimfac = xlimphys(ji,jj,jk) * zprchln(ji,jj,jk)
            zsizetmp = 1.0 + 1.3 * ( xsizern - 1.0 ) * zlimfac**3/(0.3 + zlimfac**3)
            sizena(ji,jj,jk) = MIN(xsizern, MAX( sizena(ji,jj,jk), zsizetmp ) )
            ! Maximum potential uptake rate
            zration = tr(ji,jj,jk,jpnph,Kbb) / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn )
            zratiop = tr(ji,jj,jk,jppph,Kbb) / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn )
            zratiof = tr(ji,jj,jk,jpnfe,Kbb) / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn )
            zprnutmax = zprnut(ji,jj,jk) * fvnuptk(ji,jj,jk) / rno3 * tr(ji,jj,jk,jpphy,Kbb) * rfact2
            ! Uptake of nitrogen
            zratio = 1.0 - MIN( 1., zration / (xqnnmax(ji,jj,jk) + rtrn) )
            zmax = MAX(0., MIN(1., zratio**2 / (0.05**2 + zratio**2) ) )
            zpronmaxn(ji,jj,jk) = zprnutmax * zmax * MAX(0., MIN(1., ( zratiop - xqpnmin(ji,jj,jk) )   &
            &          / ( xqpnmax(ji,jj,jk) - xqpnmin(ji,jj,jk) + rtrn ), xlimnfe(ji,jj,jk) ) )

            ! Uptake of phosphorus and DOP
            zratio = 1.0 - MIN( 1., zratiop / (xqpnmax(ji,jj,jk) + rtrn) )
            zmax = MAX(0., MIN(1., zratio**2 / (0.05**2 + zratio**2) ) )
            zpropmaxn(ji,jj,jk) = 2.0 * zprnutmax * zmax * xlimnfe(ji,jj,jk)
            ! Uptake of iron
            zqfnmax = xqfuncfecn(ji,jj,jk) + ( qfnmax - xqfuncfecn(ji,jj,jk) ) * xlimnpn(ji,jj,jk)
            zratio = 1.0 - MIN( 1., zratiof / zqfnmax )
            zmax = MAX(0., MIN(1., zratio**2/ (0.05**2 + zratio**2) ) )
            zprofmax = zprnutmax * zqfnmax * zmax 
            zprofen(ji,jj,jk) = zprofmax * xnanofer(ji,jj,jk)    &
            &          * (1. + 0.8 * xnanono3(ji,jj,jk) / ( rtrn  &
            &          + xnanono3(ji,jj,jk) + xnanonh4(ji,jj,jk) ) * (1. - xnanofer(ji,jj,jk) ) )
         ENDIF
      END_3D

      ! Computation of the various production and uptake terms of picophytoplankton 
      ! Interactions between N and P are modeled according to the Chain Model 
      ! of Pahlow et al. (2009). Iron uptake is modeled following traditional
      ! Droop kinetics. When the quota is approaching the maximum achievable
      ! quota, uptake is downregulated according to a sigmoidal function 
      ! (power 2), as proposed by Flynn (2003)
      ! ---------------------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            !
            !  production terms for picophyto.
            zprorcap(ji,jj,jk) = zprpic(ji,jj,jk)  * xlimpic(ji,jj,jk) * tr(ji,jj,jk,jppic,Kbb) * rfact2
            ! Size computation
            ! Size is made a function of the limitation of of phytoplankton growth
            ! Strongly limited cells are supposed to be smaller. sizepa is
            ! size at time step t+1 and is thus updated at the end of the 
            ! current time step
            ! --------------------------------------------------------------------
            zlimfac = zprchlp(ji,jj,jk)  * xlimpics(ji,jj,jk)
            zsizetmp = 1.0 + 1.3 * ( xsizerp - 1.0 ) * zlimfac**3/(0.3 + zlimfac**3)
            sizepa(ji,jj,jk) = min(xsizerp, max( sizepa(ji,jj,jk), zsizetmp ) )
            ! Maximum potential uptake rate of nutrients
            zration = tr(ji,jj,jk,jpnpi,Kbb) / ( tr(ji,jj,jk,jppic,Kbb) + rtrn )
            zratiop = tr(ji,jj,jk,jpppi,Kbb) / ( tr(ji,jj,jk,jppic,Kbb) + rtrn )
            zratiof = tr(ji,jj,jk,jppfe,Kbb) / ( tr(ji,jj,jk,jppic,Kbb) + rtrn )
            zprnutmax = zprnut(ji,jj,jk) * fvpuptk(ji,jj,jk) / rno3 * tr(ji,jj,jk,jppic,Kbb) * rfact2
            ! Uptake of nitrogen
            zratio = 1.0 - MIN( 1., zration / (xqnpmax(ji,jj,jk) + rtrn) )
            zmax = MAX(0., MIN(1., zratio**2/ (0.05**2 + zratio**2) ) )
            zpronmaxp(ji,jj,jk) = zprnutmax * zmax * MAX(0., MIN(1., ( zratiop - xqppmin(ji,jj,jk) )   &
            &          / ( xqppmax(ji,jj,jk) - xqppmin(ji,jj,jk) + rtrn ), xlimpfe(ji,jj,jk) ) )

            ! Uptake of phosphorus
            zratio = 1.0 - MIN( 1., zratiop / (xqppmax(ji,jj,jk) + rtrn) )
            zmax = MAX(0., MIN(1., zratio**2 / (0.05**2 + zratio**2) ) )
            zpropmaxp(ji,jj,jk) = 2.0 * zprnutmax * zmax * xlimpfe(ji,jj,jk) 
            ! Uptake of iron
            zqfpmax = xqfuncfecp(ji,jj,jk) + ( qfpmax - xqfuncfecp(ji,jj,jk) ) * xlimnpp(ji,jj,jk)
            zratio = 1.0 - MIN( 1., zratiof / zqfpmax )
            zmax = MAX(0., MIN(1., zratio**2 / (0.05**2 + zratio**2) ) )
            zprofmax = zprnutmax * zqfpmax * zmax
            zprofep(ji,jj,jk) = zprofmax * xpicofer(ji,jj,jk)  &
            &          * (1. + 0.8 * xpicono3(ji,jj,jk) / ( rtrn   &
            &          + xpicono3(ji,jj,jk) + xpiconh4(ji,jj,jk) ) * (1. - xpicofer(ji,jj,jk) ) )
         ENDIF
      END_3D

      ! Computation of the various production and uptake terms of diatoms
      ! Interactions between N and P are modeled according to the Chain Model 
      ! of Pahlow et al. (2009). Iron uptake is modeled following traditional
      ! Droop kinetics. When the quota is approaching the maximum achievable
      ! quota, uptake is downregulated according to a sigmoidal function 
      ! (power 2), as proposed by Flynn (2003)
      ! ---------------------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            !
            !  production terms for diatomees
            zprorcad(ji,jj,jk) = zprdia(ji,jj,jk) * xlimdia(ji,jj,jk) * tr(ji,jj,jk,jpdia,Kbb) * rfact2
            ! Size computation
            ! Size is made a function of the limitation of of phytoplankton growth
            ! Strongly limited cells are supposed to be smaller. sizeda is
            ! size at time step t+1 and is thus updated at the end of the 
            ! current time step. 
            ! --------------------------------------------------------------------
            zlimfac = zprchld(ji,jj,jk) * xlimdias(ji,jj,jk)
            zsizetmp = 1.0 + 1.3 * ( xsizerd - 1.0 ) * zlimfac**3/(0.3 + zlimfac**3)
            sizeda(ji,jj,jk) = min(xsizerd, max( sizeda(ji,jj,jk), zsizetmp ) )
            ! Maximum potential uptake rate of nutrients
            zration = tr(ji,jj,jk,jpndi,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
            zratiop = tr(ji,jj,jk,jppdi,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
            zratiof = tr(ji,jj,jk,jpdfe,Kbb) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
            zprnutmax = zprnut(ji,jj,jk) * fvduptk(ji,jj,jk) / rno3 * tr(ji,jj,jk,jpdia,Kbb) * rfact2
            ! Uptake of nitrogen
            zratio = 1.0 - MIN( 1., zration / (xqndmax(ji,jj,jk) + rtrn) )
            zmax = MAX(0., MIN(1., zratio**2 / (0.05**2 + zratio**2) ) )
            zpronmaxd(ji,jj,jk) = zprnutmax * zmax * MAX(0., MIN(1., ( zratiop - xqpdmin(ji,jj,jk) )   &
            &          / ( xqpdmax(ji,jj,jk) - xqpdmin(ji,jj,jk) + rtrn ), xlimdfe(ji,jj,jk) ) )

            ! Uptake of phosphorus
            zratio = 1.0 - MIN( 1., zratiop / (xqpdmax(ji,jj,jk) + rtrn) )
            zmax = MAX(0., MIN(1., zratio**2/ (0.05**2 + zratio**2) ) )
            zpropmaxd(ji,jj,jk) = 2.0 * zprnutmax * zmax * xlimdfe(ji,jj,jk)
            ! Uptake of iron
            zqfdmax = xqfuncfecd(ji,jj,jk) + ( qfdmax - xqfuncfecd(ji,jj,jk) ) * xlimnpd(ji,jj,jk)
            zratio = 1.0 - MIN( 1., zratiof / zqfdmax )
            zmax = MAX(0., MIN(1., zratio**2 / (0.05**2 + zratio**2) ) )
            zprofmax = zprnutmax * zqfdmax * zmax
            zprofed(ji,jj,jk) = zprofmax * xdiatfer(ji,jj,jk)    &
            &          * (1. + 0.8 * xdiatno3(ji,jj,jk) / ( rtrn   &
            &          + xdiatno3(ji,jj,jk) + xdiatnh4(ji,jj,jk) ) * (1. - xdiatfer(ji,jj,jk) ) )
         ENDIF
      END_3D

      ! Production of Chlorophyll. The formulation proposed by Pahlow and Oschlies 
      ! is adopted here.
      ! --------------------------------------------------------------------
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            zmxl_chl = zmxl(ji,jj,jk) / 24.
            !  production terms for nanophyto. ( chlorophyll )
            zpronewn = zpronmaxn(ji,jj,jk) * xnanono3(ji,jj,jk)
            zproregn = zpronmaxn(ji,jj,jk) * xnanonh4(ji,jj,jk)
            znanotot = enanom(ji,jj,jk) / ( zmxl_chl + rtrn )
            zprod1 = ( zprorcan(ji,jj,jk) * texcretn - xpsinh4 * zproregn   &
            &        - xpsino3 * zpronewn ) / ( tr(ji,jj,jk,jpphy,Kbb) + rtrn )
            zprod = zprod1 / ratchl * ( pislopen * znanotot / ( zprmaxn(ji,jj,jk) * rday )   &
            &   * ( 1.0 - zprchln(ji,jj,jk) ) * MAX(0.0, (1.0 - ratchl * tr(ji,jj,jk,jpnch,Kbb)    &
            &   / ( 12. * tr(ji,jj,jk,jpphy,Kbb) + rtrn ) / (xlimphy(ji,jj,jk) + rtrn ) ) )     &
            &   - ratchl * zprchln(ji,jj,jk) ) + zprod1
            zprochln = MAX(zprod * tr(ji,jj,jk,jpnch,Kbb) , chlcmin * 12 * zprorcan(ji,jj,jk) )
            !  production terms for picophyto. ( chlorophyll )
            zpronewp = zpronmaxp(ji,jj,jk) * xpicono3(ji,jj,jk)
            zproregp = zpronmaxp(ji,jj,jk) * xpiconh4(ji,jj,jk)
            zpicotot = epicom(ji,jj,jk) / ( zmxl_chl + rtrn )
            zprod1 = ( zprorcap(ji,jj,jk) * texcretp - xpsinh4 * zproregp   &
            &        - xpsino3 * zpronewp ) / ( tr(ji,jj,jk,jppic,Kbb) + rtrn )
            zprod = zprod1 / ratchl * ( pislopep * zpicotot / ( zprmaxp(ji,jj,jk) * rday )   &
            &   * ( 1.0 - zprchlp(ji,jj,jk) ) * MAX(0.0, (1.0 - ratchl * tr(ji,jj,jk,jppch,Kbb)    &
            &   / ( 12. * tr(ji,jj,jk,jppic,Kbb) + rtrn ) / (xlimpic(ji,jj,jk) + rtrn ) ) )     &
            &   - ratchl * zprchlp(ji,jj,jk) ) + zprod1
            zprochlp = MAX(zprod * tr(ji,jj,jk,jppch,Kbb) , chlcmin * 12 * zprorcap(ji,jj,jk) )
            !  production terms for diatoms ( chlorophyll )
            zpronewd = zpronmaxd(ji,jj,jk) * xdiatno3(ji,jj,jk)
            zproregd = zpronmaxd(ji,jj,jk) * xdiatnh4(ji,jj,jk)
            zdiattot = ediatm(ji,jj,jk) / ( zmxl_chl + rtrn )
            zprod1 = ( zprorcad(ji,jj,jk) * texcretd - xpsinh4 * zproregd   &
            &        - xpsino3 * zpronewd ) / ( tr(ji,jj,jk,jpdia,Kbb) + rtrn )
            zprod = zprod1 / ratchl * ( pisloped * zdiattot / ( zprmaxd(ji,jj,jk) * rday )   &
            &   * ( 1.0 - zprchld(ji,jj,jk) ) * MAX(0.0, (1.0 - ratchl * tr(ji,jj,jk,jpdch,Kbb)    &
            &   / ( 12. * tr(ji,jj,jk,jpdia,Kbb) + rtrn ) / (xlimdia(ji,jj,jk) + rtrn ) ) )     &
            &   - ratchl * zprchld(ji,jj,jk) ) + zprod1
            zprochld = MAX(zprod * tr(ji,jj,jk,jpdch,Kbb) , chlcmin * 12 * zprorcad(ji,jj,jk) )
            !   Update the arrays TRA which contain the Chla sources and sinks
            tr(ji,jj,jk,jpnch,Krhs) = tr(ji,jj,jk,jpnch,Krhs) + zprochln
            tr(ji,jj,jk,jpdch,Krhs) = tr(ji,jj,jk,jpdch,Krhs) + zprochld
            tr(ji,jj,jk,jppch,Krhs) = tr(ji,jj,jk,jppch,Krhs) + zprochlp
         ENDIF
      END_3D

      !   Update the arrays TRA which contain the biological sources and sinks
      DO_3D( 0, 0, 0, 0, 1, jpkm1)
         IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
            zpronewn = zpronmaxn(ji,jj,jk) * xnanono3(ji,jj,jk)
            zpronewp = zpronmaxp(ji,jj,jk) * xpicono3(ji,jj,jk) 
            zpronewd = zpronmaxd(ji,jj,jk) * xdiatno3(ji,jj,jk)
            !
            zproregn = zpronmaxn(ji,jj,jk) * xnanonh4(ji,jj,jk)
            zproregp = zpronmaxp(ji,jj,jk) * xpiconh4(ji,jj,jk)
            zproregd = zpronmaxd(ji,jj,jk) * xdiatnh4(ji,jj,jk)
            !
            zpropo4n = zpropmaxn(ji,jj,jk) * xnanopo4(ji,jj,jk)
            zpropo4p = zpropmaxp(ji,jj,jk) * xpicopo4(ji,jj,jk)
            zpropo4d = zpropmaxd(ji,jj,jk) * xdiatpo4(ji,jj,jk)
            !
            zprodopn = zpropmaxn(ji,jj,jk) * xnanodop(ji,jj,jk)
            zprodopp = zpropmaxp(ji,jj,jk) * xpicodop(ji,jj,jk)
            zprodopd = zpropmaxd(ji,jj,jk) * xdiatdop(ji,jj,jk)
            !
            zpo4tot  = zpropo4n + zpropo4d + zpropo4p
            zpnewtot = zpronewn + zpronewd + zpronewp
            zpregtot = zproregn + zproregd + zproregp
            !
            zpptot   = zprorcap(ji,jj,jk) + zprorcan(ji,jj,jk) + zprorcad(ji,jj,jk)

            zprontot = zpronewn + zproregn
            zproptot = zpronewp + zproregp
            zprodtot = zpronewd + zproregd
            !
            zproddoc = excretd * zprorcad(ji,jj,jk) &
            &        + excretn * zprorcan(ji,jj,jk) &
            &        + excretp * zprorcap(ji,jj,jk)
            !
            zproddop = excretd * zpropo4d - texcretd * zprodopd &
            &        + excretn * zpropo4n - texcretn * zprodopn &
            &        + excretp * zpropo4p - texcretp * zprodopp

            zproddon =  excretd * zprodtot + excretn * zprontot + excretp * zproptot

            zprodfer = texcretn * zprofen(ji,jj,jk) + texcretd * zprofed(ji,jj,jk) + texcretp * zprofep(ji,jj,jk)
            !
            tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) - zpo4tot
            tr(ji,jj,jk,jpno3,Krhs) = tr(ji,jj,jk,jpno3,Krhs) - zpnewtot
            tr(ji,jj,jk,jpnh4,Krhs) = tr(ji,jj,jk,jpnh4,Krhs) - zpregtot  
            !
            tr(ji,jj,jk,jpphy,Krhs) = tr(ji,jj,jk,jpphy,Krhs)         &
              &                     + zprorcan(ji,jj,jk) * texcretn  &
              &                     - xpsino3 * zpronewn   &
              &                     - xpsinh4 * zproregn   

            tr(ji,jj,jk,jpnph,Krhs) = tr(ji,jj,jk,jpnph,Krhs) + zprontot * texcretn
            tr(ji,jj,jk,jppph,Krhs) = tr(ji,jj,jk,jppph,Krhs) + ( zpropo4n + zprodopn ) * texcretn
            tr(ji,jj,jk,jpnfe,Krhs) = tr(ji,jj,jk,jpnfe,Krhs) + zprofen(ji,jj,jk) * texcretn

            !
            tr(ji,jj,jk,jppic,Krhs) = tr(ji,jj,jk,jppic,Krhs)         &
              &                     + zprorcap(ji,jj,jk) * texcretp  &
              &                     - xpsino3 * zpronewp   &
              &                     - xpsinh4 * zproregp   

            tr(ji,jj,jk,jpnpi,Krhs) = tr(ji,jj,jk,jpnpi,Krhs) + zproptot * texcretp
            tr(ji,jj,jk,jpppi,Krhs) = tr(ji,jj,jk,jpppi,Krhs) + ( zpropo4p + zprodopp ) * texcretp
            tr(ji,jj,jk,jppfe,Krhs) = tr(ji,jj,jk,jppfe,Krhs) + zprofep(ji,jj,jk) * texcretp

            !
            tr(ji,jj,jk,jpdia,Krhs) = tr(ji,jj,jk,jpdia,Krhs)         &
              &                     + zprorcad(ji,jj,jk) * texcretd   &
              &                     - xpsino3 * zpronewd    &
              &                     - xpsinh4 * zproregd    

            !
            zprodsil = zprmaxd(ji,jj,jk) * zysopt(ji,jj,jk) * rfact2 * tr(ji,jj,jk,jpdia,Kbb) 
            !
            tr(ji,jj,jk,jpndi,Krhs) = tr(ji,jj,jk,jpndi,Krhs) + zprodtot * texcretd
            tr(ji,jj,jk,jppdi,Krhs) = tr(ji,jj,jk,jppdi,Krhs) + ( zpropo4d + zprodopd ) * texcretd
            tr(ji,jj,jk,jpdfe,Krhs) = tr(ji,jj,jk,jpdfe,Krhs) + zprofed(ji,jj,jk) * texcretd
            tr(ji,jj,jk,jpdsi,Krhs) = tr(ji,jj,jk,jpdsi,Krhs) + zprodsil
            tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zproddoc
            tr(ji,jj,jk,jpdon,Krhs) = tr(ji,jj,jk,jpdon,Krhs) + zproddon                                        
            tr(ji,jj,jk,jpdop,Krhs) = tr(ji,jj,jk,jpdop,Krhs) + zproddop
  
            tr(ji,jj,jk,jpoxy,Krhs) = tr(ji,jj,jk,jpoxy,Krhs) &
              &                     + o2ut * zpregtot + ( o2ut + o2nit ) * zpnewtot

            tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) - zprodfer
            consfe3(ji,jj,jk)       = zprodfer * 75.0 / ( rtrn + ( plig(ji,jj,jk) + 75.0 * (1.0 - plig(ji,jj,jk) ) )   &
                &                   * tr(ji,jj,jk,jpfer,Kbb) ) / rfact2
            tr(ji,jj,jk,jpsil,Krhs) = tr(ji,jj,jk,jpsil,Krhs) - zprodsil

            tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) - zpptot  &
               &                     + xpsino3 * zpronewn + xpsinh4 * zproregn   &
               &                     + xpsino3 * zpronewp + xpsinh4 * zproregp   &
               &                     + xpsino3 * zpronewd + xpsinh4 * zproregd  

            tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) + rno3 * ( zpnewtot - zpregtot )
            !
         ENDIF
      END_3D
     
     ! Production and uptake of ligands by phytoplankton. This part is activated 
     ! when ln_ligand is set to .true. in the namelist. Ligand uptake is small 
     ! and based on the FeL model by Morel et al. (2008) and on the study of
     ! Shaked and Lis (2012)
     ! -------------------------------------------------------------------------
     IF( ln_ligand ) THEN
         DO_3D( 0, 0, 0, 0, 1, jpkm1)
            IF( etot_ndcy(ji,jj,jk) > 1.E-3 ) THEN
              zproddoc = excretd * zprorcad(ji,jj,jk) + excretn * zprorcan(ji,jj,jk) + excretp * zprorcap(ji,jj,jk)
              zprodfer = texcretn * zprofen(ji,jj,jk) + texcretd * zprofed(ji,jj,jk) + texcretp * zprofep(ji,jj,jk)
              zprodlig = plig(ji,jj,jk) / ( rtrn + plig(ji,jj,jk) + 75.0 * (1.0 - plig(ji,jj,jk) ) ) * lthet 
              !
              tr(ji,jj,jk,jplgw,Krhs) = tr(ji,jj,jk,jplgw,Krhs) + zproddoc * ldocp - zprodfer * zprodlig
            ENDIF
        END_3D
     ENDIF

    ! Output of the diagnostics
    ! Total primary production per year
    IF( l_dia_ppphy .OR. ( ln_check_mass .AND. kt == nitend .AND. knt == nrdttrc )  )  THEN
       ALLOCATE( zw3d(A2D(0),jpk) )  ;  zw3d(A2D(0),jpk) = 0._wp
       DO_3D( 0, 0, 0, 0, 1, jpkm1)
          zw3d(ji,jj,jk) = ( zprorcan(ji,jj,jk) + zprorcad(ji,jj,jk) + zprorcap(ji,jj,jk)     &
              &            - zpronmaxn(ji,jj,jk) * ( xpsino3 * xnanono3(ji,jj,jk) + xpsinh4 * xnanonh4(ji,jj,jk) )   &
              &            - zpronmaxd(ji,jj,jk) * ( xpsino3 * xdiatno3(ji,jj,jk) + xpsinh4 * xdiatnh4(ji,jj,jk) )   &
              &            - zpronmaxp(ji,jj,jk) * ( xpsino3 * xpicono3(ji,jj,jk) + xpsinh4 * xpiconh4(ji,jj,jk) ) ) &
              &            * cvol(ji,jj,jk)
       END_3D
       tpp = glob_sum( 'p5zprod', zw3d )
       DEALLOCATE ( zw3d )
    ENDIF

    IF( lk_iomput .AND.  knt == nrdttrc ) THEN
       !
       IF( l_dia_ppphy ) THEN
          zfact = 1.e+3 * rfact2r  !  conversion from mol/l/kt to  mol/m3/s
          ALLOCATE( zw3d(A2D(0),jpk) )  ;  zw3d(A2D(0),jpk) = 0._wp
          ! primary production by nanophyto
          zw3d(A2D(0),1:jpkm1) = (zprorcan(A2D(0),1:jpkm1) - xpsinh4 * zpronmaxn(A2D(0),1:jpkm1) * xnanonh4(A2D(0),1:jpkm1)   &
              &                  - xpsino3 * zpronmaxn(A2D(0),1:jpkm1) * xnanono3(A2D(0),1:jpkm1) )   &
              &                  * zfact * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "PPPHYN", zw3d )
          ! primary production by diatoms
          zw3d(A2D(0),1:jpkm1) = (zprorcad(A2D(0),1:jpkm1) - xpsinh4 * zpronmaxd(A2D(0),1:jpkm1) * xdiatnh4(A2D(0),1:jpkm1)   &
              &                  - xpsino3 * zpronmaxd(A2D(0),1:jpkm1) * xdiatno3(A2D(0),1:jpkm1) )   &
              &                  * zfact * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "PPPHYD", zw3d )
          ! primary production by pico
          zw3d(A2D(0),1:jpkm1) = (zprorcap(A2D(0),1:jpkm1) - xpsinh4 * zpronmaxp(A2D(0),1:jpkm1) * xpiconh4(A2D(0),1:jpkm1)   &
              &                  - xpsino3 * zpronmaxp(A2D(0),1:jpkm1) * xpicono3(A2D(0),1:jpkm1) )   &
              &                  * zfact * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "PPPHYP", zw3d )
          ! total primary production
          zw3d(A2D(0),1:jpkm1) = ( zprorcan(A2D(0),1:jpkm1) + zprorcad(A2D(0),1:jpkm1) + zprorcap(A2D(0),1:jpkm1)                           &
              &                 - zpronmaxn(A2D(0),1:jpkm1) * ( xpsino3 * xnanono3(A2D(0),1:jpkm1) + xpsinh4 * xnanonh4(A2D(0),1:jpkm1) )   &
              &                 - zpronmaxd(A2D(0),1:jpkm1) * ( xpsino3 * xdiatno3(A2D(0),1:jpkm1) + xpsinh4 * xdiatnh4(A2D(0),1:jpkm1) )   &
              &                 - zpronmaxp(A2D(0),1:jpkm1) * ( xpsino3 * xpicono3(A2D(0),1:jpkm1) + xpsinh4 * xpiconh4(A2D(0),1:jpkm1) ) ) & 
              &                 * zfact * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "TPP", zw3d )
          CALL iom_put( "tintpp"  , tpp * zfact )  !  global total integrated primary production molC/s
          DEALLOCATE ( zw3d )
       ENDIF
       !
       IF( l_dia_gpphy ) THEN
          zfact = 1.e+3 * rfact2r  !  conversion from mol/l/kt to  mol/m3/s
          ALLOCATE( zw3d(A2D(0),jpk) )  ;  zw3d(A2D(0),jpk) = 0._wp
          ! primary production by nanophyto
          zw3d(A2D(0),1:jpkm1) = zprorcan(A2D(0),1:jpkm1) * zfact * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "GPPHYN", zw3d )
          ! primary production by diatoms
          zw3d(A2D(0),1:jpkm1) = zprorcad(A2D(0),1:jpkm1) * zfact * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "GPPHYD", zw3d )
          ! primary production by pico
          zw3d(A2D(0),1:jpkm1) = zprorcap(A2D(0),1:jpkm1) * zfact * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "GPPHYP", zw3d )
          DEALLOCATE ( zw3d )
       ENDIF
       !
       IF( l_dia_ppnew ) THEN
          zfact = 1.e+3 * rfact2r  !  conversion from mol/l/kt to  mol/m3/s
          ALLOCATE( zw3d(A2D(0),jpk) )  ;  zw3d(A2D(0),jpk) = 0._wp
          ! new primary production by nano
          zw3d(A2D(0),1:jpkm1) = zpronmaxn(A2D(0),1:jpkm1) * xnanono3(A2D(0),1:jpkm1) * zfact * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "PPNEWN", zw3d )
          ! new primary production by diatomes
          zw3d(A2D(0),1:jpkm1) = zpronmaxd(A2D(0),1:jpkm1) * xdiatno3(A2D(0),1:jpkm1) * zfact * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "PPNEWD", zw3d )
          ! new primary production by pico
          zw3d(A2D(0),1:jpkm1) = zpronmaxp(A2D(0),1:jpkm1) * xpicono3(A2D(0),1:jpkm1) * zfact * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "PPNEWP", zw3d )
          ! total new production
          zw3d(A2D(0),1:jpkm1) = ( zpronmaxn(A2D(0),1:jpkm1) * xnanono3(A2D(0),1:jpkm1) +  &
                &                  zpronmaxd(A2D(0),1:jpkm1) * xdiatno3(A2D(0),1:jpkm1) +  &
                &                  zpronmaxp(A2D(0),1:jpkm1) * xpicono3(A2D(0),1:jpkm1) ) &
                &                  * zfact * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "TPNEW", zw3d )
          DEALLOCATE ( zw3d )
       ENDIF
       !
       IF( l_dia_ppbsi ) THEN
          zfact = 1.e+3 * rfact2r  !  conversion from mol/l/kt to  mol/m3/s
          ALLOCATE( zw3d(A2D(0),jpk) )  ;  zw3d(A2D(0),jpk) = 0._wp
          !  biogenic silica production
          zw3d(A2D(0),1:jpkm1) = zprmaxd(A2D(0),1:jpkm1) * zysopt(A2D(0),1:jpkm1) &
              &                 * zfact * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "PBSi", zw3d )
          DEALLOCATE ( zw3d )
       ENDIF
       !
       !
       IF( l_dia_ppbfe ) THEN
          zfact = 1.e+3 * rfact2r  !  conversion from mol/l/kt to  mol/m3/s
          ALLOCATE( zw3d(A2D(0),jpk) )  ;  zw3d(A2D(0),jpk) = 0._wp
          ! biogenic iron production by nanophyto
          zw3d(A2D(0),1:jpkm1) = zprofen(A2D(0),1:jpkm1) * zfact * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "PFeN", zw3d )
          ! biogenic iron production by diatomes
          zw3d(A2D(0),1:jpkm1) = zprofed(A2D(0),1:jpkm1) * zfact * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "PFeD", zw3d )
          ! biogenic iron production by pico
          zw3d(A2D(0),1:jpkm1) = zprofep(A2D(0),1:jpkm1) * zfact * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "PFeP", zw3d )
          ! total biogenic iron production
          zw3d(A2D(0),1:jpkm1) = ( zprofen(A2D(0),1:jpkm1) + zprofed(A2D(0),1:jpkm1) + zprofep(A2D(0),1:jpkm1) ) &
             &                  * zfact * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "TPBFE", zw3d )
          DEALLOCATE ( zw3d )
       ENDIF
       !
       IF( l_dia_mu ) THEN
          zfact = 1.e+3 * rfact2r  !  conversion from mol/l/kt to  mol/m3/s
          ALLOCATE( zw3d(A2D(0),jpk) )  ;  zw3d(A2D(0),jpk) = 0._wp
          zw3d(A2D(0),1:jpkm1) = zprmaxn(A2D(0),1:jpkm1)  * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "Mumax", zw3d )
          ! Realized growth rate for nanophyto
          zw3d(A2D(0),1:jpkm1) = zprbio(A2D(0),1:jpkm1) * xlimphy(A2D(0),1:jpkm1) * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "MuN", zw3d )
          ! Realized growth rate for diatoms
          zw3d(A2D(0),1:jpkm1) = zprdia(A2D(0),1:jpkm1) * xlimdia(A2D(0),1:jpkm1) * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "MuD", zw3d )
          ! Realized growth rate for pico
          zw3d(A2D(0),1:jpkm1) = zprpic(A2D(0),1:jpkm1) * xlimpic(A2D(0),1:jpkm1) * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "MuP", zw3d )
          DEALLOCATE ( zw3d )
       ENDIF
       !
       !
       IF( l_dia_light ) THEN
          zfact = 1.e+3 * rfact2r  !  conversion from mol/l/kt to  mol/m3/s
          ALLOCATE( zw3d(A2D(0),jpk) )  ;  zw3d(A2D(0),jpk) = 0._wp
          ! light limitation term for nano
          zw3d(A2D(0),1:jpkm1) = zprbio(A2D(0),1:jpkm1) / (zprmaxn(A2D(0),1:jpkm1)+rtrn) &
              &                  * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "LNlight", zw3d )
          ! light limitation term for diatomes
          zw3d(A2D(0),1:jpkm1) = zprdia(A2D(0),1:jpkm1) / (zprmaxd(A2D(0),1:jpkm1)+rtrn) &
              &                  * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "LDlight", zw3d )
          ! light limitation term for pico
          zw3d(A2D(0),1:jpkm1) = zprpic(A2D(0),1:jpkm1) / (zprmaxp(A2D(0),1:jpkm1)+rtrn) &
              &                  * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "LPlight", zw3d )
          DEALLOCATE ( zw3d )
       ENDIF
       !
        IF( l_dia_lprod ) THEN
          zfact = 1.e+3 * rfact2r  !  conversion from mol/l/kt to  mol/m3/s
          ALLOCATE( zw3d(A2D(0),jpk) )  ;  zw3d(A2D(0),jpk) = 0._wp
          zw3d(A2D(0),1:jpkm1) = ( excretd * zprorcad(A2D(0),1:jpkm1) + excretn * zprorcan(A2D(0),1:jpkm1) +  &
             &                     excretp * zprorcap(A2D(0),1:jpkm1) ) * zfact * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "LPRODP"  , zw3d * ldocp * 1e9 )
          !
          zw3d(A2D(0),1:jpkm1) = ( texcretn * zprofen(A2D(0),1:jpkm1) + texcretd * zprofed(A2D(0),1:jpkm1) +  &
            &                      texcretp * zprofep(A2D(0),1:jpkm1) ) * plig(A2D(0),1:jpkm1) &
            &                     / ( rtrn + plig(A2D(0),1:jpkm1) + 75.0 * (1.0 - plig(A2D(0),1:jpkm1) ) )  &
            &                  * zfact * tmask(A2D(0),1:jpkm1)
          CALL iom_put( "LDETP"   , zw3d * lthet * 1e9 )
          DEALLOCATE ( zw3d )
       ENDIF
       !
     ENDIF
      IF(sn_cfctl%l_prttrc)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('prod')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p5z_prod')
      !
   END SUBROUTINE p5z_prod


   SUBROUTINE p5z_prod_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p5z_prod_init  ***
      !!
      !! ** Purpose :   Initialization of phytoplankton production parameters
      !!
      !! ** Method  :   Read the namp5zprod namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist namp5zprod
      !!----------------------------------------------------------------------
      INTEGER :: ios    ! Local integer output status for namelist read
      !!
      NAMELIST/namp5zprod/ pislopen, pislopep, pisloped, excretn, excretp, excretd,     &
         &                 chlcmin, grosip, bresp
      !!----------------------------------------------------------------------

      READ_NML_REF(numnatp,namp5zprod)
      READ_NML_CFG(numnatp,namp5zprod)
      IF(lwm) WRITE ( numonp, namp5zprod )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for phytoplankton growth, namp5zprod'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    mean Si/C ratio                           grosip       =', grosip
         WRITE(numout,*) '    P-I slope                                 pislopen     =', pislopen
         WRITE(numout,*) '    P-I slope  for diatoms                    pisloped     =', pisloped
         WRITE(numout,*) '    P-I slope  for picophytoplankton          pislopep     =', pislopep
         WRITE(numout,*) '    excretion ratio of nanophytoplankton      excretn      =', excretn
         WRITE(numout,*) '    excretion ratio of picophytoplankton      excretp      =', excretp
         WRITE(numout,*) '    excretion ratio of diatoms                excretd      =', excretd
         WRITE(numout,*) '    basal respiration in phytoplankton        bresp        =', bresp
         WRITE(numout,*) '    Maximum Chl/C in phytoplankton            chlcmin      =', chlcmin
      ENDIF
      !
      r1_rday   = 1._wp / rday 
      texcretn  = 1._wp - excretn
      texcretp  = 1._wp - excretp
      texcretd  = 1._wp - excretd
      tpp       = 0._wp
      !
      xq10_n = 1. + xpsino3 * qnnmax
      xq10_d = 1. + xpsino3 * qndmax
      xq10_p = 1. + xpsino3 * qnpmax
      !
   END SUBROUTINE p5z_prod_init

   !!======================================================================
END MODULE p5zprod
