MODULE traadv
   !!==============================================================================
   !!                       ***  MODULE  traadv  ***
   !! Ocean active tracers:  advection trend
   !!==============================================================================
   !! History :  2.0  !  2005-11  (G. Madec)  Original code
   !!            3.3  !  2010-09  (C. Ethe, G. Madec)  merge TRC-TRA + switch from velocity to transport
   !!            3.6  !  2011-06  (G. Madec)  Addition of Mixed Layer Eddy parameterisation
   !!            3.7  !  2014-05  (G. Madec)  Add 2nd/4th order cases for CEN and FCT schemes
   !!             -   !  2014-12  (G. Madec) suppression of cross land advection option
   !!            3.6  !  2015-06  (E. Clementi) Addition of Stokes drift in case of wave coupling
   !!            4.5  !  2021-04  (G. Madec, S. Techene) add advective velocities as optional arguments
   !!            4.5  !  2022-06  (S. Techene, G, Madec) refactorization to reduce local memory usage
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_adv       : compute ocean tracer advection trend
   !!   tra_adv_init  : control the different options of advection scheme
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and active tracers
   USE dom_oce        ! ocean space and time domain
   ! TEMP: [tiling] This change not necessary after all lbc_lnks removed in the nn_hls = 2 case in tra_adv_fct
   USE domtile
   USE sbcwave        ! wave module
   USE sbc_oce        ! surface boundary condition: ocean
   USE traadv_cen     ! centered scheme            (tra_adv_cen  routine)
   USE traadv_fct     ! FCT      scheme            (tra_adv_fct  routine)
   USE traadv_mus     ! MUSCL    scheme            (tra_adv_mus  routine)
   USE traadv_ubs     ! UBS      scheme            (tra_adv_ubs  routine)
   USE traadv_qck     ! QUICKEST scheme            (tra_adv_qck  routine)
   USE tramle         ! Mixed Layer Eddy transport (tra_mle_trp  routine)
   USE ldftra         ! Eddy Induced transport     (ldf_eiv_trp  routine)
   USE ldfslp         ! Lateral diffusion: slopes of neutral surfaces
   USE trd_oce        ! trends: ocean variables
   USE trdtra         ! trends manager: tracers
   USE diaptr         ! Poleward heat transport
#if defined key_RK3
   USE eosbn2
#endif
   USE zdf_oce , ONLY : ln_zad_Aimp
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O module
   USE prtctl         ! Print control
   USE lib_mpp        ! MPP library
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_adv        ! called by step.F90, stpmlf.F90 and stprk3_stg.F90
   PUBLIC   tra_adv_init   ! called by nemogcm.F90
#if defined key_RK3
   PUBLIC   tra_adv_trp    ! called by stprk3_stg.F90
#endif

   !                            !!* Namelist namtra_adv *
   LOGICAL ::   ln_traadv_OFF    ! no advection on T and S
   LOGICAL ::   ln_traadv_cen    ! centered scheme flag
   INTEGER ::      nn_cen_h, nn_cen_v   ! =2/4 : horizontal and vertical choices of the order of CEN scheme
   LOGICAL ::   ln_traadv_fct    ! FCT scheme flag
   INTEGER ::      nn_fct_h, nn_fct_v   ! =2/4 : horizontal and vertical choices of the order of FCT scheme
   INTEGER ::      nn_fct_imp           ! =1/2 : optimized or accurate treatment of implicit
   LOGICAL ::   ln_traadv_mus    ! MUSCL scheme flag
   LOGICAL ::      ln_mus_ups           ! use upstream scheme in vivcinity of river mouths
   LOGICAL ::   ln_traadv_ubs    ! UBS scheme flag
   INTEGER ::      nn_ubs_v             ! =2/4 : vertical choice of the order of UBS scheme
   LOGICAL ::   ln_traadv_qck    ! QUICKEST scheme flag

   INTEGER ::   nadv             ! choice of the type of advection scheme
   !                             ! associated indices:
   INTEGER, PARAMETER ::   np_NO_adv  = 0   ! no T-S advection
   INTEGER, PARAMETER ::   np_CEN     = 1   ! 2nd/4th order centered scheme
   INTEGER, PARAMETER ::   np_FCT     = 2   ! 2nd/4th order Flux Corrected Transport scheme
   INTEGER, PARAMETER ::   np_MUS     = 3   ! MUSCL scheme
   INTEGER, PARAMETER ::   np_UBS     = 4   ! 3rd order Upstream Biased Scheme
   INTEGER, PARAMETER ::   np_QCK     = 5   ! QUICK scheme

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: traadv.F90 15514 2021-11-16 08:58:22Z techene $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

#if defined key_RK3
   SUBROUTINE tra_adv_trp( kt, kstg, kit000, Kmm, Krhs, pFu, pFv, pFw )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_adv_trp  ***
      !!
      !! ** Purpose :   compute transport for tracer advection 
      !!
      !! ** Method  : - Update pFu/v/w with stoke drift or eiv or mle
      !!----------------------------------------------------------------------
      INTEGER                                     , INTENT(in   ) ::   kt                  ! ocean time-step index
      INTEGER                                     , INTENT(in   ) ::   kstg, kit000        ! RK3 stage and init index
      INTEGER                                     , INTENT(in   ) ::   Kmm, Krhs           ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk)            , INTENT(inout) ::   pFu, pFv, pFw       ! advective transport
      !
      INTEGER ::   ji, jj, jk   ! dummy loop index
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('tra_adv_trp')
      !
      IF( ln_wave .AND. ln_sdw )  THEN
         DO jk = 1, jpkm1                                    ! At all stages : Add the Stokes Drift
            DO_2D( nn_hls, nn_hls-1, nn_hls, nn_hls-1)
               pFu(ji,jj,jk) = pFu(ji,jj,jk) + e2u(ji,jj) * e3u(ji,jj,jk,Kmm) * usd(ji,jj,jk)
               pFv(ji,jj,jk) = pFv(ji,jj,jk) + e1v(ji,jj) * e3v(ji,jj,jk,Kmm) * vsd(ji,jj,jk)
            END_2D
            DO_2D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1)
               pFw(ji,jj,jk) = pFw(ji,jj,jk) + e1e2t(ji,jj)                   * wsd(ji,jj,jk)
            END_2D
         END DO
      ENDIF
      !
      DO_2D( nn_hls, nn_hls-1, nn_hls, nn_hls-1 )
         pFu(ji,jj,jpk) = 0._wp                                                   ! no transport trough the bottom 
         pFv(ji,jj,jpk) = 0._wp
         pFw(ji,jj,jpk) = 0._wp
      END_2D
      !
      IF( kstg == 3 ) THEN                                   ! At stage 3 only
         IF( ln_ldfeiv .AND. .NOT. ln_traldf_triad )   &                     ! Add the eiv transport
            &   CALL ldf_eiv_trp( kt, kit000, pFu, pFv, pFw, Kmm, Krhs )
         !
         IF( ln_mle    )   THEN                                              ! Add the mle transport
                CALL eos( ts, Kmm, rhd, rhop )                                    ! now in potential density for tra_mle computation
                CALL tra_mle_trp( kt, pFu, pFv, pFw, Kmm )
         ENDIF
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop( 'tra_adv_trp' )
      !
   END SUBROUTINE tra_adv_trp
#endif

   SUBROUTINE tra_adv( kt, Kbb, Kmm, Kaa, pts, Krhs, pau, pav, paw, kstg )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_adv  ***
      !!
      !! ** Purpose :   compute the ocean tracer advection trend.
      !!
      !! ** Method  : - Update ts(Krhs) with the advective trend following nadv
      !!----------------------------------------------------------------------
      INTEGER                                     , INTENT(in   ) ::   kt                  ! ocean time-step index
      INTEGER                                     , INTENT(in   ) ::   Kbb, Kmm, Kaa, Krhs ! time level indices
      INTEGER,                            OPTIONAL, INTENT(in   ) ::   kstg                ! optional stage indicator
      REAL(wp), DIMENSION(:,:,:), OPTIONAL, TARGET, INTENT(in   ) ::   pau, pav, paw       ! advective velocity
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts,jpt)   , INTENT(inout) ::   pts                 ! active tracers and RHS of tracer equation
      !
      INTEGER ::   ji, jj, jk   ! dummy loop index
      REAL(wp), DIMENSION(:,:,:), POINTER             ::   zptu, zptv, zptw
      REAL(wp), DIMENSION(:,:,:), TARGET, ALLOCATABLE ::   zuu, zvv, zww   ! 3D workspace
      REAL(wp), DIMENSION(:,:,:),         ALLOCATABLE ::   ztrdt, ztrds
      ! TEMP: [tiling] This change not necessary after all lbc_lnks removed in the nn_hls = 2 case in tra_adv_fct
      LOGICAL ::   lskip
      LOGICAL ::   ll_dofct
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('tra_adv')
      !
      lskip    = .FALSE.
      ll_dofct = .TRUE.

      ! FCT at last stage only with RK3
      IF (PRESENT(kstg)) THEN
         IF (kstg/=3) ll_dofct = .FALSE.
      ENDIF

      ! TEMP: [tiling] These changes not necessary after all lbc_lnks removed in the nn_hls = 2 case in tra_adv_fct
      IF( ln_tile .AND. nadv == np_FCT )  THEN
         IF( ntile == 1 ) THEN
            CALL dom_tile_stop( ldhold=.TRUE. )
         ELSE
            lskip = .TRUE.
         ENDIF
      ENDIF
      !
      IF( .NOT. lskip ) THEN
         !                                         !==  effective advective transport  ==!
         !
         IF( PRESENT( pau ) ) THEN     ! RK3: advective velocity (pau,pav,paw) /= advected velocity (uu,vv,ww)
            zptu => pau(:,:,:)
            zptv => pav(:,:,:)
            zptw => paw(:,:,:)
         ELSE                          ! MLF: advective velocity = (uu,vv,ww)
            zptu => uu(:,:,:,Kmm)
            zptv => vv(:,:,:,Kmm)
            zptw => ww(:,:,:    )
         ENDIF
         !
         IF( l_trdtra )   THEN                    !* Save ta and sa trends
            ALLOCATE( ztrdt(jpi,jpj,jpk), ztrds(jpi,jpj,jpk) )
            ztrdt(:,:,:) = pts(:,:,:,jp_tem,Krhs)
            ztrds(:,:,:) = pts(:,:,:,jp_sal,Krhs)
         ENDIF
         !
#if ! defined key_RK3
         !
         ALLOCATE( zuu(T2D(nn_hls),jpk), zvv(T2D(nn_hls),jpk), zww(T2D(nn_hls),jpk) )
         !
         IF( ln_wave .AND. ln_sdw )  THEN
            DO_3D( nn_hls, nn_hls-1, nn_hls, nn_hls-1, 1, jpkm1 )
               zuu(ji,jj,jk) = e2u  (ji,jj) * e3u(ji,jj,jk,Kmm) * ( zptu(ji,jj,jk) + usd(ji,jj,jk) )
               zvv(ji,jj,jk) = e1v  (ji,jj) * e3v(ji,jj,jk,Kmm) * ( zptv(ji,jj,jk) + vsd(ji,jj,jk) )
            END_3D
            DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 1, jpkm1 )
               zww(ji,jj,jk) = e1e2t(ji,jj)                     * ( zptw(ji,jj,jk) + wsd(ji,jj,jk) )
            END_3D
         ELSE
            DO_3D( nn_hls, nn_hls-1, nn_hls, nn_hls-1, 1, jpkm1 )
               zuu(ji,jj,jk) = e2u  (ji,jj) * e3u(ji,jj,jk,Kmm) * zptu(ji,jj,jk)               ! eulerian transport only
               zvv(ji,jj,jk) = e1v  (ji,jj) * e3v(ji,jj,jk,Kmm) * zptv(ji,jj,jk)
            END_3D
            DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 1, jpkm1 )
               zww(ji,jj,jk) = e1e2t(ji,jj)                     * zptw(ji,jj,jk)
            END_3D
         ENDIF
         !
         DO_2D( nn_hls, nn_hls-1, nn_hls, nn_hls-1 )
            zuu(ji,jj,jpk) = 0._wp                                                      ! no transport trough the bottom 
            zvv(ji,jj,jpk) = 0._wp
            zww(ji,jj,jpk) = 0._wp
         END_2D
         !
         IF( ln_ldfeiv .AND. .NOT. ln_traldf_triad )   &
            &              CALL ldf_eiv_trp( kt, nit000, zuu, zvv, zww, Kmm, Krhs  , 'TRA' )   ! add the eiv transport (if necessary)
         !
         IF( ln_mle    )   CALL tra_mle_trp( kt        , zuu, zvv, zww, Kmm, nit000, 'TRA' )   ! add the mle transport (if necessary)
         !
         ! Change pointers to use in advection
         zptu => zuu(:,:,:)
         zptv => zvv(:,:,:)
         zptw => zww(:,:,:)
         !
#endif
         !
         SELECT CASE ( nadv )                      !==  compute advection trend and add it to general trend  ==!
         !
         CASE ( np_CEN )                                 ! Centered scheme : 2nd / 4th order
            CALL tra_adv_cen    ( kt, nit000, 'TRA',      zptu, zptv, zptw,      Kmm,      pts, jpts, Krhs, nn_cen_h, nn_cen_v )
         CASE ( np_FCT )                                 ! FCT scheme      : 2nd / 4th order
            IF ( ll_dofct ) THEN
               CALL tra_adv_fct ( kt, nit000, 'TRA', rDt, zptu, zptv, zptw, Kbb, Kmm, Kaa, pts, jpts, Krhs, nn_fct_h, nn_fct_v, &
                  &                                                                                                   nn_fct_imp )
            ELSE
               CALL tra_adv_cen ( kt, nit000, 'TRA',      zptu, zptv, zptw,      Kmm,      pts, jpts, Krhs, nn_fct_h, nn_fct_v )
            ENDIF
         CASE ( np_MUS )                                 ! MUSCL
            CALL tra_adv_mus    ( kt, nit000, 'TRA', rDt, zptu, zptv, zptw, Kbb, Kmm,      pts, jpts, Krhs, ln_mus_ups         )
         CASE ( np_UBS )                                 ! UBS
            CALL tra_adv_ubs    ( kt, nit000, 'TRA', rDt, zptu, zptv, zptw, Kbb, Kmm,      pts, jpts, Krhs, nn_ubs_v           )
         CASE ( np_QCK )                                 ! QUICKEST
            CALL tra_adv_qck    ( kt, nit000, 'TRA', rDt, zptu, zptv, zptw, Kbb, Kmm,      pts, jpts, Krhs                     )
         !
         END SELECT
         !
         IF( l_iom ) THEN
            CALL iom_put( "uocetr_eff", zptu )                                        ! output effective transport
            CALL iom_put( "vocetr_eff", zptv )
            CALL iom_put( "wocetr_eff", zptw )
         ENDIF
         !
!!gm ???
         IF( l_diaptr ) CALL dia_ptr( kt, Kmm, zptv(:,:,:) )                          ! diagnose the effective MSF
!!gm ???
         !
#if ! defined key_RK3
         DEALLOCATE( zuu, zvv, zww )
#endif
         !
         IF( l_trdtra )   THEN                      ! save the advective trends for further diagnostics
            DO jk = 1, jpkm1
               ztrdt(:,:,jk) = pts(:,:,jk,jp_tem,Krhs) - ztrdt(:,:,jk)
               ztrds(:,:,jk) = pts(:,:,jk,jp_sal,Krhs) - ztrds(:,:,jk)
            END DO
            CALL trd_tra( kt, Kmm, Krhs, 'TRA', jp_tem, jptra_totad, ztrdt )
            CALL trd_tra( kt, Kmm, Krhs, 'TRA', jp_sal, jptra_totad, ztrds )
            DEALLOCATE( ztrdt, ztrds )
         ENDIF

         ! TEMP: [tiling] This change not necessary after all lbc_lnks removed in the nn_hls = 2 case in tra_adv_fct
         IF( ln_tile .AND. .NOT. l_istiled ) CALL dom_tile_start( ldhold=.TRUE. )
         !
      ENDIF
      !                                              ! print mean trends (used for debugging)
      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=pts(:,:,:,jp_tem,Krhs), clinfo1=' adv  - Ta: ', mask1=tmask, &
         &                                  tab3d_2=pts(:,:,:,jp_sal,Krhs), clinfo2=       ' Sa: ', mask2=tmask, clinfo3='tra' )
      !
      IF( ln_timing )   CALL timing_stop( 'tra_adv' )
      !
   END SUBROUTINE tra_adv


   SUBROUTINE tra_adv_init
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE tra_adv_init  ***
      !!
      !! ** Purpose :   Control the consistency between namelist options for
      !!              tracer advection schemes and set nadv
      !!----------------------------------------------------------------------
      INTEGER ::   ioptio, ios   ! Local integers
      !
      NAMELIST/namtra_adv/ ln_traadv_OFF,                                    &   ! No advection
         &                 ln_traadv_cen , nn_cen_h, nn_cen_v,               &   ! CEN
         &                 ln_traadv_fct , nn_fct_h, nn_fct_v, nn_fct_imp,   &   ! FCT
         &                 ln_traadv_mus , ln_mus_ups,                       &   ! MUSCL
         &                 ln_traadv_ubs ,           nn_ubs_v,               &   ! UBS
         &                 ln_traadv_qck                                         ! QCK
      !!----------------------------------------------------------------------
      !
      !                                !==  Namelist  ==!
      READ_NML_REF(numnam,namtra_adv)
      READ_NML_CFG(numnam,namtra_adv)
      IF(lwm) WRITE( numond, namtra_adv )
      !
      IF(lwp) THEN                           ! Namelist print
         WRITE(numout,*)
         WRITE(numout,*) 'tra_adv_init : choice/control of the tracer advection scheme'
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namtra_adv : chose a advection scheme for tracers'
         WRITE(numout,*) '      No advection on T & S                     ln_traadv_OFF = ', ln_traadv_OFF
         WRITE(numout,*) '      centered scheme                           ln_traadv_cen = ', ln_traadv_cen
         WRITE(numout,*) '            horizontal 2nd/4th order               nn_cen_h   = ', nn_cen_h
         WRITE(numout,*) '            vertical   2nd/4th order               nn_cen_v   = ', nn_cen_v
         WRITE(numout,*) '      Flux Corrected Transport scheme           ln_traadv_fct = ', ln_traadv_fct
         WRITE(numout,*) '            horizontal 2nd/4th order               nn_fct_h   = ', nn_fct_h
         WRITE(numout,*) '            vertical   2nd/4th order               nn_fct_v   = ', nn_fct_v
         WRITE(numout,*) '            implicit   optimized(1)/accurate(2)    nn_fct_imp = ', nn_fct_imp
         WRITE(numout,*) '      MUSCL scheme                              ln_traadv_mus = ', ln_traadv_mus
         WRITE(numout,*) '            + upstream scheme near river mouths    ln_mus_ups = ', ln_mus_ups
         WRITE(numout,*) '      UBS scheme                                ln_traadv_ubs = ', ln_traadv_ubs
         WRITE(numout,*) '            vertical   2nd/4th order               nn_ubs_v   = ', nn_ubs_v
         WRITE(numout,*) '      QUICKEST scheme                           ln_traadv_qck = ', ln_traadv_qck
      ENDIF
      !
      !                                !==  Parameter control & set nadv ==!
      ioptio = 0
      IF( ln_traadv_OFF ) THEN   ;   ioptio = ioptio + 1   ;   nadv = np_NO_adv   ;   ENDIF
      IF( ln_traadv_cen ) THEN   ;   ioptio = ioptio + 1   ;   nadv = np_CEN      ;   ENDIF
      IF( ln_traadv_fct ) THEN   ;   ioptio = ioptio + 1   ;   nadv = np_FCT      ;   ENDIF
      IF( ln_traadv_mus ) THEN   ;   ioptio = ioptio + 1   ;   nadv = np_MUS      ;   ENDIF
      IF( ln_traadv_ubs ) THEN   ;   ioptio = ioptio + 1   ;   nadv = np_UBS      ;   ENDIF
      IF( ln_traadv_qck ) THEN   ;   ioptio = ioptio + 1   ;   nadv = np_QCK      ;   ENDIF
      !
      IF( ioptio /= 1 )   CALL ctl_stop( 'tra_adv_init: Choose ONE advection option in namelist namtra_adv' )
      !
      IF( ln_traadv_cen .AND. ( nn_cen_h /= 2 .AND. nn_cen_h /= 4 )   &          ! Centered
                        .AND. ( nn_cen_v /= 2 .AND. nn_cen_v /= 4 )   ) THEN
        CALL ctl_stop( 'tra_adv_init: CEN scheme, choose 2nd or 4th order' )
      ENDIF
      IF( ln_traadv_fct .AND. ( nn_fct_h /= 2 .AND. nn_fct_h /= 4 )   &          ! FCT
         &              .AND. ( nn_fct_v /= 2 .AND. nn_fct_v /= 4 ) .AND. ( nn_fct_imp /= 1 .AND. nn_fct_imp /= 2 ) ) THEN
        CALL ctl_stop( 'tra_adv_init: FCT scheme, choose 2nd or 4th order, and optimized or accurate treatment of implicit' )
      ENDIF
      ! TEMP: [tiling] This change not necessary after all lbc_lnks removed in the nn_hls = 2 case in tra_adv_fct
      IF( ln_traadv_fct .AND. ln_tile ) THEN
         CALL ctl_warn( 'tra_adv_init: FCT scheme does not yet work with tiling' )
      ENDIF
      IF( ln_traadv_ubs .AND. ( nn_ubs_v /= 2 .AND. nn_ubs_v /= 4 )   ) THEN     ! UBS
         CALL ctl_stop( 'tra_adv_init: UBS scheme, choose 2nd or 4th order' )
      ENDIF
      IF( ln_traadv_ubs .AND. nn_ubs_v == 4 ) THEN
         CALL ctl_warn( 'tra_adv_init: UBS scheme, only 2nd FCT scheme available on the vertical. It will be used' )
      ENDIF
      IF( ln_traadv_ubs .AND. ln_zad_Aimp )   CALL ctl_stop( 'tra_adv_init: UBS scheme, vertical implicit is not coded' )
      IF( ln_isfcav ) THEN                                                       ! ice-shelf cavities
         IF(  ln_traadv_cen .AND. nn_cen_v == 4    .OR.   &                            ! NO 4th order with ISF
            & ln_traadv_fct .AND. nn_fct_v == 4   )   CALL ctl_stop( 'tra_adv_init: 4th order COMPACT scheme not allowed with ISF' )
      ENDIF
      !
      !                                !==  Print the choice  ==!
      IF(lwp) THEN
         WRITE(numout,*)
         SELECT CASE ( nadv )
         CASE( np_NO_adv  )   ;   WRITE(numout,*) '   ==>>>   NO T-S advection'
         CASE( np_CEN     )   ;   WRITE(numout,*) '   ==>>>   CEN      scheme is used. Horizontal order  : ', nn_cen_h, &
            &                                                                        ' Vertical   order  : ', nn_cen_v
         CASE( np_FCT     )   ;   WRITE(numout,*) '   ==>>>   FCT      scheme is used. Horizontal order  : ', nn_fct_h, &
            &                                                                        ' Vertical   order  : ', nn_fct_v, &
            &                                                                        ' Implicit treatment: ', nn_fct_imp
         CASE( np_MUS     )   ;   WRITE(numout,*) '   ==>>>   MUSCL    scheme is used'
         CASE( np_UBS     )   ;   WRITE(numout,*) '   ==>>>   UBS      scheme is used'
         CASE( np_QCK     )   ;   WRITE(numout,*) '   ==>>>   QUICKEST scheme is used'
         END SELECT
      ENDIF
      !
      CALL tra_mle_init            !== initialisation of the Mixed Layer Eddy parametrisation (MLE)  ==!
      !
   END SUBROUTINE tra_adv_init

  !!======================================================================
END MODULE traadv
