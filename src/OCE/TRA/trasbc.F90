MODULE trasbc
   !!==============================================================================
   !!                       ***  MODULE  trasbc  ***
   !! Ocean active tracers:  surface boundary condition
   !!==============================================================================
   !! History :  OPA  !  1998-10  (G. Madec, G. Roullet, M. Imbard)  Original code
   !!            8.2  !  2001-02  (D. Ludicone)  sea ice and free surface
   !!  NEMO      1.0  !  2002-06  (G. Madec)  F90: Free form and module
   !!            3.3  !  2010-04  (M. Leclair, G. Madec)  Forcing averaged over 2 time steps
   !!             -   !  2010-09  (C. Ethe, G. Madec) Merge TRA-TRC
   !!            3.6  !  2014-11  (P. Mathiot) isf melting forcing
   !!            4.1  !  2019-09  (P. Mathiot) isf moved in traisf
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_sbc       : update the tracer trend at ocean surface
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and active tracers
   USE sbc_oce        ! surface boundary condition: ocean
   USE dom_oce        ! ocean space domain variables
   USE phycst         ! physical constant
   USE eosbn2         ! Equation Of State
   USE sbcmod         ! ln_rnf
   USE sbcrnf         ! River runoff
   USE traqsr         ! solar radiation penetration
   USE trd_oce        ! trends: ocean variables
   USE trdtra         ! trends manager: tracers
#if defined key_asminc
   USE asminc         ! Assimilation increment
#endif
   !
   USE in_out_manager ! I/O manager
   USE prtctl         ! Print control
   USE iom            ! xIOS server
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_sbc   ! routine called by stprk3_stg.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_sbc ( kt, Kbb, Kmm, pts, Krhs, kstg )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_sbc  ***
      !!                  ( formerly tra_sbc_RK3 ) 
      !!
      !! ** Purpose :   Compute the tracer surface boundary condition trend of
      !!      (flux through the interface, concentration/dilution effect)
      !!      and add it to the general trend of tracer equations.
      !!
      !! ** Method :   The (air+ice)-sea flux has two components:
      !!      (1) Fext, external forcing (i.e. flux through the (air+ice)-sea interface);
      !!      (2) Fwe , tracer carried with the water that is exchanged with air+ice.
      !!               The input forcing fields (emp, rnf, sfx) contain Fext+Fwe,
      !!             they are simply added to the tracer trend (ts(Krhs)).
      !!               In linear free surface case (lk_linssh=T), the volume of the
      !!             ocean does not change with the water exchanges at the (air+ice)-sea
      !!             interface. Therefore another term has to be added, to mimic the
      !!             concentration/dilution effect associated with water exchanges.
      !!
      !! ** Action  : - Update ts(Krhs) with the surface boundary condition trend
      !!              - send trends to trdtra module for further diagnostics(l_trdtra=T)
      !!----------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) ::   kt, Kbb, Kmm, Krhs   ! ocean time-step and time-level indices
      INTEGER                                  , INTENT(in   ) ::   kstg            ! RK3 stage index
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts,jpt), INTENT(inout) ::   pts             ! active tracers and RHS of tracer Eq.
      !
      INTEGER  ::   ji, jj, jk, jn               ! dummy loop indices
      REAL(wp) ::   z1_rho0_e3t, zdep, ztim    ! local scalar
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::  ztrdt, ztrds
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('tra_sbc')
      !
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == nit000 ) THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'tra_sbc : TRAcer Surface Boundary Condition'
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~~ '
         ENDIF
      ENDIF
      !
      IF( l_trdtra ) THEN                    !* Save ta and sa trends
         ALLOCATE( ztrdt(T2D(0),jpk), ztrds(T2D(0),jpk) )
         ztrdt(:,:,:) = pts(T2D(0),:,jp_tem,Krhs)
         ztrds(:,:,:) = pts(T2D(0),:,jp_sal,Krhs)
      ENDIF
      !
!!gm  This should be moved into sbcmod.F90 module ? (especially now that ln_traqsr is read in namsbc namelist)
      IF( .NOT.ln_traqsr  .AND. kstg == 1) THEN     ! no solar radiation penetration
         DO_2D( 0, 0, 0, 0 )
            qns(ji,jj) = qns(ji,jj) + qsr(ji,jj)         ! total heat flux in qns
            qsr(ji,jj) = 0._wp                           ! qsr set to zero
         END_2D
      ENDIF

      !----------------------------------------
      !        EMP, SFX and QNS effects
      !----------------------------------------
      !                             !==  update tracer trend  ==!
      SELECT CASE( kstg )
         !
      CASE( 1 , 2 )                       !=  stage 1 and 2  =!   only in non linear ssh
         !
         IF( .NOT.lk_linssh ) THEN           !* only heat and salt fluxes associated with mass fluxes
            DO_2D( 0, 0, 0, 0 )
               z1_rho0_e3t = r1_rho0 / e3t(ji,jj,1,Kmm)
               pts(ji,jj,1,jp_tem,Krhs) = pts(ji,jj,1,jp_tem,Krhs) - emp(ji,jj)*pts(ji,jj,1,jp_tem,Kbb) * z1_rho0_e3t
               pts(ji,jj,1,jp_sal,Krhs) = pts(ji,jj,1,jp_sal,Krhs) - emp(ji,jj)*pts(ji,jj,1,jp_sal,Kbb) * z1_rho0_e3t
            END_2D
         ENDIF
         !
      CASE( 3 )
         !
         IF( lk_linssh ) THEN                !* linear free surface
            DO_2D( 0, 0, 0, 0 )
               z1_rho0_e3t = r1_rho0 / e3t(ji,jj,1,Kmm)
               pts(ji,jj,1,jp_tem,Krhs) = pts(ji,jj,1,jp_tem,Krhs) + (  r1_rcp * qns(ji,jj)   &                                ! non solar heat flux
                  &                                                +             emp(ji,jj)*pts(ji,jj,1,jp_tem,Kbb)  ) * z1_rho0_e3t  ! add concentration/dilution effect due to constant volume cell
               pts(ji,jj,1,jp_sal,Krhs) = pts(ji,jj,1,jp_sal,Krhs) + (           sfx(ji,jj)    &                               ! salt flux due to freezing/melting
                  &                                                +             emp(ji,jj)*pts(ji,jj,1,jp_sal,Kbb)  ) * z1_rho0_e3t  ! add concentration/dilution effect due to constant volume cell
            END_2D
            IF( .NOT. l_istiled .OR. ntile == nijtile ) THEN             ! Do only on the last tile
               IF( iom_use('emp_x_sst') )   CALL iom_put( "emp_x_sst", emp (:,:) * pts(:,:,1,jp_tem,Kbb) )
               IF( iom_use('emp_x_sss') )   CALL iom_put( "emp_x_sss", emp (:,:) * pts(:,:,1,jp_sal,Kbb) )
            ENDIF
         ELSE
            DO_2D( 0, 0, 0, 0 )
               z1_rho0_e3t = r1_rho0 / e3t(ji,jj,1,Kmm)
               pts(ji,jj,1,jp_tem,Krhs) = pts(ji,jj,1,jp_tem,Krhs) +  r1_rcp * qns(ji,jj) * z1_rho0_e3t
               pts(ji,jj,1,jp_sal,Krhs) = pts(ji,jj,1,jp_sal,Krhs) +           sfx(ji,jj) * z1_rho0_e3t
            END_2D
         ENDIF
      END SELECT
      !
      !
      !----------------------------------------
      !        River Runoff effects
      !----------------------------------------
      !
      IF( ln_rnf ) THEN         ! input of heat and salt due to river runoff
         DO_2D( 0, 0, 0, 0 )
            IF( rnf(ji,jj) /= 0._wp ) THEN
               zdep = 1._wp / h_rnf(ji,jj)
               DO jk = 1, nk_rnf(ji,jj)
                                     pts(ji,jj,jk,jp_tem,Krhs) = pts(ji,jj,jk,jp_tem,Krhs)  + rnf_tsc(ji,jj,jp_tem) * zdep
                  IF( ln_rnf_sal )   pts(ji,jj,jk,jp_sal,Krhs) = pts(ji,jj,jk,jp_sal,Krhs)  + rnf_tsc(ji,jj,jp_sal) * zdep
               END DO
            ENDIF
         END_2D
      ENDIF
      !
      IF( kstg == 3 .AND. ( .NOT. l_istiled .OR. ntile == nijtile ) )  THEN                ! Do only on the last tile
         IF( iom_use('rnf_x_sst') )   CALL iom_put( "rnf_x_sst", rnf*pts(:,:,1,jp_tem,Kbb) )   ! runoff term on sst
         IF( iom_use('rnf_x_sss') )   CALL iom_put( "rnf_x_sss", rnf*pts(:,:,1,jp_sal,Kbb) )   ! runoff term on sss
      ENDIF

#if defined key_asminc
      !
      !----------------------------------------
      !        Assmilation effects
      !----------------------------------------
      !
      IF( ln_sshinc .AND. kstg == 3 ) THEN         ! input of heat and salt due to assimilation
!!st a priori this should be done at each stage not only at last stage since it is associated with ssh change (included in step 2D)
         !
         IF( lk_linssh ) THEN
            DO_2D( 0, 0, 0, 0 )
               ztim = ssh_iau(ji,jj) / e3t(ji,jj,1,Kmm)
               pts(ji,jj,1,jp_tem,Krhs) = pts(ji,jj,1,jp_tem,Krhs) + pts(ji,jj,1,jp_tem,Kbb) * ztim
               pts(ji,jj,1,jp_sal,Krhs) = pts(ji,jj,1,jp_sal,Krhs) + pts(ji,jj,1,jp_sal,Kbb) * ztim
            END_2D
         ELSE
            DO_2D( 0, 0, 0, 0 )
               ztim = ssh_iau(ji,jj) / ( ht(ji,jj,Kmm) + 1. - ssmask(ji, jj) )
               pts(ji,jj,:,jp_tem,Krhs) = pts(ji,jj,:,jp_tem,Krhs) + pts(ji,jj,:,jp_tem,Kbb) * ztim
               pts(ji,jj,:,jp_sal,Krhs) = pts(ji,jj,:,jp_sal,Krhs) + pts(ji,jj,:,jp_sal,Kbb) * ztim
            END_2D
         ENDIF
         !
      ENDIF
      !
#endif
      !
      IF( l_trdtra )   THEN                      ! save the horizontal diffusive trends for further diagnostics
         ztrdt(:,:,:) = pts(T2D(0),:,jp_tem,Krhs) - ztrdt(:,:,:)
         ztrds(:,:,:) = pts(T2D(0),:,jp_sal,Krhs) - ztrds(:,:,:)
         CALL trd_tra( kt, Kbb, Krhs, 'TRA', jp_tem, jptra_nsr, ztrdt )
         CALL trd_tra( kt, Kbb, Krhs, 'TRA', jp_sal, jptra_nsr, ztrds )
         DEALLOCATE( ztrdt , ztrds )
      ENDIF
      !
      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=pts(:,:,:,jp_tem,Krhs), clinfo1=' sbc  - Ta: ', mask1=tmask,   &
         &                                  tab3d_2=pts(:,:,:,jp_sal,Krhs), clinfo2=       ' Sa: ', mask2=tmask, clinfo3='tra' )
      !
      IF( ln_timing )   CALL timing_stop('tra_sbc')
      !
   END SUBROUTINE tra_sbc

   !!======================================================================
END MODULE trasbc
