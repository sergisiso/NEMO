MODULE trctrp
   !!======================================================================
   !!                       ***  MODULE trctrp  ***
   !! Ocean Physics    : manage the passive tracer transport
   !!======================================================================
   !! History :   1.0  !  2004-03 (C. Ethe) Original code
   !!             3.3  !  2010-07 (C. Ethe) Merge TRA-TRC
   !!             4.x  !  2021-08 (S. Techene, G. Madec) Adapt for RK3 time-stepping
   !!----------------------------------------------------------------------
#if defined key_top
   !!----------------------------------------------------------------------
   !!   'key_top'                                                TOP models
   !!----------------------------------------------------------------------
   !!   trc_trp        : passive tracer transport
   !!----------------------------------------------------------------------
   USE par_trc         ! need jptra, number of passive tracers
   USE oce_trc         ! ocean dynamics and active tracers variables
   USE trc             ! ocean passive tracers variables 
   USE trcbbl          ! bottom boundary layer               (trc_bbl routine)
   USE trcdmp          ! internal damping                    (trc_dmp routine)
   USE trcldf          ! lateral mixing                      (trc_ldf routine)
   USE trcadv          ! advection                           (trc_adv routine)
   USE trczdf          ! vertical diffusion                  (trc_zdf routine)
   USE trcrad          ! positivity                          (trc_rad routine)
   USE trcsbc          ! surface boundary condition          (trc_sbc routine)
   USE bdy_oce   , ONLY: ln_bdy
   USE trcbdy          ! BDY open boundaries
   USE in_out_manager
   USE domtile         ! tiling utilities

# if defined key_agrif
   USE agrif_top_sponge ! tracers sponges
   USE agrif_top_interp
# endif

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_trp    ! called by trc_stp and stprk3_stg

   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE trc_trp( kt, Kbb, Kmm, Krhs, Kaa )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE trc_trp  ***
      !!                      
      !! ** Purpose :   Management of passive tracers transport
      !! 
      !! ** Method  : - Compute the passive tracers trends 
      !!              - Update the passive tracers
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) :: kt                  ! ocean time-step index
      INTEGER, INTENT( in ) :: Kbb, Kmm, Krhs, Kaa ! time level indices (not swapped in this routine)
      INTEGER               :: jtile               ! dummy loop index
      !! ---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_trp')
      !
      IF( .NOT. ln_c1d ) THEN
         !
         IF( ln_tile ) CALL dom_tile_start
         DO jtile = 1, nijtile
            IF( ln_tile ) CALL dom_tile( ntsi, ntsj, ntei, ntej, ktile = jtile )
            IF( ln_trabbl )        CALL trc_bbl( kt, Kbb, Kmm, tr, Krhs )      ! advective (and/or diffusive) bottom boundary layer scheme
            IF( ln_trcdmp )        CALL trc_dmp( kt, Kbb, Kmm, tr, Krhs )      ! internal damping trends
         END DO
         IF( ln_tile ) CALL dom_tile_stop

         IF( ln_top .AND. ln_bdy ) CALL trc_bdy_dmp( kt, Kbb,      Krhs )      ! BDY damping trends
# if defined key_agrif
         IF(.NOT. Agrif_Root()) CALL Agrif_Sponge_trc       ! tracers sponge
# endif
         !
         IF( ln_tile ) CALL dom_tile_start
         DO jtile = 1, nijtile
            IF( ln_tile ) CALL dom_tile( ntsi, ntsj, ntei, ntej, ktile = jtile )
                                CALL trc_ldf( kt, Kbb, Kmm,       tr, Krhs )  ! lateral mixing
                                CALL trc_zdf( kt, Kbb, Kmm, Krhs, tr, Kaa  )  ! vert. mixing & after tracer	==> after
         END DO
         IF( ln_tile ) CALL dom_tile_stop
         !                                                 ! RK3: only manage lateral boundary
# if defined key_agrif
                                CALL Agrif_trc( kt )                            ! AGRIF zoom boundaries
# endif
         !                                                                        ! Update after tracer on domain lateral boundaries
                                CALL lbc_lnk( 'stprk3_stg', tr(:,:,:,:,Kaa), 'T', 1._wp )   
         !
         IF( ln_bdy )           CALL trc_bdy( kt, Kbb, Kmm, Kaa )
         !
         IF( ln_trcrad )        CALL trc_rad    ( kt, Kmm, Kaa, tr )    ! Correct artificial negative concentrations
         IF( ln_trcdmp_clo )    CALL trc_dmp_clo( kt, Kmm, Kaa )              ! internal damping trends on closed seas only

         !
      ELSE                                               ! 1D vertical configuration
         IF( ln_trcdmp )        CALL trc_dmp( kt, Kbb, Kmm, tr, Krhs )  ! internal damping trends
                                CALL trc_zdf( kt, Kbb, Kmm, Krhs, tr, Kaa  )  ! vert. mixing & after tracer	==> after
         !
         IF( ln_trcrad )        CALL trc_rad( kt, Kmm, Kaa, tr       )  ! Correct artificial negative concentrations
         !
      END IF
      !
      IF( ln_timing )   CALL timing_stop('trc_trp')
      !
   END SUBROUTINE trc_trp

#else
   !!----------------------------------------------------------------------
   !!   Dummy module :                                        No TOP models
   !!----------------------------------------------------------------------
   IMPLICIT NONE
CONTAINS
   SUBROUTINE trc_trp( kt )              ! Empty routine
      INTEGER, INTENT(in) ::   kt
      WRITE(*,*) 'trc_trp: You should not have seen this print! error?', kt
   END SUBROUTINE trc_trp
#endif
   
   !!======================================================================
END MODULE trctrp
