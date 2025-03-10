MODULE usrdef_hgr
   !!======================================================================
   !!                     ***  MODULE usrdef_hgr   ***
   !!
   !!                     ===  ADIAB_WAVE configuration  ===
   !!
   !! User defined :   mesh and Coriolis parameter of a user configuration
   !!======================================================================
   !! History :  4.0 ! 2016-03  (S. Flavoni) 
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   usr_def_hgr   : initialize the horizontal mesh 
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain
   USE par_oce        ! ocean space and time domain
   USE phycst         ! physical constants
   USE usrdef_nam     !
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   usr_def_hgr   ! called in domhgr.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE usr_def_hgr( plamt , plamu , plamv  , plamf  ,   &   ! geographic position (required)
      &                    pphit , pphiu , pphiv  , pphif  ,   &   !
      &                    kff   , pff_f , pff_t  ,            &   ! Coriolis parameter  (if domain not on the sphere)
      &                    pe1t  , pe1u  , pe1v   , pe1f   ,   &   ! scale factors       (required)
      &                    pe2t  , pe2u  , pe2v   , pe2f   ,   &   !
      &                    ke1e2u_v      , pe1e2u , pe1e2v     )   ! u- & v-surfaces (if gridsize reduction is used in strait(s))
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE usr_def_hgr  ***
      !!
      !! ** Purpose :   user defined mesh and Coriolis parameter
      !!
      !! ** Method  :   set all intent(out) argument to a proper value
      !!
      !!                Here ADIAB_WAVE configuration :
      !!          Rectangular domain 
      !!          - a constant horizontal resolution of 10m 
      !!          - no Coriolis effect 
      !!
      !! ** Action  : - define longitude & latitude of t-, u-, v- and f-points (in degrees) 
      !!              - define coriolis parameter at f-point if the domain in not on the sphere (on beta-plane)
      !!              - define i- & j-scale factors at t-, u-, v- and f-points (in meters)
      !!              - define u- & v-surfaces (if gridsize reduction is used in some straits) (in m2)
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:), INTENT(out) ::   plamt, plamu, plamv, plamf   ! longitude outputs                     [degrees]
      REAL(wp), DIMENSION(:,:), INTENT(out) ::   pphit, pphiu, pphiv, pphif   ! latitude outputs                      [degrees]
      INTEGER                 , INTENT(out) ::   kff                          ! =1 Coriolis parameter computed here, =0 otherwise
      REAL(wp), DIMENSION(:,:), INTENT(out) ::   pff_f, pff_t                 ! Coriolis factor at f-point                [1/s]
      REAL(wp), DIMENSION(:,:), INTENT(out) ::   pe1t, pe1u, pe1v, pe1f       ! i-scale factors                             [m]
      REAL(wp), DIMENSION(:,:), INTENT(out) ::   pe2t, pe2u, pe2v, pe2f       ! j-scale factors                             [m]
      INTEGER                 , INTENT(out) ::   ke1e2u_v                     ! =1 u- & v-surfaces computed here, =0 otherwise 
      REAL(wp), DIMENSION(:,:), INTENT(out) ::   pe1e2u, pe1e2v               ! u- & v-surfaces (if reduction in strait)   [m2]
      !
      INTEGER  ::   ji, jj               ! dummy loop indices
      REAL(wp) ::   zfact, zfact2        ! local scalars
      !!-------------------------------------------------------------------------------
      !
      !
      IF(lwp) WRITE(numout,*) 'usr_def_hgr : ADIAB_GLM configuration bassin'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~   uniform grid spacing WITHOUT Coriolis force (f=0)'
      !
      !                       !==  grid point position  ==!   (in kilometers)
      zfact = rn_dx * 1.e-3         ! conversion in km
      zfact2= rn_dy * 1.e-3
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
!      DO_2D( 1, 1, 1, 1 )
         !                       ! longitude
         plamt(ji,jj) = zfact * (          REAL( mig(ji,0)-1 , wp )  )
         plamu(ji,jj) = zfact * (    0.5 + REAL( mig(ji,0)-1 , wp )  )
         plamv(ji,jj) = plamt(ji,jj)
         plamf(ji,jj) = plamu(ji,jj)
         !                       ! latitude
         pphit(ji,jj) = zfact2 * (          REAL( mjg(jj,0)-1 , wp )  )
         pphiu(ji,jj) = pphit(ji,jj)
         pphiv(ji,jj) = zfact2 * (    0.5 + REAL( mjg(jj,0)-1 , wp )  )
         pphif(ji,jj) = pphiv(ji,jj)
      END_2D
      !
      !                       !==  Horizontal scale factors  ==!   (in meters) 
      pe1t(:,:) = rn_dx   ;   pe2t(:,:) = rn_dy
      pe1u(:,:) = rn_dx   ;   pe2u(:,:) = rn_dy
      pe1v(:,:) = rn_dx   ;   pe2v(:,:) = rn_dy
      pe1f(:,:) = rn_dx   ;   pe2f(:,:) = rn_dy
      !
      !                             ! NO reduction of grid size in some straits 
      ke1e2u_v = 0                  !    ==>> u_ & v_surfaces will be computed in dom_ghr routine
      pe1e2u(:,:) = 0._wp           !    CAUTION: set to zero to avoid error with some compilers that
      pe1e2v(:,:) = 0._wp           !             require an initialization of INTENT(out) arguments
      !
      !
      !                       !==  Coriolis parameter  ==!
      kff = 1                       !  indicate not to compute Coriolis parameter afterward
      !
      pff_f(:,:) = 0._wp            ! here No earth rotation: f=0
      pff_t(:,:) = 0._wp
      !
   END SUBROUTINE usr_def_hgr

   !!======================================================================
END MODULE usrdef_hgr
