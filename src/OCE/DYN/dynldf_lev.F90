MODULE dynldf_lev
   !!======================================================================
   !!                   ***  MODULE  dynldf_lev  ***
   !! Ocean dynamics:  lateral viscosity trend (laplacian and bilaplacian)
   !!======================================================================
   !! History : 3.7  ! 2014-01  (G. Madec, S. Masson)  Original code, re-entrant laplacian
   !!           4.0  ! 2020-04  (A. Nasser, G. Madec)  Add symmetric mixing tensor
   !!           4.5  ! 2022-10  (S. Techene, G, Madec)  refactorization to reduce local memory usage
   !!                !                                + removal of old partial-step treatment
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dynldf_lev_lap   : update the momentum trend with the lateral viscosity using an iso-level   laplacian operator
   !!   dynldf_lev_blp   : update the momentum trend with the lateral viscosity using an iso-level bilaplacian operator
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE domutl, ONLY : is_tile
   USE ldfdyn         ! lateral diffusion: eddy viscosity coef.
   USE ldfslp         ! iso-neutral slopes 
   USE zdf_oce        ! ocean vertical physics
   !
   USE in_out_manager ! I/O manager
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp

   IMPLICIT NONE
   PRIVATE

   PUBLIC dynldf_lev_lap  ! called by dynldf.F90
   PUBLIC dynldf_lev_blp  ! called by dynldf.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: dynldf_lap_blp.F90 15033 2021-06-21 10:24:45Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dynldf_lev_lap( kt, Kbb, Kmm, pu, pv, Krhs )
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE dynldf_lev_lap  ***
      !!                       
      !! ** Purpose :   Compute the before horizontal momentum diffusive 
      !!      trend and add it to the general trend of momentum equation.
      !!
      !! ** Method  :   The Laplacian operator apply on horizontal velocity is 
      !!      writen as :   grad_h( ahmt div_h(U )) - curl_h( ahmf curl_z(U) ) 
      !!
      !! ** Action : - pu(Krhs), pv(Krhs) increased by the harmonic operator applied on pu(Kbb), pv(Kbb).
      !!
      !! Reference : S.Griffies, R.Hallberg 2000 Mon.Wea.Rev., DOI:/ 
      !!----------------------------------------------------------------------
      INTEGER                     , INTENT(in   ) ::   kt, Kbb, Kmm, Krhs   ! ocean time-step index and time-level indices
      REAL(wp), DIMENSION(:,:,:,:), INTENT(inout) ::   pu, pv               ! velocity [m/s2]
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp), DIMENSION(T2D(1)) ::   zwf, zwt
      !!----------------------------------------------------------------------
      !
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == nit000 )  THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'dynldf_lev_lap : laplacian operator momentum '
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~'
         ENDIF
      ENDIF
      !
#     define   lap
#     define   INN              0
#     define   pu_in(i,j,k,t)   pu(i,j,k,t)
#     define   pv_in(i,j,k,t)   pv(i,j,k,t)
      !
      SELECT CASE( nn_dynldf_typ )  
      !              
      CASE ( np_typ_rot )       !==  Vorticity-Divergence operator  ==!
         !
#        define zcur   zwf
#        define zdiv   zwt
         !
         DO jk = 1, jpkm1                                 ! Horizontal slab
#           include "dynldf_lev_rot_scheme.h90"
         END DO
         !
#        undef  zcur   
#        undef  zdiv
         !
      CASE ( np_typ_sym )       !==  Symmetric operator  ==!
         !
#        define zshe   zwf
#        define zten   zwt
         !
         DO jk = 1, jpkm1                                 ! Horizontal slab
#           include "dynldf_lev_sym_scheme.h90"
         END DO
         !
#        undef  zshe  
#        undef  zten
         !
      END SELECT
      ! 
#     undef    lap
#     undef    INN
#     undef    pu_in
#     undef    pv_in
      !
    END SUBROUTINE dynldf_lev_lap


    SUBROUTINE dynldf_lev_blp( kt, Kbb, Kmm, pu, pv, Krhs )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE dynldf_lev_blp  ***
      !!                    
      !! ** Purpose :   Compute the before lateral momentum viscous trend 
      !!              and add it to the general trend of momentum equation.
      !!
      !! ** Method  :   The lateral viscous trends is provided by a bilaplacian
      !!      operator applied to before field (forward in time).
      !!      It is computed by two successive calls to dyn_ldf_lap routine
      !!
      !! ** Action :   pt(:,:,:,:,Krhs)   updated with the before rotated bilaplacian diffusion
      !!----------------------------------------------------------------------
      !!
      INTEGER                         , INTENT(in   ) ::   kt, Kbb, Kmm, Krhs   ! ocean time-step index and time-level indices
      REAL(wp), DIMENSION(:,:,:,:)    , INTENT(inout) ::   pu, pv               ! velocity [m/s2]
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      REAL(wp), DIMENSION(T2D(2)) ::   zulap   ! laplacian at u-point
      REAL(wp), DIMENSION(T2D(2)) ::   zvlap   ! laplacian at v-point
      REAL(wp), DIMENSION(T2D(2)) ::   zwf, zwt
      !!----------------------------------------------------------------------
      !
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == nit000 )  THEN
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'dynldf_lev_blp : bilaplacian operator momentum '
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~'
         ENDIF
      ENDIF
      !
      SELECT CASE( nn_dynldf_typ )  
      !              
      CASE ( np_typ_rot )       !==  Vorticity-Divergence operator  ==!
         !
#        define zcur   zwf
#        define zdiv   zwt
         !
         DO jk = 1, jpkm1                                 ! Horizontal slab
            !                                                   !-  1st pass 
#           define   blp_p1
#           define   INN                1
#           define   pu_in(i,j,k,t)   pu(i,j,k,t)
#           define   pv_in(i,j,k,t)   pv(i,j,k,t)
!
#           include "dynldf_lev_rot_scheme.h90"
!
#           undef    blp_p1
#           undef    INN
#           undef    pu_in
#           undef    pv_in

            !                                                   !-  2nd pass 
#           define   blp_p2
#           define   INN                0
#           define   pu_in(i,j,k,t)   zulap(i,j)
#           define   pv_in(i,j,k,t)   zvlap(i,j)
!
#           include "dynldf_lev_rot_scheme.h90"
!
#           undef    blp_p2
#           undef    INN
#           undef    pu_in
#           undef    pv_in
         END DO
         !
#        undef  zcur 
#        undef  zdiv
         !
      CASE ( np_typ_sym )       !==  Symmetric operator  ==!
         !
#        define zshe   zwf
#        define zten   zwt
         !
         DO jk = 1, jpkm1                                 ! Horizontal slab
            !                                                   !-  1st pass 
#           define   blp_p1
#           define   INN                1
#           define   pu_in(i,j,k,t)   pu(i,j,k,t)
#           define   pv_in(i,j,k,t)   pv(i,j,k,t)
!
#           include "dynldf_lev_sym_scheme.h90"
!
#           undef    blp_p1
#           undef    INN
#           undef    pu_in
#           undef    pv_in

            !                                                   !-  2nd pass 
#           define   blp_p2
#           define   INN                0
#           define   pu_in(i,j,k,t)   zulap(i,j)
#           define   pv_in(i,j,k,t)   zvlap(i,j)
!
#           include "dynldf_lev_sym_scheme.h90"
!
#           undef    blp_p2
#           undef    INN
#           undef    pu_in
#           undef    pv_in
         END DO
         !
#        undef  zshe  
#        undef  zten
      END SELECT
      !
   END SUBROUTINE dynldf_lev_blp

   !!======================================================================
END MODULE dynldf_lev
