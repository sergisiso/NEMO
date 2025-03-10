MODULE usrdef_zgr
   !!======================================================================
   !!                   ***  MODULE  usrdef_zgr  ***
   !!
   !!                   ===      ICE_ADV1D case     ===
   !!
   !! Ocean domain : user defined vertical coordinate system 
   !!======================================================================
   !! History :  4.0  ! 2016-08  (G. Madec, S. Flavoni)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   usr_def_zgr   : user defined vertical coordinate system (required)
   !!---------------------------------------------------------------------
   USE oce            ! ocean variables
   USE usrdef_nam     ! User defined : namelist variables
   !
   USE in_out_manager ! I/O manager
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp        ! distributed memory computing library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   usr_def_zgr   ! called by domzgr.F90

   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS             

   SUBROUTINE usr_def_zgr( ld_zco  , ld_zps  , ld_sco  , ld_isfcav,    &   ! type of vertical coordinate
      &                    k_top   , k_bot                        ,    &   ! top & bottom ocean level
      &                    pdept_1d, pdepw_1d, pe3t_1d , pe3w_1d  ,    &   ! 1D reference vertical coordinate
      &                    pe3t  , pe3u  , pe3v   , pe3f ,             &   ! vertical scale factors
      &                    pdept , pdepw ,                             &   ! 3D t & w-points depth
      &                    pe3w  , pe3uw , pe3vw                       )   ! vertical scale factors
      !!---------------------------------------------------------------------
      !!              ***  ROUTINE usr_def_zgr  ***
      !!
      !! ** Purpose :   User defined the vertical coordinates
      !!
      !!----------------------------------------------------------------------
      LOGICAL                   , INTENT(out) ::   ld_zco, ld_zps, ld_sco      ! vertical coordinate flags
      LOGICAL                   , INTENT(out) ::   ld_isfcav                   ! under iceshelf cavity flag
      INTEGER , DIMENSION(:,:)  , INTENT(out) ::   k_top, k_bot                ! first & last ocean level
      REAL(wp), DIMENSION(:)    , INTENT(out) ::   pdept_1d, pdepw_1d          ! 1D grid-point depth     [m]
      REAL(wp), DIMENSION(:)    , INTENT(out) ::   pe3t_1d , pe3w_1d           ! 1D grid-point depth     [m]
      REAL(wp), DIMENSION(:,:,:), OPTIONAL, INTENT(out) ::   pdept, pdepw                ! grid-point depth        [m]
      REAL(wp), DIMENSION(:,:,:), OPTIONAL, INTENT(out) ::   pe3t , pe3u , pe3v , pe3f   ! vertical scale factors  [m]
      REAL(wp), DIMENSION(:,:,:), OPTIONAL, INTENT(out) ::   pe3w , pe3uw, pe3vw         ! i-scale factors 
      !
      INTEGER  ::   jk, k_dz  ! dummy indices
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'usr_def_zgr : ICE_ADV1D configuration (slab ocean - advection of ice in one direction)'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
      !
      !
      ! type of vertical coordinate  ==>>>   here ICE_ADV1D : slab ocean always
      ! ---------------------------
      ld_zco    = .TRUE.       ! z-full-step coordinate
      ld_zps    = .FALSE.      ! z-partial-step coordinate
      ld_sco    = .FALSE.      ! s-coordinate
      ld_isfcav = .FALSE.      ! ISF Ice Shelves Flag
      !
      !
      ! Build the vertical coordinate system
      ! ------------------------------------
      !
      !                       !==  UNmasked meter bathymetry  ==!
      !
      ! 
      k_dz = 1
      DO jk = 1, jpk
         pdepw_1d(jk) =    k_dz
         pdept_1d(jk) =    k_dz
         pe3w_1d (jk) =    k_dz
         pe3t_1d (jk) =    k_dz
      END DO
      !                       !==  top masked level bathymetry  ==!  (all coordinates)
      !
      ! no ocean cavities : top ocean level is ONE, except over land
      k_top(:,:) = 1
      !
      !                       !==  z-coordinate  ==!   (step-like topography)
      !                                !* bottom ocean compute from the depth of grid-points
      !!jpkm1 = jpk
      k_bot(:,:) = 1    ! here use k_top as a land mask
      !                                !* horizontally uniform coordinate (reference z-co everywhere)
#if defined key_vco_1d3d
      DO jk = 1, jpk
         pe3t (:,:,jk) = pe3t_1d (jk)
         pe3u (:,:,jk) = pe3t_1d (jk)
         pe3v (:,:,jk) = pe3t_1d (jk)
         pe3f (:,:,jk) = pe3t_1d (jk)
      END DO
#endif
      !
#if defined key_vco_3d
      DO jk = 1, jpk
         pdept(:,:,jk) = pdept_1d(jk)
         pdepw(:,:,jk) = pdepw_1d(jk)
         pe3t (:,:,jk) = pe3t_1d (jk)
         pe3u (:,:,jk) = pe3t_1d (jk)
         pe3v (:,:,jk) = pe3t_1d (jk)
         pe3f (:,:,jk) = pe3t_1d (jk)
         pe3w (:,:,jk) = pe3w_1d (jk)
         pe3uw(:,:,jk) = pe3w_1d (jk)
         pe3vw(:,:,jk) = pe3w_1d (jk)
      END DO
#endif
      !
   END SUBROUTINE usr_def_zgr

   !!======================================================================
END MODULE usrdef_zgr
