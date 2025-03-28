MODULE usrdef_istate
   !!======================================================================
   !!                     ***  MODULE usrdef_istate   ***
   !!
   !!                  ===  LOCK_EXCHANGE configuration  ===
   !!
   !! User defined : set the initial state of a user configuration
   !!======================================================================
   !! History :  NEMO ! 2016-03  (S. Flavoni, G. Madec) Original code
   !!                 ! 2020-11  (S. Techene, G. Madec) separate tsuv from ssh
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!  usr_def_istate : initial state in Temperature and salinity
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean space and time domain
   USE dom_oce , ONLY : glamt 
   USE phycst         ! physical constants
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   usr_def_istate       ! called by istate.F90
   PUBLIC   usr_def_istate_ssh   ! called by domqco.F90

   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
  
   SUBROUTINE usr_def_istate( pdept, ptmask, pts, pu, pv )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE usr_def_istate  ***
      !! 
      !! ** Purpose :   Initialization of the dynamics and tracers
      !!                Here LOCK_EXCHANGE configuration 
      !!
      !! ** Method  : - set temprature field
      !!              - set salinity   field
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(in   ) ::   pdept   ! depth of t-point               [m]
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(in   ) ::   ptmask  ! t-point ocean mask             [m]
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts), INTENT(  out) ::   pts     ! T & S fields      [Celsius ; g/kg]
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(  out) ::   pu      ! i-component of the velocity  [m/s] 
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(  out) ::   pv      ! j-component of the velocity  [m/s] 
      !
      INTEGER  ::   jk     ! dummy loop indices
      REAL(wp) ::   zdam   ! location of dam [Km]
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'usr_def_istate : LOCK_EXCHANGE configuration, analytical definition of initial state'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~   Ocean at rest, with a constant salinity (not used as rho=F(T) '
      IF(lwp) WRITE(numout,*) '                 and a vertical density front with a 5 kg/m3 difference located at glam=32km'
      IF(lwp) WRITE(numout,*) '                 (i.e. a temperature difference of 25 degrees with rn_a0 = 0.2'
      !
      !  rn_a0 =  0.2   !  thermal expension coefficient (nn_eos= 1)
      !  rho = rho0 - rn_a0 * (T-10) 
      !  delta_T = 25 degrees  ==>>  delta_rho = 25 * rn_a0 = 5 kg/m3
      !
      pu  (:,:,:) = 0._wp        ! ocean at rest
      pv  (:,:,:) = 0._wp
      !
      !                          ! T & S profiles
      zdam = 32.                      ! density front position in kilometers
      pts(:,:,:,jp_tem) = 30._wp * ptmask(:,:,:)
      DO jk = 1, jpkm1
         WHERE( glamt(:,:) <= zdam )   pts(:,:,jk,jp_tem) = 5._wp * ptmask(:,:,jk)
      END DO
      !
      pts(:,:,:,jp_sal) = 35._wp * ptmask(:,:,:)
      !   
   END SUBROUTINE usr_def_istate


   SUBROUTINE usr_def_istate_ssh( ptmask, pssh )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE usr_def_istate_ssh  ***
      !! 
      !! ** Purpose :   Initialization of ssh
      !!                Here LOCK_EXCHANGE configuration 
      !!
      !! ** Method  :   set ssh to 0
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(in   ) ::   ptmask  ! t-point ocean mask   [m]
      REAL(wp), DIMENSION(jpi,jpj)         , INTENT(  out) ::   pssh    ! sea-surface height   [m]
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'usr_def_istate_ssh : LOCK_EXCHANGE configuration, analytical definition of initial state'
      !
      pssh(:,:)   = 0._wp
      !
   END SUBROUTINE usr_def_istate_ssh

   !!======================================================================
END MODULE usrdef_istate
