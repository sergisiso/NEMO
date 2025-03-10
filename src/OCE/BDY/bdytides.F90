MODULE bdytides
   !!======================================================================
   !!                       ***  MODULE  bdytides  ***
   !! Ocean dynamics:   Tidal forcing at open boundaries
   !!======================================================================
   !! History :  2.0  !  2007-01  (D.Storkey)  Original code
   !!            2.3  !  2008-01  (J.Holt)  Add date correction. Origins POLCOMS v6.3 2007
   !!            3.0  !  2008-04  (NEMO team)  add in the reference version
   !!            3.3  !  2010-09  (D.Storkey and E.O'Dea)  bug fixes
   !!            3.4  !  2012-09  (G. Reffray and J. Chanut) New inputs + mods
   !!            3.5  !  2013-07  (J. Chanut) Compliant with time splitting changes
   !!----------------------------------------------------------------------
   !!   bdytide_init  : read of namelist and initialisation of tidal harmonics data
   !!   tide_update   : calculation of tidal forcing at each timestep
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers 
   USE dom_oce        ! ocean space and time domain
   USE phycst         ! physical constants
   USE bdy_oce        ! ocean open boundary conditions
   USE tide_mod       ! 
   USE daymod         ! calendar
   !
   USE in_out_manager ! I/O units
   USE iom            ! xIO server
   USE fldread        !
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)

   IMPLICIT NONE
   PRIVATE

   PUBLIC   bdytide_init     ! routine called in bdy_init
   PUBLIC   bdy_dta_tides    ! routine called in dyn_spg_ts

   TYPE, PUBLIC ::   TIDES_DATA     !: Storage for external tidal harmonics data
      REAL(wp), POINTER, DIMENSION(:,:,:)    ::   ssh0     !: Tidal constituents : SSH0   (read in file)
      REAL(wp), POINTER, DIMENSION(:,:,:)    ::   u0, v0   !: Tidal constituents : U0, V0 (read in file)
      REAL(wp), POINTER, DIMENSION(:,:,:)    ::   ssh      !: Tidal constituents : SSH    (after nodal cor.)
      REAL(wp), POINTER, DIMENSION(:,:,:)    ::   u , v    !: Tidal constituents : U , V  (after nodal cor.)
   END TYPE TIDES_DATA

!$AGRIF_DO_NOT_TREAT
   TYPE(TIDES_DATA), PUBLIC, DIMENSION(jp_bdy), TARGET :: tides  !: External tidal harmonics data
!$AGRIF_END_DO_NOT_TREAT
   TYPE(OBC_DATA)  , PUBLIC, DIMENSION(jp_bdy) :: dta_bdy_s  !: bdy external data (slow component)

   INTEGER ::   kt_tide

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "read_nml_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE bdytide_init
      !!----------------------------------------------------------------------
      !!                    ***  SUBROUTINE bdytide_init  ***
      !!                     
      !! ** Purpose : - Read in namelist for tides and initialise external
      !!                tidal harmonics data
      !!
      !!----------------------------------------------------------------------
      !! namelist variables
      !!-------------------
      CHARACTER(len=80)                         ::   filtide             ! Filename root for tidal input files
      LOGICAL                                   ::   ln_bdytide_2ddta    ! If true, read 2d harmonic data
      !!
      INTEGER                                   ::   ib_bdy, itide, ib   ! dummy loop indices
      INTEGER                                   ::   ii, ij              ! dummy loop indices
      INTEGER                                   ::   inum, igrd
      INTEGER                                   ::   isz                 ! bdy data size
      INTEGER                                   ::   ios                 ! Local integer output status for namelist read
      INTEGER                                   ::   nbdy_rdstart, nbdy_loc
      CHARACTER(LEN=50)                         ::   cerrmsg             ! error string
      CHARACTER(len=80)                         ::   clfile              ! full file name for tidal input file 
      REAL(wp),ALLOCATABLE, DIMENSION(:,:,:)    ::   dta_read            ! work space to read in tidal harmonics data
      REAL(wp),ALLOCATABLE, DIMENSION(:,:)      ::   ztr, zti            !  "     "    "   "   "   "        "      " 
      !!
      TYPE(TIDES_DATA), POINTER                 ::   td                  ! local short cut   
      TYPE(  OBC_DATA), POINTER                 ::   dta                 ! local short cut
      !!
      NAMELIST/nambdy_tide/filtide, ln_bdytide_2ddta
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'bdytide_init : initialization of tidal harmonic forcing at open boundaries'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~'


      nbdy_rdstart = 1
      DO ib_bdy = 1, nb_bdy
         IF( nn_dyn2d_dta(ib_bdy) >= 2 ) THEN
            !
            td  => tides(ib_bdy)
            dta => dta_bdy(ib_bdy)
         
            ! Namelist nambdy_tide : tidal harmonic forcing at open boundaries
            filtide(:) = ''

            READ_NML_REF(numnam,nambdy_tide)
            !
            ! Need to support possibility of reading more than one
            ! nambdy_tide from the namelist_cfg internal file.
            ! Do this by finding the ib_bdy'th occurence of nambdy_tide in the
            ! character buffer as the starting point.
            !
            nbdy_loc = INDEX( numnam_cfg( nbdy_rdstart: ), 'nambdy_tide' )
            IF( nbdy_loc .GT. 0 ) THEN
               nbdy_rdstart = nbdy_rdstart + nbdy_loc
            ELSE
               WRITE(cerrmsg,'(A,I4,A)') 'Error: entry number ',ib_bdy,' of nambdy_tide not found'
               ios = -1
               CALL ctl_nam ( ios , cerrmsg )
            ENDIF
            READ( numnam_cfg( MAX( 1, nbdy_rdstart - 2 ): ), nambdy_tide, IOSTAT = ios )
            CALL ctl_nam ( ios , 'nambdy_tide (numnam_cfg)', .FALSE. )
            IF(lwm) WRITE ( numond, nambdy_tide )
            !                                               ! Parameter control and print
            IF(lwp) WRITE(numout,*) '  '
            IF(lwp) WRITE(numout,*) '          Namelist nambdy_tide : tidal harmonic forcing at open boundaries'
            IF(lwp) WRITE(numout,*) '             read tidal data in 2d files: ', ln_bdytide_2ddta
            IF(lwp) WRITE(numout,*) '             Number of tidal components to read: ', nb_harmo
            IF(lwp) THEN 
                    WRITE(numout,*) '             Tidal components: ' 
               DO itide = 1, nb_harmo
                  WRITE(numout,*)  '                 ', tide_harmonics(itide)%cname_tide 
               END DO
            ENDIF 
            IF(lwp) WRITE(numout,*) ' '

            ! Allocate space for tidal harmonics data - get size from BDY data arrays
            ! Allocate also slow varying data in the case of time splitting:
            ! Do it anyway because at this stage knowledge of free surface scheme is unknown
            ! -----------------------------------------------------------------------
            IF( ASSOCIATED(dta%ssh) ) THEN   ! we use bdy ssh on this mpi subdomain
               isz = SIZE(dta%ssh)
               ALLOCATE( td%ssh0( isz, nb_harmo, 2 ), td%ssh( isz, nb_harmo, 2 ), dta_bdy_s(ib_bdy)%ssh( isz ) )
               dta_bdy_s(ib_bdy)%ssh(:) = 0._wp   ! needed?
            ENDIF
            IF( ASSOCIATED(dta%u2d) ) THEN   ! we use bdy u2d on this mpi subdomain
               isz = SIZE(dta%u2d)
               ALLOCATE( td%u0  ( isz, nb_harmo, 2 ), td%u  ( isz, nb_harmo, 2 ), dta_bdy_s(ib_bdy)%u2d( isz ) )
               dta_bdy_s(ib_bdy)%u2d(:) = 0._wp   ! needed?
            ENDIF
            IF( ASSOCIATED(dta%v2d) ) THEN   ! we use bdy v2d on this mpi subdomain
               isz = SIZE(dta%v2d)
               ALLOCATE( td%v0  ( isz, nb_harmo, 2 ), td%v  ( isz, nb_harmo, 2 ), dta_bdy_s(ib_bdy)%v2d( isz ) )
               dta_bdy_s(ib_bdy)%v2d(:) = 0._wp   ! needed?
            ENDIF

            ! fill td%ssh0, td%u0, td%v0
            ! -----------------------------------------------------------------------
            IF( ln_bdytide_2ddta ) THEN
               !
               ! It is assumed that each data file contains all complex harmonic amplitudes
               ! given on the global domain (ie global, jpiglo x jpjglo)
               !
               ALLOCATE( zti(jpi,jpj), ztr(jpi,jpj) )
               !
               ! SSH fields
               clfile = TRIM(filtide)//'_grid_T.nc'
               CALL iom_open( clfile , inum ) 
               igrd = 1                       ! Everything is at T-points here
               DO itide = 1, nb_harmo
                  CALL iom_get( inum, jpdom_auto, TRIM(tide_harmonics(itide)%cname_tide)//'_z1', ztr(:,:) )
                  CALL iom_get( inum, jpdom_auto, TRIM(tide_harmonics(itide)%cname_tide)//'_z2', zti(:,:) ) 
                  IF( ASSOCIATED(dta%ssh) ) THEN   ! we use bdy ssh on this mpi subdomain
                     DO ib = 1, SIZE(dta%ssh)
                        ii = idx_bdy(ib_bdy)%nbi(ib,igrd)
                        ij = idx_bdy(ib_bdy)%nbj(ib,igrd)
                        td%ssh0(ib,itide,1) = ztr(ii,ij)
                        td%ssh0(ib,itide,2) = zti(ii,ij)
                     END DO
                  ENDIF
               END DO
               CALL iom_close( inum )
               !
               ! U fields
               clfile = TRIM(filtide)//'_grid_U.nc'
               CALL iom_open( clfile , inum ) 
               igrd = 2                       ! Everything is at U-points here
               DO itide = 1, nb_harmo
                  CALL iom_get(inum, jpdom_auto, TRIM(tide_harmonics(itide)%cname_tide)//'_u1', ztr(:,:),cd_type='U',psgn=-1._wp)
                  CALL iom_get(inum, jpdom_auto, TRIM(tide_harmonics(itide)%cname_tide)//'_u2', zti(:,:),cd_type='U',psgn=-1._wp)
                  IF( ASSOCIATED(dta%u2d) ) THEN   ! we use bdy u2d on this mpi subdomain
                     DO ib = 1, SIZE(dta%u2d)
                        ii = idx_bdy(ib_bdy)%nbi(ib,igrd)
                        ij = idx_bdy(ib_bdy)%nbj(ib,igrd)
                        td%u0(ib,itide,1) = ztr(ii,ij)
                        td%u0(ib,itide,2) = zti(ii,ij)
                     END DO
                  ENDIF 
               END DO 
               CALL iom_close( inum )
               !
               ! V fields
               clfile = TRIM(filtide)//'_grid_V.nc'
               CALL iom_open( clfile , inum ) 
               igrd = 3                       ! Everything is at V-points here
               DO itide = 1, nb_harmo
                  CALL iom_get(inum, jpdom_auto, TRIM(tide_harmonics(itide)%cname_tide)//'_v1', ztr(:,:),cd_type='V',psgn=-1._wp)
                  CALL iom_get(inum, jpdom_auto, TRIM(tide_harmonics(itide)%cname_tide)//'_v2', zti(:,:),cd_type='V',psgn=-1._wp)
                  IF( ASSOCIATED(dta%v2d) ) THEN   ! we use bdy v2d on this mpi subdomain
                     DO ib = 1, SIZE(dta%v2d)
                        ii = idx_bdy(ib_bdy)%nbi(ib,igrd)
                        ij = idx_bdy(ib_bdy)%nbj(ib,igrd)
                        td%v0(ib,itide,1) = ztr(ii,ij)
                        td%v0(ib,itide,2) = zti(ii,ij)
                     END DO
                  ENDIF 
               END DO
               CALL iom_close( inum )
               !
               DEALLOCATE( ztr, zti ) 
               !
            ELSE            
               !
               ! Read tidal data only on bdy segments
               ! 
               ALLOCATE( dta_read( MAXVAL( idx_bdy(ib_bdy)%nblen(:) ), 1, 1 ) )
               !
               ! Open files and read in tidal forcing data
               ! -----------------------------------------

               DO itide = 1, nb_harmo
                  !                                                              ! SSH fields
                  IF( ASSOCIATED(dta%ssh) ) THEN   ! we use bdy ssh on this mpi subdomain
                     isz = SIZE(dta%ssh)
                     clfile = TRIM(filtide)//TRIM(tide_harmonics(itide)%cname_tide)//'_grid_T.nc'
                     CALL iom_open( clfile, inum )
                     CALL fld_map( inum, 'z1', dta_read(1:isz,1:1,1:1) , 1, idx_bdy(ib_bdy)%nbmap(:,1) )
                     td%ssh0(:,itide,1) = dta_read(1:isz,1,1)
                     CALL fld_map( inum, 'z2', dta_read(1:isz,1:1,1:1) , 1, idx_bdy(ib_bdy)%nbmap(:,1) )
                     td%ssh0(:,itide,2) = dta_read(1:isz,1,1)
                     CALL iom_close( inum )
                  ENDIF
                  !                                                              ! U fields
                  IF( ASSOCIATED(dta%u2d) ) THEN   ! we use bdy u2d on this mpi subdomain
                     isz = SIZE(dta%u2d)
                     clfile = TRIM(filtide)//TRIM(tide_harmonics(itide)%cname_tide)//'_grid_U.nc'
                     CALL iom_open( clfile, inum )
                     CALL fld_map( inum, 'u1', dta_read(1:isz,1:1,1:1) , 1, idx_bdy(ib_bdy)%nbmap(:,2) )
                     td%u0(:,itide,1) = dta_read(1:isz,1,1)
                     CALL fld_map( inum, 'u2', dta_read(1:isz,1:1,1:1) , 1, idx_bdy(ib_bdy)%nbmap(:,2) )
                     td%u0(:,itide,2) = dta_read(1:isz,1,1)
                     CALL iom_close( inum )
                  ENDIF
                  !                                                              ! V fields
                  IF( ASSOCIATED(dta%v2d) ) THEN   ! we use bdy v2d on this mpi subdomain
                     isz = SIZE(dta%v2d)
                     clfile = TRIM(filtide)//TRIM(tide_harmonics(itide)%cname_tide)//'_grid_V.nc'
                     CALL iom_open( clfile, inum )
                     CALL fld_map( inum, 'v1', dta_read(1:isz,1:1,1:1) , 1, idx_bdy(ib_bdy)%nbmap(:,3) )
                     td%v0(:,itide,1) = dta_read(1:isz,1,1)
                     CALL fld_map( inum, 'v2', dta_read(1:isz,1:1,1:1) , 1, idx_bdy(ib_bdy)%nbmap(:,3) )
                     td%v0(:,itide,2) = dta_read(1:isz,1,1)
                     CALL iom_close( inum )
                  ENDIF
                  !
               END DO ! end loop on tidal components
               !
               DEALLOCATE( dta_read )
               !
            ENDIF ! ln_bdytide_2ddta=.true.
            !
         ENDIF ! nn_dyn2d_dta(ib_bdy) >= 2
         !
      END DO ! loop on ib_bdy
      !
   END SUBROUTINE bdytide_init


   SUBROUTINE bdy_dta_tides( kt, kit, pt_offset )
      !!----------------------------------------------------------------------
      !!                 ***  SUBROUTINE bdy_dta_tides  ***
      !!                
      !! ** Purpose : - Add tidal forcing to ssh, u2d and v2d OBC data arrays. 
      !!                
      !!----------------------------------------------------------------------
      INTEGER,           INTENT(in) ::   kt          ! Main timestep counter
      INTEGER, OPTIONAL, INTENT(in) ::   kit         ! Barotropic timestep counter (for timesplitting option)
      REAL(wp),OPTIONAL, INTENT(in) ::   pt_offset   ! time offset in units of timesteps
      !
      LOGICAL  ::   lk_first_btstp            ! =.TRUE. if time splitting and first barotropic step
      INTEGER  ::   itide, ib_bdy, ib         ! loop indices
      REAL(wp) ::   z_arg, z_sarg, zramp, zoff, z_cost, z_sist, zt_offset   
      !!----------------------------------------------------------------------
      !
      lk_first_btstp=.TRUE.
      IF ( PRESENT(kit).AND.( kit /= 1 ) ) THEN ; lk_first_btstp=.FALSE. ; ENDIF

      zt_offset = 0._wp
      IF( PRESENT(pt_offset) )   zt_offset = pt_offset
      
      ! Absolute time from model initialization:   
      IF( PRESENT(kit) ) THEN  
         z_arg = ( REAL(kt, wp) + ( REAL(kit, wp) + zt_offset - 1. ) / REAL(nn_e, wp) ) * rn_Dt
      ELSE                              
         z_arg = ( REAL(kt, wp) + zt_offset ) * rn_Dt
      ENDIF

      ! Linear ramp on tidal component at open boundaries 
      zramp = 1.
      IF (ln_tide_ramp) zramp = MIN(MAX( (z_arg - REAL(nit000,wp)*rn_Dt)/(rn_tide_ramp_dt*rday),0.),1.)

      DO ib_bdy = 1,nb_bdy
         !
         IF( nn_dyn2d_dta(ib_bdy) >= 2 ) THEN
            !
            ! We refresh nodal factors every day below
            ! This should be done somewhere else
            IF ( ( nsec_day == NINT(0.5_wp * rn_Dt) .OR. kt==nit000 ) .AND. lk_first_btstp ) THEN
               !
               kt_tide = kt - NINT((REAL(nsec_day,wp) - 0.5_wp * rn_Dt)/rn_Dt)
               !
               IF(lwp) THEN
               WRITE(numout,*)
               WRITE(numout,*) 'bdy_tide_dta : Refresh nodal factors for tidal open bdy data at kt=',kt
               WRITE(numout,*) '~~~~~~~~~~~~~~ '
               ENDIF
               !
               CALL tide_init_elevation ( idx=idx_bdy(ib_bdy), td=tides(ib_bdy) )
               CALL tide_init_velocities( idx=idx_bdy(ib_bdy), td=tides(ib_bdy) )
               !
            ENDIF
            zoff = REAL(-kt_tide,wp) * rn_Dt ! time offset relative to nodal factor computation time
            !
            ! If time splitting, initialize arrays from slow varying open boundary data:
            IF ( PRESENT(kit) ) THEN           
               IF ( ASSOCIATED(dta_bdy(ib_bdy)%ssh) ) dta_bdy(ib_bdy)%ssh(:) = dta_bdy_s(ib_bdy)%ssh(:)
               IF ( ASSOCIATED(dta_bdy(ib_bdy)%u2d) ) dta_bdy(ib_bdy)%u2d(:) = dta_bdy_s(ib_bdy)%u2d(:)
               IF ( ASSOCIATED(dta_bdy(ib_bdy)%v2d) ) dta_bdy(ib_bdy)%v2d(:) = dta_bdy_s(ib_bdy)%v2d(:)
            ENDIF
            !
            ! Update open boundary data arrays:
            DO itide = 1, nb_harmo
               !
               z_sarg = (z_arg + zoff) * tide_harmonics(itide)%omega
               z_cost = zramp * COS( z_sarg )
               z_sist = zramp * SIN( z_sarg )
               !
               IF ( ASSOCIATED(dta_bdy(ib_bdy)%ssh) ) THEN   ! SSH on tracer grid
                  DO ib = 1, SIZE(dta_bdy(ib_bdy)%ssh)
                     dta_bdy(ib_bdy)%ssh(ib) = dta_bdy(ib_bdy)%ssh(ib) + &
                        &                      ( tides(ib_bdy)%ssh(ib,itide,1)*z_cost + &
                        &                        tides(ib_bdy)%ssh(ib,itide,2)*z_sist )
                  END DO
               ENDIF
               !
               IF ( ASSOCIATED(dta_bdy(ib_bdy)%u2d) ) THEN  ! U grid
                  DO ib = 1, SIZE(dta_bdy(ib_bdy)%u2d)
                     dta_bdy(ib_bdy)%u2d(ib) = dta_bdy(ib_bdy)%u2d(ib) + &
                        &                      ( tides(ib_bdy)%u(ib,itide,1)*z_cost + &
                        &                        tides(ib_bdy)%u(ib,itide,2)*z_sist )
                  END DO
               ENDIF
               !
               IF ( ASSOCIATED(dta_bdy(ib_bdy)%v2d) ) THEN   ! V grid
                  DO ib = 1, SIZE(dta_bdy(ib_bdy)%v2d)
                     dta_bdy(ib_bdy)%v2d(ib) = dta_bdy(ib_bdy)%v2d(ib) + &
                        &                      ( tides(ib_bdy)%v(ib,itide,1)*z_cost + &
                        &                        tides(ib_bdy)%v(ib,itide,2)*z_sist )
                  END DO
               ENDIF
               !
            END DO             
         ENDIF
      END DO
      !
   END SUBROUTINE bdy_dta_tides


   SUBROUTINE tide_init_elevation( idx, td )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE tide_init_elevation  ***
      !!----------------------------------------------------------------------
      TYPE(OBC_INDEX) , INTENT(in   ) ::   idx   ! OBC indices
      TYPE(TIDES_DATA), INTENT(inout) ::   td    ! tidal harmonics data
      !
      INTEGER ::   itide, isz, ib       ! dummy loop indices
      REAL(wp),ALLOCATABLE, DIMENSION(:) ::   mod_tide, phi_tide
      !!----------------------------------------------------------------------
      !
      IF( ASSOCIATED(td%ssh0) ) THEN   ! SSH on tracer grid.
         !
         isz = SIZE( td%ssh0, dim = 1 )
         ALLOCATE( mod_tide(isz), phi_tide(isz) )
         !
         DO itide = 1, nb_harmo
            DO ib = 1, isz
               mod_tide(ib)=SQRT( td%ssh0(ib,itide,1)*td%ssh0(ib,itide,1) + td%ssh0(ib,itide,2)*td%ssh0(ib,itide,2) )
               phi_tide(ib)=ATAN2(-td%ssh0(ib,itide,2),td%ssh0(ib,itide,1))
            END DO
            DO ib = 1, isz
               mod_tide(ib)=mod_tide(ib)*tide_harmonics(itide)%f
               phi_tide(ib)=phi_tide(ib)+tide_harmonics(itide)%v0+tide_harmonics(itide)%u
            END DO
            DO ib = 1, isz
               td%ssh(ib,itide,1)= mod_tide(ib)*COS(phi_tide(ib))
               td%ssh(ib,itide,2)=-mod_tide(ib)*SIN(phi_tide(ib))
            END DO
         END DO
         !
         DEALLOCATE( mod_tide, phi_tide )
         !
      ENDIF
      !
   END SUBROUTINE tide_init_elevation


   SUBROUTINE tide_init_velocities( idx, td )
      !!----------------------------------------------------------------------
      !!                 ***  ROUTINE tide_init_elevation  ***
      !!----------------------------------------------------------------------
      TYPE(OBC_INDEX) , INTENT(in   ) ::   idx   ! OBC indices
      TYPE(TIDES_DATA), INTENT(inout) ::   td    ! tidal harmonics data
      !
      INTEGER ::   itide, isz, ib        ! dummy loop indices
      REAL(wp),ALLOCATABLE, DIMENSION(:) ::   mod_tide, phi_tide
      !!----------------------------------------------------------------------
      !
      IF( ASSOCIATED(td%u0) ) THEN   ! U grid. we use bdy u2d on this mpi subdomain
         !
         isz = SIZE( td%u0, dim = 1 )
         ALLOCATE( mod_tide(isz), phi_tide(isz) )
         !
         DO itide = 1, nb_harmo
            DO ib = 1, isz
               mod_tide(ib)=SQRT( td%u0(ib,itide,1)*td%u0(ib,itide,1) + td%u0(ib,itide,2)*td%u0(ib,itide,2) )
               phi_tide(ib)=ATAN2(-td%u0(ib,itide,2),td%u0(ib,itide,1))
            END DO
            DO ib = 1, isz
               mod_tide(ib)=mod_tide(ib)*tide_harmonics(itide)%f
               phi_tide(ib)=phi_tide(ib)+tide_harmonics(itide)%v0 + tide_harmonics(itide)%u
            END DO
            DO ib = 1, isz
               td%u(ib,itide,1)= mod_tide(ib)*COS(phi_tide(ib))
               td%u(ib,itide,2)=-mod_tide(ib)*SIN(phi_tide(ib))
            END DO
         END DO
         !
         DEALLOCATE( mod_tide, phi_tide )
         !
      ENDIF
      !
      IF( ASSOCIATED(td%v0) ) THEN   ! V grid. we use bdy u2d on this mpi subdomain
         !
         isz = SIZE( td%v0, dim = 1 )
         ALLOCATE( mod_tide(isz), phi_tide(isz) )
         !
         DO itide = 1, nb_harmo
            DO ib = 1, isz
               mod_tide(ib)=SQRT( td%v0(ib,itide,1)*td%v0(ib,itide,1) + td%v0(ib,itide,2)*td%v0(ib,itide,2) )
               phi_tide(ib)=ATAN2(-td%v0(ib,itide,2),td%v0(ib,itide,1))
            END DO
            DO ib = 1, isz
               mod_tide(ib)=mod_tide(ib)*tide_harmonics(itide)%f
               phi_tide(ib)=phi_tide(ib)+tide_harmonics(itide)%v0 + tide_harmonics(itide)%u
            END DO
            DO ib = 1, isz
               td%v(ib,itide,1)= mod_tide(ib)*COS(phi_tide(ib))
               td%v(ib,itide,2)=-mod_tide(ib)*SIN(phi_tide(ib))
            END DO
         END DO
         !
         DEALLOCATE( mod_tide, phi_tide )
         !
      ENDIF
      !
   END SUBROUTINE tide_init_velocities

   !!======================================================================
END MODULE bdytides

