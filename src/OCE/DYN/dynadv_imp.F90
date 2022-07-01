MODULE dynimp
   !!==============================================================================
   !!                 ***  MODULE  dynimp  ***
   !! Ocean dynamics :  implicit vertical component of the momentum advective trend
   !!==============================================================================
   !! History :  4.2  !  2022  (A. C. Coward) 
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dyn_adv_imp   : compute the after velocity through implicit calculation of vertical advection
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers variables
   USE phycst         ! physical constants
   USE dom_oce        ! ocean space and time domain variables 
   USE zdf_oce        ! ocean vertical physics variables
   USE dynadv    ,ONLY: ln_dynadv_vec    ! dynamics: advection form
   USE traadv_fct,ONLY: tridia_solver    ! tridiagonal solver
   USE trd_oce        ! trends: ocean variables
   USE trddyn         ! trend manager: dynamics
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   USE lib_fortran
   USE prtctl         ! Print control
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dyn_adv_imp   !  routine called by stprk3_stg.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.2 , NEMO Consortium (2022)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
   
   SUBROUTINE dyn_adv_imp( kt, Kbb, Kmm, Krhs, puu, pvv, pwi, Kaa )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_adv_imp  ***
      !!
      !! ** Purpose :   compute the trend due to the vert. momentum advection
      !!              that has been partitioned to be treated implicitly by
      !!              the adaptive-implicit vertical advection scheme
      !!              (ln_zad_Aimp = .true.)
      !!
      !! ** Method  : 
      !!               - update the velocity trends with the implicit vertical advection.
      !!      This requires to solver the following system: 
      !!         u(after) = u(after) + 1/e3u_after  dk+1[ mi(avm) / e3uw_after dk[ua] ]
      !!
      !! ** Action :   (puu(:,:,:),pvv(:,:,:))   after velocity 
      !!---------------------------------------------------------------------
      INTEGER                         , INTENT( in )  ::  kt                  ! ocean time-step index
      INTEGER                         , INTENT( in )  ::  Kbb, Kmm, Krhs, Kaa ! ocean time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT( in )  ::  pwi                 ! part of ocean vertical velocity to be treated implicitly
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(inout) ::  puu, pvv            ! ocean velocities and RHS of momentum equation
      !
      INTEGER  ::   ji, jj, jk           ! dummy loop indices
      INTEGER  ::   iku, ikv             ! local integers
      REAL(wp) ::   zzwi, ze3ua, zDt_2   ! local scalars
      REAL(wp) ::   zzws, ze3va          !   -      -
      REAL(wp) ::   z1_e3ua, z1_e3va     !   -      -
      REAL(wp) ::   zWu , zWv            !   -      -
      REAL(wp) ::   zWui, zWvi           !   -      -
      REAL(wp) ::   zWus, zWvs           !   -      -
      REAL(wp), DIMENSION(A2D(nn_hls),jpk)        ::  zwi, zwd, zws   ! 3D workspace
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   ztrdu, ztrdv   !  -      -
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('dyn_adv_imp')
      !
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == nit000 ) THEN       !* initialization
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'dyn_adv_imp : vertical momentum advection implicit operator'
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~~ '
         ENDIF
      ENDIF
      !
      IF( MAXVAL( ABS( pwi(A2D(1),:) ) ) < rsmall ) RETURN  ! to be used once the glob_sum debugging statements are removed
      !
      zDt_2 = rDt * 0.5_wp      ! 0.5 to average wi onto U-V points
      !
      !
      IF( l_trddyn )   THEN         !* temporary save of trends
         ALLOCATE( ztrdu(jpi,jpj,jpk), ztrdv(jpi,jpj,jpk) ) 
         ztrdu(:,:,:) = puu(:,:,:)
         ztrdv(:,:,:) = pvv(:,:,:)
      ENDIF
      !
      !              !==  Implicit vertical advection ==!
      !
      !              !* U- Matrix construction
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         z1_e3ua =  1._wp  / e3u(ji,jj,jk,Kaa)   ! after scale factor at U-point
         zWui = - ( wi(ji,jj,jk  ) + wi(ji+1,jj,jk  ) ) * z1_e3ua
         zWus = ( wi(ji,jj,jk+1) + wi(ji+1,jj,jk+1) ) * z1_e3ua
         zwi(ji,jj,jk) =    zDt_2 * MIN( zWui, 0._wp ) 
         zws(ji,jj,jk) =  - zDt_2 * MAX( zWus, 0._wp )
         zwd(ji,jj,jk) = 1._wp + zDt_2 * ( MAX( zWui, 0._wp ) - MIN( zWus, 0._wp ) )
      END_3D
      DO_2D( 0, 0, 0, 0 )     !* Surface boundary conditions
         zwi(ji,jj,1) = 0._wp
         zWus = ( wi(ji  ,jj,2) +  wi(ji+1,jj,2) ) / e3u(ji,jj,1,Kaa)
         zws(ji,jj,1 ) =       - zDt_2 * MAX( zWus, 0._wp )
         zwd(ji,jj,1 ) = 1._wp - zDt_2 * MIN( zWus, 0._wp )
      END_2D
      !
      CALL tridia_solver( zwd, zws, zwi, puu(:,:,:), puu(:,:,:) , 1 )
      !
      !              !* V- Matrix construction
      DO_3D( 0, 0, 0, 0, 1, jpkm1 )
         z1_e3va = 1._wp / e3v(ji,jj,jk,Kaa)   ! after scale factor at V-point
         zWvi = ( wi(ji,jj,jk  ) + wi(ji,jj+1,jk  ) ) * z1_e3va
         zWvs = ( wi(ji,jj,jk+1) + wi(ji,jj+1,jk+1) ) * z1_e3va
         zwi(ji,jj,jk) =  + zDt_2 * MIN( zWvi, 0._wp )
         zws(ji,jj,jk) =  - zDt_2 * MAX( zWvs, 0._wp )
         zwd(ji,jj,jk) = 1._wp - zDt_2 * ( - MAX( zWvi, 0._wp ) + MIN( zWvs, 0._wp ) )
      END_3D
      DO_2D( 0, 0, 0, 0 )   !* Surface boundary conditions
         zwi(ji,jj,1) = 0._wp
         zWvs = ( wi(ji,jj  ,2) +  wi(ji,jj+1,2) ) / e3v(ji,jj,1,Kaa)
         zws(ji,jj,1 ) =       - zDt_2 * MAX( zWvs, 0._wp )
         zwd(ji,jj,1 ) = 1._wp - zDt_2 * MIN( zWvs, 0._wp )
      END_2D
      !
      CALL tridia_solver( zwd, zws, zwi, pvv(:,:,:), pvv(:,:,:) , 1 )
      !
      IF( l_trddyn )   THEN                      ! save the vertical implicit advection trends for further diagnostics
         ztrdu(:,:,:) = ( puu(:,:,:) - ztrdu(:,:,:) )
         ztrdv(:,:,:) = ( pvv(:,:,:) - ztrdv(:,:,:) )
         !CALL trd_dyn( ztrdu, ztrdv, jpdyn_zdf, kt, Kmm ) ! TBD
         DEALLOCATE( ztrdu, ztrdv ) 
      ENDIF
      !                                          ! print mean trends (used for debugging)
      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=puu(:,:,:), clinfo1=' zdf  - Ua: ', mask1=umask,               &
         &                                  tab3d_2=pvv(:,:,:), clinfo2=       ' Va: ', mask2=vmask, clinfo3='dyn' )
         !
      IF( ln_timing )   CALL timing_stop('dyn_adv_imp')
      !
   END SUBROUTINE dyn_adv_imp

   !!==============================================================================
END MODULE dynimp
