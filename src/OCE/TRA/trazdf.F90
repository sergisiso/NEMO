MODULE trazdf
   !!==============================================================================
   !!                 ***  MODULE  trazdf  ***
   !! Ocean active tracers:  vertical component of the tracer mixing trend
   !!==============================================================================
   !! History :  1.0  !  2005-11  (G. Madec)  Original code
   !!            3.0  !  2008-01  (C. Ethe, G. Madec)  merge TRC-TRA
   !!            4.0  !  2017-06  (G. Madec)  remove explict time-stepping option
   !!            4.5  !  2022-06  (G. Madec)  refactoring to reduce memory usage (j-k-i loops)
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   tra_zdf       : Update the tracer trend with the vertical diffusion
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers variables
   USE dom_oce        ! ocean space and time domain variables
   USE phycst         ! physical constant
   USE zdf_oce        ! ocean vertical physics variables
   USE zdfmfc         ! Mass FLux Convection
   USE sbc_oce        ! surface boundary condition: ocean
   USE ldftra         ! lateral diffusion: eddy diffusivity
   USE ldfslp         ! lateral diffusion: iso-neutral slope
   USE trd_oce        ! trends: ocean variables
   USE trdtra         ! trends: tracer trend manager
   USE eosbn2   , ONLY: ln_SEOS, rn_b0
   !
   USE in_out_manager ! I/O manager
   USE prtctl         ! Print control
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp        ! MPP library
   USE timing         ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   tra_zdf       ! called by step.F90
   PUBLIC   tra_zdf_imp   ! called by trczdf.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: trazdf.F90 14834 2021-05-11 09:24:44Z hadcv $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE tra_zdf( kt, Kbb, Kmm, Krhs, pts, Kaa )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_zdf  ***
      !!
      !! ** Purpose :   compute the vertical ocean tracer physics.
      !!---------------------------------------------------------------------
      INTEGER                                  , INTENT(in)    :: kt                  ! ocean time-step index
      INTEGER                                  , INTENT(in)    :: Kbb, Kmm, Krhs, Kaa ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts,jpt), INTENT(inout) :: pts                 ! active tracers and RHS of tracer equation
      !
      INTEGER  ::   ji, jj, jk   ! Dummy loop indices
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   ztrdt, ztrds   ! 3D workspace
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('tra_zdf')
      !
      IF( kt == nit000 )  THEN
         IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                   ! Do only on the first tile
            IF(lwp)WRITE(numout,*)
            IF(lwp)WRITE(numout,*) 'tra_zdf : implicit vertical mixing on T & S'
            IF(lwp)WRITE(numout,*) '~~~~~~~ '
         ENDIF
      ENDIF
      !
      IF( l_trdtra )   THEN                  !* Save ta and sa trends
         ALLOCATE( ztrdt(jpi,jpj,jpk), ztrds(jpi,jpj,jpk) )
         ztrdt(:,:,:) = pts(:,:,:,jp_tem,Kaa)
         ztrds(:,:,:) = pts(:,:,:,jp_sal,Kaa)
      ENDIF
      !
      !                                      !* compute lateral mixing trend and add it to the general trend
      CALL tra_zdf_imp( 'TRA', Kbb, Kmm, Krhs, pts, Kaa, jpts )

!!gm WHY here !   and I don't like that !
      ! DRAKKAR SSS control {
      ! JMM avoid negative salinities near river outlet ! Ugly fix
      ! JMM : restore negative salinities to small salinities:
!!jc: discard this correction in case salinity is not used in eos
      IF ( .NOT.(ln_SEOS.AND.(rn_b0==0._wp)) ) THEN
         WHERE( pts(T2D(0),:,jp_sal,Kaa) < 0._wp )   pts(T2D(0),:,jp_sal,Kaa) = 0.1_wp
      ENDIF
!!gm

      IF( l_trdtra )   THEN                      ! save the vertical diffusive trends for further diagnostics
         DO jk = 1, jpk
            ztrdt(:,:,jk) = (   (  pts(:,:,jk,jp_tem,Kaa)*e3t(:,:,jk,Kaa)     &
               &                 - pts(:,:,jk,jp_tem,Kbb)*e3t(:,:,jk,Kbb)  )  &
               &              / (  e3t(:,:,jk,Kmm)*rDt  )   )                 &
               &          - ztrdt(:,:,jk)
            ztrds(:,:,jk) = (   (  pts(:,:,jk,jp_sal,Kaa)*e3t(:,:,jk,Kaa)     &
               &                 - pts(:,:,jk,jp_sal,Kbb)*e3t(:,:,jk,Kbb)  )  &
               &             / (   e3t(:,:,jk,Kmm)*rDt  )   )                 &
               &          - ztrds(:,:,jk)
         END DO
!!gm this should be moved in trdtra.F90 and done on all trends
         CALL lbc_lnk( 'trazdf', ztrdt, 'T', 1.0_wp , ztrds, 'T', 1.0_wp )
!!gm
         CALL trd_tra( kt, Kmm, Krhs, 'TRA', jp_tem, jptra_zdf, ztrdt )
         CALL trd_tra( kt, Kmm, Krhs, 'TRA', jp_sal, jptra_zdf, ztrds )
         DEALLOCATE( ztrdt , ztrds )
      ENDIF
      !                                          ! print mean trends (used for debugging)
      IF(sn_cfctl%l_prtctl)   CALL prt_ctl( tab3d_1=pts(:,:,:,jp_tem,Kaa), clinfo1=' zdf  - Ta: ', mask1=tmask,               &
         &                                  tab3d_2=pts(:,:,:,jp_sal,Kaa), clinfo2=       ' Sa: ', mask2=tmask, clinfo3='tra' )
      !
      IF( ln_timing )   CALL timing_stop('tra_zdf')
      !
   END SUBROUTINE tra_zdf


   SUBROUTINE tra_zdf_imp( cdtype, Kbb, Kmm, Krhs, pt, Kaa, kjpt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE tra_zdf_imp  ***
      !!
      !! ** Purpose :   Compute the after tracer through a implicit computation
      !!     of the vertical tracer diffusion (including the vertical component
      !!     of lateral mixing (only for 2nd order operator, for fourth order
      !!     it is already computed and add to the general trend in traldf)
      !!
      !! ** Method  :  The vertical diffusion of a tracer ,t , is given by:
      !!          difft = dz( avt dz(t) ) = 1/e3t dk+1( avt/e3w dk(t) )
      !!      It is computed using a backward time scheme (t=after field)
      !!      which provide directly the after tracer field.
      !!      If ln_zdfddm=T, use avs for salinity or for passive tracers
      !!      Surface and bottom boundary conditions: no diffusive flux on
      !!      both tracers (bottom, applied through the masked field avt).
      !!      If iso-neutral mixing, add to avt the contribution due to lateral mixing.
      !!
      !! ** Action  : - pt(:,:,:,:,Kaa)  becomes the after tracer
      !!---------------------------------------------------------------------
      INTEGER                                  , INTENT(in   ) ::   Kbb, Kmm, Krhs, Kaa  ! ocean time level indices
      CHARACTER(len=3)                         , INTENT(in   ) ::   cdtype   ! =TRA or TRC (tracer indicator)
      INTEGER                                  , INTENT(in   ) ::   kjpt     ! number of tracers
      REAL(wp), DIMENSION(jpi,jpj,jpk,kjpt,jpt), INTENT(inout) ::   pt       ! tracers and RHS of tracer equation
      !
      INTEGER  ::  ji, jj, jk, jn   ! dummy loop indices
      REAL(wp) ::  zrhs, zzwi, zzws ! local scalars
      REAL(wp), DIMENSION(T1Di(0),jpk) ::  zwi, zwt, zwd, zws
      !!---------------------------------------------------------------------
      !
      !                                               ! ================= !
      DO_1Dj( 0, 0 )                                  !  i-k slices loop  !
         !                                            ! ================= !
         DO jn = 1, kjpt                              !    tracer loop    !
            !                                         ! ================= !
            !
            !  Matrix construction
            ! --------------------
            ! Build matrix if temperature or salinity (only in double diffusion case) or first passive tracer
            !
            IF(  ( cdtype == 'TRA' .AND. ( jn == jp_tem .OR. ( jn == jp_sal .AND. ln_zdfddm ) ) ) .OR.   &
               & ( cdtype == 'TRC' .AND. jn == 1 )  )  THEN
               !
               ! vertical mixing coef.: avt for temperature, avs for salinity and passive tracers
               !
               IF( cdtype == 'TRA' .AND. jn == jp_tem ) THEN     ! use avt  for temperature
                  !
                  IF( l_ldfslp ) THEN            ! use avt + isoneutral diffusion contribution
                     IF( ln_traldf_msc  ) THEN        ! MSC iso-neutral operator
                        DO_2Dik( 0, 0,   2, jpk, 1 )
                           zwt(ji,jk) = avt(ji,jj,jk) + akz(ji,jj,jk)
                        END_2D
                     ELSE                             ! standard or triad iso-neutral operator
                        DO_2Dik( 0, 0,   2, jpk, 1 )
                           zwt(ji,jk) = avt(ji,jj,jk) + ah_wslp2(ji,jj,jk)
                        END_2D
                     ENDIF
                  ELSE                          ! use avt only
                     DO_2Dik( 0, 0,   2, jpk, 1 )
                        zwt(ji,jk) = avt(ji,jj,jk)
                     END_2D
                  ENDIF
                  !
               ELSE                                               ! use avs for salinty or passive tracers
                  !
                  IF( l_ldfslp ) THEN            ! use avs + isoneutral diffusion contribution
                     IF( ln_traldf_msc  ) THEN        ! MSC iso-neutral operator
                        DO_2Dik( 0, 0,   2, jpk, 1 )
                           zwt(ji,jk) = avs(ji,jj,jk) + akz(ji,jj,jk)
                        END_2D
                     ELSE                             ! standard or triad iso-neutral operator
                        DO_2Dik( 0, 0,   2, jpk, 1 )
                           zwt(ji,jk) = avs(ji,jj,jk) + ah_wslp2(ji,jj,jk)
                        END_2D
                     ENDIF
                  ELSE                          !
                     DO_2Dik( 0, 0,   2, jpk, 1 )
                        zwt(ji,jk) = avs(ji,jj,jk)
                     END_2D
                  ENDIF
               ENDIF
               zwt(:,1) = 0._wp
               !
               ! Diagonal, lower (i), upper (s)  (including the bottom boundary condition since avt is masked)
               IF( ln_zad_Aimp ) THEN         ! Adaptive implicit vertical advection
                  DO_2Dik( 0, 0,   1, jpkm1, 1 )
                     zzwi = - rDt * zwt(ji,jk  ) / e3w(ji,jj,jk  ,Kmm)
                     zzws = - rDt * zwt(ji,jk+1) / e3w(ji,jj,jk+1,Kmm)
                     zwd(ji,jk) = e3t(ji,jj,jk,Kaa) - ( zzwi + zzws )   &
                        &              + rDt * ( MAX( wi(ji,jj,jk  ) , 0._wp ) &
                        &                      - MIN( wi(ji,jj,jk+1) , 0._wp ) )
                     zwi(ji,jk) = zzwi + rDt *   MIN( wi(ji,jj,jk  ) , 0._wp )
                     zws(ji,jk) = zzws - rDt *   MAX( wi(ji,jj,jk+1) , 0._wp )
                  END_2D
               ELSE
                  DO_2Dik( 0, 0,   1, jpkm1, 1 )
                     zwi(ji,jk) = - rDt * zwt(ji,jk  ) / e3w(ji,jj,jk,Kmm)
                     zws(ji,jk) = - rDt * zwt(ji,jk+1) / e3w(ji,jj,jk+1,Kmm)
                     zwd(ji,jk) = e3t(ji,jj,jk,Kaa) - ( zwi(ji,jk) + zws(ji,jk) )
                  END_2D
               ENDIF
               !
!!gm  BUG?? : if edmfm is equivalent to a w  ==>>>   just add +/-  rDt * edmfm(ji,jj,jk+1/jk  )
!!            but edmfm is at t-point !!!!   crazy???  why not keep it at w-point????
               !
               IF( ln_zdfmfc ) THEN    ! add upward Mass Flux in the matrix
                  DO_2Dik( 0, 0,   1, jpkm1, 1 )
                     zws(ji,jk) = zws(ji,jk) + e3t(ji,jj,jk,Kaa) * rDt * edmfm(ji,jj,jk+1) / e3w(ji,jj,jk+1,Kmm)
                     zwd(ji,jk) = zwd(ji,jk) - e3t(ji,jj,jk,Kaa) * rDt * edmfm(ji,jj,jk  ) / e3w(ji,jj,jk+1,Kmm)
                  END_2D
               ENDIF
!       DO_3D( 0, 0, 0, 0, 1, jpkm1 )
!          edmfa(ji,jj,jk) =  0._wp
!          edmfb(ji,jj,jk) = -edmfm(ji,jj,jk  ) / e3w(ji,jj,jk+1,Kmm)
!          edmfc(ji,jj,jk) =  edmfm(ji,jj,jk+1) / e3w(ji,jj,jk+1,Kmm)
!       END_3D
!!gm    BUG :  level jpk never used in the inversion
!       DO_2D( 0, 0, 0, 0 )
!          edmfa(ji,jj,jpk)   = -edmfm(ji,jj,jpk-1) / e3w(ji,jj,jpk,Kmm)
!          edmfb(ji,jj,jpk)   =  edmfm(ji,jj,jpk  ) / e3w(ji,jj,jpk,Kmm)
!          edmfc(ji,jj,jpk)   =  0._wp
!       END_2D
!!
!!gm   BUG ???   below  e3t_Kmm  should be used ?
!!               or even no multiplication by e3t unless there is a bug in wi calculation
!!
!                   DO_3D( 0, 0, 0, 0, 1, jpkm1 )
!!gm edmfa = 0._wp except at jpk which is not used  ==>>  zdiagi update is useless !
!                      zdiagi(ji,jj,jk) = zdiagi(ji,jj,jk) + e3t(ji,jj,jk,Kaa) * p2dt *edmfa(ji,jj,jk)
!                      zdiags(ji,jj,jk) = zdiags(ji,jj,jk) + e3t(ji,jj,jk,Kaa) * p2dt *edmfc(ji,jj,jk)
!                      zdiagd(ji,jj,jk) = zdiagd(ji,jj,jk) + e3t(ji,jj,jk,Kaa) * p2dt *edmfb(ji,jj,jk)
!                   END_3D
!!gm                  CALL diag_mfc( zwi, zwd, zws, rDt, Kaa )
!!gm   SUBROUTINE diag_mfc( zdiagi, zdiagd, zdiags, p2dt, Kaa )
               !
               !! Matrix inversion from the first level
               !!----------------------------------------------------------------------
               !   solve m.x = y  where m is a tri diagonal matrix ( jpk*jpk )
               !
               !        ( zwd1 zws1   0    0    0  )( zwx1 ) ( zwy1 )
               !        ( zwi2 zwd2 zws2   0    0  )( zwx2 ) ( zwy2 )
               !        (  0   zwi3 zwd3 zws3   0  )( zwx3 )=( zwy3 )
               !        (        ...               )( ...  ) ( ...  )
               !        (  0    0    0   zwik zwdk )( zwxk ) ( zwyk )
               !
               !   m is decomposed in the product of an upper and lower triangular matrix.
               !   The 3 diagonal terms are in 3d arrays: zwd, zws, zwi.
               !   Suffices i,s and d indicate "inferior" (below diagonal), diagonal
               !   and "superior" (above diagonal) components of the tridiagonal system.
               !   The solution will be in the 4d array pta.
               !   The 3d array zwt is used as a work space array.
               !   En route to the solution pt(:,:,:,:,Kaa) is used a to evaluate the rhs and then
               !   used as a work space array: its value is modified.
               !
               DO_1Di( 0, 0 )          !* 1st recurrence:   Tk = Dk - Ik Sk-1 / Tk-1   (increasing k) ! done one for all passive tracers (so included in the IF instruction)
                  zwt(ji,1) = zwd(ji,1)
               END_1D
               DO_2Dik( 0, 0,   2, jpkm1, 1 )
                  zwt(ji,jk) = zwd(ji,jk) - zwi(ji,jk) * zws(ji,jk-1) / zwt(ji,jk-1)
               END_2D
               !
            ENDIF
            !
            IF( ln_zdfmfc ) THEN    ! add Mass Flux to the RHS
               DO_2Dik( 0, 0,   1, jpkm1, 1 )
                  pt(ji,jj,jk,jn,Krhs) = pt(ji,jj,jk,jn,Krhs) + edmftra(ji,jj,jk,jn)
               END_2D
!!gm               CALL rhs_mfc( pt(:,:,:,jn,Krhs), jn )
            ENDIF
            !
            DO_1Di( 0, 0 )             !* 2nd recurrence:    Zk = Yk - Ik / Tk-1  Zk-1
               pt(ji,jj,1,jn,Kaa) =       e3t(ji,jj,1,Kbb) * pt(ji,jj,1,jn,Kbb )    &
                  &               + rDt * e3t(ji,jj,1,Kmm) * pt(ji,jj,1,jn,Krhs)
            END_1D
            DO_2Dik( 0, 0,   2, jpkm1, 1 )
               zrhs =       e3t(ji,jj,jk,Kbb) * pt(ji,jj,jk,jn,Kbb )   &
                  & + rDt * e3t(ji,jj,jk,Kmm) * pt(ji,jj,jk,jn,Krhs)   ! zrhs=right hand side
               pt(ji,jj,jk,jn,Kaa) = zrhs - zwi(ji,jk) / zwt(ji,jk-1) * pt(ji,jj,jk-1,jn,Kaa)
            END_2D
            !
            DO_1Di( 0, 0 )             !* 3d recurrence:    Xk = (Zk - Sk Xk+1 ) / Tk   (result is the after tracer)
               pt(ji,jj,jpkm1,jn,Kaa) = pt(ji,jj,jpkm1,jn,Kaa) / zwt(ji,jpkm1) * tmask(ji,jj,jpkm1)
            END_1D
            DO_2Dik( 0, 0,   jpk-2, 1, -1 )
               pt(ji,jj,jk,jn,Kaa) = ( pt(ji,jj,jk,jn,Kaa) - zws(ji,jk) * pt(ji,jj,jk+1,jn,Kaa) )   &
                  &             / zwt(ji,jk) * tmask(ji,jj,jk)
            END_2D
            !                                         ! ================= !
         END DO                                       !    tracer loop    !
         !                                            ! ================= !
      END_1D                                          !  i-k slices loop  !
      !                                               ! ================= !
   END SUBROUTINE tra_zdf_imp

   !!==============================================================================
END MODULE trazdf
