MODULE trcstp_rk3
   !!======================================================================
   !!                       ***  MODULE trcstp_rk3  ***
   !! Time-stepping    : time loop of opa for passive tracer
   !!======================================================================
   !! History :  1.0  !  2004-03  (C. Ethe)  Original
   !!            4.1  !  2019-08  (A. Coward, D. Storkey) rewrite in preparation for new timestepping scheme
   !!            4.x  !  2021-08  (S. Techene, G. Madec) preparation and finalisation for RK3 time-stepping only
   !!----------------------------------------------------------------------
#if defined key_top
   !!----------------------------------------------------------------------
   !!   trc_stp_start : prepare  passive tracer system time-stepping
   !!   trc_stp_end   : finalise passive tracer system time-stepping
   !!----------------------------------------------------------------------
   USE par_trc        ! need jptra, number of passive tracers
   USE oce_trc        ! ocean dynamics and active tracers variables
   USE sbc_oce
   USE trc
   USE trctrp         ! passive tracers transport
   USE trcsms         ! passive tracers sources and sinks
   USE trcwri
   USE trcrst
   USE trdtrc_oce
   USE trdmxl_trc
   USE sms_pisces,  ONLY : ln_check_mass
   !
   USE prtctl         ! Print control for debbuging
   USE iom            !
   USE in_out_manager !

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_stp_start   ! called by stprk3_stg
   PUBLIC   trc_stp_end     ! called by stprk3_stg

   LOGICAL  ::   llnew                   ! ???
   LOGICAL  ::   l_trcstat               ! flag for tracer statistics
   REAL(wp) ::   rdt_sampl               ! ???
   INTEGER  ::   nb_rec_per_day, ktdcy   ! ???
   REAL(wp) ::   rsecfst, rseclast       ! ???
   REAL(wp), DIMENSION(:,:,:), SAVE, ALLOCATABLE ::   qsr_arr   ! save qsr during TOP time-step

      !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcstp.F90 14086 2020-12-04 11:37:14Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
  
   SUBROUTINE trc_stp_start( kt, Kbb, Kmm, Krhs, Kaa )
      !!-------------------------------------------------------------------
      !!                     ***  ROUTINE trc_stp_start  ***
      !!                      
      !! ** Purpose :   Prepare time loop of opa for passive tracer
      !! 
      !! ** Method  :   Compute the passive tracers trends 
      !!                Update the passive tracers
      !!                Manage restart file
      !!-------------------------------------------------------------------
      INTEGER, INTENT( in ) :: kt                  ! ocean time-step index
      INTEGER, INTENT( in ) :: Kbb, Kmm, Krhs, Kaa ! time level indices
      !
      INTEGER ::   jk, jn   ! dummy loop indices
      CHARACTER (len=25) ::   charout   !
      !!-------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_stp_start')
      !
      l_trcstat  = ( sn_cfctl%l_trcstat ) .AND. &
           &       ( ( MOD( kt, sn_cfctl%ptimincr ) == 0 ) .OR. ( kt == nitend ) )
      !
      IF( kt == nittrc000 )                      CALL trc_stpsctl   ! control 
      IF( kt == nittrc000 .AND. lk_trdmxl_trc )  CALL trd_mxl_trc_init    ! trends: Mixed-layer
      !
      IF( .NOT.ln_linssh ) THEN                                           ! update ocean volume due to ssh temporal evolution
         DO jk = 1, jpk
            cvol(:,:,jk) = e1e2t(:,:) * e3t(:,:,jk,Kmm) * tmask(:,:,jk)
         END DO
         IF( l_trcstat .OR. kt == nitrst ) areatot = glob_sum( 'trcstp', cvol(:,:,:) )
      ENDIF
      !
      IF( l_trcdm2dc )   CALL trc_mean_qsr( kt )
      !    
      IF(sn_cfctl%l_prttrc) THEN
         WRITE(charout,FMT="('kt =', I4,'  d/m/y =',I2,I2,I4)") kt, nday, nmonth, nyear
         CALL prt_ctl_info( charout, cdcomp = 'top' )
      ENDIF
      !
      CALL trc_rst_opn  ( kt )                            ! Open tracer restart file 
      IF( lrst_trc )  CALL trc_rst_cal  ( kt, 'WRITE' )   ! calendar
      !
      IF( ln_timing )   CALL timing_stop('trc_stp_start')
      !
   END SUBROUTINE trc_stp_start


   SUBROUTINE trc_stp_end( kt, Kbb, Kmm, Kaa )
      !!-------------------------------------------------------------------
      !!                     ***  ROUTINE trc_stp_end  ***
      !!                      
      !! ** Purpose :   Finalise time loop of opa for passive tracer
      !! 
      !! ** Method  :   Write restart and outputs 
      !!-------------------------------------------------------------------
      INTEGER, INTENT( in ) :: kt                  ! ocean time-step index
      INTEGER, INTENT( in ) :: Kbb, Kmm, Kaa ! time level indices
      !
      INTEGER ::   jk, jn   ! dummy loop indices
      REAL(wp)::   ztrai    ! local scalar
      CHARACTER (len=25) ::   charout   !
      !!-------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_stp_end')
      !
      !
           ! Note passive tracers have been time-filtered in trc_trp but the time level
           ! indices will not be swapped until after tra_atf/dyn_atf/ssh_atf in stp. Subsequent calls here
           ! anticipate this update which will be: Nrhs= Nbb ; Nbb = Nnn ; Nnn = Naa ; Naa = Nrhs
           ! and use the filtered levels explicitly.
           !
      IF( kt == nittrc000 ) THEN
         CALL iom_close( numrtr )                         ! close input tracer restart file
         IF(lrxios) CALL iom_context_finalize(      cr_toprst_cxt          )
         IF(lwm) CALL FLUSH( numont )                     ! flush namelist output
      ENDIF
      IF( lrst_trc )            CALL trc_rst_wri  ( kt, Kbb, Kmm, Kaa )       ! write tracer restart file
      IF( lk_trdmxl_trc  )      CALL trd_mxl_trc  ( kt,           Kaa )       ! trends: Mixed-layer
      !
      IF (l_trcstat) THEN
         ztrai = 0._wp                                    !  content of all tracers
         DO jn = 1, jptra
            ztrai = ztrai + glob_sum( 'trcstp_rk3', tr(:,:,:,jn,Kaa) * cvol(:,:,:)   ) !!st cvol@Kmm weird !!
         END DO
         IF( lwm ) WRITE(numstr,9300) kt,  ztrai / areatot
      ENDIF
      !
9300  FORMAT(i10,D23.16)
      !
      CALL trc_wri      ( kt,      Kaa            )       ! output of passive tracers with iom I/O manager before time level swap 
      !
      IF( ln_timing )   CALL timing_stop('trc_stp_end')
      !
   END SUBROUTINE trc_stp_end


   SUBROUTINE trc_mean_qsr( kt )
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE trc_mean_qsr  ***
      !!
      !! ** Purpose :  Compute daily mean qsr for biogeochemical model in case
      !!               of diurnal cycle
      !!
      !! ** Method  : store in TOP the qsr every hour ( or every time-step if the latter 
      !!              is greater than 1 hour ) and then, compute the  mean with 
      !!              a moving average over 24 hours. 
      !!              In coupled mode, the sampling is done at every coupling frequency 
      !!----------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt   ! ocean time-step index
      !
      INTEGER  ::   ji,jj,jn   ! dummy loop indices
      REAL(wp) ::   zkt, zrec     ! local scalars
      CHARACTER(len=1) ::   cl1   ! 1 character
      CHARACTER(len=2) ::   cl2   ! 2 characters
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_mean_qsr')
      !
      IF( kt == nittrc000 ) THEN
         !
         rdt_sampl = REAL( ncpl_qsr_freq )
         nb_rec_per_day = INT( rday / ncpl_qsr_freq )
         !
         IF(lwp) THEN
            WRITE(numout,*) 
            WRITE(numout,*) ' Sampling frequency dt = ', rdt_sampl, 's','   Number of sampling per day  nrec = ', nb_rec_per_day
            WRITE(numout,*) 
         ENDIF
         !
         ALLOCATE( qsr_arr(A2D(0),nb_rec_per_day ) )
         !
         !                                            !* Restart: read in restart file
         IF( ln_rsttr .AND. nn_rsttr /= 0 .AND. iom_varid( numrtr, 'qsr_mean' , ldstop = .FALSE. ) > 0  &
           &                              .AND. iom_varid( numrtr, 'qsr_arr_1', ldstop = .FALSE. ) > 0  &
           &                              .AND. iom_varid( numrtr, 'ktdcy'    , ldstop = .FALSE. ) > 0  &
           &                              .AND. iom_varid( numrtr, 'nrdcy'    , ldstop = .FALSE. ) > 0  ) THEN
            CALL iom_get( numrtr, 'ktdcy', zkt )  
            rsecfst = INT( zkt ) * rn_Dt
            IF(lwp) WRITE(numout,*) 'trc_qsr_mean:   qsr_mean read in the restart file at time-step rsecfst =', rsecfst, ' s '
            CALL iom_get( numrtr, jpdom_auto, 'qsr_mean', qsr_mean )   !  A mean of qsr
            CALL iom_get( numrtr, 'nrdcy', zrec )   !  Number of record per days
            IF( INT( zrec ) == nb_rec_per_day ) THEN
               DO jn = 1, nb_rec_per_day 
                  IF( jn <= 9 )  THEN
                    WRITE(cl1,'(i1)') jn
                    CALL iom_get( numrtr, jpdom_auto, 'qsr_arr_'//cl1, qsr_arr(:,:,jn) )   !  A mean of qsr
                  ELSE
                    WRITE(cl2,'(i2.2)') jn
                    CALL iom_get( numrtr, jpdom_auto, 'qsr_arr_'//cl2, qsr_arr(:,:,jn) )   !  A mean of qsr
                  ENDIF
              END DO
            ELSE
               DO jn = 1, nb_rec_per_day
                  qsr_arr(:,:,jn) = qsr_mean(:,:)
               END DO
            ENDIF
         ELSE                                         !* no restart: set from nit000 values
            IF(lwp) WRITE(numout,*) 'trc_qsr_mean:   qsr_mean set to nit000 values'
            rsecfst  = kt * rn_Dt
            !
            DO_2D( 0, 0, 0, 0 )
               qsr_mean(ji,jj) = qsr(ji,jj)
            END_2D
            DO jn = 1, nb_rec_per_day
               qsr_arr(:,:,jn) = qsr_mean(:,:)
            END DO
         ENDIF
         !
      ENDIF
      !
      rseclast = kt * rn_Dt
      !
      llnew   = ( rseclast - rsecfst ) >=  rdt_sampl    !   new shortwave to store
      IF( llnew ) THEN
          ktdcy = kt
          IF( lwp .AND. kt < nittrc000 + 100 ) WRITE(numout,*) ' New shortwave to sample for TOP at time kt = ', ktdcy, &
             &                      ' time = ', rseclast/3600.,'hours '
          rsecfst = rseclast
          DO jn = 1, nb_rec_per_day - 1
             qsr_arr(:,:,jn) = qsr_arr(:,:,jn+1)
          END DO
          qsr_arr (:,:,nb_rec_per_day) = qsr(:,:)
          qsr_mean(:,:) = SUM( qsr_arr(:,:,:), 3 ) / nb_rec_per_day
      ENDIF
      !
      IF( lrst_trc ) THEN    !* Write the mean of qsr in restart file 
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'trc_mean_qsr : write qsr_mean in restart file  kt =', kt
         IF(lwp) WRITE(numout,*) '~~~~~~~'
         zkt  = REAL( ktdcy, wp )
         zrec = REAL( nb_rec_per_day, wp )
         CALL iom_rstput( kt, nitrst, numrtw, 'ktdcy', zkt  )
         CALL iom_rstput( kt, nitrst, numrtw, 'nrdcy', zrec )
          DO jn = 1, nb_rec_per_day 
             IF( jn <= 9 )  THEN
               WRITE(cl1,'(i1)') jn
               CALL iom_rstput( kt, nitrst, numrtw, 'qsr_arr_'//cl1, qsr_arr(:,:,jn) )
             ELSE
               WRITE(cl2,'(i2.2)') jn
               CALL iom_rstput( kt, nitrst, numrtw, 'qsr_arr_'//cl2, qsr_arr(:,:,jn) )
             ENDIF
         END DO
         CALL iom_rstput( kt, nitrst, numrtw, 'qsr_mean', qsr_mean(:,:) )
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('trc_mean_qsr')
      !
   END SUBROUTINE trc_mean_qsr

#else
   !!----------------------------------------------------------------------
   !!   Default key                                     NO passive tracers
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_stp( kt )        ! Empty routine
      WRITE(*,*) 'trc_stp: You should not have seen this print! error?', kt
   END SUBROUTINE trc_stp
#endif

   !!======================================================================
END MODULE trcstp_rk3
