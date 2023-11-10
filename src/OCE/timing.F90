MODULE timing
   !!========================================================================
   !!                     ***  MODULE  timing  ***
   !!========================================================================
   !! History : 4.0  ! 2001-05  (R. Benshila)
   !!           4.x  ! 2023-05  (G. Irrmann, S. Masson)
   !!------------------------------------------------------------------------

   !!------------------------------------------------------------------------
   !!   timing_open     : open timing.output file
   !!   timing_start    : start Timer
   !!   timing_stop     : stop  Timer
   !!   timing_finalize : compute stats and write timing.output
   !!------------------------------------------------------------------------
   USE par_kind, ONLY: dp
   USE par_oce , ONLY: ntile
#if defined key_agrif
   USE dom_oce , ONLY: l_istiled
#else
   USE dom_oce , ONLY: l_istiled, Agrif_Root, Agrif_CFixed
#endif
   !! WARNING: we cannot use lib_mpp because of circular dependencies
   
#if ! defined key_mpi_off
   USE MPI
#endif

   IMPLICIT NONE
   PRIVATE

   PUBLIC   timing_start, timing_stop, timing_open      ! called in each routine to time
   
#if ! defined key_agrif
   ! Variables for fine grain timing
   TYPE timer
      CHARACTER(LEN=32)  :: cname
      INTEGER(8) :: n8start, n8tnet, n8tfull, n8childsum
      INTEGER :: niter
      LOGICAL :: ldone, lstatplot
      REAL(dp) ::  tnet , tnetavg,  tnetmin,  tnetmax,  tnetblc   ! net  time average, min, max and load balance (max-min)
      REAL(dp) :: tfull, tfullavg, tfullmin, tfullmax, tfullblc   ! full time average, min, max and load balance (max-min)
      TYPE(timer), POINTER :: s_next, s_prev, s_parent
   END TYPE timer

   TYPE(timer), POINTER :: s_timer_root => NULL()
   TYPE(timer), POINTER :: s_timer      => NULL()

   INTEGER    :: numtime         =   -1      !: logical unit for timing
   INTEGER    :: ncall_clock
   INTEGER    :: nmpicom   !: we cannot use mpi_comm_oce as we cannot use lib_mpp
   INTEGER(8) :: n8start000
   REAL(dp)   :: secondclock

   INTEGER :: jp_cname = 1
   INTEGER :: jp_tnet  = 2   ! local net time (of the current MPI process)
   INTEGER :: jp_tavg  = 3   ! mean net time (among MPI processes)
   INTEGER :: jp_tmin  = 4   ! min net time (among MPI processes)
   INTEGER :: jp_tmax  = 5   ! max net time (among MPI processes)
   INTEGER :: jp_tblc  = 6   ! load balance between MPI processes (max-min)

   INTEGER :: jpmaxline = 50   !: max number of line to be printed

   INTEGER, DIMENSION(8)           :: nvalues
   CHARACTER(LEN= 8), DIMENSION(2) :: cdate
   CHARACTER(LEN=10), DIMENSION(2) :: ctime
   CHARACTER(LEN= 5)               :: czone
#else
   INTEGER    :: numtime         =   -1      !: logical unit for timing
   INTEGER    :: nmpicom   !: we cannot use mpi_comm_oce as we cannot use lib_mpp
#endif

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: timing.F90 14834 2021-05-11 09:24:44Z hadcv $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE timing_open( ldwp, kmpicom, cdname )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE timing_open  ***
      !! ** Purpose :   Open timing output file.
      !!----------------------------------------------------------------------
      LOGICAL                   , INTENT(in) :: ldwp
      INTEGER                   , INTENT(in) :: kmpicom
      CHARACTER(len=*), OPTIONAL, INTENT(in) :: cdname
      !
      CHARACTER(len=32) ::   cln
      CHARACTER(LEN=10) ::   clfmt
      INTEGER           ::   idg, irank, isize, icode
      !!----------------------------------------------------------------------

      IF( .NOT. Agrif_Root() ) RETURN

#if ! defined key_mpi_off
      nmpicom = kmpicom
      CALL MPI_COMM_RANK( nmpicom, irank, icode )
      CALL MPI_COMM_SIZE( nmpicom, isize, icode )
#else
      nmpicom = -1
      irank = 0
      isize = 1
#endif

      IF( PRESENT(cdname) ) THEN   ;   cln = cdname
      ELSE                         ;   cln = 'timing.output'
      ENDIF

      IF( ldwp ) THEN
         ! we cannot use ctl_open as we cannot use lib_mpp
         IF( irank > 0 ) THEN
            idg = MAX( INT(LOG10(REAL(MAX(1,isize)))) + 1, 4 )      ! how many digits to we need to write? min=4, max=9
            WRITE(clfmt, "('(a,a,i', i1, '.', i1, ')')") idg, idg   ! '(a,a,ix.x)'
            WRITE(cln, clfmt) TRIM(cln), '_', irank
         ENDIF
         OPEN(NEWUNIT = numtime, FILE = TRIM(cln), STATUS = "REPLACE", ACTION = "write")
         WRITE(numtime,*)
         WRITE(numtime,*) '      CNRS - NERC - Met OFFICE - MERCATOR-ocean - CMCC'
         WRITE(numtime,*) '                             NEMO team'
         WRITE(numtime,*) '                  Ocean General Circulation Model'
         WRITE(numtime,*) '                        version 4.x  (2023) '
         WRITE(numtime,*)
#if ! defined key_agrif
         WRITE(numtime,*) '                        Timing Informations '
         CALL write_separator()
#else
         WRITE(numtime,*) '              The timing is not yet working with AGRIF'
         WRITE(numtime,*) '           because the conv is not abble to conv timing.F90'
         CLOSE(numtime)
#endif
      ELSE
         numtime = -1
      ENDIF
      !
   END SUBROUTINE timing_open


#if ! defined key_agrif
   SUBROUTINE timing_start( cdinfo, ldstatplot )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE timing_start  ***
      !! ** Purpose :   collect execution time
      !!----------------------------------------------------------------------
      CHARACTER(len=*) , INTENT(in) :: cdinfo
      LOGICAL, OPTIONAL, INTENT(in) :: ldstatplot   ! .true. if you want to call gnuplot analyses on this timing
      !
      CHARACTER(LEN=32)  :: clinfo
      TYPE(timer), POINTER :: s_wrk
      INTEGER(8) :: i8rate
      !!----------------------------------------------------------------------
      !
      clinfo = cdinfo
      IF( .NOT. Agrif_Root() )   clinfo = TRIM(Agrif_CFixed())//'_'//clinfo
      
      IF( .NOT. ASSOCIATED(s_timer_root) ) THEN              ! this is the first call to timing_start
         s_timer_root => def_newlink( clinfo, ldstatplot )   ! define the root link
         CALL SYSTEM_CLOCK( COUNT_RATE = i8rate )            ! define rateclock
         secondclock = 1._dp / REAL(i8rate, dp)
         CALL DATE_AND_TIME( cdate(1), ctime(1), czone, nvalues )
         ncall_clock = 0
      ENDIF
      
      ! store s_timer chain link in s_wrk
      s_wrk => s_timer
      !
      ! make s_timer pointing toward the chain link corresponding to clinfo
      s_timer => find_link( clinfo, s_timer_root, ldstatplot )

      ! we must take care if we do a timing inside another timing...
      ! if s_wrk did not finish is timing, this means that s_timer in part of s_wrk.
      ! in this case we link s_timer to s_wrk
      s_timer%s_parent => NULL()             ! default not the part of another timing
      IF( ASSOCIATED(s_wrk) ) THEN
         IF( .NOT. s_wrk%ldone )   s_timer%s_parent => s_wrk
      ENDIF

      ! initialisation
      s_timer%ldone = .FALSE.                                                   ! we are just starting the timing (not done)
      s_timer%n8childsum = 0_8                                                  ! not yet any my children count

      ! clock time collection
      CALL SYSTEM_CLOCK( COUNT = s_timer%n8start )   ;   ncall_clock = ncall_clock + 1
      IF( ncall_clock == 1 )   n8start000 =  s_timer%n8start   ! keep track of the first call
      
   END SUBROUTINE timing_start


   SUBROUTINE timing_stop( cdinfo, kt, ld_finalize )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE timing_stop  ***
      !! ** Purpose :   stop timing window
      !!----------------------------------------------------------------------
      CHARACTER(len=*) , INTENT(in) ::   cdinfo
      INTEGER, OPTIONAL, INTENT(in) ::   kt
      LOGICAL, OPTIONAL, INTENT(in) ::   ld_finalize
      !
      CHARACTER(LEN=32)  :: clinfo
      INTEGER(8) :: i8end, i8full, i8net
      INTEGER    :: icode
      LOGICAL    :: ll_finalize
      !!----------------------------------------------------------------------
      !
      clinfo = cdinfo
      IF( .NOT. Agrif_Root() )   clinfo = TRIM(Agrif_CFixed())//'_'//clinfo

      IF( s_timer%cname /= clinfo ) THEN   ! we cannot use ctl_stop as we cannot use lib_mpp
         WRITE(*,*) ' STOP from timing_stop: try to stop '//TRIM(clinfo)//' but we point toward '//TRIM(s_timer%cname)
#if ! defined key_mpi_off
         CALL mpi_abort( MPI_COMM_WORLD, 123, icode )
#else
         STOP 123
#endif
      ENDIF
      IF( PRESENT(ld_finalize) ) THEN   ;   ll_finalize = ld_finalize
      ELSE                              ;   ll_finalize = .FALSE.
      ENDIF

      IF( .NOT. l_istiled .OR. ntile == 1 ) s_timer%niter = s_timer%niter + 1   ! All tiles count as one iteration

      ! clock time collection
      CALL SYSTEM_CLOCK( COUNT = i8end )   ;   ncall_clock = ncall_clock + 1
      i8full = i8end - s_timer%n8start  ! count between  timing_start and timing_stop
      s_timer%n8tfull = s_timer%n8tfull + i8full   ! cumulate my full time

      ! time print
      IF( s_timer%lstatplot .AND. numtime /= -1 )   &
         &   WRITE(numtime,*) 'timing '//TRIM(clinfo)//' ', kt, ' : ', REAL(i8full,dp) * secondclock

      ! time diagnostics
      i8net = i8full - s_timer%n8childsum       ! don't take into account my cildren count
      s_timer%n8tnet = s_timer%n8tnet + i8net   ! cumulate my net time
      s_timer%ldone  = .TRUE.                   ! I am done with this counting
        
      ! we come back to the parent
      s_timer => s_timer%s_parent
      IF ( ASSOCIATED(s_timer) )   s_timer%n8childsum = s_timer%n8childsum + i8full   ! add myself to the children of my parents

      IF( ll_finalize ) CALL timing_finalize( s_timer_root )
      
   END SUBROUTINE timing_stop


   SUBROUTINE timing_finalize( sd_root )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE timing_finalize ***
      !! ** Purpose :  compute average time
      !!               write timing output file
      !!----------------------------------------------------------------------
      TYPE(timer), POINTER, INTENT(inout) :: sd_root      ! root chain link of the chain
      !
      TYPE(timer), POINTER :: s_wrk
      REAL(dp) :: zmytime, zval, zavgtime, zavgextra, zmin, zmax, zblc, zsum
      REAL(dp), DIMENSION(:), ALLOCATABLE :: zalltime
      INTEGER :: idum, isize, icode
      INTEGER :: jpnbtest = 100
      INTEGER :: ji
      INTEGER, DIMENSION(:), ALLOCATABLE :: iallmpi
      INTEGER(8) :: i8start, i8end
      CHARACTER(len=128) :: clfmt
      LOGICAL :: ll_avg, llwrt
      !!----------------------------------------------------------------------

      llwrt = numtime /= -1

      ! check that all timing are done (all childrend count are properly reported)
      s_wrk => sd_root
      DO WHILE( ASSOCIATED(s_wrk) )
         IF( .NOT. s_wrk%ldone ) THEN   ! we cannot use ctl_stop as we cannot use lib_mpp
            WRITE(*,*) 'STOP from timing_finalize: '//TRIM(s_wrk%cname)//'did not finish its timing'
#if ! defined key_mpi_off
            CALL mpi_abort( MPI_COMM_WORLD, 123, icode )
#else
            STOP 123
#endif
         ENDIF
         s_wrk => s_wrk%s_next
      END DO

      ! get elapsed time between now and the first call to timing start
      CALL SYSTEM_CLOCK( COUNT = i8end )
      zmytime = REAL(i8end - n8start000, dp) * secondclock

      ! End of timings on date & time
      CALL DATE_AND_TIME( cdate(2), ctime(2), czone, nvalues )      
      !
      IF( llwrt ) THEN
         CALL write_header('Timing report:')
         clfmt='(1X,"Timing started on ",2(A2,"/"),A4," at ",2(A2,":"),A2," MET ",A3,":",A2," from GMT")'
         WRITE(numtime, clfmt) &
            &       cdate(1)(7:8), cdate(1)(5:6), cdate(1)(1:4),   &
            &       ctime(1)(1:2), ctime(1)(3:4), ctime(1)(5:6),   &
            &       czone(1:3),    czone(4:5)
         clfmt='(1X,"Timing   ended on ",2(A2,"/"),A4," at ",2(A2,":"),A2," MET ",A3,":",A2," from GMT")'
         WRITE(numtime, clfmt) &
            &       cdate(2)(7:8), cdate(2)(5:6), cdate(2)(1:4),   &
            &       ctime(2)(1:2), ctime(2)(3:4), ctime(2)(5:6),   &
            &       czone(1:3),    czone(4:5)
      ENDIF

      ! call gnuplot statplot
      IF( llwrt ) THEN
         s_wrk => sd_root
         DO WHILE( ASSOCIATED(s_wrk) )
            IF( s_wrk%lstatplot ) CALL gnuplot_statplot( 'timing', s_wrk%cname ) 
            s_wrk => s_wrk%s_next
         END DO
      ENDIF

#if ! defined key_mpi_off
      ! Compute the number of routines
      idum = 0
      s_wrk => sd_root
      DO WHILE( ASSOCIATED(s_wrk) )
         idum = idum + 1
         s_wrk => s_wrk%s_next
      END DO
      CALL MPI_COMM_SIZE( nmpicom, isize, icode )
      ALLOCATE(iallmpi(isize))
      CALL MPI_ALLGATHER(   idum , 1, MPI_INTEGER,   &
         &                iallmpi, 1, MPI_INTEGER, nmpicom, icode)
      IF( SUM( iallmpi ) /= idum*isize ) THEN
         IF( llwrt ) THEN
            CALL write_separator()
            WRITE(numtime,*) '        ===> W A R N I N G: '
            WRITE(numtime,*) ' Some CPU have different number of routines instrumented for timing'
            WRITE(numtime,*) ' No detailed report on averaged timing can be provided'
            WRITE(numtime,*) ' The following detailed report only deals with the current processor'
         ENDIF
         ll_avg = .FALSE.
      ELSE
         ll_avg = .TRUE.
      ENDIF
      DEALLOCATE(iallmpi)
#else
      ll_avg = .FALSE.
#endif      

      ! get largest elapsed time
      CALL mpp_avgminmax( zmytime, zavgtime, zmin, zmax, zblc, ll_avg )
      IF( llwrt ) THEN
         CALL write_header('Elapsed Time mesured by timing (s):')
         WRITE(clfmt, "('(a,f',i2.2,'.6,''s'')')") INT(LOG10(MAX(1._dp,zmax))) + 8
         IF(ll_avg) THEN
            WRITE(numtime,clfmt) '       avg over all MPI processes = ', zavgtime
            WRITE(numtime,clfmt) '       min over all MPI processes = ', zmin
            WRITE(numtime,clfmt) '       max over all MPI processes = ', zmax 
            WRITE(numtime,clfmt) '              local MPI process   = ', zmytime
         ELSE
            WRITE(numtime,clfmt) '                    local process = ', zmytime
         ENDIF
      ENDIF

      ! add an aveluation of the timing itself...
      CALL SYSTEM_CLOCK( COUNT = i8start ) 
      DO ji = 1, jpnbtest
         CALL SYSTEM_CLOCK( COUNT = i8end )
      ENDDO
      zval = REAL( (i8end-i8start) * ncall_clock, dp) * secondclock / REAL(jpnbtest, dp)      
      CALL mpp_avgminmax( zval, zavgextra, zmin, zmax, zblc, ll_avg )
      IF( llwrt ) THEN
         CALL write_header('Evaluation of the extra coast due to the timing itself (% of avg elapsed):')
         WRITE(numtime,*) '   Number calls to SYSTEM_CLOCK = ', ncall_clock
         WRITE(numtime,'(a,i3,a)') '    Avg Estimation over ', jpnbtest,' tests'
         WRITE(clfmt, "('(a,f',i2.2,'.6,''s ('', f6.3,''%)'')')") INT(LOG10(MAX(1._dp,zmax))) + 8
         IF(ll_avg) THEN
            WRITE(numtime,clfmt) '       avg over all MPI processes = ', zavgextra, zavgextra / zavgtime * 100._dp 
            WRITE(numtime,clfmt) '       min over all MPI processes = ', zmin, zmin / zavgtime * 100._dp
            WRITE(numtime,clfmt) '       max over all MPI processes = ', zmax, zmax / zavgtime * 100._dp 
            WRITE(numtime,clfmt) '              local MPI process   = ', zval, zval / zavgtime * 100._dp
         ELSE
            WRITE(numtime,clfmt) '                    local process = ', zval, zval / zavgtime * 100._dp
         ENDIF
      ENDIF

      ! reorder the chain list accprding to cname to make sure that each MPI process has the chain links in the same order
      CALL sort_chain( sd_root, jp_cname )   ! I think it is not necessary... 
      
      s_wrk => sd_root
      DO WHILE ( ASSOCIATED(s_wrk) )
         s_wrk%tnet  = REAL(s_wrk%n8tnet , dp) * secondclock
         s_wrk%tfull = REAL(s_wrk%n8tfull, dp) * secondclock
         CALL mpp_avgminmax( s_wrk%tnet , s_wrk%tnetavg , s_wrk%tnetmin , s_wrk%tnetmax , s_wrk%tnetblc , ll_avg, s_wrk%cname )
         CALL mpp_avgminmax( s_wrk%tfull, s_wrk%tfullavg, s_wrk%tfullmin, s_wrk%tfullmax, s_wrk%tfullblc, ll_avg )
         s_wrk => s_wrk%s_next
      END DO

      IF( ll_avg .AND. llwrt ) THEN
         CALL timer_write( 'Timing : AVG values over all MPI processes:', sd_root, jp_tavg, zavgtime, zavgextra )
         zsum = 0._dp
         s_wrk => sd_root
         DO WHILE ( ASSOCIATED(s_wrk) )
            zsum = zsum + s_wrk%tnetblc
            s_wrk => s_wrk%s_next
         END DO
         CALL timer_write( 'Timing : Load unbalance over all MPI processes (max-min) :', sd_root, jp_tblc, zsum, 0. )
         CALL timer_write( 'Timing : MIN values over all MPI processes:', sd_root, jp_tmin, zavgtime, zavgextra )
         CALL timer_write( 'Timing : MAX values over all MPI processes:', sd_root, jp_tmax, zavgtime, zavgextra )
      ENDIF
      IF( llwrt ) THEN
         CALL timer_write( 'Timing : values for local MPI process:', sd_root, jp_tnet, zmytime, zavgextra )
      ENDIF

      IF( llwrt ) CLOSE(numtime)
      !
   END SUBROUTINE timing_finalize


   SUBROUTINE timer_write( cdinfo, sd_root, kswitch, ptimetot, ptextra )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE wcurrent_info ***
      !! ** Purpose :  compute and write timing output file
      !!----------------------------------------------------------------------
      CHARACTER(len=*), INTENT(in) :: cdinfo
      TYPE(timer), POINTER, INTENT(inout) :: sd_root      ! root chain link of the chain
      INTEGER,              INTENT(in   ) :: kswitch      ! select the variable to be printed
      REAL(dp),             INTENT(in   ) :: ptimetot     ! total elapsed time
      REAL(dp),             INTENT(in   ) :: ptextra      ! extrat total elapsed time from the calls to system_clock
      ! 
      TYPE(timer), POINTER :: s_wrk
      REAL(dp) :: ztnet, ztfull, zpcent
      INTEGER  :: icnt
      INTEGER  :: idgnet, idgfull, idgpct
      INTEGER  :: itot0, itot1, itot2, itot3
      INTEGER  :: iblk1, iblk2
      CHARACTER(len= 32) :: cltitle0, cltitle1, cltitle2, cltitle3
      CHARACTER(len=128) :: clfmt, clflt1, clflt2
      LOGICAL :: llwarning
      !!----------------------------------------------------------------------

      llwarning = .FALSE.
      zpcent = 100._dp / ptimetot
      
      CALL sort_chain( sd_root, kswitch )   ! reorder the current list by decreassing values of kswitch

      IF(     kswitch == jp_tnet ) THEN   ;   ztnet = sd_root%tnet
      ELSEIF( kswitch == jp_tavg ) THEN   ;   ztnet = sd_root%tnetavg
      ELSEIF( kswitch == jp_tmin ) THEN   ;   ztnet = sd_root%tnetmin
      ELSEIF( kswitch == jp_tmax ) THEN   ;   ztnet = sd_root%tnetmax
      ELSEIF( kswitch == jp_tblc ) THEN   ;   ztnet = sd_root%tnetblc
      ENDIF
      idgnet  = INT(LOG10(MAX(1._dp,ztnet   ))) + 1 + 7   ! how many digits to we need to write? add 7 difgits for '.6'
      idgfull = INT(LOG10(MAX(1._dp,ptimetot))) + 1 + 7   ! how many digits to we need to write? add 7 difgits for '.6'
      idgpct  =                                   3 + 4   ! add 4 digits for '.3'

      cltitle0 = 'Section'
      cltitle1 = 'Net elapsed Time'
      cltitle2 = 'Full elapsed Time'
      cltitle3 = 'Frequency'

      itot0 = MAX(LEN_TRIM(cltitle0),               12)
      itot1 = MAX(LEN_TRIM(cltitle1), idgnet +idgpct+6)   ;   iblk1 = itot1 - (idgnet +idgpct+6) + 1   ! add 6 for 's ( %)'
      itot2 = MAX(LEN_TRIM(cltitle1), idgfull+idgpct+6)   ;   iblk2 = itot2 - (idgfull+idgpct+6) + 1   ! add 6 for 's ( %)'
      itot3 = MAX(LEN_TRIM(cltitle3),                9) 
      
      ! write current info
      CALL write_header(cdinfo)
      WRITE(clfmt, '(a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)')  &
         &         '(1x,a', itot0, ",' | ',a", itot1, ",' | ',a", itot2, ",' | ',a", itot3, ')'
       WRITE(numtime,clfmt) cltitle0, cltitle1, cltitle2, cltitle3
      
      WRITE(clfmt, "('(1x,',i2,a,i2,a,i2,a,i2,a)")   &
         &         itot0+1, "('-'),'|',", itot1+2, "('-'),'|',", itot2+2, "('-'),'|',", itot3+1,"('-'))"
      WRITE(numtime,clfmt)   

      WRITE(clflt1, "('f',i2.2,'.6,',a,',f',i2.2,'.3,',a)") idgnet , "'s ('", idgpct, "' %) |',"    ! "fx.6,'s (',fx.3,' %) |',"
      WRITE(clflt2, "('f',i2.2,'.6,',a,',f',i2.2,'.3,',a)") idgfull, "'s ('", idgpct, "' %) |',i"   ! "fx.6,'s (',fx.3,' %) | ,i'"
      WRITE(clfmt, "('(1x,a',i2.2,a,i2,'x,',a,i2,'x,',a,i2.2,')')")   &
         &         itot0, ",' |',", iblk1, TRIM(clflt1), iblk2, TRIM(clflt2), itot3+1

      icnt = 0
      s_wrk => sd_root
      DO WHILE ( ASSOCIATED(s_wrk) .AND. icnt < jpmaxline )   ! we print only the first imaxline lines
         IF(     kswitch == jp_tnet ) THEN   ;   ztnet = s_wrk%tnet      ;   ztfull = s_wrk%tfull
         ELSEIF( kswitch == jp_tavg ) THEN   ;   ztnet = s_wrk%tnetavg   ;   ztfull = s_wrk%tfullavg
         ELSEIF( kswitch == jp_tmin ) THEN   ;   ztnet = s_wrk%tnetmin   ;   ztfull = s_wrk%tfullmin
         ELSEIF( kswitch == jp_tmax ) THEN   ;   ztnet = s_wrk%tnetmax   ;   ztfull = s_wrk%tfullmax
         ELSEIF( kswitch == jp_tblc ) THEN   ;   ztnet = s_wrk%tnetblc   ;   ztfull = s_wrk%tfullblc
         ENDIF
         IF( ztnet < ptextra .AND. .NOT. llwarning ) THEN
            WRITE(clflt1, "('(a,f',i2.2,'.6,''s'')')")   INT(LOG10(MAX(1._dp,ptextra)))+8   ! "(a,fx.6,'s')"
            WRITE(numtime,clflt1) 'WARNING: timings bellow are smaller than the estimation of the timing itself: ', ptextra
            llwarning = .TRUE.
         ENDIF
         WRITE(numtime,clfmt) s_wrk%cname, ztnet, ztnet*zpcent, ztfull, ztfull*zpcent, s_wrk%niter
         s_wrk => s_wrk%s_next
         icnt = icnt + 1
      END DO

      IF(  ASSOCIATED(s_wrk) )   WRITE(numtime,*) '...'   ! show that there is still more lines that could have been printed
     !
   END SUBROUTINE timer_write


   FUNCTION find_link( cdinfo, sd_root, ldstatplot )   RESULT( ptr )
      !!----------------------------------------------------------------------
      !!               ***  FUNCTION find_link  ***
      !! ** Purpose :   find the link named cdinfo in the chain link starting with the link sd_root
      !!----------------------------------------------------------------------
      CHARACTER(len=*)    , INTENT(in   ) :: cdinfo
      TYPE(timer), POINTER, INTENT(inout) :: sd_root      ! root chain link of the chain
      LOGICAL, OPTIONAL   , INTENT(in   ) :: ldstatplot   ! .true. if you want to call gnuplot analyses on this timing
      ! 
      TYPE(timer), POINTER :: ptr, s_wrk
      !!----------------------------------------------------------------------

      ! case of already existing area (typically inside a loop)
      ptr => sd_root
      DO WHILE( ASSOCIATED(ptr) )
         IF( ptr%cname == cdinfo )   RETURN   ! cdinfo is already in the chain
         s_wrk => ptr                         ! store ptr in s_wrk
         ptr   => ptr%s_next
      END DO

      ! cdinfo not found, we earch the end of the chain list -> ptr is NULL() 
      ptr => s_wrk   ! go back to the last chain link.

      ! we are at the end of the chain -> add a new chain link
      ptr%s_next => def_newlink( cdinfo, ldstatplot )   ! define a new chain link and link it to the end of the chain
      ptr%s_next%s_prev => ptr                          ! link the new chain link to the current chain link
      ptr => ptr%s_next                                 ! move the current chain link to this new chain link
      !
   END FUNCTION find_link

   
   FUNCTION def_newlink( cdinfo, ldstatplot )   RESULT( ptr )
      !!----------------------------------------------------------------------
      !!               ***  FUNCTION def_newlink  ***
      !! ** Purpose :   add a new link to the chain link
      !!----------------------------------------------------------------------
      CHARACTER(len=*) , INTENT(in) :: cdinfo
      LOGICAL, OPTIONAL, INTENT(in) :: ldstatplot
      !
      TYPE(timer), POINTER :: ptr
      LOGICAL :: ll_statplot
      !!----------------------------------------------------------------------

      IF( PRESENT(ldstatplot) ) THEN   ;   ll_statplot = ldstatplot
      ELSE                             ;   ll_statplot = .FALSE.
      ENDIF
      
      ALLOCATE(ptr)   ! allocate memory space associated with ptr
      ! default required definitions
      ptr%cname     = cdinfo
      ptr%n8tnet    = 0_8
      ptr%n8tfull   = 0_8
      ptr%niter     = 0
      ptr%lstatplot = ll_statplot
      ptr%s_parent  => NULL()
      ptr%s_prev    => NULL()
      ptr%s_next    => NULL()
      
   END FUNCTION def_newlink


   SUBROUTINE switch_links(sd_root, sd_current)
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE switch_links  ***
      !! ** Purpose :   switch current and current%s_next chain links
      !!                switch_links is called only if current%s_next is associated 
      !!----------------------------------------------------------------------
      TYPE(timer), POINTER, INTENT(inout) :: sd_root      ! root chain link of the chain
      TYPE(timer), POINTER, INTENT(inout) :: sd_current   ! current chain link
      !!----------------------------------------------------------------------
      !
      ! initial setup: current%prev <-> current <-> current%next <-> current%next%next
      ! --------------
      !
      !                      current
      !                         2
      !        current%prev 1       4 current%next%next 
      !                         3
      !                    current%next
      !
      ! after switch: current%prev%prev <-> current%prev <-> current <-> current%next
      ! ------------
      !                      current
      !                         3
      !   current%prev%prev 1       4 current%next
      !                         2
      !                   current*prev
      !
      ! link current%s_prev and current%s_next
      IF( ASSOCIATED( sd_current%s_prev ) ) THEN
         sd_current%s_prev%s_next => sd_current%s_next   ! link forward current%s_prev to current%s_next
         sd_current%s_next%s_prev => sd_current%s_prev   ! link backward current%s_next to current%s_prev
      ELSE
         sd_current%s_next%s_prev => NULL()              ! current%next becomes the new root of the chain
         sd_root => sd_current%s_next
      ENDIF
         
      sd_current%s_prev => sd_current%s_next             ! link backward current to its new prev (former next)
      
      ! cannot yet modify sd_current%s_prev%s_next (which is also sd_current%s_next%s_next) as
      IF( ASSOCIATED( sd_current%s_next%s_next ) ) THEN
         sd_current%s_next => sd_current%s_next%s_next   ! link forward current to its new next (former next%next)
         sd_current%s_next%s_prev => sd_current          ! link backward new current next to current
      ELSE
         sd_current%s_next => NULL()   ! current is now at the end of the chain
      ENDIF
      ! we can now update sd_current%s_prev%s_next
      sd_current%s_prev%s_next => sd_current   ! link new current prev to current
            !
   END SUBROUTINE switch_links


   SUBROUTINE sort_chain( sd_root, kswitch )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE sort_chain  ***
      !! ** Purpose :   sort the chain link
      !!----------------------------------------------------------------------
      TYPE(timer), POINTER, INTENT(inout) :: sd_root      ! root chain link of the chain
      INTEGER,              INTENT(in   ) :: kswitch      ! select the sorting variable
      !
      TYPE(timer), POINTER :: s_wrk
      LOGICAL              :: ll_ord, ll_test
      !!----------------------------------------------------------------------
      
      ll_ord = .FALSE.
      DO WHILE ( .NOT. ll_ord )
         ll_ord = .TRUE.
         s_wrk => sd_root
         DO WHILE ( ASSOCIATED( s_wrk%s_next ) )
            IF(     kswitch == jp_cname ) THEN   ;   ll_test = s_wrk%cname    < s_wrk%s_next%cname
            ELSEIF( kswitch == jp_tnet  ) THEN   ;   ll_test = s_wrk%tnet     < s_wrk%s_next%tnet
            ELSEIF( kswitch == jp_tavg  ) THEN   ;   ll_test = s_wrk%tnetavg  < s_wrk%s_next%tnetavg
            ELSEIF( kswitch == jp_tmin  ) THEN   ;   ll_test = s_wrk%tnetmin  < s_wrk%s_next%tnetmin
            ELSEIF( kswitch == jp_tmax  ) THEN   ;   ll_test = s_wrk%tnetmax  < s_wrk%s_next%tnetmax
            ELSEIF( kswitch == jp_tblc  ) THEN   ;   ll_test = s_wrk%tnetblc  < s_wrk%s_next%tnetblc
            ENDIF
            IF ( ll_test ) THEN
               CALL switch_links(sd_root, s_wrk)
               ll_ord = .FALSE.
               CYCLE
            ENDIF
            IF( ASSOCIATED(s_wrk%s_next) ) s_wrk => s_wrk%s_next
         END DO
      END DO
      
   END SUBROUTINE sort_chain


   SUBROUTINE mpp_avgminmax( pval, pavg, pmin, pmax, pblc, ld_mpi, cdname )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE mpp_avgminmax  ***
      !! ** Purpose :   get average, min, max over all MPI processes
      !!----------------------------------------------------------------------
      REAL(dp)                  , INTENT(in   ) ::   pval                    ! local value (of the current MPI process)
      REAL(dp)                  , INTENT(  out) ::   pavg, pmin, pmax, pblc  ! mean, min, max and load balance (among MPI processes)
      LOGICAL                   , INTENT(in   ) ::   ld_mpi
      CHARACTER(len=*), OPTIONAL, INTENT(in   ) ::   cdname
      !
      REAL(dp), DIMENSION(:), ALLOCATABLE ::  zallmpi
      INTEGER :: icode, isize
      !!----------------------------------------------------------------------
     
      IF( ld_mpi ) THEN
#if ! defined key_mpi_off
         CALL MPI_COMM_SIZE( nmpicom, isize, icode )
         ALLOCATE(zallmpi(isize))
         CALL MPI_ALLGATHER(    pval, 1, MPI_DOUBLE_PRECISION,   &
            &                zallmpi, 1, MPI_DOUBLE_PRECISION, nmpicom, icode)
#endif
         pavg = SUM(    zallmpi ) / REAL(isize, dp)
         pmin = MINVAL( zallmpi )
         pmax = MAXVAL( zallmpi )
         IF( PRESENT(cdname) )   CALL gnuplot_statplot( 'timing_mpitasks', cdname, zallmpi )
         DEALLOCATE(    zallmpi )
      ELSE
         pavg = pval
         pmin = pval
         pmax = pval
      ENDIF
      pblc = pmax - pmin
         
   END SUBROUTINE mpp_avgminmax
  

   SUBROUTINE write_header(cdname)
      !!----------------------------------------------------------------------
      CHARACTER(len=*), INTENT(in) :: cdname
      !
      INTEGER :: ji
      CHARACTER(LEN=128) :: cldash
      !!----------------------------------------------------------------------
      
      CALL write_separator()
      WRITE(numtime,'(a)') ' '//TRIM(cdname)
      cldash(1:128) = " "
      DO ji = 1, LEN_TRIM(cdname)
         cldash(ji+1:ji+1) = "-"
      ENDDO
      WRITE(numtime,'(a)') TRIM(cldash)
      WRITE(numtime,*)

   END SUBROUTINE write_header
  

   SUBROUTINE write_separator()
      WRITE(numtime,*)
      WRITE(numtime,*) '   ================================================='
      WRITE(numtime,*)
      WRITE(numtime,*)
   END SUBROUTINE write_separator


   SUBROUTINE gnuplot_statplot( cdprefix, cdname, pmpitime )
      !!----------------------------------------------------------------------
      !!               ***  ROUTINE gnuplot_statplot  ***
      !! ** Purpose :   Perform gnuplot statplot/plots
      !!----------------------------------------------------------------------
      CHARACTER(len=*)                , INTENT(in) :: cdprefix
      CHARACTER(len=*)                , INTENT(in) :: cdname
      REAL(dp), DIMENSION(:), OPTIONAL, INTENT(in) :: pmpitime
      !
      INTEGER :: ji
      INTEGER :: istatus, isz1, isz2, isz3, inumsh
      INTEGER :: irank, isize, icode, icmdstat
      CHARACTER(LEN=512) :: clcmd
      CHARACTER(LEN=256) :: clfile
      CHARACTER(LEN=1  ) :: cl1
      CHARACTER(LEN=10 ) :: clfmt
      CHARACTER(:), ALLOCATABLE :: clname
      LOGICAL :: ll_execmd, llwm, llwrt
      !!----------------------------------------------------------------------

#if ! defined key_mpi_off
      CALL MPI_COMM_RANK( nmpicom, irank, icode )
      CALL MPI_COMM_SIZE( nmpicom, isize, icode )
#else
      irank = 0
      isize = 1
#endif

      llwm = irank == 0   ! local definition of lwm, as we minimize the number of USE in this module (avoid circular dependency)

      llwrt = numtime /= -1                              ! this process is writing a timing.output file
      IF( PRESENT(pmpitime) ) llwrt = llwrt .AND. llwm   ! only the first process is doing the plot (same for all plots)
      IF( .NOT. llwrt )   RETURN

      ! do we have gnuplot on the machine?
      CALL EXECUTE_COMMAND_LINE('which gnuplot &> /dev/null', EXITSTAT = istatus, CMDSTAT = icmdstat)
      IF( icmdstat /= 0 )   RETURN   ! EXECUTE_COMMAND_LINE is not properly working on this machine
      ll_execmd = istatus == 0

      ! define clname
      ! proc 0 => cdprefix//'_'//cdname         + all non-alphanumeric characters are replaced by '_'
      ! orhers => cdprefix//'_'//cdname//'_xxx' + all non-alphanumeric characters are replaced by '_'     
      isz1 = LEN_TRIM(cdprefix) + 1
      isz2 = LEN_TRIM(cdname)
      IF ( llwm ) THEN   ;   isz3 = -1   ! to make 0 when we do isz3+1 bellow
      ELSE               ;   isz3 = MAX( INT(LOG10(REAL(MAX(1,isize)))) + 1, 4 )   ! 'xxx'
      ENDIF
      ALLOCATE(CHARACTER(isz1+isz2+isz3+1) :: clname)
      clname(1:isz1) = TRIM(cdprefix)//'_'   ! must use this syntaxe.
      DO ji = isz1+1, isz1+isz2
         cl1 = cdname(ji-isz1:ji-isz1)
         IF( ('0' <= cl1 .AND. cl1 <= '9') .OR. ('A' <= cl1 .AND. cl1 <= 'Z') .OR. ('a' <= cl1 .AND. cl1 <= 'z') ) THEN
            clname(ji:ji) = cl1   ! alphanumeric characters: keep it
         ELSE
            clname(ji:ji) = '_'   ! non-alphanumeric characters: replace it by '_'
         ENDIF
      END DO
      IF( .NOT. llwm ) THEN   ! add proc number to the name
         WRITE(clfmt, "('( a, i', i1, '.', i1, ')')") isz3, isz3   ! '(a,a,ix.x)'
         WRITE(clname(isz1+isz2+1:isz1+isz2+isz3+1), clfmt) '_', irank
      ENDIF
      !
      IF( .NOT. PRESENT(pmpitime) ) THEN
         CALL write_header('Stats report on timing '//TRIM(cdname)//':')
         ! get numtime file name (i.e. timing.output)
         INQUIRE(unit = numtime, NAME = clfile)
         ! write gnoplot stats in clfile (i.e. timing.output)
         IF( ll_execmd ) THEN
            clcmd = 'grep "^ *timing '//TRIM(cdname)//' *[0-9]* *: *[0-9]" '//TRIM(clfile)//' | sed -e "s/.*: *//" | gnuplot -p -e ''stats "/dev/stdin"'' &>> '//TRIM(clfile)
            CLOSE(numtime)       ! close it EXECUTE_COMMAND_LINE will do a grep and write in this file
            CALL EXECUTE_COMMAND_LINE(TRIM(clcmd), CMDSTAT = icmdstat)
            OPEN(NEWUNIT = numtime, FILE = TRIM(clfile), POSITION = "append", STATUS = "old", ACTION = "write")
            IF( icmdstat /= 0 )   RETURN   ! EXECUTE_COMMAND_LINE is not properly working on this machine
         ENDIF
      ENDIF

      ! write the shell script to perform gnuplot stats and plots
      OPEN(NEWUNIT = inumsh, FILE = clname//'.sh', POSITION = "rewind", STATUS = "replace", ACTION = "write")
      WRITE(inumsh,'(a)') '#!/bin/sh'
      WRITE(inumsh,'(a)') '#'
      WRITE(inumsh,'(a)') 'if [ "$1" == "--help" ] ; then'
      WRITE(inumsh,'(a)') '    echo "   Usage:"'
      WRITE(inumsh,'(a)') '    echo "      ./'//clname//'.sh         # to create the plots on screen"'
      WRITE(inumsh,'(a)') '    echo "      ./'//clname//'.sh --png   # to create the plots in '//clname//'.png"'
      WRITE(inumsh,'(a)') '    exit 0'
      WRITE(inumsh,'(a)') 'fi'
      WRITE(inumsh,'(a)') '[ "$1" == "--png" ] && dopng=1 || dopng=0'
      WRITE(inumsh,'(a)') '#'
      WRITE(inumsh,'(a)') 'which gnuplot &> /dev/null'
      WRITE(inumsh,'(a)') '[ $? -ne 0 ] && echo "$( basename $0 ) uses gnuplot which was not found" && exit 1'
      WRITE(inumsh,'(a)') '#'
     ! write the timing data directly in clname//'.sh' so its becomes a stand alone script
      IF( PRESENT(pmpitime) ) THEN
         DO ji = 1, SIZE(pmpitime)
            WRITE(inumsh,'(a,i9,a,f12.6)') '# timing '//TRIM(cdname)//' MPI rank ', ji-1, ' : ', pmpitime(ji)   ! micro second
         END DO
      ELSE
         clcmd = 'grep "^ *timing '//TRIM(cdname)//' *[0-9]* *: *[0-9]" '//TRIM(clfile)//' | sed -e "s/^/#/" &>> '//clname//'.sh'
         CLOSE(inumsh )       ! close it as EXECUTE_COMMAND_LINE will write in this file
         CLOSE(numtime)       ! close it as EXECUTE_COMMAND_LINE will do a grep on this file
         CALL EXECUTE_COMMAND_LINE(TRIM(clcmd), CMDSTAT = icmdstat)
         OPEN(NEWUNIT = numtime, FILE = TRIM(clfile) , POSITION = "append", STATUS = "old", ACTION = "write")
         IF( icmdstat /= 0 )   RETURN   ! EXECUTE_COMMAND_LINE is not properly working on this machine
         OPEN(NEWUNIT = inumsh , FILE = clname//'.sh', POSITION = "append", STATUS = "old", ACTION = "write")
      ENDIF
      WRITE(inumsh,'(a)') '#'
      WRITE(inumsh,'(a)') 'grep "^# *timing .* *[0-9]* *: *[0-9]" $( basename $0 ) | sed -e "s/.*: *//" > '//clname//'.txt'
      WRITE(inumsh,'(a)') '#'
      WRITE(inumsh,'(a)') 'gnuplot -persist << EOF'
      WRITE(inumsh,'(a)') 'dopng=$dopng'
      WRITE(inumsh,'(a)') 'if ( dopng == 0 ) {'
      WRITE(inumsh,'(a)') '   set terminal x11 size 800,800'
      WRITE(inumsh,'(a)') '} else {'
      WRITE(inumsh,'(a)') '   set terminal png size 800,800'
      WRITE(inumsh,'(a)') '   set output "'//clname//'.png"'
      WRITE(inumsh,'(a)') '}'
      WRITE(inumsh,'(a)') 'stats "'//clname//'.txt" name "ST"'
      WRITE(inumsh,'(a)') 'mn = ST_mean ; md = ST_median ; std = ST_stddev'
      WRITE(inumsh,'(a)') 'lo = ST_lo_quartile ; up = ST_up_quartile ; iqr = up-lo'
      WRITE(inumsh,'(a)') 'set xrange [0:ST_records]'
      WRITE(inumsh,'(a)') 'set multiplot layout 2,1'
      WRITE(inumsh,'(a)') 'set title sprintf("FULL RANGE: timing '//TRIM(cdname)//', mean = %f, median = %f", mn, md)'
      WRITE(inumsh,'(a)') 'plot "'//clname//'.txt" notitle, mn title "Mean" lw 2, mn-std title "Mean-StdDev" lw 2, mn+std title "Mean+StdDev" lw 2'
      WRITE(inumsh,'(a)') 'set yrange [lo-1.5*iqr:up+1.5*iqr]'
      WRITE(inumsh,'(a)') 'set title sprintf("ZOOM: timing '//TRIM(cdname)//', mean = %f, median = %f", mn, md)'
      WRITE(inumsh,'(a)') 'plot "'//clname//'.txt" notitle, md title "Median" lw 2, lo title "1st Quatile" lw 2, up title "3rd Quartile" lw 2'
      WRITE(inumsh,'(a)') 'EOF'
      WRITE(inumsh,'(a)') '#'
      WRITE(inumsh,'(a)') 'rm -f '//clname//'.txt &>/dev/null'
      CLOSE(inumsh)
      CALL EXECUTE_COMMAND_LINE('chmod u+x '//clname//'.sh', CMDSTAT = icmdstat)
      IF( icmdstat /= 0 )   RETURN   ! EXECUTE_COMMAND_LINE is not properly working on this machine

      IF( .NOT. PRESENT(pmpitime) ) THEN
         WRITE(numtime,*) '   ==== info:'
         IF( ll_execmd .AND. llwm )  THEN   ! by default, create the png only for proc 0
            CALL EXECUTE_COMMAND_LINE('./'//clname//'.sh --png &>/dev/null', CMDSTAT = icmdstat)
            IF( icmdstat /= 0 )   RETURN   ! EXECUTE_COMMAND_LINE is not properly working on this machine
            WRITE(numtime,*) '        Shell script used to create '//clname//'.png: '//clname//'.sh'
         ELSE
            WRITE(numtime,*) '        Gnuplot not found, we created the shell script'//clname//'.sh'
            WRITE(numtime,*) '        that can be used to create gnuplot stats and analyses'      
         ENDIF
         WRITE(numtime,*) '        Usage:'
         WRITE(numtime,*) '           ./'//clname//'.sh         # to create the plots on screen'
         WRITE(numtime,*) '           ./'//clname//'.sh --png   # to create the plots in '//clname//'.png'
      ENDIF

      DEALLOCATE(clname)

   END SUBROUTINE gnuplot_statplot
   
#else
   ! Dummy routines for AGRIF : they must do nothing
   SUBROUTINE timing_start( cdinfo, ldstatplot )
      CHARACTER(len=*) , INTENT(in) :: cdinfo
      LOGICAL, OPTIONAL, INTENT(in) :: ldstatplot
      IF(.FALSE.)   WRITE(*,*) cdinfo, PRESENT(ldstatplot)   ! to avoid compilation warnings
   END SUBROUTINE timing_start
   
   SUBROUTINE timing_stop( cdinfo, kt, ld_finalize )
      CHARACTER(len=*) , INTENT(in) ::   cdinfo
      INTEGER, OPTIONAL, INTENT(in) ::   kt
      LOGICAL, OPTIONAL, INTENT(in) ::   ld_finalize
      IF(.FALSE.)   WRITE(*,*) cdinfo, kt, PRESENT(ld_finalize)   ! to avoid compilation warnings
   END SUBROUTINE timing_stop
#endif
 


   !!=====================================================================
END MODULE timing
