MODULE trcstp
   !!======================================================================
   !!                       ***  MODULE trcstp  ***
   !! Time-stepping    : time loop of opa for passive tracer
   !!======================================================================
   !! History :  1.0  !  2004-03  (C. Ethe)  Original
   !!            4.1  !  2019-08  (A. Coward, D. Storkey) rewrite in preparation for new timestepping scheme
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   Default key                                     NO passive tracers
   !!----------------------------------------------------------------------
   IMPLICIT NONE
CONTAINS
   SUBROUTINE trc_stp( kt, Kbb, Kmm, Krhs, Kaa )        ! Empty routine
      INTEGER, INTENT(in   ) ::   kt                    ! time-step index
      INTEGER, INTENT(in   ) ::   Kbb, Kmm, Krhs, Kaa   ! time-level indices
      WRITE(*,*) 'trc_stp: You should not have seen this print! error?', kt
   END SUBROUTINE trc_stp

   !!======================================================================
END MODULE trcstp
