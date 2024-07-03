MODULE trcatf
   !!======================================================================
   !!                       ***  MODULE  trcatf  ***
   !! Ocean passive tracers:  time stepping on passives tracers
   !!======================================================================
   !! History :  7.0  !  1991-11  (G. Madec)  Original code
   !!                 !  1993-03  (M. Guyon)  symetrical conditions
   !!                 !  1995-02  (M. Levy)   passive tracers
   !!                 !  1996-02  (G. Madec & M. Imbard)  opa release 8.0
   !!            8.0  !  1996-04  (A. Weaver)  Euler forward step
   !!            8.2  !  1999-02  (G. Madec, N. Grima)  semi-implicit pressure grad.
   !!  NEMO      1.0  !  2002-08  (G. Madec)  F90: Free form and module
   !!                 !  2002-08  (G. Madec)  F90: Free form and module
   !!                 !  2002-11  (C. Talandier, A-M Treguier) Open boundaries
   !!                 !  2004-03  (C. Ethe) passive tracers
   !!                 !  2007-02  (C. Deltel) Diagnose ML trends for passive tracers
   !!            2.0  !  2006-02  (L. Debreu, C. Mazauric) Agrif implementation
   !!            3.0  !  2008-06  (G. Madec)  time stepping always done in trazdf
   !!            3.1  !  2009-02  (G. Madec, R. Benshila)  re-introduce the vvl option
   !!            3.3  !  2010-06  (C. Ethe, G. Madec) Merge TRA-TRC
   !!            4.1  !  2019-08  (A. Coward, D. Storkey) rename trcnxt.F90 -> trcatf.F90. Now only does time filtering.
   !!            4.x  !  2022-12  (S. Techene, G.Madec) remove vvl use qco exclusively 
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   Default option                                         Empty module
   !!----------------------------------------------------------------------
   USE par_oce
   USE par_trc
   IMPLICIT NONE
CONTAINS
   SUBROUTINE trc_atf( kt, Kbb, Kmm, Kaa, ptr )  
      INTEGER                                   , INTENT(in)    :: kt
      INTEGER,                                    INTENT(in   ) :: Kbb, Kmm, Kaa ! time level indices
      REAL(wp), DIMENSION(jpi,jpj,jpk,jptra,jpt), INTENT(inout) :: ptr           ! passive tracers and RHS of tracer equation
      WRITE(*,*) 'trc_atf: You should not have seen this print! error?', kt
   END SUBROUTINE trc_atf
   !!======================================================================
END MODULE trcatf
