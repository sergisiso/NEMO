MODULE par_my_trc
   !!======================================================================
   !!                        ***  par_my_trc  ***
   !! TOP :   set the MY_TRC parameters
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec)  revised architecture
   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

   IMPLICIT NONE

   ! Starting/ending PISCES do-loop indices (N.B. no PISCES : jpl_pcs < jpf_pcs the do-loop are never done)
   INTEGER, PUBLIC ::   jp_myt0             !: First index of MY_TRC passive tracers
   INTEGER, PUBLIC ::   jp_myt1             !: Last  index of MY_TRC passive tracers
   !!======================================================================
END MODULE par_my_trc