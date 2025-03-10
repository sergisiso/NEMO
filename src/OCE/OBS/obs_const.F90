MODULE obs_const
   !!=====================================================================
   !!                       ***  MODULE obs_const  ***
   !! Observation diagnostics: Constants used by many modules
   !!===================================================================== 
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

   !! * Modules used
   USE par_kind, ONLY : & ! Precision variables
      & sp         
   IMPLICIT NONE

   !! * Routine/type accessibility
   PUBLIC

   REAL(kind=sp), PARAMETER :: obfillflt=99999.

END MODULE obs_const

