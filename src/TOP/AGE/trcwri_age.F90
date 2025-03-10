MODULE trcwri_age
   !!======================================================================
   !!                       *** MODULE trcwri ***
   !!    age :   Output of age tracers
   !!======================================================================
   !! History :   1.0  !  2009-05 (C. Ethe)  Original code
   !!----------------------------------------------------------------------
#if defined key_top &&  defined key_xios
   !!----------------------------------------------------------------------
   !! trc_wri_age   :  outputs of concentration fields
   !!----------------------------------------------------------------------
   USE par_age     
   USE trc         
   USE iom

   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_wri_age 

CONTAINS

   SUBROUTINE trc_wri_age( kt, Kmm )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_wri_trc  ***
      !!
      !! ** Purpose :   output passive tracers fields 
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in)  :: kt    ! time step
      INTEGER, INTENT(in)  :: Kmm   ! time level indices
      !
      CHARACTER (len=20)   :: cltra
      !!---------------------------------------------------------------------

      ! write the tracer concentrations in the file

      cltra = TRIM( ctrcnm(jp_age) )                  ! short title for tracer
      CALL iom_put( cltra, tr(:,:,:,jp_age,Kmm) )

      !
   END SUBROUTINE trc_wri_age

#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
   IMPLICIT NONE
   PUBLIC trc_wri_age
CONTAINS
   SUBROUTINE trc_wri_age                     ! Empty routine  
   END SUBROUTINE trc_wri_age
#endif

   !!----------------------------------------------------------------------
   !! NEMO/TOP 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!======================================================================
END MODULE trcwri_age
