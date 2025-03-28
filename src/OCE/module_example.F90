MODULE exampl
   !!======================================================================
   !!                       ***  MODULE  exampl  ***
   !! Ocean physics:  brief description of the purpose of the module
   !!                 (please no more than 2 lines)
   !!======================================================================
   !! History : 3.0  !  2008-06  (Author Names)  Original code
   !!            -   !  2008-08  (Author names)  brief description of modifications
   !!           3.3  !  2010-11  (Author names)        -              -
   !!----------------------------------------------------------------------
#if defined key_example
   !!----------------------------------------------------------------------
   !!   'key_example'  :                brief description of the key option
   !!----------------------------------------------------------------------
   !!   exa_mpl       : list of module subroutine (caution, never use the
   !!   exa_mpl_init  : name of the module for a routine)
   !!   exa_mpl_stp   : Please try to use 3 letter block for routine names
   !!----------------------------------------------------------------------
   USE module_name1               ! brief description of the used module
   USE module_name2               ! ....
   USE par_kind, ONLY : wp        ! definition of the working precision
   USE lib_mpp , ONLY : nam_ctl   ! subroutine nam_ctl is utilised for namelist-input error handling

   IMPLICIT NONE
   PRIVATE

   PUBLIC   exa_mpl        ! routine called in xxx.F90 module
   PUBLIC   exa_mpl_init   ! routine called in nemogcm.F90 module

   TYPE ::   FLD_E                !: Structure type definition
      CHARACTER(lc) ::   clname      ! clname description (default length, lc, is 256, see par_kind.F90)
      INTEGER       ::   nfreqh      ! nfreqh description 
   END TYPE FLD_E 

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   var1         !: var1 description. CAUTION always use !: to describe
   !                                                          !  a PUBLIC variable: simplify its search : 
   !                                                          !  grep var1 *90 | grep '!:'
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   var2, var2   !: several variable on a same line OK, but 
   !                                                          !  DO NOT use continuation lines in declaration

   !                               !!* namelist nam_xxx *
   LOGICAL   ::   ln_opt = .TRUE.   ! give the default value of each namelist parameter
   CHARACTER ::   cn_tex = 'T'      ! short description  of the variable
   INTEGER   ::   nn_opt = 1        ! please respect the DOCTOR norm for namelist variable
   REAL(wp)  ::   rn_var = 2._wp    ! (it becomes easy to identify them in the code)
   TYPE(FLD) ::   sn_ex             ! structure

   INTEGER                          ::   nint    ! nint  description (local permanent variable)
   REAL(wp)                         ::   var     ! var         -                -
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   array   ! array       -                -

   !! * Substitutions
   ! for DO macro
#  include "do_loop_substitute.h90"
   ! namelist-input macros
#  include "read_nml_substitute.h90"
   !for other substitutions
#  include "exampl_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION exa_mpl_alloc()
      !!----------------------------------------------------------------------
      !!                ***  FUNCTION exa_mpl_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( array(jpi,jpj,jpk) , STAT= exa_mpl_alloc )   ! Module array                                                                
      !
      CALL mpp_sum ( 'module_example', exa_mpl_alloc )
      IF( exa_mpl_alloc /= 0 )   CALL ctl_stop( 'STOP', 'exa_mpl_alloc: failed to allocate arrays' )
      !
   END FUNCTION exa_mpl_alloc
   

   SUBROUTINE exa_mpl( kt, pvar1, pvar2, ptab )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE exa_mpl  ***
      !!
      !! ** Purpose :   Brief description of the routine
      !!
      !! ** Method  :   description of the methodoloy used to achieve the
      !!                objectives of the routine. Be as clear as possible!
      !!
      !! ** Action  : - first action (share memory array/varible modified
      !!                in this routine
      !!              - second action .....
      !!              - .....
      !!
      !! References :   Author et al., Short_name_review, Year
      !!                Give references if exist otherwise suppress these lines
      !!----------------------------------------------------------------------
      INTEGER , INTENT(in   )                         ::   kt      ! short description
      INTEGER , INTENT(inout)                         ::   pvar1   !   -         -
      REAL(wp), INTENT(  out)                         ::   pvar2   !   -         -
      REAL(wp), INTENT(  out), DIMENSION(A2D(nn_hls)) ::   pvar2   !   -         -
      !!
      INTEGER  ::   ji, jj, jk       ! dummy loop arguments  (DOCTOR : start with j, but not jp)
      INTEGER  ::   itoto, itata     ! temporary integers    (DOCTOR : start with i
      REAL(wp) ::   zmlmin, zbbrho   ! temporary scalars     (DOCTOR : start with z)
      REAL(wp) ::   zfact1, zfact2   ! do not use continuation lines in declaration
      REAL(wp), DIMENSION(T2D(nn_hls))     ::   zwrk_2d   ! 2D workspace
      REAL(wp), DIMENSION(T2D(nn_hls),jpk) ::   zwrk_3d   ! 3D workspace
      !!--------------------------------------------------------------------
      !
      IF( .NOT. l_istiled .OR. ntile == 1 )  THEN                       ! Do only on the first tile
         IF( kt == nit000  )   CALL exa_mpl_init    ! Initialization (first time-step only)

         zmlmin = 1.e-8                             ! Local constant initialization
         zbbrho =  .5 * ebb / rho0
         zfact1 = -.5 * rdt * efave
         zfact2 = 1.5 * rdt * ediss
      ENDIF
	  
      SELECT CASE ( npdl )                       ! short description of the action
      !
      CASE ( 0 )                                      ! describe case 1
         DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 1, jpkm1 )
            avm(ji,jj,jk) = ....
         END_3D
         !
      CASE ( 1 )                                      ! describe case 2
         DO_3D( nn_hls-1, nn_hls-1, nn_hls-1, nn_hls-1, 1, jpkm1 )
            avm(ji,jj,jk) = ....
         END_3D
         !
      END SELECT
      !
      ! WARNING! the lbc_lnk call could not be compatible with the tiling approach
      ! please refer to the manual for how to adapt your code
      CALL lbc_lnk( 'module_example', avm, 'T', 1._wp )     ! Lateral boundary conditions (unchanged sign)
      !
   END SUBROUTINE exa_mpl


   SUBROUTINE exa_mpl_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE exa_mpl_init  ***
      !!                   
      !! ** Purpose :   initialization of ....
      !!
      !! ** Method  :   blah blah blah ...
      !!
      !! ** input   :   Namlist namexa
      !!
      !! ** Action  :   ...  
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj, jk, jit   ! dummy loop indices
      INTEGER ::   ios               ! Local integer output status for namelist read
      !!
      NAMELIST/namexa/ exa_v1, exa_v2, nexa_0, sn_ex     
      !!----------------------------------------------------------------------
      !
      ! Namelist group namexa in reference namelist input file : Example
      READ_NML_REF(numnam,namexa)
      ! Namelist group namexa in configuration namelist input file : Example
      READ_NML_CFG(numnam,namexa)
   ! Output namelist for control
      WRITE ( numond, namexa )
      !
      IF(lwp) THEN                              ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'exa_mpl_init : example '
         WRITE(numout,*) '~~~~~~~~~~~~'
         WRITE(numout,*) '   Namelist namexa : set example parameters'
         WRITE(numout,*) '      brief desciption               exa_v1  = ', exa_v1
         WRITE(numout,*) '      brief desciption               exa_v2  = ', exa_v2
         WRITE(numout,*) '      brief desciption               nexa_0  = ', nexa_0
         WRITE(numout,*) '      brief desciption          sn_ex%clname = ', sn_ex%clname
         WRITE(numout,*) '      brief desciption          sn_ex%nfreqh = ', sn_ex%nfreqh
      ENDIF
      !
      !                              ! allocate exa_mpl arrays      
      IF( exa_mpl_alloc() /= 0 )   CALL ctl_stop( 'STOP', 'exa_mpl_init : unable to allocate arrays' )
      !                              ! Parameter control
      IF( ln_tile ) CALL ctl_stop( 'exa_mpl_init: tiling is not supported in this module by default, see manual for how to adapt your code' )
      IF( ln_opt      )   CALL ctl_stop( 'exa_mpl_init: this work and option xxx are incompatible'   )
      IF( nn_opt == 2 )   CALL ctl_stop( 'STOP',  'exa_mpl_init: this work and option yyy may cause problems'  )
      !
   END SUBROUTINE exa_mpl_init

#else
   !!----------------------------------------------------------------------
   !!   Default option :                                         NO example
   !!----------------------------------------------------------------------
   USE par_kind, ONLY : wp        ! definition of the working precision
CONTAINS
   SUBROUTINE exa_mpl( kt, pvar1, pvar2, ptab )              ! Empty routine
      INTEGER                 ::   kt
      REAL(wp)                ::   pvar1, pvar2
      REAL(wp),DIMENSION(:,:) ::   ptab
      WRITE(*,*) 'exa_mpl: You should not have seen this print! error?', kt, pvar1, pvar2, ptab(1,1)
   END SUBROUTINE exa_mpl
#endif

   !!======================================================================
END MODULE exampl
