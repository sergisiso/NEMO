#! /usr/bin/python
# ======================================================================
#                 ***  psct-LocalToGlobalArrays.py  ***
# ======================================================================
#  History : 5.0  !  2024  (S. Mueller)
# ----------------------------------------------------------------------
#
# PSyclone transformation script for the promotion of some local arrays to
# module scope. PSyclone provides this functionality through its
# HoistLocalArraysTrans transformation
# (https://psyclone.readthedocs.io/en/stable/transformations.html#available-transformations),
# which is applied to most procedures.
#
# ----------------------------------------------------------------------
# NEMO 5.0 , NEMO Consortium (2024)
# Software governed by the CeCILL license (see ./LICENSE)
# ----------------------------------------------------------------------

from psyclone.psyir.nodes import Routine
from psyclone.psyir.transformations import HoistLocalArraysTrans

# ----------------------------------------------------------------------
# Rejection of modules and subroutines (all names in lowercase)
# ----------------------------------------------------------------------
#
# Reject for compatibility with PSyclone 2.5.0
MODULES_REJECT = [ 'agrif2model', 'agrif_user', 'sedini' ]
# Subroutines for which local arrays should not be promoted
ROUTINE_REJECT = [ 'dia_dct' ]   # Issue with RESHAPE operation

# ----------------------------------------------------------------------
#              ***  PSyclone transformation procedure  ***
# ----------------------------------------------------------------------
def trans(psyir):

    if not len([ m for m in MODULES_REJECT if psyir.name.lower()==m+".f90" ]):
        for routine in psyir.walk(Routine):
            if not routine.name.lower() in ROUTINE_REJECT:
                HoistLocalArraysTrans().apply(routine)
