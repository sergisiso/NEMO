#! /usr/bin/python
# ======================================================================
#                     ***  psct-list_symbols.py  ***
# ======================================================================
# History : 4.3  ! 2023-06  (S. Mueller) Initial version
# ----------------------------------------------------------------------
#
# PSyclone (https://github.com/stfc/PSyclone) transformation script, which,
# when supplied to the PSyclone source-code processing build stage (./makenemo
# -p list_symbols [...]), results in the output of the module and procedure
# symbol tables of the processed source-code file and otherwise in the passing
# of the source code through the PSyclone processing stage without functional
# transformations. See
# https://psyclone.readthedocs.io/en/stable/transformation.html#sec-transformation-script
# for information on PSyclone transformation-script conventions.
#
# ----------------------------------------------------------------------
# NEMO 4.3 , NEMO Consortium (2023)
# Software governed by the CeCILL license (see ./LICENSE)
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
#               ***  PSyclone transformation procedure  ***
# ----------------------------------------------------------------------
from psyclone.psyir.nodes import Routine

def trans(psyir):

    print()
    print("[PSyclone transformation] Passthrough and list of module ('Container') and procedure ('Routine') symbols")
    print("===================================================================================================================")
    print()

    # Print module and procedure symbol tables
    routines = psyir.walk(Routine)
    if len(routines):
        for routine in routines:
            st = routine.symbol_table
            if routine == routines[0]:
                pst = st.parent_symbol_table()
                if len(pst.symbols):
                    print(pst)
            if len(st.symbols):
                print(st)
