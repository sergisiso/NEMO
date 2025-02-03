#! /usr/bin/python
# ======================================================================
#                        ***  psct_utils.py  ***
# ======================================================================
#  History : 5.0  !  2025-01  (S. Mueller) Initial version
# ----------------------------------------------------------------------
#
# Auxiliary functionality for PSyclone (https://github.com/stfc/PSyclone)
# source-code processing recipe scripts
#
# P3API            : Indicator of the PSyclone API variant that is in use:
#                    either that of PSyclone release version 3.0.0 (True) or
#                    that of PSyclone release versions 2.5.0 and 2.4.0 (False)
#
# P3APICompat(psy) : Wrapper function that can be used to restore
#                    compatibility with PSyclone release version 3.0.0 (and
#                    possibly later) of some PSyclone-processing recipe scripts
#                    that are compatible with PSyclone release version 2.5.0 or
#                    2.4.0.
#
# ----------------------------------------------------------------------
# NEMO 5.0 , NEMO Consortium (2025)
# Software governed by the CeCILL license (see ./LICENSE)
# ----------------------------------------------------------------------

from psyclone.psyir.nodes import Routine, Container

# Class NemoPSy is available from PSyclone release versions 2.5.0 or 2.4.0, but
# not from PSyclone release version 3.0.0
P3API = False
try:
    from psyclone.nemo import NemoPSy
except:
    P3API = True

# Simple mock variant of the PSy class that mimics some properties (invokes
# list and name) of the object that is passed to PSyclone processing recipe
# scripts when using PSyclone release version 2.5.0 or 2.4.0
class P3APICompatPSy():
    invokes = None
    name = ''
    def __init__(self,psyir):
        self.invokes = P3APICompatInvokes(psyir.walk(Routine))
        # The name of the wrapped object is based on the module name if 'obj'
        # represents a module, otherwise either the name of the first procedure
        # if there is any, or the file name
        if len(psyir.walk(Container)) > 1:
            name = psyir.walk(Container)[1].name
        elif len(psyir.walk(Routine)) > 0:
            name = psyir.walk(Routine)[0].name
        else:
            name = psyir.name
        self.name = 'psy_'+name+'_psy'

# Simple mock variant of PSyclone's Invokes class that simply holds a list of
# P3APICompatInvoke instances
class P3APICompatInvokes():
    invoke_list = []
    def __init__(self,routines):
        self.invoke_list = [P3APICompatInvoke(routine) for routine in routines]

# Simple mock variant of PSyclone's Invoke class that simply holds a Routine
# object and its name
class P3APICompatInvoke():
    name = ''
    schedule = None
    def __init__(self,routine):
        self.name = routine.name
        self.schedule = routine

# When run with PSyclone release version 3.0.0, this wrapper function ensures
# that the procedures represented inside object 'psy' are embedded in a mock
# object, which mimics relevant properties of the corresponding 'psy' object
# that would be passed by PSyclone release version 2.5.0 or 2.4.0; otherwise
# 'psy' is returned unchanged.
def P3APICompat(psy):
    # Build the mock structure only if required
    if P3API and not isinstance(psy,P3APICompatPSy):
        return P3APICompatPSy(psy)
    else:
        return psy
