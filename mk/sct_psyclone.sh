#!/bin/bash
# ======================================================================
#                         *** sct_psyclone.sh ***
# ======================================================================
# History : 4.3  ! 2023-03  (S. Mueller) Incorporation of PSyclone processing into the build system
# ----------------------------------------------------------------------
#
# Wrapper script to launch the transformation of an individual source-code file
# by the PSyclone system (https://github.com/stfc/PSyclone)
#
# Transformation mode:
#     sct_psyclone.sh <psyclone path> <transformation> <configuration directory> <input file>
#
# Passthrough mode:
#     sct_psyclone.sh <psyclone path> 'passthrough' <configuration directory> <input file>
#
# ----------------------------------------------------------------------
# NEMO 4.3 , NEMO Consortium (2023)
# Software governed by the CeCILL license (see ./LICENSE)
# ----------------------------------------------------------------------
set -o posix
#
# PSyclone version 2.4.0 (default) or 2.3.1
PSYCLONE_VERSION="2.4.0"
# Path to PSyclone installation
PSYCLONE_PATH=$1
# Transformation or 'passthrough'
TPSYCLONE=$2
# Configuration directory
BLD_DIR=$3
# Input file
FILENAME=$(basename "$4")
#
# Set action for the file to transformation,
ACTION='TRANSFORM'
#    but explicitly disable the processing of files that PSyclone version 2.4.0
#    would fail to process or not correctly reproduce in the PSyclone
#    passthrough,
[[ "${FILENAME}" == 'diaptr.f90'            ]] && ACTION='EXCLUDE'   # protect array bounds in 'WHERE' constructs
[[ "${FILENAME}" == 'icedyn_rhg_eap.f90'    ]] && ACTION='EXCLUDE'   # protect 'ELEMENTAL' procedure prefix
[[ "${FILENAME}" == 'iceistate.f90'         ]] && ACTION='EXCLUDE'   # protect reduction inside 'WHERE' conditional expression
[[ "${FILENAME}" == 'icethd_do.f90'         ]] && ACTION='EXCLUDE'   # protect array bounds in 'WHERE' construct
[[ "${FILENAME}" == 'icewri.f90'            ]] && ACTION='EXCLUDE'   # protect array bounds in 'WHERE' construct
[[ "${FILENAME}" == 'iom.f90'               ]] && ACTION='EXCLUDE'   # see PSyclone issue #2340
                                                                     # (https://github.com/stfc/PSyclone/issues/2340)
[[ "${FILENAME}" == 'isfpar.f90'            ]] && ACTION='EXCLUDE'   # protect array bounds in 'WHERE' construct
[[ "${FILENAME}" == 'lib_fortran.f90'       ]] && ACTION='EXCLUDE'   # protect 'ELEMENTAL' procedure prefix
[[ "${FILENAME}" == 'p4zpoc.f90'            ]] && ACTION='EXCLUDE'   # protect 'ELEMENTAL' procedure prefix
[[ "${FILENAME}" == 'sbc_phy.f90'           ]] && ACTION='EXCLUDE'   # protect 'ELEMENTAL' procedure prefix
[[ "${FILENAME}" == 'sbcblk.f90'            ]] && ACTION='EXCLUDE'   # protect array bounds in 'WHERE' constructs
[[ "${FILENAME}" == 'sbcblk_skin_coare.f90' ]] && ACTION='EXCLUDE'   # protect 'ELEMENTAL' procedure prefix
[[ "${FILENAME}" == 'sbcdcy.f90'            ]] && ACTION='EXCLUDE'   # protect 'ELEMENTAL' procedure prefix
[[ "${FILENAME}" == 'trosk.f90'             ]] && ACTION='EXCLUDE'   # see PSyclone issue #1254
                                                                     # (https://github.com/stfc/PSyclone/issues/1254)
[[ "${FILENAME}" == 'vremap.f90'            ]] && ACTION='EXCLUDE'   # protect bulk assignment of a structure component in
                                                                     # structure arrays
#    adjust the action for some files when using PSyclone 2.3.1,
if [ ${PSYCLONE_VERSION} == "2.3.1" ]; then
    [[ "${FILENAME}" == 'diu_layers.f90'        ]] && ACTION='EXCLUDE'
    [[ "${FILENAME}" == 'eosbn2.f90'            ]] && ACTION='EXCLUDE'
    [[ "${FILENAME}" == 'icedyn_rhg_eap.f90'    ]] && ACTION='TRANSFORM'
    [[ "${FILENAME}" == 'iom.f90'               ]] && ACTION='TRANSFORM'
    [[ "${FILENAME}" == 'isftbl.f90'            ]] && ACTION='EXCLUDE'
    [[ "${FILENAME}" == 'julian.f90'            ]] && ACTION='EXCLUDE'   # protect 'RECURSIVE' procedure prefix
    [[ "${FILENAME}" == 'lbcnfd.f90'            ]] && ACTION='EXCLUDE'
    [[ "${FILENAME}" == 'step.f90'              ]] && ACTION='EXCLUDE'   # protect 'RECURSIVE' procedure prefix (AGRIF)
    [[ "${FILENAME}" == 'storng.f90'            ]] && ACTION='EXCLUDE'
    [[ "${FILENAME}" == 'stpmlf.f90'            ]] && ACTION='EXCLUDE'   # protect 'RECURSIVE' procedure prefix (AGRIF)
    [[ "${FILENAME}" == 'timing.f90'            ]] && ACTION='EXCLUDE'   # protect 'RECURSIVE' procedure prefix
    [[ "${FILENAME}" == 'trosk.f90'             ]] && ACTION='TRANSFORM'
fi
#    and downgrade the transformation action to passthrough if the passthrough
#    mode is active.
[[ "${TPSYCLONE}" == 'passthrough' ]] && [[ ${ACTION} == 'TRANSFORM' ]] && ACTION='PASSTHROUGH'
#
# Exit if PSyclone path has not been set or 'psyclone' command is not available
if [[ "${PSYCLONE_PATH}" == "%PSYCLONE_HOME" ]] || [[ -z "${PSYCLONE_PATH}" ]] || [[ ! -x "${PSYCLONE_PATH}/bin/psyclone" ]]; then
    echo -n "sct_psyclone.sh failure: "
    [[ "${PSYCLONE_PATH}" == "%PSYCLONE_HOME" ]] && echo "PSyclone path (%PSYCLONE_HOME) is undefined"  && exit 1
    [[ "${PSYCLONE_PATH}" == "notdef"         ]] && echo "PSyclone path (%PSYCLONE_HOME) is undefined"  && exit 1
    [[ -z "${PSYCLONE_PATH}" ]]                  && echo "PSyclone path (%PSYCLONE_HOME) is empty"      && exit 1
    [[ ! -e "${PSYCLONE_PATH}/bin/psyclone" ]]   && echo "${PSYCLONE_PATH}/bin/psyclone not found"      && exit 1
    [[ ! -x "${PSYCLONE_PATH}/bin/psyclone" ]]   && echo "${PSYCLONE_PATH}/bin/psyclone not executable" && exit 1
fi
#
case ${ACTION} in
  TRANSFORM)   "${PSYCLONE_PATH}/bin/psyclone" -api nemo -l output -s "${BLD_DIR}/psct-${TPSYCLONE}.py" -oalg /dev/null -I "${BLD_DIR}/ppsrc/nemo/" \
                                               -opsy "${BLD_DIR}/obj/${FILENAME}" "${BLD_DIR}/ppsrc/nemo/${FILENAME}" ;;
  PASSTHROUGH) "${PSYCLONE_PATH}/bin/psyclone" -api nemo -l output -oalg /dev/null -I "${BLD_DIR}/ppsrc/nemo/" \
                                               -opsy "${BLD_DIR}/obj/${FILENAME}" "${BLD_DIR}/ppsrc/nemo/${FILENAME}" ;;
  EXCLUDE|*)   cp "${BLD_DIR}/ppsrc/nemo/${FILENAME}" "${BLD_DIR}/obj/${FILENAME}" ;;
esac
