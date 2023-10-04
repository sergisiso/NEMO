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
#    but downgrade to passthrough if the passthrough mode is active,
[[ "${TPSYCLONE}" == 'passthrough' ]] && [[ ${ACTION} == 'TRANSFORM' ]] && ACTION='PASSTHROUGH'
#    and explictely disable any processing of files that after having passed
#    through PSyclone version 2.3.1 would cause a compilation or run-time
#    failure
[[ "${FILENAME}" == 'diaptr.f90'            ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'diu_layers.f90'        ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'eosbn2.f90'            ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'icblbc.f90'            ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'iceistate.f90'         ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'icethd_do.f90'         ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'icewri.f90'            ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'isftbl.f90'            ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'isfpar.f90'            ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'lbcnfd.f90'            ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'mpp_map.f90'           ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'nemogcm.f90'           ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'obs_mpp.f90'           ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'p4zpoc.f90'            ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'storng.f90'            ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'sbc_phy.f90'           ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'sbcblk.f90'            ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'sbcblk_skin_coare.f90' ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'sbcdcy.f90'            ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'timing.f90'            ]] && ACTION='EXCLUDE'
[[ "${FILENAME}" == 'vremap.f90'            ]] && ACTION='EXCLUDE'
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
