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
# PSyclone version 2.5.0 (default) or 2.4.0
PSYCLONE_VERSION="2.5.0"
# Path to PSyclone installation
PSYCLONE_PATH=$1
# Transformation or 'passthrough'
TPSYCLONE=$2
# Configuration directory
BLD_DIR=$3
# Input file
FILENAME=$(basename "$4")

if [[ "${PSYCLONE_PATH}" == "%PSYCLONE_HOME" ]] || [[ -z "${PSYCLONE_PATH}" ]] || [[ ! -x "${PSYCLONE_PATH}/bin/psyclone" ]]; then
    echo -n "sct_psyclone.sh failure: "
    [[ "${PSYCLONE_PATH}" == "%PSYCLONE_HOME" ]] && echo "PSyclone path (%PSYCLONE_HOME) is undefined"  && exit 1
    [[ "${PSYCLONE_PATH}" == "notdef"         ]] && echo "PSyclone path (%PSYCLONE_HOME) is undefined"  && exit 1
    [[ -z "${PSYCLONE_PATH}" ]]                  && echo "PSyclone path (%PSYCLONE_HOME) is empty"      && exit 1
    [[ ! -e "${PSYCLONE_PATH}/bin/psyclone" ]]   && echo "${PSYCLONE_PATH}/bin/psyclone not found"      && exit 1
    [[ ! -x "${PSYCLONE_PATH}/bin/psyclone" ]]   && echo "${PSYCLONE_PATH}/bin/psyclone not executable" && exit 1
fi
psyclone -l output -s "${TPSYCLONE}" -I "${BLD_DIR}/ppsrc/nemo/" \
    -o "${BLD_DIR}/obj/${FILENAME}" "${BLD_DIR}/ppsrc/nemo/${FILENAME}"
