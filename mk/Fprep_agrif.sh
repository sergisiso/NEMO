#!/bin/bash
#set -x
set -o posix
#set -u
#set -e
#+
#
# ==============
# Fprep_agrif.sh
# ==============
#
# ---------------------
# Preparation for AGRIF
# ---------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Fprep_agrif.sh
#
#
# DESCRIPTION
# ===========
#
#
# Prepare directories for AGRIF and copy files needed
#
# Compile the conv
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fprep_agrif.sh CONFIG_NAME
#
#
# TODO
# ====
#
# option debug
#
#
# EVOLUTIONS
# ==========
#
# $Id: Fprep_agrif.sh 14230 2020-12-20 12:46:41Z smasson $
#
#
#
#   * creation
#
#-

#- AGRIF conv

#-MPI for AGRIF
if [ ! -f ${1}/ext/AGRIF/nemo_mpi.h ]; then
   echo '#if ! defined key_mpi_off' > ${1}/ext/AGRIF/nemo_mpi.h
   echo '#define AGRIF_MPI'        >> ${1}/ext/AGRIF/nemo_mpi.h
   echo '#endif'                   >> ${1}/ext/AGRIF/nemo_mpi.h
fi

#- AGRIF sources
mkdir -p ${2}/NEMOFILES
