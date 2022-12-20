#!/bin/bash
#set -x
set -o posix
#set -u
#set -e
#+
#
# ===============
# Fmake_config.sh
# ===============
#
# ---------------
# Make the config 
# ---------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Fmake_config.sh
#
#
# DESCRIPTION
# ===========
#
#
# - Make the config directory 
# - Create repositories needed :
#  
#  - EXP00 for namelist
#  - MY_SRC for user sources
#  - BLD for compilation 
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fmake_config.sh CONFIG_NAME REF_CONFIG_NAME 
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
# $Id: Fmake_config.sh 9719 2018-05-31 15:57:27Z nicolasmartin $
#
#
#
#   * creation
#
#-
\mkdir -p ${1}
\mkdir -p ${1}/EXP00
\mkdir -p ${1}/MY_SRC
[ "${2}/cpp_${2##*/}.fcm" != "${1}/cpp_${1##*/}.fcm" ] && \cp -R -n ${2}/cpp_${2##*/}.fcm ${1}/cpp_${1##*/}.fcm
for f in ${2}/EXPREF/*.xml ${2}/EXPREF/*namelist*; do
  if [[ -L ${f} && $(readlink -f ${f}) =~ "SHARED" ]]; then ln -sf $(readlink -f ${f}) ${1}/EXP00/$(basename ${f}); else cp -R -n ${f} ${1}/EXP00/.; fi
done
[ -f ${2}/EXPREF/AGRIF_FixedGrids.in ] && \cp -a -n ${2}/EXPREF/AGRIF_FixedGrids.in ${1}/EXP00/.
[ -d    ${2}/MY_SRC ] && \cp -an ${2}/MY_SRC/* ${1}/MY_SRC/. 2> /dev/null
