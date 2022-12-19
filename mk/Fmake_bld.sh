#!/bin/bash
#set -x
set -o posix
#set -u
#set -e
#+
#
# ============
# Fmake_bld.sh
# ============
#
# --------------------
# Make build directory
# --------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Fmake_bld.sh
#
#
# DESCRIPTION
# ===========
#
#
# Under CONFIG_NAME :
# - Make the build directory 
# - Create repositories needed :
# - BLD for compilation 
#
# A tmpdir can be specified for memory issues.
#
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fmake_bld.sh NEMOGCM/cfgs GYRE  /usr/tmp
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
# $Id: Fmake_bld.sh 9651 2018-05-28 06:47:14Z nicolasmartin $
#
#
#
#   * creation
#
#-
[ ! -d ${3}/${2}     ] && \mkdir -p ${3}/${2}
[ ! -d ${3}/${2}/BLD ] && \mkdir -p ${3}/${2}/BLD
[ -f ${3}/${2}/cpp_${2}.fcm ] && ln -sf ${3}/${2}/cpp_${2}.fcm ${3}/${2}/BLD/cpp.fcm
rm -f  ${1}/${2}/BLD/fcm.bld.lock
