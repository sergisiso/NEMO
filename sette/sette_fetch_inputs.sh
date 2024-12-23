#!/bin/bash -f
# set -vx
# SETTE input file importer
#
# This version should be run in the SETTE directory. 
# The location of the FORCING_DIR will be retrieved from the param.cfg file
#
#########################################################################################
#
# LOAD param variable ( only FORCING_DIR required)
  . ./param.cfg
  if [ $# -gt 0 ]; then
    while getopts lh option ; do 
       case $option in
          h | *) echo ''
                 echo 'sette_fetch_inputs.sh : ' 
                 echo '     Fetch 5.0.0 input files from remote store'
                 exit 42;;
       esac
    done
    shift $((OPTIND - 1))
  fi
# 
  if [ ! -d $FORCING_DIR ] ; then
   while true; do
       read -p "$FORCING_DIR does not exist. Do you wish to create it? " yn
       case $yn in
           [Yy]* ) mkdir -p $FORCING_DIR ; break;;
           [Nn]* ) echo "Ok, exiting instead"; exit;;
           * ) echo "Please answer yes or no.";;
       esac
   done
  fi
#
  orgdir=`pwd`
  cd ${FORCING_DIR}
#
  suff="5.0.0"
#
  for file in AGRIF_DEMO AMM12 C1D ICE_AGRIF ISOMIP+ ORCA2_ICE ORCA2_OFF SAS WED025 ORCA2_ABL
  do
    full_file=${file}_v${suff}.tar.gz
    if [ ! -f $full_file ] ; then
        wget "https://gws-access.jasmin.ac.uk/public/nemo/sette_inputs/r${suff}/$full_file"
    else
       echo $full_file "already exist. Delete and re-run to fetch a fresh copy"
    fi
  done
#
  cd $orgdir
#
exit
