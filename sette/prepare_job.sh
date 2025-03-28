#####################################################
# Author : Simona Flavoni for NEMO
# Contact : sflod@locean-ipsl.upmc.fr
#
# ----------------------------------------------------------------------
# NEMO/SETTE , NEMO Consortium (2010)
# Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
# ----------------------------------------------------------------------
#
# Some scripts called by sette.sh
# prepare_job.sh   : creates the job script for running job 
######################################################
#set -vx
set -o posix
#set -u
#set -e
#+
#
# ================
# prepare_job.sh
# ================
#
# -------------------------------------------------
# script that creates the job script for NEMO tests 
# -------------------------------------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ ./prepare_job.sh INPUT_FILE_CONFIG_NAME NUMBER_PROC TEST_NAME MPI_FLAG JOB_FILE
#
#
# DESCRIPTION
# ===========
#
# Part of the SETTE package to run tests for NEMO
# 
# prepare the script $JOB_FILE to run the tests 
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./prepare_job.sh INPUT_FILE_CONFIG_NAME NUMBER_PROC TEST_NAME MPI_FLAG $JOB_FILE
#
# prepare the $JOB_FILE for execution 
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
# $Id: prepare_job.sh 3050 2011-11-07 14:11:34Z acc $
#
#
#
#   * creation
#
#-
#

usage=" Usage : ./prepare_job.sh INPUT_FILE_CONFIG_NAME NUMBER_PROC TEST_NAME MPI_FLAG JOB_FILE NUM_XIO_SERVERS"
usage=" example : ./prepare_job.sh input_ORCA2_ICE_PISCES.cfg 8 SHORT no/yes $JOB_FILE 0 2"


minargcount=6
        if [ ${#} -lt ${minargcount} ]
        then
                echo "not enough arguments for prepare_job.sh script"
                echo "control number of argument of prepare_job.sh in sette.sh"
                echo "${usage}"
        exit 1
        fi
        unset minargcount
	if [ ! -f ${SETTE_DIR}/output.sette ] ; then
	        touch ${SETTE_DIR}/output.sette
	fi
       
#
# set and export TEST_NAME. It will be used within the post_test_tidyup function
#
INPUTARFILE=$1
NB_PROC=$2
TEST_NAME=$3
MPI_FLAG=$4
JOB_FILE=$5
NXIO_PROC=$6
NEMO_VALID=$7

# export EXE_DIR. This directory is used to execute model 
#
#
#
echo "date: `date`" >> ${SETTE_DIR}/output.sette
echo "" >> ${SETTE_DIR}/output.sette
echo "running config: ${SETTE_CONFIG}" >> ${SETTE_DIR}/output.sette
echo "" >> ${SETTE_DIR}/output.sette
echo "list of cpp_keys: " >> ${SETTE_DIR}/output.sette
echo "`more ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/cpp_${SETTE_CONFIG}.fcm`" >> ${SETTE_DIR}/output.sette
echo "" >> ${SETTE_DIR}/output.sette
echo "compiling with: ${CMP_NAM}" >> ${SETTE_DIR}/output.sette
echo "" >> ${SETTE_DIR}/output.sette
echo "executing script : \"fcm_job $@\" " >> ${SETTE_DIR}/output.sette
echo "            " >> ${SETTE_DIR}/output.sette

################################################################
# SET INPUT 
# get the input tarfile if needed
if [ "$(cat ${SETTE_DIR}/$INPUTARFILE | wc -w)" -ne 0 ] ; then
   echo "looking for input files in ${SETTE_DIR}/$INPUTARFILE " >> ${SETTE_DIR}/output.sette
# number of tarfiles: NBTAR
   NBTAR=`cat ${SETTE_DIR}/$INPUTARFILE |wc -l` 
   echo "NB of tarfiles ${NBTAR} " >> ${SETTE_DIR}/output.sette
# loop on tarfiles
# read file name and directory
   while read tar_file dir_conf_forc 
   do
       echo looking for tarfile ${tar_file} and directory ${FORCING_DIR}/${dir_conf_forc}
       echo looking for tarfile ${tar_file} and directory ${FORCING_DIR}/${dir_conf_forc} >> ${SETTE_DIR}/output.sette
       if [ -d ${FORCING_DIR}/${dir_conf_forc} ] ; then
	   # input dir ar there, only check the links
           echo "input dir ar there, only check the links" >> ${SETTE_DIR}/output.sette
	   # extract tarfile
       else 	   
	   if [ ! -f ${FORCING_DIR}/${tar_file} ] ; then  
	       echo "tarfile  ${FORCING_DIR}/${tar_file} cannot be found we stop " ; exit 2 ; fi 

	   echo " extract from tarfile ${FORCING_DIR}/${tar_file} in  ${FORCING_DIR}/${dir_conf_forc}" >> ${SETTE_DIR}/output.sette

	   istgz=$( echo ${FORCING_DIR}/${tar_file} | grep -c "gz$" )
	   if [ $istgz -eq 1 ]
	   then
	       withdir=$( tar tfz ${FORCING_DIR}/${tar_file} | head -n 1  | grep -c "${dir_conf_forc}/$" )
	   else
	       withdir=$( tar tf  ${FORCING_DIR}/${tar_file} | head -n 1  | grep -c "${dir_conf_forc}/$" )
	   fi
	   if [ $withdir -eq 0 ]
	   then
	       mkdir ${FORCING_DIR}/${dir_conf_forc}  
	       cd    ${FORCING_DIR}/${dir_conf_forc}
	   else
	       cd    ${FORCING_DIR}
	   fi
	   if [ $istgz -eq 1 ]
	   then
	       tar xvfz ${FORCING_DIR}/${tar_file}
	   else
               tar xvf ${FORCING_DIR}/${tar_file}
	       [ $( ls -1 *gz 2>/dev/null | wc -l ) -gt 0 ] && gunzip -f *gz
	   fi
       fi
       # Tarfile and input dir ar there, only check the links
       cd ${FORCING_DIR}/${dir_conf_forc}
       for fida in *
       do
           [ -f ${EXE_DIR}/${fida} ] || ln -sf ${FORCING_DIR}/${dir_conf_forc}/${fida} ${EXE_DIR}/.
       done
   done < ${SETTE_DIR}/$INPUTARFILE
   
else
    echo "no input file to be searched "
fi
################################################################

################################################################
# RUN OPA
cd ${EXE_DIR}
if [ ! -r ${EXE_DIR}/nemo ]
    then
    echo "executable nemo does not exist"
    echo "executable nemo does not exist, exit"  >> ${SETTE_DIR}/output.sette
    exit 1
fi

# example for NOCS ClusterVision system using SLURM batch submission (requires ${SETTE_DIR}/sette_batch_template file)
#
  #  if [ ${MPI_FLAG} == "no" ] ; then
		case ${COMPILER} in 
			X64_MOBILIS*)
                                NB_REM=$( echo $NB_PROC $NXIO_PROC | awk '{print ( $1 + $2 ) % 16}')
		        	if [ ${NB_REM} == 0 ] ; then
					# number of processes required is an integer multiple of 16
					#
					NB_NODES=$( echo $NB_PROC $NXIO_PROC | awk '{print ($1 + $2 ) / 16}')
				else
					#
					# number of processes required is not an integer multiple of 16
					# round up the number of nodes required.
					#
					NB_NODES=$( echo $NB_PROC $NXIO_PROC | awk '{printf("%d",($1 + $2 ) / 16 + 1 )}')
	       			fi
				;;
			X86_ARCHER2-Cray*)
                                MK_TEMPLATE=$( /work/n01/shared/nemo/mkslurm_settejob_5.0 -S $NXIO_PROC -s 8 -m 4 -C $NB_PROC -g 2 -a n01-CLASS -j sette_job -t 20:00 > ${SETTE_DIR}/job_batch_template )
				;;
			X86_ARCHER2-Gnu*)
                                MK_TEMPLATE=$( /work/n01/shared/nemo/mkslurm_settejob_5.0_Gnu -S $NXIO_PROC -s 8 -m 4 -C $NB_PROC -g 2 -a n01-CLASS -j sette_job -t 20:00 > ${SETTE_DIR}/job_batch_template )
				;;
			ANEMONE-ifort*)
                                MK_TEMPLATE=$( /home/acc/mkslurm_settejob_5.0 -S $NXIO_PROC -s 8 -m 4 -C $NB_PROC -g 2 -j sette_job -t 20:00 > ${SETTE_DIR}/job_batch_template )
				;;
                        XC40_METO*) #Setup for Met Office XC40 with any compiler
                                # ocean cores are packed 32 to a node
                                # If we need more than one node then have to use parallel queue and XIOS must have a node to itself
                                NB_REM=$( echo $NB_PROC | awk '{print ( $1 % 32 ) }')
                                if [ ${NB_REM} == 0 ] ; then
                                        # number of processes required is an integer multiple of 32
                                        #
                                        NB_NODES=$( echo $NB_PROC $NXIO_PROC | awk '{print ($1) / 32}')
                                else
                                        #
                                        # number of processes required is not an integer multiple of 32
                                        # round up the number of nodes required.
                                        #
                                        NB_NODES=$( echo $NB_PROC $NXIO_PROC | awk '{printf("%d",($1) / 32 + 1 )}')
                                fi
                                # xios cores are sparsely packed at 4 to a node
                                if [ $NXIO_PROC == 0 ] ; then
                                    NB_XNODES=0
                                else
                                    NB_REM=$( echo $NXIO_PROC | awk '{print ( $1 % 4 ) }')
                                    if [ ${NB_REM} == 0 ] ; then
                                            # number of processes required is an integer multiple of 4                           
                                            #
                                            NB_XNODES=$( echo $NXIO_PROC | awk '{print (( $1 / 4 ) + 1)}') 
                                    else
                                            #
                                            # number of processes required is not an integer multiple of 4                             
                                            # round up the number of nodes required.
                                            #
                                            NB_XNODES=$( echo $NXIO_PROC | awk '{printf("%d",($1) / 4 + 1) }')                    
                                    fi
                                fi
                                if [ ${NB_XNODES} -ge 1 ] ; then
                                   NB_NODES=$((NB_NODES+NB_XNODES))
                                fi
                                echo NB_XNODES=${NB_XNODES} 
                                echo Total NB_NODES=${NB_NODES}
                                QUEUE=normal
                                SELECT="select=$NB_NODES"
                                module unload cray-snplauncher #Make sure snplauncher module is not loaded
                                ;;
                        X64_JEANZAY*) #Setup for Jean-Zay
                                export GROUP_IDRIS=`echo ${IDRPROJ}`
                                ;;
                        X64_KARA*)
                                NB_PROC_NODE=32
                                NB_NODES=$( echo $NB_PROC | awk '{print $1 - $1 % $NB_PROC_NODE }' | awk '{print $1 / $NB_PROC_NODE }' )
                                if [ ${NB_PROC} -le 128 ] ; then
                                      QUEUE=multi
                                fi
                                ;;
			*)
				NB_NODES=${NB_PROC}
				;;
		esac
#
# Pass settings into job file by using sed to edit predefined strings
#
        TOTAL_NPROCS=$(( $NB_PROC + $NXIO_PROC ))
        cat ${SETTE_DIR}/job_batch_template | sed -e"s/\(=\| \)NODES/\1${NB_NODES}/" \
             -e"s/TOTAL_NPROCS/${TOTAL_NPROCS}/" \
             -e"s/NPROCS/${NB_PROC}/" \
             -e"s/NXIOPROCS/${NXIO_PROC}/" \
             -e"s:DEF_SETTE_DIR:${SETTE_DIR}:" -e"s:DEF_INPUT_DIR:${INPUT_DIR}:" \
             -e"s:DEF_EXE_DIR:${EXE_DIR}:" \
             -e"s:DEF_CONFIG_DIR:${CMP_DIR:-${CONFIG_DIR0}}:" \
             -e"s:DEF_TOOLS_DIR:${TOOLS_DIR}:" \
             -e"s:MPI_FLAG:${MPI_FLAG}:" \
             -e"s:DEF_NEMO_VALIDATION:${NEMO_VALID}:" -e"s:DEF_NEW_CONF:${SETTE_CONFIG}:" \
             -e"s:DEF_CMP_NAM:${CMP_NAM}:" -e"s:DEF_TEST_NAME:${TEST_NAME}:" > run_sette_test.job

        case ${COMPILER} in
              X64_KARA*)
                    cat run_sette_test.job | sed -e"s/NPROC_NODE/${NB_PROC_NODE}/" \
                                                 -e"s:QUEUE:${QUEUE}:" > run_sette_test1.job
                    mv run_sette_test1.job run_sette_test.job
                    ;;
              XC40_METO*)
                    cat run_sette_test.job | sed -e"s/SELECT/${SELECT}/" > run_sette_test1.job
                    mv run_sette_test1.job run_sette_test.job
                    ;;
              X64_JEANZAY*)
                    cat run_sette_test.job | sed -e"s/GROUP_IDRIS/${GROUP_IDRIS}/" > run_sette_test1.job
                    mv run_sette_test1.job run_sette_test.job
                    ;;
	esac
#
# create the unique submission job script
#
	if [ ! -f $JOB_FILE ] ; then
		mv run_sette_test.job $JOB_FILE
	else
	    e=`grep -n "# END_BODY" ${JOB_FILE} | cut -d : -f 1`
            e=$(($e - 1))
	    head -$e $JOB_FILE > ${JOB_FILE}_new 
	    mv ${JOB_FILE}_new ${JOB_FILE}
	    l=`wc -l run_sette_test.job | sed -e "s:run_sette_test.job::"`
	    b=`grep -n "# BODY" run_sette_test.job | cut -d : -f 1`
	    t=$(($l - $b))
	    tail -$t run_sette_test.job >> $JOB_FILE
	fi
	
	chmod a+x $JOB_FILE ; echo "$JOB_FILE is ready"

#fi
