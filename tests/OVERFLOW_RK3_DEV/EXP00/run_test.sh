#!/bin/bash
#SBATCH --job-name=sette_job
#SBATCH --partition=compute
#SBATCH --time=40:00

#SBATCH --nodes=1
#SBATCH --ntasks=5
#SBATCH --ntasks-per-core=1
#SBATCH --exclusive

    # Created by: mkslurm_settejob_4.2 -S 4 -s 8 -m 4 -C 1 -g 2 -N 64 -t 20:00 -a n01 -j sette_job -v False
    # set programming environment and XIO_HOME 
    # failed to improve anything: module swap cce/11.0.4 cce/12.0.3

    module load HDF5/1.12.1-iimpi-2021b     
    module load Szip/2.1.1-GCCcore-11.2.0   
    module load netCDF/4.8.1-iimpi-2021b
    module load netCDF-Fortran/4.5.3-iimpi-2021b

    module load impi/2021.4.0-intel-compilers-2021.4.0  
    module load intel-compilers/2021.4.0      
    module load iimpi/2021b                             
    module load slurm/21.08.5
    module load lmod

    export XIO_HOME=/home/acc/xios-trunk
    #
    export OMP_NUM_THREADS=1
    #export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$LIBRARY_PATH
    export I_MPI_SHM=off
    export I_MPI_PIN_RESPECT_CPUSET=0
    export I_MPI_PMI_LIBRARY=/opt/software/slurm/21.08.5/lib/libpmi2.so

    export OCORES=1
    export XCORES=4
    export SETTE_DIR=/dssgfs01/scratch/acc/branches/dev_zad_Aimp_RK3/nemo/sette
    export FORT_FMT_RECL=256
    #
    # load sette functions (only post_test_tidyup needed)
    #
    . ${SETTE_DIR}/all_functions.sh
    ###############################################################
    #
    # set up mpp computing environment
    #
    #
    # Local settings. These settings are for a particular machine ( ARCHER2 )
    # at a particular stage of that machine's evolution. This template file
    # is provided for illustration purposes only and will not work on any other machine. There
    # should, however, be sufficient similarity with other MPP platforms and batch systems
    # for this example to provide a useful guide for experienced users
    #
    # Don't remove neither change the following line
    # BODY
    #
    # Test specific settings. Do not hand edit these lines; the fcm_job.sh script will set these
    # (via sed operating on this template job file). Note that the number of compute nodes required
    # is also set by the fcm_job.sh on the PBS select header line above.
    #
    # These variables are needed by post_test_tidyup function in all_functions.sh
    #
    export INPUT_DIR=//EXP00
    export CONFIG_DIR=/dssgfs01/scratch/acc/branches/dev_zad_Aimp_RK3/nemo/tests
    export TOOLS_DIR=/dssgfs01/scratch/acc/branches/dev_zad_Aimp_RK3/nemo/tools
    export NEMO_VALIDATION_DIR=/dssgfs01/scratch/acc/branches/dev_zad_Aimp_RK3/nemo/sette/NEMO_VALIDATION/63-zad_Aimp-with-rk3/ANEMONE-ifort/22168_0e782f9+/OVERFLOW/LONG
    export NEW_CONF=OVERFLOW_RK3_DEV
    export CMP_NAM=ANEMONE-ifort
    export TEST_NAME=LONG
    export EXE_DIR=/dssgfs01/scratch/acc/branches/dev_zad_Aimp_RK3/nemo/tests/OVERFLOW_RK3_DEV/EXP00
    #
    # end of set up
    ###############################################################
    #
    # change to the working directory 
    #
    cd $EXE_DIR
    echo Directory is `pwd`
#add comma after logical
for nml in `find ./ -name '*namelist_*'`
do
chk=$(grep -c -i -e"= *.false.," -e"= *.true.," $nml)
if test $chk -eq 0 ; then
echo "Changing : "$nml
sed -e's/=\( *\).false./=\1.false.,/' \
    -e's/=\( *\).FALSE./=\1.FALSE.,/' \
    -e's/=\( *\).true./=\1.true.,/' \
    -e's/=\( *\).TRUE./=\1.TRUE.,/' $nml > $nml.$$
mv $nml.$$ $nml
else
echo $nml " may have already been processed: "$chk" lines already correct"
fi
done
#end add comma after logical
    if [ $XCORES -gt 0 ]; then
       if [ ! -f ./xios_server.exe ] && [ -f ${XIO_HOME}/bin/xios_server.exe ]; then
          cp ${XIO_HOME}/bin/xios_server.exe .
       fi
       if [ ! -f ./xios_server.exe ]; then
          echo "./xios_server.exe not found"
          echo "run aborted"
          exit
       fi
    fi

cat > myscript_wrapper.sh << EOFB
#!/bin/ksh
#
set -A map ./xios_server.exe ./nemo
exec_map=( 0 0 0 0 1 )
#
exec \${map[\${exec_map[\$SLURM_PROCID]}]}
##
EOFB
chmod u+x ./myscript_wrapper.sh

srun --mem-bind=local --mpi=pmi2 --hint=nomultithread \
--ntasks=5  --cpu-bind=v,mask_cpu:0x1,0x100,0x10000,0x1000000,0x100000000 ./myscript_wrapper.sh
#
# END_BODY
# Don't remove neither change the previous line
exit

