#!/bin/bash
#
######
# Keith (muggle) to run this whole thing go to directory where the files are and type at the promt "./run_simulations.sh", dont type qsub you fool, 
# Keith (muggle) if bash comes back with permision denied type at the promt "chmod 755" to get permision
# Name:        run_simulations.sh
# Description: master script to
#              1. create temporary working directory
#              2. submits apsim simulations to cluster 
#              3. submits r testing of simulation run results
#              4. cleanup temporary working directory
#
######

#set up the R scripts with python
#NOTE: this line was changed
module load python/3.8.8-gcc8.3.1-2un
python generate_R_scripts.py


# get number of files for pbs array jobs below
num_apsim_input_files=`find . -name "*.apsim" -ls|wc -l`
num_r_input_files=`find . -name "*.R" -ls|wc -l`

# setup temporary working directory
run_sims_setupnum=`qsub run_simulation_setup.pbs`

# submit apsim simulations to cluster
if [ $num_apsim_input_files -gt 1 ]; then
   run_sims_arraynum=`qsub -J 1-${num_apsim_input_files} -W depend=afterany:${run_sims_setupnum} -v TMP_WORKDIR=${run_sims_setupnum} run_simulations.pbs`
else
   run_sims_arraynum=`qsub -W depend=afterany:${run_sims_setupnum} -v TMP_WORKDIR=${run_sims_setupnum} run_simulations.pbs`
fi

if [ $num_r_input_files -gt 1 ]; then

   # submit apsim simulations results to cluster for testing
   run_tests_arraynum=`qsub -J 1-${num_r_input_files} -W depend=afterany:${run_sims_arraynum}:${run_sims_setupnum} -v TMP_WORKDIR=${run_sims_setupnum} run_simulations_tests.pbs`
   
   # cleanup simulation temporary directory
   qsub  -W depend=afterany:${run_sims_arraynum}:${run_tests_arraynum}:${run_sims_setupnum} -v TMP_WORKDIR=${run_sims_setupnum} run_simulation_cleanup.pbs

else
   run_tests_arraynum=`qsub -W depend=afterany:${run_sims_setupnum}:${run_sims_arraynum} -v TMP_WORKDIR=${run_sims_setupnum} run_simulations_tests.pbs`
   # if there are no R tests to be run just
   # cleanup simulation temporary directory
   qsub  -W depend=afterany:${run_sims_arraynum}:${run_sims_setupnum}:${run_tests_arraynum} -v TMP_WORKDIR=${run_sims_setupnum} run_simulation_cleanup.pbs

fi

#
# The End
