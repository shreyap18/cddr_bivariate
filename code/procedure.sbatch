#!/bin/bash
#SBATCH --job-name myjob        		   # Set a name for your job. This is especially useful if you have multiple jobs queued.
#SBATCH --partition short       		   # Slurm partition to use
#SBATCH --ntasks 50            		   # Number of tasks to run. By default, one CPU core will be allocated per task
#SBATCH --time 0-11:00            		   # Wall time limit in D-HH:MM
#SBATCH --mem-per-cpu=300       		   # Memory limit for each tasks (in MB)
#SBATCH -o out/myscript_%j.out    # File to which STDOUT will be written
#SBATCH -e out/myscript_%j.err    # File to which STDERR will be written
#SBATCH --mail-type=ALL       			   # Type of email notification- NONE,BEGIN,END,FAIL,ALL
#SBATCH --mail-user=shreyap1@uw.edu        # Email to which notifications will be sent
 
source /etc/profile.d/z00_lmod.sh
module load R
Rscript run_procedure.R $S
