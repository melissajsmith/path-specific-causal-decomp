#!/bin/bash
#
#SBATCH --job-name=run_one_simulation
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16GB
#SBATCH --partition=short
#SBATCH --time=011:00:00
#SBATCH --error=simError.err
#SBATCH --output=simOutput.out
#SBATCH --array=1-1000

module load GSL/2.7-GCC-10.3.0
module load MPFR/4.1.0-GCCcore-11.2.0
module load R/4.1.0-foss-2018a-X11-20180131-bare
srun R CMD BATCH --no-save --no-restore '--args 16' 'runOneIterationSimulation.R'