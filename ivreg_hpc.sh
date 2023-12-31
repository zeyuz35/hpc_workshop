#!/bin/env bash
#SBATCH --job-name=ivreg_hpc
#SBATCH --time=2:00:00
#SBATCH --mem-per-cpu=1024
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --array=1-27
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=ze-yu.zhong@monash.edu
#SBATCH --output=ivreg_output.txt
module unload gsl/2.2-system
module load gsl/2.7
module load R/4.2.2-mkl
export R_LIBS=~/R/libs:${R_LIBS}

R --vanilla < ivreg_hpc.R
