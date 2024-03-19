#!/bin/bash

#SBATCH --job-name=simulation_doublekernel
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=1
#SBATCH --time=96:00:00
#SBATCH --mem=150gb
#SBATCH --nodelist=ecoc9z

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./simulation_doublekernel.R ./output-file-simulation_doublekernel.Rout
