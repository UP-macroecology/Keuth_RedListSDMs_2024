#!/bin/bash

#SBATCH --job-name=simulations
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=10
#SBATCH --time=96:00:00
#SBATCH --mem=400gb
#SBATCH --nodelist=ecoc9

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./02_Simulations.R ./output-file-simlations.Rout
