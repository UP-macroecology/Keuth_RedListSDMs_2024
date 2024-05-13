#!/bin/bash

#SBATCH --job-name=landscapes_cc
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=3
#SBATCH --time=96:00:00
#SBATCH --mem=150gb
#SBATCH --nodelist=ecoc9z

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./01_CreateLandscapes.R ./output-file-landscapes_cc.Rout
