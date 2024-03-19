#!/bin/bash

#SBATCH --job-name=landscape_cc
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=1
#SBATCH --time=96:00:00
#SBATCH --mem=150gb
#SBATCH --nodelist=ecoc9z

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./01_CreateLandscape.R ./output-file-landscape_cc.Rout
