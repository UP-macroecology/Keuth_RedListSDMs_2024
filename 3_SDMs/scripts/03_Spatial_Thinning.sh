#!/bin/bash

#SBATCH --job-name=thinning
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=48
#SBATCH --time=168:00:00
#SBATCH --mem=200gb
#SBATCH --nodelist=ecoc9z

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./03_Spatial_Thinning.R ./output-file-thinning.Rout
