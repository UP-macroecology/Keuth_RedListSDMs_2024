#!/bin/bash

#SBATCH --job-name=thinning
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=24
#SBATCH --time=96:00:00
#SBATCH --mem=400gb
#SBATCH --nodelist=ecoc9z

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./03_Spatial_Thinning.R ./output-file-thinning.Rout
