#!/bin/bash

#SBATCH --job-name=3d_landscape_breadth
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=1
#SBATCH --time=96:00:00
#SBATCH --mem=250gb
#SBATCH --nodelist=ecoc9z

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./03d_Landscape_NicheBreadths.R ./output-file-d_landscape_breadth.Rout
