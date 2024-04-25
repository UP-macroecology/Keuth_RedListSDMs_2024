#!/bin/bash

#SBATCH --job-name=10d_longdisp_land_breadth
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=5
#SBATCH --time=96:00:00
#SBATCH --mem=250gb
#SBATCH --nodelist=ecoc9

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./10d_longdisp_NicheBreadths_landscapes.R ./output-file-10d_longdisp_breadth_landscapes.Rout
