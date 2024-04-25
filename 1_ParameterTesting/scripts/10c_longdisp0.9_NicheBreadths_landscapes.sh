#!/bin/bash

#SBATCH --job-name=10c_longdisp0.9_land_breadth
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=5
#SBATCH --time=96:00:00
#SBATCH --mem=250gb
#SBATCH --nodelist=ecoc9

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./10c_longdisp0.9_NicheBreadths_landscapes.R ./output-file-10c_longdisp0.9_breadth_landscapes.Rout
