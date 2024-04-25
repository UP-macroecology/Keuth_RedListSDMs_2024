#!/bin/bash

#SBATCH --job-name=10a_longdisp0.8_land_breadth
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=5
#SBATCH --time=96:00:00
#SBATCH --mem=250gb
#SBATCH --nodelist=ecoc9

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./10a_longdisp0.8_NicheBreadths_landscapes.R ./output-file-10a_longdisp0.8_breadth_landscapes.Rout
