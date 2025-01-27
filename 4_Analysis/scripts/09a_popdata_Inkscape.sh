#!/bin/bash

#SBATCH --job-name=popdata_Inkscape
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=16
#SBATCH --time=24:00:00
#SBATCH --mem=100gb
#SBATCH --nodelist=ecoc9

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./09a_popdata_Inkscape.R ./output-file-popdata_Inkscape.Rout
