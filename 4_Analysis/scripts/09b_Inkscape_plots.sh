#!/bin/bash

#SBATCH --job-name=Inkscape_plots
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=16
#SBATCH --time=24:00:00
#SBATCH --mem=100gb
#SBATCH --nodelist=ecoc9z

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./09b_Inkscape_plots.R ./output-file-Inkscape_plots.Rout
