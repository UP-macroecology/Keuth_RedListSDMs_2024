#!/bin/bash

#SBATCH --job-name=6_landscape_breadth
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=1
#SBATCH --time=96:00:00
#SBATCH --mem=250gb
#SBATCH --nodelist=ecoc9z

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./06_Plot_Landscape_NicheBreadths.R ./output-file-6_plot_landscape_breadth.Rout
