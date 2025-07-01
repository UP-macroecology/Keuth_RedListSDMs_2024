#!/bin/bash

#SBATCH --job-name=plot_landscapes
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=1
#SBATCH --time=24:00:00
#SBATCH --mem=100gb
#SBATCH --nodelist=ecoc9z

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./plot_landscapes.R ./output-file-plot_landscapes.Rout
