#!/bin/bash

#SBATCH --job-name=SDMs_predictions
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=48
#SBATCH --time=24:00:00
#SBATCH --mem=50gb
#SBATCH --nodelist=ecoc9z

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./04b_plot_predictions.R ./output-file-SDM_predictions.Rout
