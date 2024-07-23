#!/bin/bash

#SBATCH --job-name=plot_predictions
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=24
#SBATCH --time=24:00:00
#SBATCH --mem=600gb
#SBATCH --nodelist=ecoc9

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./08_plot_SDM_predictions.R ./output-file-plot_predictions.Rout
