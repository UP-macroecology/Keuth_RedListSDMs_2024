#!/bin/bash

#SBATCH --job-name=modelsummary
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=4
#SBATCH --time=48:00:00
#SBATCH --mem=100gb
#SBATCH --nodelist=ecoc9

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./Model_table.R ./output-file-modeltable.Rout
