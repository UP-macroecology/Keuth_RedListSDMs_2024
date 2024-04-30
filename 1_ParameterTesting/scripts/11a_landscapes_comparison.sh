#!/bin/bash

#SBATCH --job-name=11_landcomparison
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=5
#SBATCH --time=96:00:00
#SBATCH --mem=250gb
#SBATCH --nodelist=ecoc9

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./11a_landscapes_comparison.R ./output-file-11a_landscapes_comparison.Rout
