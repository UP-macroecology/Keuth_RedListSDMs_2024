#!/bin/bash

#SBATCH --job-name=7_landscape_breadth
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=5
#SBATCH --time=96:00:00
#SBATCH --mem=250gb
#SBATCH --nodelist=ecoc9z

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./07_landscapes_NicheBreadths.R ./output-file-7_landscapes_breadth.Rout
