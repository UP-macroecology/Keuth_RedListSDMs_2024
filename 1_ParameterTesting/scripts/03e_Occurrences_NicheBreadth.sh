#!/bin/bash

#SBATCH --job-name=3e_occurrences_breadth
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=1
#SBATCH --time=96:00:00
#SBATCH --mem=250gb
#SBATCH --nodelist=ecoc9z

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./03e_Occurrences_NicheBreadth.R ./output-file-e_occurrences_breadth.Rout
