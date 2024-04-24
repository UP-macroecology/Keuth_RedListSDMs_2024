#!/bin/bash

#SBATCH --job-name=testing_breadth_shortdisp
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=6
#SBATCH --time=96:00:00
#SBATCH --mem=250gb
#SBATCH --nodelist=ecoc9z

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./03_Testing_NicheBreadths_shortdisp.R ./output-file-3_testing_breadth_shortdisp.Rout
