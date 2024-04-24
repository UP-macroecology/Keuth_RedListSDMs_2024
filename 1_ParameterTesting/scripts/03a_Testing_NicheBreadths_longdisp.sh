#!/bin/bash

#SBATCH --job-name=3a_testing_breadth_longdisp
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=6
#SBATCH --time=96:00:00
#SBATCH --mem=250gb
#SBATCH --nodelist=ecoc9z

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./03a_Testing_NicheBreadths_longdisp.R ./output-file-3a_testing_breadth_longdisp.Rout
