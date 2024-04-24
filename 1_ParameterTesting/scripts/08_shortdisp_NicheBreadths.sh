#!/bin/bash

#SBATCH --job-name=8_shortdisp_breadth
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=5
#SBATCH --time=96:00:00
#SBATCH --mem=250gb
#SBATCH --nodelist=ecoc9z

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./08_shortdisp_NicheBreadths.R ./output-file-8_shortdisp_breadth.Rout
