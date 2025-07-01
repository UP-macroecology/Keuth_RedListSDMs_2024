#!/bin/bash

#SBATCH --job-name=MW_ExtProb
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=1
#SBATCH --time=24:00:00
#SBATCH --mem=600gb
#SBATCH --nodelist=ecoc9

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./09a_MW_Extinctionprob.R ./output-file-MW_ExtProb.Rout
