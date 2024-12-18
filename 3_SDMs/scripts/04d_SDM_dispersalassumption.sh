#!/bin/bash

#SBATCH --job-name=SDM_dispersalassumptions
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=12
#SBATCH --time=90:00:00
#SBATCH --mem=150gb
#SBATCH --nodelist=ecoc9

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./04d_SDM_dispersalassumption.R ./output-file-SDM_dispersal_assumption.Rout
