#!/bin/bash

#SBATCH --job-name=SDM_dispersalassumptions
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=1
#SBATCH --time=24:00:00
#SBATCH --mem=50gb
#SBATCH --nodelist=ecoc9

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./04d_SDM_dispersalassumption.R ./output-file-SDM_dispersal_assumption.Rout
