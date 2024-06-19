#!/bin/bash

#SBATCH --job-name=SDMs
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=24
#SBATCH --time=130:00:00
#SBATCH --mem=200gb
#SBATCH --nodelist=ecoc9

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./04_SDM.R ./output-file-SDM.Rout
