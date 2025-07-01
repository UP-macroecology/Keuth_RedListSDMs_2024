#!/bin/bash

#SBATCH --job-name=create_data_dispersal
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=1
#SBATCH --time=24:00:00
#SBATCH --mem=100gb
#SBATCH --nodelist=ecoc9z

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./06a_dispersal_assumptions_dataset.R ./output-file-create_dataset_dispersal.Rout
