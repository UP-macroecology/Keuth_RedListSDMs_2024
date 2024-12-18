#!/bin/bash

#SBATCH --job-name=simulations_dispersal
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=12
#SBATCH --time=96:00:00
#SBATCH --mem=100gb
#SBATCH --nodelist=ecoc9

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./02c_Simulation_extract_dispersal.R ./output-file-simlations_dispersal.Rout
