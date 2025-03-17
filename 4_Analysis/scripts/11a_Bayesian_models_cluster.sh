#!/bin/bash

#SBATCH --job-name=Bayesian_model
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=4
#SBATCH --time=120:00:00
#SBATCH --mem=100gb
#SBATCH --nodelist=ecoc9z

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./11a_Bayesian_models_cluster.R ./output-file-Bayesian_model.2.Rout
