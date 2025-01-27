#!/bin/bash

#SBATCH --job-name=Cluster_clean
#SBATCH --mail-type=ALL
#SBATCH --mail-user=keuth@uni-potsdam.de
#SBATCH --cpus-per-task=48
#SBATCH --time=24:00:00
#SBATCH --mem=300gb
#SBATCH --nodelist=ecoc9z

cd ${SLURM_SUBMIT_DIR}
R CMD BATCH ./Cluster_clean.R ./output-file-cluster_clean.Rout
