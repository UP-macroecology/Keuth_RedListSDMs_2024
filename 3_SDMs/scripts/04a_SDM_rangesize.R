# Calcualte the change in range size
# Using the SDM results to calculate the change in range size instead of the change in habitat suitability sums

# load packages
library(doParallel)
library(foreach)
library(data.table)

#define file path
sdm_dir <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/03_SDMs/")

# create data frame with all parameter combinations
land_rep <- 1:3
optima <- c(0.27, 0.5)
breadth <- c(0.045, 0.055)
rmax <- c(3, 5)
dispersal <- c(5000, 15000)

sims <- expand.grid(land_rep = land_rep, optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims$BatchNum <- rep(1:16, each = 3)

# select 10 random replications from the 100
set.seed(8765)
replicates <- sample(0:99, 10)

#set up cluster
ncores <- 24
cl <- makeCluster(ncores)
registerDoParallel(cl)

#For loop for every scenario and landscape replication
foreach(sim_nr=1:nrow(sims), .packages = c("raster", "maxnet", "gbm", "dplyr", "tibble", "terra", "randomForest", "mecofun", "data.table")) %dopar% {
  
  # Prepare variables --------------
  rep_nr <- sims[sim_nr,]$land_rep
  optima <- sims[sim_nr,]$optima
  breadth <- sims[sim_nr,]$breadth
  BatchNum <- sims[sim_nr,]$BatchNum
  
  #prepare objects
  range_size <- vector("list", length = length(replicates))
  
  # Start loop for the 10 different replicates
  for(replicate_nr in 1:length(replicates)){
    
    #Load data
    load(paste0(sdm_dir, "predictions/Predictions_curr_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".RData"))
    load(paste0(sdm_dir, "predictions/Predictions_fut_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".RData"))
    
    #Calculate current range size
    range_loop <- length(which(ens_preds_bin$mean_prob == 1))
    
    #Calculate future range size
    for(year_nr in 1:length(ens_fut_preds_bin)){
      range_loop <- append(range_loop, length(which(ens_fut_preds_bin[[year_nr]]$mean_prob == 1)))
    }
    
    # add range size calculation to list
    range_size[[replicate_nr]] <-range_loop
    
  }
  
  saveRDS(range_size, file = paste0(sdm_dir, "results/range_size_SDM_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  
} #close foreach loop
stopCluster(cl)

gc()
rm(list=ls())
