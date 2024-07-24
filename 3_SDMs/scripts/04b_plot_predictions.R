# Plot the predictions to see where the differences between the different algorithms occurr

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
ncores <- 48
cl <- makeCluster(ncores)
registerDoParallel(cl)

# Loops for the SDM fitting
foreach(sim_nr=1:nrow(sims), .packages = c("terra", "ggplot2", "gridExtra", "tidyterra")) %dopar% {
  
  # Prepare variables --------------
  rep_nr <- sims[sim_nr,]$land_rep
  optima <- sims[sim_nr,]$optima
  breadth <- sims[sim_nr,]$breadth
  BatchNum <- sims[sim_nr,]$BatchNum
  
  # Start loop for the 10 different replicates (fitting a SDM to each of them)
  for(replicate_nr in 1){#:length(replicates)
  
  load(paste0(sdm_dir, "predictions/Predictions_curr_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".RData"))
  
  # merge data sets
  all_bin <- merge(ens_preds_bin, all_preds_bin, by = c("x", "y"))
  all <- merge(ens_preds, all_preds, by = c("x", "y"))
    
  r_all_bin <- rast(all_bin)
  r_all <- rast(all)
  
  #Plot predictions
  pdf(paste0(sdm_dir, "predictions/prediction_maps/occurrences_SDMpredictions_BatchNum", BatchNum, "_land", rep_nr, "_Replication", replicates[replicate_nr], ".pdf"))
  
  g1 <- ggplot() +
    geom_spatraster(data = r_all) +
    facet_wrap(~lyr, ncol = 2) +
    scale_fill_whitebox_c(
      palette = "muted", limits = c(0,1)
    ) +
    labs(fill = "Occ. prob.")

 g2 <- ggplot() +
    geom_spatraster(data = r_all_bin) +
    facet_wrap(~lyr, ncol = 2) +
    scale_fill_whitebox_c(
      palette = "muted", limits = c(0,1)
    ) +
    labs(fill = "Occ. prob.")
 
  grid.arrange(g1)
  grid.arrange(g2)
 
 dev.off()

  }

} #close foreach loop
stopCluster(cl)

gc()
rm(list=ls())