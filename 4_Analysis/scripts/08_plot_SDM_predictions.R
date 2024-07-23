#plotting predictions of SDMs

#load packages
library(terra)
library(ggplot2)
library(tidyterra)
library(gridExtra)
library(doParallel)
library(foreach)
library(data.table)

#define file path
sdm_dir <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/03_SDMs/")
sim_dir <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/02_Simulations/")

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
#replicates <- sample(1:100, 10)
replicates <- sample(0:99, 10)

#set up cluster
ncores <- 24
cl <- makeCluster(ncores)
registerDoParallel(cl)

# Start loops for the SDM fitting
foreach(sim_nr = 1:nrow(sims), .packages = c("terra", "tibble", "ggplot2", "gridExtra", "tidyterra", "data.table", "dplyr")) %dopar% {
  
  # Prepare variables --------------
  rep_nr <- sims[sim_nr,]$land_rep
  optima <- sims[sim_nr,]$optima
  breadth <- sims[sim_nr,]$breadth
  BatchNum <- sims[sim_nr,]$BatchNum
  
  # Start loop for the 10 different replicates (fitting a SDM to each of them)
  for(replicate_nr in 1:length(replicates)){

  #load data
    #load(paste0(path_batch, "Predictions_curr_list_Batch", BatchNum, "_Replication", replicates[replicate_nr], "_", timestamp, ".RData"))
    load(paste0(sdm_dir, "predictions/Predictions_fut_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".RData"))
    pop <- fread(paste0(sim_dir, "Outputs/Batch", BatchNum, "_Sim", rep_nr, "_Land1_Pop.txt"))
    
    #remove unimportant columns
    pop_short <- pop %>% dplyr::select(-c(RepSeason, Species))
    # extract occurrences
    occ_short <- subset(pop_short, pop_short$NInd >= 1)
    #change column names
    occ_short$x <- (occ_short$x + 0.5) * 1000
    occ_short$y <- (occ_short$y + 0.5) * 1000
    colnames(occ_short)[colnames(occ_short) == "x"] <- "X"
    colnames(occ_short)[colnames(occ_short) == "y"] <- "Y"
    # extract only the replication from the predictions
    occ_Rep <- subset(occ_short, occ_short$Rep == replicates[replicate_nr]-1)
    
    #transform predictions into raster
    rast_ens_fut_preds <- lapply(ens_fut_preds, function(x){x <- terra::rast(x); return(x)})
    
    # Occurrences for individuals without long dispersal
    pdf(paste0(sdm_dir, "results/occurrences_SDMpredictions_BatchNum", BatchNum, "_land", rep_nr, "_Replication", replicates[replicate_nr], ".pdf"))
    
    for(i in 1:length(rast_ens_fut_preds)){
      occ_sub <- subset(occ_Rep, occ_Rep$Year == i+100)
      if(nrow(occ_sub) > 0){
         p1 <- ggplot() +
       geom_spatraster(data = rast_ens_fut_preds[[i]]$mean_prob) +
          geom_point(data = occ_sub, aes(x=X, y=Y)) +
       scale_fill_whitebox_c(
         palette = "muted", limits = c(0,1)
       ) +
       labs(fill = "Occ. prob.")+
      ggtitle(paste0("Year ", i+100))
         grid.arrange(p1)
      } else {
        p1 <- ggplot() +
          geom_spatraster(data = rast_ens_fut_preds[[i]]$mean_prob) +
        scale_fill_whitebox_c(
          palette = "muted", limits = c(0,1)
        ) +
          labs(fill = "Occ. prob.")+
          ggtitle(paste0("Year ", i+100))
        grid.arrange(p1)
      }
    }
      dev.off()
  }
}
stopCluster(cl)
