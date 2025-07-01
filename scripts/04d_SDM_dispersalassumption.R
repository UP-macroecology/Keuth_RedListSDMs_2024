# test using a realistic dispersal assumption for SDMs

# Loading packages
library(terra)
library(data.table)
library(foreach)
library(doParallel)

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

# obtain the 10 random replications from the 100
set.seed(8765)
replicates <- sample(0:99, 10)

# load in the dispersal assumption values
#load(paste0(sdm_dir, "data/values_dispersal_assumption.Rdata"))

#set up cluster
ncores <- 48
cl <- makeCluster(ncores)
registerDoParallel(cl)

# foreach loop through every single scenario
foreach(sim_nr=1:nrow(sims), .packages = c("terra", "data.table")) %dopar% {
#for(sim_nr in 27){
  
  # Prepare variables --------------
  rep_nr <- sims[sim_nr,]$land_rep
  optima <- sims[sim_nr,]$optima
  breadth <- sims[sim_nr,]$breadth
  BatchNum <- sims[sim_nr,]$BatchNum
  dispersal <- sims[sim_nr,]$dispersal
  
  # Load all individual dispersal files
  # vec_distances <- c()
  # for (replicate in 0:99) {
  #   BatchNum_disp <- BatchNum + 16
  #   dist <- read.table(paste0(sim_dir, "Outputs/Batch", BatchNum_disp, "_Sim", rep_nr, "_Land1_Rep", replicate, "_Inds.txt"), header = T, sep = "\t")
  #   dist <- subset(dist, dist$Year == 99)
  #   vec_distances <- append(vec_distances, dist$DistMoved)
  # }

  #save(vec_distances, file = paste0(sdm_dir, "results/vec_distances_ Batch", BatchNum, "_Sim", rep_nr, ".Rdata"))
  load(paste0(sdm_dir, "results/vec_distances_ Batch", BatchNum, "_Sim", rep_nr, ".Rdata"))
  #load(paste0(sdm_dir, "results/vec_distances.Rdata"))
  
  # remove the 0s out of the data set
  vec_distances <- vec_distances[!vec_distances == 0]
  
  mean_dist <- mean(vec_distances)
  
  #hs_list_median <- vector("list", length = length(replicates))
  #hs_list_quant <- vector("list", length = length(replicates))
  hs_list_mean <- vector("list", length = length(replicates))

  # loop for every replicated run
  for (replicate_nr in 1:10) {

    # Create data set for the final hs values
    # df_median <- data.frame(startYear = 0:79, hs_startYear = NA, hs_plus10 = NA)
    # df_quant <- data.frame(startYear = 0:79, hs_startYear = NA, hs_plus10 = NA)
    df_mean <- data.frame(startYear = 0:79, hs_startYear = NA, hs_plus10 = NA)

    # load the already calculated habitat suitabilities
    habitat <- readRDS(paste0(sdm_dir, "results/habitat_suitability_SDM_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
    habitat <- habitat[[replicate_nr]]

    # append the already calculated habitat values to the data set
    # df_median$hs_startYear <- habitat[1:80]
    # df_quant$hs_startYear <- habitat[1:80]
    df_mean$hs_startYear <- habitat[1:80]

    # Load data
    load(paste0(sdm_dir, "predictions/Predictions_curr_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".RData"))
    load(paste0(sdm_dir, "predictions/Predictions_fut_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".RData"))
    performance_mean <- readRDS(paste0(sdm_dir, "evaluation/performance_measures/performance_mean_SDM_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
    performance_mean <- performance_mean[[replicate_nr]]

    # Create a background mask with target resolution and extent from climate layers
    bg <- terra::rast(paste0(sdm_dir, "data/landscapes/land", rep_nr, "_optima", optima, "_breadth", breadth, "_ccYear0.grd"))
    bg <- bg$temp

    # Prepare pdf for plotting the results
    pdf(paste0(sdm_dir, "plots/plots_dispersal_assumption_empirical_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".pdf"))

    # loop through every single year (necessary for our approach later)
    for(year_nr in 0:79){

      # create SpatVector of presences (data set changes depending on the year)
      if (year_nr == 0){
        occ_rast <- terra::vect(as.matrix(ens_preds_bin[ens_preds_bin$mean_prob == 1, 1:2]))
      } else {
        occ_rast <- terra::vect(as.matrix(ens_fut_preds_bin[[year_nr]][ens_fut_preds_bin[[year_nr]]$mean_prob == 1, 1:2]))
      }

      # only calculate the buffer if presences are available
      if(length(occ_rast) > 0){

      # create a buffer of the radius of the sampled median dispersal distance around the presences (buffer size depends on the dispersal value of the respective scenario)
      # if(dispersal == 5000){
      #   # v_buf_median <- terra::buffer(occ_rast, width = 10 *  med_dist_short)
      #   # v_buf_quant <- terra::buffer(occ_rast, width = 10 *  quant_dist_short)
      #   #v_buf_mean <- terra::buffer(occ_rast, width = 10 *  mean_dist_short)
      #   v_buf_mean <- terra::buffer(occ_rast, width = 10 *  mean_dist)
      # } else {
      #   # v_buf_median <- terra::buffer(occ_rast, width = 10 * med_dist_long)
      #   # v_buf_quant <- terra::buffer(occ_rast, width = 10 * quant_dist_long)
      #   #v_buf_mean <- terra::buffer(occ_rast, width = 10 * mean_dist_long)
      #   v_buf_mean <- terra::buffer(occ_rast, width = 10 * mean_dist)
      # }
        
      v_buf_mean <- terra::buffer(occ_rast, width = 10 * mean_dist)

      # Create the buffer layer with the same resolution and spatial extent
      # disp_buf_median <- terra::mask(bg, v_buf_median)
      # disp_buf_quant <- terra::mask(bg, v_buf_quant)
      disp_buf_mean <- terra::mask(bg, v_buf_mean)

      # plot(bg, col='grey90', legend=F)
      # plot(disp_buf_mean, add=T, col='grey60', legend=F)
      # plot(occ_rast, add = T, col= "red")

      # Extract the habitat suitabilities for 10 years into the future
      fut_hs <- ens_fut_preds[[year_nr+10]]

      r_fut <- terra::rast(fut_hs[,1:3])

      # remove values below the threshold
      # fut_hs_thresh <- fut_hs[which(fut_hs$mean_prob >= performance_mean[performance_mean$Algorithm == "mean_prob",'mean_thresh']),]
      # if(nrow(fut_hs_thresh) > 1){
      # r_fut_thresh <- terra::rast(fut_hs_thresh[,1:3])
      # }

      # set future HS values outside of the buffer to 0
      #
      # fut_new_median <- terra::mask(r_fut, disp_buf_median, updatevalue=0)
      # fut_new_quant <- terra::mask(r_fut, disp_buf_quant, updatevalue=0)
      fut_new_mean <- terra::mask(r_fut, disp_buf_mean, updatevalue=0)

      # Plot predictions with dispersal assumptions
      #plot(fut_new_mean)
      plot(r_fut)
      plot(disp_buf_mean, add=T, col='grey60', legend=F, main = "Future predictions")

      plot(fut_new_mean, main = "Future predictions with dispersal assumption")

      # Extract HS values
      # df_hs_median <- terra::as.data.frame(fut_new_median, xy = T)
      # df_hs_quant <- terra::as.data.frame(fut_new_quant, xy = T)
      df_hs_mean <- terra::as.data.frame(fut_new_mean, xy = T)

      #remove cells below a threshold of habitat suitability
      # df_hs_median <- df_hs_median[which(df_hs_median$mean_prob >= performance_mean[performance_mean$Algorithm == "mean_prob",'mean_thresh']),]
      # df_hs_quant <- df_hs_quant[which(df_hs_quant$mean_prob >= performance_mean[performance_mean$Algorithm == "mean_prob",'mean_thresh']),]
      df_hs_mean <- df_hs_mean[which(df_hs_mean$mean_prob >= performance_mean[performance_mean$Algorithm == "mean_prob",'mean_thresh']),]

      # calculate the habitat suitability sum
      # df_median[which(df_median$startYear == year_nr), "hs_plus10"] <- sum(df_hs_median[,"mean_prob"], na.rm = T)
      # df_quant[which(df_quant$startYear == year_nr), "hs_plus10"] <- sum(df_hs_quant[,"mean_prob"], na.rm = T)
      df_mean[which(df_mean$startYear == year_nr), "hs_plus10"] <- sum(df_hs_mean[,"mean_prob"], na.rm = T)
      } else {
        # add 0 for the hs value when there were no occurrences in the start year
        # df_median[which(df_median$startYear == year_nr), "hs_plus10"] <- 0
        # df_quant[which(df_quant$startYear == year_nr), "hs_plus10"] <- 0
        df_mean[which(df_mean$startYear == year_nr), "hs_plus10"] <- 0
      }
    }
    dev.off()

    # add the data set as an element of the list
    # hs_list_median[[replicate_nr]] <- df_median
    # hs_list_quant[[replicate_nr]] <- df_quant
    hs_list_mean[[replicate_nr]] <- df_mean

  }

  # save the calculated habitat suitability values
  # saveRDS(hs_list_median, file = paste0(sdm_dir, "results/habitat_suitability_SDM_dispersal_assumptions_median_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  # saveRDS(hs_list_quant, file = paste0(sdm_dir, "results/habitat_suitability_SDM_dispersal_assumptions_0.95quantile_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  #saveRDS(hs_list_mean, file = paste0(sdm_dir, "results/habitat_suitability_SDM_dispersal_assumptions_mean_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  saveRDS(hs_list_mean, file = paste0(sdm_dir, "results/habitat_suitability_SDM_dispersal_assumptions_empirical_Batch", BatchNum, "_Sim", rep_nr, ".rds"))

} #close foreach loop
#stopCluster(cl)

gc()
rm(list=ls())

# 
# # Use this code for running it on your local machine 
# load("3_SDMs/data/values_dispersal_assumption.Rdata")
# 
# # Test it with one SDM scenario
# load("3_SDMs/data/Predictions_curr_Batch6_Sim3_Replication33.RData")
# load("3_SDMs/data/Predictions_fut_Batch7_Sim3_Replication87.RData")
# bg <- terra::rast("3_SDMs/data/land1_optima0.27_breadth0.045_ccYear0.grd")
# bg <- bg$temp
#performance_mean <- readRDS("3_SDMs/data/performance_mean_SDM_Batch7_Sim3.rds")
#performance_mean <- performance_mean[[1]]
# 
# # Plot occurrences in landscape
# plot(ens_preds_bin[ens_preds_bin$mean_prob== 1, "y"] ~ ens_preds_bin[ens_preds_bin$mean_prob== 1, "x"], xlim = c(0, 511000),
#      ylim = c(0, 511000))
# 
# # create SpatVector of presences
# occ_rast <- terra::vect(as.matrix(ens_preds_bin[ens_preds_bin$mean_prob == 1, 1:2]))
# #crs(occ_rast) <- crs(bg)
# 
# # create a buffer of the radius of the sampled median dispersal distance around the presences
# v_buf <- terra::buffer(occ_rast, width = 10 * med_dist_short)
# 
# # Create a background mask with target resolution and extent from climate layers
# # Set all raster cells outside the buffer to NA.
# disp_buf <- terra::mask(bg, v_buf)
# 
# # Plot results
# plot(bg, col='grey90', legend=F)
# plot(disp_buf, add=T, col='grey60', legend=F)
# plot(occ_rast, add = T, col= "red")
# 
# # use buffer for deleting habitat suitability values
# fut_hs <- ens_fut_preds[[6]]
# 
# #Plot habitat suitabilities and buffer
# plot(r_fut)
# plot(disp_buf, add=T, col='grey60', legend=F)
# 
# # set HS values outside of the buffer to 0
# r_fut <- terra::rast(fut_hs[,1:3])
# fut_new <- terra::mask(r_fut, disp_buf, updatevalue=0)
# 
# # Plot predictions with dispersal assumptions
# plot(fut_new)
# 
# # Extract HS values
# df <- terra::as.data.frame(fut_new, xy = T)

#test <- readRDS("3_SDMs/data/habitat_suitability_SDM_dispersal_assumptions_median_Batch9_Sim3.rds")
