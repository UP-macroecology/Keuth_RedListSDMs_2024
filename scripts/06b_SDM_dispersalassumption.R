# Red List criteria underestimate climate-related extinction risk of range-shifting species


#-------------------------------------------------------------------------------

# --------------------------------------------------------------------- #
#                         06a. SDMs with dispersal assumptions          #
# --------------------------------------------------------------------- #


#-------------------------------------------------------------------------------

# SDMs with dispersal assumptions for all species

# Loading packages
library(terra)
library(data.table)
library(foreach)
library(doParallel)

# define file paths
home_folder <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/") #needs to be adjusted based on own folder structure
sim_dir <- file.path(paste0(home_folder, "Simulations/"))
sdm_dir <- file.path(paste0(home_folder, "output_data/SDMs/"))

# create data frame with all parameter combinations
land_rep <- 1:3
position <- c(0.27, 0.5)
breadth <- c(0.045, 0.055)
rmax <- c(3, 5)
dispersal <- c(5000, 15000)

sims <- expand.grid(land_rep = land_rep, position = position, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims$BatchNum <- rep(1:16, each = 3)

# obtain the 10 random replications from the 100
set.seed(8765)
replicates <- sample(0:99, 10)

#set up cluster
ncores <- 48
cl <- makeCluster(ncores)
registerDoParallel(cl)

# foreach loop through every single scenario
foreach(sim_nr=1:nrow(sims), .packages = c("terra", "data.table")) %dopar% {
  
  # Prepare variables --------------
  rep_nr <- sims[sim_nr,]$land_rep
  position <- sims[sim_nr,]$position
  breadth <- sims[sim_nr,]$breadth
  BatchNum <- sims[sim_nr,]$BatchNum
  dispersal <- sims[sim_nr,]$dispersal
  
  # Load in distance values
  load(paste0(sim_dir, "Outputs/vec_distances_ Batch", BatchNum, "_Sim", rep_nr, ".Rdata"))
  
  # remove the 0s out of the data set
  vec_distances <- vec_distances[!vec_distances == 0]
  
  # calculate mean distance
  mean_dist <- mean(vec_distances)
  
  hs_list_mean <- vector("list", length = length(replicates))

  # loop for every replicated run
  for (replicate_nr in 1:10) {

    # Create data set for the final hs values
    df_mean <- data.frame(startYear = 0:79, hs_startYear = NA, hs_plus10 = NA)

    # load the already calculated habitat suitabilities
    habitat <- readRDS(paste0(home_folder, "analysis_data/habitat_suitability_SDM_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
    habitat <- habitat[[replicate_nr]]

    # append the already calculated habitat values to the data set
    df_mean$hs_startYear <- habitat[1:80]

    # Load data
    load(paste0(sdm_dir, "predictions/Predictions_curr_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".RData"))
    load(paste0(sdm_dir, "predictions/Predictions_fut_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".RData"))
    performance_mean <- readRDS(paste0(sdm_dir, "evaluation/performance_measures/performance_mean_SDM_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
    performance_mean <- performance_mean[[replicate_nr]]

    # Create a background mask with target resolution and extent from climate layers
    bg <- terra::rast(paste0(home_folder, "output_data/landscapes/land", rep_nr, "_position", position, "_breadth", breadth, "_ccYear0.grd"))
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

      # create a buffer of the radius of the sampled mean dispersal distance around the presences
      v_buf_mean <- terra::buffer(occ_rast, width = 10 * mean_dist)

      # Create the buffer layer with the same resolution and spatial extent
      disp_buf_mean <- terra::mask(bg, v_buf_mean)

      # Extract the habitat suitabilities for 10 years into the future
      fut_hs <- ens_fut_preds[[year_nr+10]]

      r_fut <- terra::rast(fut_hs[,1:3])

      # set future HS values outside of the buffer to 0
      fut_new_mean <- terra::mask(r_fut, disp_buf_mean, updatevalue=0)

      # Plot predictions with dispersal assumptions
      plot(r_fut)
      plot(disp_buf_mean, add=T, col='grey60', legend=F, main = "Future predictions")
      plot(fut_new_mean, main = "Future predictions with dispersal assumption")

      # Extract HS values
      df_hs_mean <- terra::as.data.frame(fut_new_mean, xy = T)

      #remove cells below a threshold of habitat suitability
      df_hs_mean <- df_hs_mean[which(df_hs_mean$mean_prob >= performance_mean[performance_mean$Algorithm == "mean_prob",'mean_thresh']),]

      # calculate the habitat suitability sum
      df_mean[which(df_mean$startYear == year_nr), "hs_plus10"] <- sum(df_hs_mean[,"mean_prob"], na.rm = T)
      } else {
        # add 0 for the hs value when there were no occurrences in the start year
        df_mean[which(df_mean$startYear == year_nr), "hs_plus10"] <- 0
      }
    }
    dev.off()

    # add the data set as an element of the list
    hs_list_mean[[replicate_nr]] <- df_mean

  }

  # save the calculated habitat suitability values
  saveRDS(hs_list_mean, file = paste0(home_folder, "analysis_data/habitat_suitability_SDM_dispersal_assumptions_Batch", BatchNum, "_Sim", rep_nr, ".rds"))

} #close foreach loop
#stopCluster(cl)

gc()
rm(list=ls())
