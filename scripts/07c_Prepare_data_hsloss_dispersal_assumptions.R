# Red List criteria underestimate climate-related extinction risk of range-shifting species


#-------------------------------------------------------------------------------

# ----------------------------------------------------------------------- #
#        07c. Prepare hs loss data set with dispersal assumptions         #
# ----------------------------------------------------------------------- #


#-------------------------------------------------------------------------------

# Prepare data set of hs_loss for the SDMs when using dispersal assumptions

# Load packages
library(data.table)
library(doParallel)
library(foreach)

# define file paths
home_folder <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/") #needs to be adjusted based on own folder structure
sim_dir <- file.path(paste0(home_folder, "Simulations/"))
sdm_dir <- file.path(paste0(home_folder, "output_data/SDMs/"))

# Create the parameter table
land_rep <- 1:3
optima <- c(0.27, 0.5)
breadth <- c(0.045, 0.055)
rmax <- c(3, 5)
dispersal <- c(5000, 15000)

sims <- expand.grid(land_rep = land_rep, optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims$BatchNum <- rep(1:16, each = 3)

# obtain the 10 replicated runs
set.seed(8765)
replicates <- sample(0:99, 10)

#set up HPC
ncores <- 1
cl <- makeCluster(ncores)
registerDoParallel(cl)

# Loops for the single scenarios
foreach(sim_nr=1:nrow(sims), .packages = c("data.table", "dplyr", "tidyr", "stringr")) %dopar% {
  
  # Prepare variables --------------
  rep_nr <- sims[sim_nr,]$land_rep
  BatchNum <- sims[sim_nr,]$BatchNum
  
  # Load data set
  hs_mean <- readRDS(paste0(home_path, "analysis_data/habitat_suitability_SDM_dispersal_assumptions_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  
  # calculate hs loss
  hs_mean <- lapply(hs_mean, function(x){x$rel.hs.loss <- 100 - ((x$hs_plus10/ x$hs_startYear) * 100); return(x)})
  
  #append columns to new data frame
  df_mean <- data.frame(startYear = 0:79)
  
  for (i in 1:length(replicates)) {
    df_mean$tmp <- NA
    colnames(df_mean)[colnames(df_mean) == 'tmp'] <- paste0("hs_change", replicates[i])
  }
  
  for (i in 1:length(hs_mean)) {
    df_mean[,i+1] <- hs_mean[[i]]$rel.hs.loss
  }
  
  # save data sets
  saveRDS(df_mean, file = paste0(home_path, "analysis_data/hs_loss_SDM_dispersal_assumptions_analysis_Batch", BatchNum, "_Sim", rep_nr, ".rds"))

} #close foreach loop
stopCluster(cl)

gc()
rm(list=ls())