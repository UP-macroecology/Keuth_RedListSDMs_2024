# Red List criteria underestimate climate-related extinction risk of range-shifting species


#-------------------------------------------------------------------------------

# -------------------------------------------------------------------------- #
#                         02. Create virtual species niche                   #
# -------------------------------------------------------------------------- #


#-------------------------------------------------------------------------------

# Creation of virtual species niche for 4 different niche combinations (central vs. marginal niche position, narrow vs. wide niche)

# The different values for the different virtual species niches are
# niche position: marginal (0.27), central (0.5)
# niche breadth: narrow (0.025), wide (0.035)

# Load packages
library(raster)
library(virtualspecies)
library(dplyr)
library(scales)
library(data.table)
require(foreach)
require(doParallel)

# define file paths
home_folder <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/") #needs to be adjusted based on own folder structure
sim_dir <- file.path(paste0(home_folder, "Simulations/"))
sdm_dir <- file.path(paste0(home_folder, "output_data/SDMs/"))

# create data frame with all landscape combinations
land_rep <- 1:3
position <- c(0.27, 0.5)
breadth <- c(0.045, 0.055)
sims <- expand.grid(land_rep = land_rep, position = position, breadth = breadth)

# Prepare script to run on HPC
ncores <- 12
cl <- makeCluster(ncores)
registerDoParallel(cl)

foreach(sim_nr = 1:nrow(sims), .packages = c("raster", "virtualspecies", "dplyr", "scales")) %dopar% {
  # Extract values for individual run  
  rep_nr <- sims[sim_nr,]$land_rep
  position <- sims[sim_nr,]$position
  breadth <- sims[sim_nr,]$breadth
  
  # Load landscape stacks
  load(paste("output_data/landscapes/land", rep_nr, "_position",  position, "_breadth", breadth, "_stack.Rdata"))
  
  # create virtual species niche
  
  #response curves for the different virtual species (mean of temperature and sd are adapted to the respective scenarios)
  param <- formatFunctions(temp = c(fun = "dnorm", mean = position, sd = breadth),
                           pre = c(fun = "dnorm", mean = 0.5, sd = breadth))
  
  ls_hs <- vector("list", length = length(ls_cc)) #list for the different habitat suitability maps
  hs_name <- c() #create name vector to rename later
  
  for (i in 1:length(ls_cc)) {
    ls_hs[[i]] <- generateSpFromFun(raster.stack = ls_cc[[i]][[c("temp", "pre")]], parameters = param, plot = F)
    hs_name <- append(hs_name, paste0("cc_Year", (i-1)))
  }
  names(ls_hs) <- hs_name
  
  #save created HS maps ---------------------------------------------------------------------------
  
  # save habitat suitability maps for simulations
  for (i in 1:length(ls_hs)) {
    temp <- ls_hs[[i]][["suitab.raster"]]
    values(temp) <- values(temp)*100 #make values to percentages
    writeRaster(temp, filename = paste(sim_dir,"Inputs/land", rep_nr, "_position",  position, "_breadth", breadth, "_ccYear", (i-1), ".asc", sep = ""), overwrite = T, format = "ascii")
  }
  
  # plots of landscape under climate landscapes ------------------------------------------------------------------
  
  #plot all maps under climate change
  pdf(paste0(sim_dir, "Output_Maps/land", rep_nr, "_position",  position, "_breadth", breadth, "_under_cc.pdf"))
  par(mfrow=c(2,3))
  for (i in 1:length(ls_hs)) {
    plot(ls_hs[[i]])
  }
  par(mfrow=c(2,2))
  dev.off()
  
  # calculate real habitat change ----------------------------------------------
  habitat_change <- data.frame(Year = c(0:(length(ls_hs)-1), habitat_size = NA, habitat_loss = NA))
  
  for(i in 1:length(ls_hs)){
    tmp <- ls_hs[[i]][["suitab.raster"]]
    values(temp) <- values(temp)*100
    habitat_change[i, "habitat_size"] <- sum(values(tmp)[values(tmp) != 0 & !is.na(values(tmp))])
  }
  for (i in 2:nrow(habitat_change)) {
    habitat_change[i, "habitat_loss"] <- (habitat_change[i,"habitat_size"] / habitat_change[1,"habitat_size"])
  }
  habitat_change[1,"habitat_loss"] <- 1
  
  saveRDS(habitat_change, file = paste0(sim_dir, "Outputs/real_habitatloss_land", rep_nr, "_position",  position, "_breadth", breadth, ".rds"))
}

stopCluster(cl)

#clean all temporary files
gc()
rm(list=ls())