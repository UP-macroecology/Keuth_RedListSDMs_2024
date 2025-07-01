# Red List criteria underestimate climate-related extinction risk of range-shifting species


#-------------------------------------------------------------------------------

# -------------------------------------------------------------------------- #
#                         01. Create Artificial landscapes                   #
# -------------------------------------------------------------------------- #


#-------------------------------------------------------------------------------

# Creation of artificial landscapes representing temperature and precipitation under climate change

# Load packages
library(raster)
library(NLMR)
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

foreach(sim_nr = 1:nrow(sims), .packages = c("raster", "NLMR", "dplyr", "scales")) %dopar% {
    # Extract values for individual run  
    rep_nr <- sims[sim_nr,]$land_rep
    position <- sims[sim_nr,]$position
    breadth <- sims[sim_nr,]$breadth
    
    #Create temperature landscape ------------------------------------------------------------------------------------------------
    l_temp <- nlm_planargradient(ncol = 511, nrow = 511, direction = 180, resolution = 1000) 
    #needs to be one cell smaller than the precipitation landscape otherwise the spatial extent is different
    
    #Adding the spatial noise to the temp landscape
    if(rep_nr == 1){
      set.seed(765)
    } else if(rep_nr == 2){
      set.seed(352)
    } else {
      set.seed(836)
      }
    l_noise <- nlm_mpd(ncol = 512, nrow = 512, roughness =  0.96, rand_dev = 1, resolution = 1000) 
    l_tn <- mosaic(l_temp, l_noise, fun = "mean") #merge them by using the mean
    
    #Simulation of climate change ------------------------------------------------------------------------------------------------
    
    temp_cc <- l_tn # preserve original landscape
    temp_name <- c() # start vector for renaming
    
    # create climate change values with temporal autocorrelation
    t <- 1:90
    alpha <- 0.5
    beta <- 0.9 
    theta <- 0.3
    set.seed(5678)
    ts <- alpha + beta * t + arima.sim(list(ma = theta), n = length(t))
    x <- as.vector(ts)
    temp_rise <- scales::rescale(x, c(0,0.9))
    
    # create 89 temperature landscapes under climate change
    # add to every cell the value of temperature increase and if the cell value is then over 1 reduce it to 1
    for (k in 1:length(temp_rise)){
      tmp <- l_tn
      values(tmp) <-  values(tmp) + temp_rise[k]
      for (i in 1:length(values(tmp))) {
        if (tmp@data@values[i] > 1)
          tmp@data@values[i] <- 1
      }
      temp_name <- append(temp_name, paste0("cc_", temp_rise[k])) #add a name to the name vector for the value that was added
      temp_cc <-stack(temp_cc,tmp) 
    }
    temp_cc <- dropLayer(temp_cc, 1) #drop first layer since the first two layers are identical 
    names(temp_cc) <- temp_name
    
    # Create precipitation landscape -----------------------------------------------------------------------------------------------
    if(rep_nr == 1){
      set.seed(234)
    } else if(rep_nr == 2){
      set.seed(987)
    } else {
      set.seed(748)
    }
    l_pre <- nlm_mpd(ncol = 512, nrow = 512, roughness =  0.75, rand_dev = 2, resolution = 1000)
    
    # save landscape maps as stacks for the SDMs
    for (i in 1:length(temp_cc@layers)){
      tmp <- stack(temp_cc[[i]], l_pre)
      names(tmp) <- c("temp", "pre")
      writeRaster(tmp, filename = paste("output_data/landscapes/land", rep_nr, "_position",  position, "_breadth", breadth, "_ccYear", (i-1), ".grd", sep = ""), overwrite = T)
    }
    
    # save stack for creating virtual species niche ---------------
    
    ls_cc <- list() #create an empty list to put the raster stacks in
    l_name <- c() #empty vector to rename later
    
    # create raster stack for every temperature and precipitation landscape for creating habitat suitability maps for the virtual species
    for (i in 1:length(temp_cc@layers)){
      l_name <- append(l_name, paste0("l_env_Year", (i - 1))) #add name to name vector
      temp <- stack(temp_cc[[i]], l_pre)
      names(temp) <- c("temp", "pre")
      ls_cc <- append(ls_cc, temp)
    }
    names(ls_cc) <- l_name
    
    save(ls_cc, file = paste("output_data/landscapes/land", rep_nr, "_position",  position, "_breadth", breadth, "_stack.Rdata", sep = ""))
}

stopCluster(cl)

#clean all temporary files
gc()
rm(list=ls())