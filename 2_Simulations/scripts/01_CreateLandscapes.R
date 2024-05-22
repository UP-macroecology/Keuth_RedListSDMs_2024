# Create the landscapes under climate change 
# Goal: create four different landscapes for the different niche combinations, each landscape is replicated three-times
# niche optima: cold (0.27) and warm (0.5)
# niche breadth: narrow (0.045) wide (0.055)

#define file path
sim_dir <- file.path("/import/ecoc9z/data-zurell/keuth/02_Simulations/")
sdm_dir <- file.path("/import/ecoc9z/data-zurell/keuth/03_SDMs/")

# Load packages
library(raster)
library(NLMR)
library(virtualspecies)
library(dplyr)
library(scales)
library(data.table)
require(foreach)
require(doParallel)

# create data frame with all landscape combinations
land_rep <- 1:3
optima <- c(0.27, 0.5)
breadth <- c(0.045, 0.055)
sims <- expand.grid(land_rep = land_rep, optima = optima, breadth = breadth)

ncores <- 20
cl <- makeCluster(ncores)
registerDoParallel(cl)

foreach(sim_nr = 1:nrow(sims), .packages = c("raster", "NLMR", "virtualspecies", "dplyr", "scales")) %dopar% {
    rep_nr <- sims[sim_nr,]$land_rep
    optima <- sims[sim_nr,]$optima
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
    
    # Create habitat suitability maps -----------------------------------------------------------------------------------------
    
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
    
    #response curves for the different virtual species (mean of temperature and sd are adapted to the respective scenarios)
    param <- formatFunctions(temp = c(fun = "dnorm", mean = optima, sd = breadth),
                             pre = c(fun = "dnorm", mean = 0.5, sd = breadth))
    
    hs_spec <- vector("list", length = length(ls_cc)) #list for the different habitat suitability maps
    hs_name <- c() #create name vector to rename later
    
    for (i in 1:length(ls_cc)) {
      hs_spec[[i]] <- generateSpFromFun(raster.stack = ls_cc[[i]][[c("temp", "pre")]], parameters = param, plot = F)
      hs_name <- append(hs_name, paste0("cc_Year", (i-1)))
    }
    names(hs_spec) <- hs_name
    
    #save created HS maps and landscapes ---------------------------------------------------------------------------
    
    # save habitat suitability maps for simulations
    for (i in 1:length(ls_spec)) {
      temp <- ls_spec[[i]][["suitab.raster"]]
      values(temp) <- values(temp)*100 #make values to percentages
      writeRaster(temp, filename = paste(sim_dir,"Inputs/land", rep_nr, "_optima",  optima, "_breadth", breadth, "_ccYear", (i-1), ".asc", sep = ""), overwrite = T, format = "ascii")
    }
    
    # save landscape maps as stacks for the SDMs
    for (i in 1:length(temp_values)){
      tmp <- stack(temp_cc[[i]], l_pre)
      names(tmp) <- c("temp", "pre")
      writeRaster(tmp, filename = paste(sdm_dir, "landscapes/land", rep_nr, "_optima",  optima, "_breadth", breadth, "_ccYear", (i-1), ".grd", sep = ""), overwrite = T)
    }
    
    # plots of landscape under climate landscapes ------------------------------------------------------------------
    
    #plot all maps under climate change
    pdf(paste0(sim_dir, "Output_Maps/land", rep_nr, "_optima",  optima, "_breadth", breadth, "_under_cc.pdf"))
    par(mfrow=c(2,3))
    for (i in 1:length(ls_spec)) {
      plot(ls_spec[[i]])
    }
    par(mfrow=c(2,2))
    dev.off()
    
    # calculate real habitat change ----------------------------------------------
    habitat_change <- data.frame(Year = c(0:(length(ls_spec)-1), habitat_size = NA, habitat_loss = NA))
    
    for(i in 1:length(ls_spec)){
      tmp <- ls_spec[[i]][["suitab.raster"]]
      values(temp) <- values(temp)*100
      habitat_change[i, "habitat_size"] <- sum(values(tmp)[values(tmp) != 0 & !is.na(values(tmp))])
    }
    for (i in 2:nrow(habitat_change)) {
      habitat_change[i, "habitat_loss"] <- (habitat_change[i,"habitat_size"] / habitat_change[1,"habitat_size"])
    }
    habitat_change[1,"habitat_loss"] <- 1
    
    saveRDS(habitat_change, file = paste0(sim_dir, "Outputs/real_habitatloss_land", rep_nr, "_optima",  optima, "_breadth", breadth, ".rds"))
}

stopCluster(cl)

#clean all temporary files
gc()
rm(list=ls())