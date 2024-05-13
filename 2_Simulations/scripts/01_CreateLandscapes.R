# Create the landscapes under climate change for the simulations for three different landscape settings
# 4 different landscapes are created (4 different niche combinations)
# niche optima: cold (0.27) vs. warm (0.5)
# niche breadth: narrow (0.045) vs. wide (0.055)

#define path
path_input <- file.path("/import/ecoc9z/data-zurell/keuth/02_Simulations/")

# Load packages
library(raster)
library(NLMR)
library(virtualspecies)
library(dplyr)
library(scales)
require(foreach)
require(doParallel)

# create landscape parameters
optimum <- c(0.27, 0.5)
breadth <- c(0.045, 0.055)
land <- c("land1", "land2", "land3")

ncores <- 3
cl <- makeCluster(ncores)
registerDoParallel(cl)

foreach(l = 1:length(land), .packages = c("raster", "NLMR", "virtualspecies", "dplyr", "scales")) %dopar% {
  for(b in 1:length(breadth)){
    for (a in 1:length(optimum)){
    
    # Create parameter for creating the folder
    if(optimum[a] == 0.27){
      nP <- "c"
    } else {
      nP <- "w"
    }
    if(breadth[b] == 0.035){
      nW <- "n"
    } else {
      nW <- "w"
    }
    
    # create new folder for every parameter combination
    if(!file.exists(paste0(path_input, nP, nW))) {
      dir.create(paste0(path_input, nP, nW))}
    
    # File path to individual folder
    path_loop <- file.path(paste0("/import/ecoc9z/data-zurell/keuth/02_Simulations/", nP, nW, "/"))
    
    #Create temperature landscape ------------------------------------------------------------------------------------------------
    l_temp <- nlm_planargradient(ncol = 511, nrow = 511, direction = 180, resolution = 1000) 
    #needs to be one cell smaller than the precipitation landscape otherwise the spatial extent is different
    
    #Adding the spatial noise to the temp landscape
    if(land[l] == "land1"){
      set.seed(765)
    } else if(land[l] == "land2"){
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
    t <- 1:91
    alpha <- 0.5
    beta <- 0.9 
    theta <- 0.3
    set.seed(5678)
    ts <- alpha + beta * t + arima.sim(list(ma = theta), n = length(t))
    x <- as.vector(ts)
    temp_rise <- scales::rescale(x, c(0,0.9))
    
    # create 90 temperature landscapes under climate change
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
    
    #Create precipitation landscape -----------------------------------------------------------------------------------------------
    if(land[l] == "land1"){
      set.seed(234)
    } else if(land[l] == "land2"){
      set.seed(987)
    } else {
      set.seed(748)
    }
    l_pre <- nlm_mpd(ncol = 512, nrow = 512, roughness =  0.75, rand_dev = 2, resolution = 1000)
    
    # create habitat suitability maps -----------------------------------------------------------------------------------------
    
    ls_cc <- list() #create an empty list to put the raster stacks in
    l_name <- c() #empty vector to rename later
    
    # create raster stack for every temperature and precipitation landscape for creating habitat suitability maps for the virtual species
    for (i in 1:length(temp_cc@layers)){
      l_name <- append(l_name, paste0("l_env_", temp_rise[i])) #add name to name vector
      temp <- stack(temp_cc[[i]], l_pre)
      names(temp) <- c("temp", "pre")
      ls_cc <- append(ls_cc, temp)
    }
    
    #response curves for the different virtual species (mean of temperature and sd are adapted to the respective scenarios)
    param <- formatFunctions(temp = c(fun = "dnorm", mean= optimum[a], sd=breadth[b]),
                             pre = c(fun = "dnorm", mean = 0.5, sd=breadth[b]))
    
    hs_spec <- vector("list", length = length(ls_cc)) #list for the different habitat suitability maps
    hs_name <- c() #create name vector to rename later
    
    for (i in 1:length(ls_cc)) {
      hs_spec[[i]] <- generateSpFromFun(raster.stack = ls_cc[[i]][[c("temp", "pre")]], parameters = param, plot = F)
      hs_name <- append(s_name, paste0("cc_", temp_rise[i]))
    }
    names(ls_spec) <- s_name
    
    #save created HS maps and landscapes ---------------------------------------------------------------------------
    
    #create folders necessary for simulation
    if(!file.exists(paste0(path_loop,"Inputs"))) {
      dir.create(paste0(path_loop,"Inputs"))}
    if(!file.exists(paste0(path_loop,"Outputs"))) {
      dir.create(paste0(path_loop,"Outputs"))}
    if(!file.exists(paste0(path_loop,"Output_Maps"))) {
      dir.create(paste0(path_loop,"Output_Maps"))}
    if(!file.exists(paste0(path_loop,"Inputs/SDM"))) {
      dir.create(paste0(path_loop, "Inputs/SDM"))}
    
    # save habitat suitability maps for simulations
    for (i in 1:length(ls_spec)) {
      temp <- ls_spec[[i]][["suitab.raster"]]
      values(temp) <- values(temp)*100 #make values to percentages
      writeRaster(temp, filename = paste(path_loop,"Inputs/", land[l], "_cc", temp_rise[i], ".asc", sep = ""), overwrite = T, format = "ascii")
    }
    
    # save landscape maps as stacks for the SDMs
    for (i in 1:length(temp_values)){
      tmp <- stack(temp_cc[[i]], l_pre)
      names(tmp) <- c("temp", "pre")
      writeRaster(tmp, filename = paste(path_loop, "Inputs/SDM/landscape_", land[l], "_cc", temp_rise[i], ".grd", sep = ""), overwrite = T)
    }
    
    #plots of landscape under climate landscapes ------------------------------------------------------------------
    
    # Plot some selected HS maps under climate change
    pdf(paste0(path_loop, land[l], "_cc_short.pdf"))
    par(mfrow=c(2,3))
    plot(ls_spec[[1]], main = "Climate Change year 1")
    plot(ls_spec[[12]], main = "year 12")
    plot(ls_spec[[25]], main = "year 25")
    plot(ls_spec[[35]], main = "year 35")
    plot(ls_spec[[45]], main = "year 45")
    plot(ls_spec[[55]], main = "year 55")
    par(mfrow=c(2,2))
    dev.off()
    
    #plot all maps under climate change
    pdf(paste0(path_loop, land[l], "_cc_long.pdf"))
    par(mfrow=c(2,3))
    for (i in 1:length(ls_spec)) {
      plot(ls_spec[[i]])
    }
    par(mfrow=c(2,2))
    dev.off()
    
    #calculate real habitat change
    real_rangechange <- data.frame(Year = c(0:(length(ls_spec)-1), range = NA))
    
    for(i in 1:length(ls_spec)){
      tmp <- ls_spec[[i]][["suitab.raster"]]
      values(temp) <- values(temp)*100
      real_rangechange[i,2] <- sum(values(tmp)[values(tmp) != 0 & !is.na(values(tmp))])
    }
    real_rangechange$diff <- NA
    for (i in 2:nrow(real_rangechange)) {
      real_rangechange[i, 3] <- (real_rangechange[i,2] / real_rangechange[1,2])
    }
    real_rangechange[1,3] <- 1
    
    saveRDS(real_rangechange, file = paste0(path_loop, "Outputs/real_habitatloss_", land[l], ".rds"))
    }
  }
}

stopCluster(cl)

#clean all temporary files
gc()
rm(list=ls())