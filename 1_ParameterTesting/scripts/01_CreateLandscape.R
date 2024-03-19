# saving the temperature landscape with climate change
# With this script I create the temperature landscape under climate change and save it as and .Rdata file to just reload it
# instead of remodelling it every single time

# Load packages
library(raster)
library(NLMR)
library(dplyr)
library(scales)

tempdir()

# File path to individual folder
path_loop <- file.path(paste0("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/01_TestingParameters/Inputs/"))

#Create temperature landscape ------------------------------------------------------------------------------------------------
l_temp <- nlm_planargradient(ncol = 511, nrow = 511, direction = 180, resolution = 1000) 
#needs to be one cell smaller than the precipitation landscape otherwise the spatial extent is different

#Adding the spatial noise to the temp landscape
set.seed(765)
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
temp_rise <- rescale(x, c(0,0.9))

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

#Create precipitation landscape -----------------------------------------------------------------------------------------------
set.seed(234)
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

save(ls_cc, file = (paste0(path_loop, "landscape_stack.RData")))

#clean all temporary files
gc()
rm(list=ls())