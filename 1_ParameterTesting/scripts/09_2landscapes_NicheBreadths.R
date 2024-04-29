# Create two different landscapes

path_input <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/01_TestingParameters/")

#Packages
library(raster)
library(virtualspecies)
require(foreach)
require(doParallel)

# define vector for parameter combinations
width <- c(0.035, 0.04, 0.045, 0.05, 0.055)

#Prepare cluster
ncores <- 5
cl <- makeCluster(ncores)
registerDoParallel(cl)

# Start loops for the SDM fitting
results <- foreach(b=1:length(width), .packages = c("raster", "virtualspecies", "dplyr", "scales", "tibble", "NLMR"),
                   .multicombine = T) %dopar% {


                     pdf(paste0(path_input, "Output_Maps/landscape2_Breadth",width[b], ".pdf"))

                     par(mfrow=c(2,2))

                     l_temp <- nlm_planargradient(ncol = 511, nrow = 511, direction = 180, resolution = 1000)
                     #needs to be one cell smaller than the precipitation landscape otherwise the spatial extent is different

                     #Adding the spatial noise to the temp landscape
                     set.seed(352)#set.seed(765)
                     l_noise <- nlm_mpd(ncol = 512, nrow = 512, roughness =  0.96, rand_dev = 1, resolution = 1000)
                     l_tn <- mosaic(l_temp, l_noise, fun = "mean") #merge them by using the mean
                     plot(l_tn, main = "Temperature")

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
                     set.seed(987)#set.seed(234)
                     l_pre <- nlm_mpd(ncol = 512, nrow = 512, roughness =  0.75, rand_dev = 2, resolution = 1000)
                     plot(l_pre, main = "Precipitation")

                     #Plot combination of both landscapes
                     l_env <- mosaic(l_tn, l_pre, fun = "mean")
                     plot(l_env, main = "Both")

                     par(mfrow=c(1,1))

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

                     #response curves to the environmental variables
                     param <- formatFunctions(temp = c(fun = "dnorm", mean= 0.5, sd=width[b]),
                                              pre = c(fun = "dnorm", mean = 0.5, sd=width[b]))


                     ls_spec <- vector("list", length = length(ls_cc)) #create a vector of a specific length
                     s_name <- c() #create name vector to rename later

                     # create habitat suitability maps -----------------------------------------------------------------------------------------

                     for (i in 1:length(ls_cc)) {
                       temp <- ls_cc[[i]]
                       tmp <- generateSpFromFun(raster.stack = temp[[c("temp", "pre")]], parameters = param, plot = F)
                       ls_spec[[i]] <- tmp #add the habitat suitability object as a new element in the list
                       d <- temp_rise[i]
                       s_name <- append(s_name, paste0("cc_", d))
                     }
                     names(ls_spec) <- s_name

                     # Plot HS maps under climate change
                     par(mfrow=c(2,3))
                     plot(ls_spec[[1]], main = "Climate Change year 1")
                     plot(ls_spec[[12]], main = "year 12")
                     plot(ls_spec[[25]], main = "year 25")
                     plot(ls_spec[[35]], main = "year 35")
                     plot(ls_spec[[45]], main = "year 45")
                     plot(ls_spec[[55]], main = "year 55")
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

                     real_rangechange$Breadth <- paste0("Breadth: ", width[b])


                     #save maps
                     for (i in 1:length(ls_spec)) {
                       d <- temp_rise[i]
                       temp <- ls_spec[[i]][["suitab.raster"]]
                       values(temp) <- values(temp)*100 #make values to percentages
                       writeRaster(temp, filename = paste(path_input,"Inputs/habitat_per_land2_breadth", width[b], "_cc", d, ".asc", sep = ""),
                                   overwrite = T, format = "ascii")
                     }

                     real_rangechange
                   }

stopCluster(cl)

saveRDS(results, file = paste0(path_input, "Outputs/habitatloss_nichebreadths_land2.rds"))

#Prepare cluster
ncores <- 5
cl <- makeCluster(ncores)
registerDoParallel(cl)

# Start loops for the SDM fitting
results <- foreach(b=1:length(width), .packages = c("raster", "virtualspecies", "dplyr", "scales", "tibble", "NLMR"),
                   .multicombine = T) %dopar% {
                     
                     
                     pdf(paste0(path_input, "Output_Maps/landscape3_Breadth",width[b], ".pdf"))
                     
                     par(mfrow=c(2,2))
                     
                     l_temp <- nlm_planargradient(ncol = 511, nrow = 511, direction = 180, resolution = 1000) 
                     #needs to be one cell smaller than the precipitation landscape otherwise the spatial extent is different
                     
                     #Adding the spatial noise to the temp landscape
                     set.seed(836)#set.seed(352)#set.seed(765)
                     l_noise <- nlm_mpd(ncol = 512, nrow = 512, roughness =  0.96, rand_dev = 1, resolution = 1000) 
                     l_tn <- mosaic(l_temp, l_noise, fun = "mean") #merge them by using the mean
                     plot(l_tn, main = "Temperature")
                     
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
                     set.seed(748)#set.seed(987)#set.seed(234)
                     l_pre <- nlm_mpd(ncol = 512, nrow = 512, roughness =  0.75, rand_dev = 2, resolution = 1000)
                     plot(l_pre, main = "Precipitation")
                     
                     #Plot combination of both landscapes
                     l_env <- mosaic(l_tn, l_pre, fun = "mean")
                     plot(l_env, main = "Both")
                     
                     par(mfrow=c(1,1))
                     
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
                     
                     #response curves to the environmental variables
                     param <- formatFunctions(temp = c(fun = "dnorm", mean= 0.5, sd=width[b]),
                                              pre = c(fun = "dnorm", mean = 0.5, sd=width[b]))
                     
                     
                     ls_spec <- vector("list", length = length(ls_cc)) #create a vector of a specific length
                     s_name <- c() #create name vector to rename later
                     
                     # create habitat suitability maps -----------------------------------------------------------------------------------------
                     
                     for (i in 1:length(ls_cc)) {
                       temp <- ls_cc[[i]]
                       tmp <- generateSpFromFun(raster.stack = temp[[c("temp", "pre")]], parameters = param, plot = F)
                       ls_spec[[i]] <- tmp #add the habitat suitability object as a new element in the list
                       d <- temp_rise[i]
                       s_name <- append(s_name, paste0("cc_", d))
                     }
                     names(ls_spec) <- s_name
                     
                     # Plot HS maps under climate change
                     par(mfrow=c(2,3))
                     plot(ls_spec[[1]], main = "Climate Change year 1")
                     plot(ls_spec[[12]], main = "year 12")
                     plot(ls_spec[[25]], main = "year 25")
                     plot(ls_spec[[35]], main = "year 35")
                     plot(ls_spec[[45]], main = "year 45")
                     plot(ls_spec[[55]], main = "year 55")
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

                     real_rangechange$Breadth <- paste0("Breadth: ", width[b])


                     #save maps
                     for (i in 1:length(ls_spec)) {
                       d <- temp_rise[i]
                       temp <- ls_spec[[i]][["suitab.raster"]]
                       values(temp) <- values(temp)*100 #make values to percentages
                       writeRaster(temp, filename = paste(path_input,"Inputs/habitat_per_land3_breadth", width[b], "_cc", d, ".asc", sep = ""),
                                   overwrite = T, format = "ascii")
                     }

                     real_rangechange
                   }

stopCluster(cl)

saveRDS(results, file = paste0(path_input, "Outputs/habitatloss_nichebreadths_land3.rds"))

#Prepare cluster
ncores <- 5
cl <- makeCluster(ncores)
registerDoParallel(cl)

# Start loops for the SDM fitting
foreach(b=1:length(width), .packages = c("raster", "virtualspecies", "dplyr", "scales", "tibble", "NLMR"),
                   .multicombine = T) %dopar% {
                     
                     
                     pdf(paste0(path_input, "Output_Maps/landscape1_Breadth",width[b], ".pdf"))
                     
                     par(mfrow=c(2,2))
                     
                     l_temp <- nlm_planargradient(ncol = 511, nrow = 511, direction = 180, resolution = 1000) 
                     #needs to be one cell smaller than the precipitation landscape otherwise the spatial extent is different
                     
                     #Adding the spatial noise to the temp landscape
                     set.seed(765)
                     l_noise <- nlm_mpd(ncol = 512, nrow = 512, roughness =  0.96, rand_dev = 1, resolution = 1000) 
                     l_tn <- mosaic(l_temp, l_noise, fun = "mean") #merge them by using the mean
                     plot(l_tn, main = "Temperature")
                     
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
                     plot(l_pre, main = "Precipitation")
                     
                     #Plot combination of both landscapes
                     l_env <- mosaic(l_tn, l_pre, fun = "mean")
                     plot(l_env, main = "Both")
                     
                     par(mfrow=c(1,1))
                     
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
                     
                     #response curves to the environmental variables
                     param <- formatFunctions(temp = c(fun = "dnorm", mean= 0.5, sd=width[b]),
                                              pre = c(fun = "dnorm", mean = 0.5, sd=width[b]))
                     
                     
                     ls_spec <- vector("list", length = length(ls_cc)) #create a vector of a specific length
                     s_name <- c() #create name vector to rename later
                     
                     # create habitat suitability maps -----------------------------------------------------------------------------------------
                     
                     for (i in 1:length(ls_cc)) {
                       temp <- ls_cc[[i]]
                       tmp <- generateSpFromFun(raster.stack = temp[[c("temp", "pre")]], parameters = param, plot = F)
                       ls_spec[[i]] <- tmp #add the habitat suitability object as a new element in the list
                       d <- temp_rise[i]
                       s_name <- append(s_name, paste0("cc_", d))
                     }
                     names(ls_spec) <- s_name
                     
                     # Plot HS maps under climate change
                     par(mfrow=c(2,3))
                     plot(ls_spec[[1]], main = "Climate Change year 1")
                     plot(ls_spec[[12]], main = "year 12")
                     plot(ls_spec[[25]], main = "year 25")
                     plot(ls_spec[[35]], main = "year 35")
                     plot(ls_spec[[45]], main = "year 45")
                     plot(ls_spec[[55]], main = "year 55")
                     par(mfrow=c(2,2))
                     
                     dev.off()
                   }

stopCluster(cl)

#clean all temporary files
gc()
rm(list=ls())