# Creating landscapes for different niche breadths

# In this script I will create the landscapes for different niche breadths to test dispersal limitation with different dispersal parameters

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
results <- foreach(b=1:length(width), .packages = c("raster", "virtualspecies", "dplyr", "scales", "tibble"),
                   .multicombine = T) %dopar% {
                     
                     
                     #foreach(b=1:length(width), .packages = c("raster", "virtualspecies", "RangeShiftR", "dplyr", "scales", "tibble", "ggplot2", "gridExtra")) %dopar% {
                     
                     #obtain number for temperature increase
                     t <- 1:90
                     alpha <- 0.5
                     beta <- 0.9 
                     theta <- 0.3
                     set.seed(5678)
                     ts <- alpha + beta * t + arima.sim(list(ma = theta), n = length(t))
                     x <- as.vector(ts)
                     temp_rise <- scales::rescale(x, c(0,0.9))
                     
                     #Load landscape stack under cc
                     load(paste0(path_input, "Inputs/landscape_stack.RData"))
                     
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
                     # par(mfrow=c(2,3))
                     # plot(ls_spec[[1]], main = "Climate Change year 1")
                     # plot(ls_spec[[12]], main = "year 12")
                     # plot(ls_spec[[25]], main = "year 25")
                     # plot(ls_spec[[35]], main = "year 35")
                     # plot(ls_spec[[45]], main = "year 45")
                     # plot(ls_spec[[55]], main = "year 55")
                     # par(mfrow=c(2,2))
                     
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
                       writeRaster(temp, filename = paste(path_input,"Inputs/habitat_per_breadth", width[b], "_cc", d, ".asc", sep = ""), 
                                   overwrite = T, format = "ascii")
                     }
                     
                     real_rangechange
                   }

stopCluster(cl)

saveRDS(results, file = paste0(path_input, "Outputs/habitatloss_nichebreadths.rds"))

#clean all temporary files
gc()
rm(list=ls())