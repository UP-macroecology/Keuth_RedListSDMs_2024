# Testing different niche breadths with long and short distance dispersal


# Testing different niche breadths with short dispersal values

path_input <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/01_TestingParameters/")
#path_input <- file.path("Bugs/")

# Function for calculating extinction probability
source(paste0(path_input, "Functions/Extinction_probability.R"))

#Packages
library(raster)
library(landscapetools)
library(NLMR)
library(virtualspecies)
require(RangeShiftR)
library(dplyr)
library(scales)
require(foreach)
require(doParallel)
library(tibble)
library(ggplot2)
library(gridExtra)

# define vector for parameter combinations
width <- c(0.035, 0.04, 0.045, 0.05, 0.055)

# prepare lists
#pop_mean <- vector("list", length(width))
#extProb_list <- vector("list", length(width))

#Prepare cluster
ncores <- 5
cl <- makeCluster(ncores)
registerDoParallel(cl)

#Function for combining output later
comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

# Start loops for the SDM fitting
results <- foreach(b=1:length(width), .packages = c("raster", "RangeShiftR", "dplyr", "scales", "tibble", "ggplot2", "gridExtra", "terra"), .combine = "comb",
                   .multicombine = T, .init = list(list(), list())) %dopar% {
                     
                     #obtain number for temperature increase
                     t <- 1:90
                     alpha <- 0.5
                     beta <- 0.9 
                     theta <- 0.3
                     set.seed(5678)
                     ts <- alpha + beta * t + arima.sim(list(ma = theta), n = length(t))
                     x <- as.vector(ts)
                     temp_rise <- scales::rescale(x, c(0,0.9))
                     
                     # Set up the dynamic landscapes ------------------------------------------------------------------------------------
                     # Numbers of spinup years in dynamic landscape
                     spinup <- 100
                     
                     # Set dynamic landscape parameters
                     #create vector to read in landscapes:
                     val <- temp_rise
                     val <- c("0", val)
                     landnames <- c()
                     for (i in 1:length(val)){
                       d <- val[i]
                       k <- paste0("habitat_per_land2_breadth", width[b], "_cc", d, ".asc")
                       landnames <- append(landnames, k)
                     }
                     
                     #create vector for the years
                     years <- seq(spinup,((length(landnames)-2)*1)+100,1) #minus 2 because we have the spinup and the year 0
                     years <- c("0", years)
                     years  <- as.numeric(years)
                     
                     # Define Landscape module ------------------------------------------------------------------------------------
                     
                     land <- ImportedLandscape(LandscapeFile = landnames,
                                               DynamicLandYears = years,
                                               Resolution = 1000,
                                               HabPercent = TRUE,
                                               K_or_DensDep = 0.05)
                     
                     # Define transition matrix ------------------------------------------------------------------------------------
                     
                     
                     # Define demography module ------------------------------------------------------------------------------------
                     
                     demo <- Demography(Rmax = 3, ReproductionType = 0) # sexual model with no stage structure
                     
                     # # Define dispersal module ------------------------------------------------------------------------------------
                     # disp <-  Dispersal(
                     #   # Emigration phase: stage 0 has constant emigration probability of 0.4
                     #   Emigration = Emigration(EmigProb = 0.4),
                     #   # Transfer phase: negative exponential dispersal kernel with mean dispersal distance of 8km
                     #   Transfer = DispersalKernel(DoubleKernel = T, Distances = matrix(c(15000, 250000, 0.95), ncol = 3)),
                     #   # Settlement: if individual arrives in unsuitable cells, it can randomly chose a suitable neighbouring cell or will die
                     #   Settlement = Settlement(Settle = 2)
                     # )
                     
                     # Define dispersal module ------------------------------------------------------------------------------------
                     disp <-  Dispersal(
                       # Emigration phase: stage 0 has constant emigration probability of 0.4
                       Emigration = Emigration(EmigProb = 0.4),
                       # Transfer phase: negative exponential dispersal kernel with mean dispersal distance of 8km
                       Transfer = DispersalKernel(Distances = 5000),
                       # Settlement: if individual arrives in unsuitable cells, it can randomly chose a suitable neighbouring cell or will die
                       Settlement = Settlement(Settle = 2)
                     )
                     
                     # Define initial conditions for simulations ------------------------------------------------------------------------------------
                     
                     init <- Initialise(InitType = 0, FreeType = 1, InitDens = 1)
                     
                     RepNb <- 10 #100 replicated runs
                     sim_years <- 89 #let simulation run for 89 years
                     sim <- Simulation(Simulation = 0,
                                       Replicates = RepNb,
                                       Years = spinup + sim_years,
                                       OutIntPop = 1,
                                       OutIntOcc = 1)
                     
                     s <- RSsim(batchnum = b , land = land, demog = demo, dispersal = disp, simul = sim,
                                init = init)
                     
                     # Run simulations ------------------------------------------------------------------------------------
                     
                     RunRS(s, path_input)
                     
                     # Calculate population and occupancy mean and extinction probability ------------------------------------
                     range <- readRange(s, path_input)
                     #range <- read.table(paste0(path_loop, "Outputs/Batch1_Sim0_Land1_Range.txt"), h = T, sep = "\t")
                     pop_mean <- range %>% group_by(Year) %>% summarise(Abundance = mean(NInds), sd_Ab = sd(NInds),
                                                                        Occupancy = mean(NOccupCells), sd_Oc = sd(NOccupCells)) %>% add_column(Breadth = paste0("Breadth: ", width[b]))
                     pop <- readPop(s, path_input)
                     extProb_list <- Calc_ExtProb(pop, s) %>% add_column(Breadth = paste0("Breadth: ", width[b]))
                     
                     # Plot occurrences in landscape under cc -----------------------------------------------
                     
                     # Occurrences for individuals without long dispersal
                     pdf(paste0(path_input, "Output_Maps/occurrences_landscape_Breadth",width[b], "shortdisp_Rmax3_land2.pdf"))
                     
                     #Load specific pop data set
                     pop <- read.table(paste0(path_input, "Outputs/Batch", g, "_Sim0_Land1_Pop.txt"), header = T, sep = "\t")
                     
                     #remove unimportant columns
                     pop_short <- pop %>% dplyr::select(-c(RepSeason, Species))
                     # extract occurrences
                     occ_short <- subset(pop_short, pop_short$NInd >= 1)
                     #change column names
                     colnames(occ_short)[colnames(occ_short) == "x"] <- "X"
                     colnames(occ_short)[colnames(occ_short) == "y"] <- "Y"
                     # extract only the first replication
                     occ_Rep0 <- subset(occ_short, occ_short$Rep == 0)
                     # Plot the occurrences under climate change
                     for (i in 1:length(temp_rise)) {
                       tmp <-rast(paste0(path_input, "Inputs/habitat_per_land2_breadth", width[b], "_cc", temp_rise[i], ".asc"))
                       occ_sub <- subset(occ_Rep0, occ_Rep0$Year == i+99)
                       occ_sub$X <- occ_sub$X * 1000
                       occ_sub$Y <- occ_sub$Y * 1000
                       m <- vect(occ_sub, geom = c("X", "Y"))
                       if(length(m) >0){
                         plot(tmp)
                         plot(m, add = T)
                       } else {
                         plot(tmp)
                       }
                     }
                     dev.off()
                     
                     list(pop_mean, extProb_list)
                   }

stopCluster(cl)

saveRDS(results, file = paste0(path_input, "Outputs/results_nichebreadths_shortdisp_Rmax3_land2.rds"))

#Prepare cluster
ncores <- 5
cl <- makeCluster(ncores)
registerDoParallel(cl)

#Function for combining output later
comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

# Start loops for the SDM fitting
results <- foreach(b=1:length(width), .packages = c("raster", "RangeShiftR", "dplyr", "scales", "tibble", "ggplot2", "gridExtra", "terra"), .combine = "comb",
                   .multicombine = T, .init = list(list(), list())) %dopar% {
                     
                     #obtain number for temperature increase
                     t <- 1:90
                     alpha <- 0.5
                     beta <- 0.9 
                     theta <- 0.3
                     set.seed(5678)
                     ts <- alpha + beta * t + arima.sim(list(ma = theta), n = length(t))
                     x <- as.vector(ts)
                     temp_rise <- scales::rescale(x, c(0,0.9))
                     
                     # Set up the dynamic landscapes ------------------------------------------------------------------------------------
                     # Numbers of spinup years in dynamic landscape
                     spinup <- 100
                     
                     # Set dynamic landscape parameters
                     #create vector to read in landscapes:
                     val <- temp_rise
                     val <- c("0", val)
                     landnames <- c()
                     for (i in 1:length(val)){
                       d <- val[i]
                       k <- paste0("habitat_per_land3_breadth", width[b], "_cc", d, ".asc")
                       landnames <- append(landnames, k)
                     }
                     
                     #create vector for the years
                     years <- seq(spinup,((length(landnames)-2)*1)+100,1) #minus 2 because we have the spinup and the year 0
                     years <- c("0", years)
                     years  <- as.numeric(years)
                     
                     # Define Landscape module ------------------------------------------------------------------------------------
                     
                     land <- ImportedLandscape(LandscapeFile = landnames,
                                               DynamicLandYears = years,
                                               Resolution = 1000,
                                               HabPercent = TRUE,
                                               K_or_DensDep = 0.05)
                     
                     # Define transition matrix ------------------------------------------------------------------------------------
                     
                     
                     # Define demography module ------------------------------------------------------------------------------------
                     
                     demo <- Demography(Rmax = 3, ReproductionType = 0) # sexual model with no stage structure
                     
                     # # Define dispersal module ------------------------------------------------------------------------------------
                     # disp <-  Dispersal(
                     #   # Emigration phase: stage 0 has constant emigration probability of 0.4
                     #   Emigration = Emigration(EmigProb = 0.4),
                     #   # Transfer phase: negative exponential dispersal kernel with mean dispersal distance of 8km
                     #   Transfer = DispersalKernel(DoubleKernel = T, Distances = matrix(c(15000, 250000, 0.95), ncol = 3)),
                     #   # Settlement: if individual arrives in unsuitable cells, it can randomly chose a suitable neighbouring cell or will die
                     #   Settlement = Settlement(Settle = 2)
                     # )
                     
                     # Define dispersal module ------------------------------------------------------------------------------------
                     disp <-  Dispersal(
                       # Emigration phase: stage 0 has constant emigration probability of 0.4
                       Emigration = Emigration(EmigProb = 0.4),
                       # Transfer phase: negative exponential dispersal kernel with mean dispersal distance of 8km
                       Transfer = DispersalKernel(Distances = 5000),
                       # Settlement: if individual arrives in unsuitable cells, it can randomly chose a suitable neighbouring cell or will die
                       Settlement = Settlement(Settle = 2)
                     )
                     
                     # Define initial conditions for simulations ------------------------------------------------------------------------------------
                     
                     init <- Initialise(InitType = 0, FreeType = 1, InitDens = 1)
                     
                     RepNb <- 10 #100 replicated runs
                     sim_years <- 89 #let simulation run for 89 years
                     sim <- Simulation(Simulation = 0,
                                       Replicates = RepNb,
                                       Years = spinup + sim_years,
                                       OutIntPop = 1,
                                       OutIntOcc = 1)
                     
                     g <- b+5
                     s <- RSsim(batchnum = g , land = land, demog = demo, dispersal = disp, simul = sim,
                                init = init)
                     
                     # Run simulations ------------------------------------------------------------------------------------
                     
                     RunRS(s, path_input)
                     
                     # Calculate population and occupancy mean and extinction probability ------------------------------------
                     range <- readRange(s, path_input)
                     #range <- read.table(paste0(path_loop, "Outputs/Batch1_Sim0_Land1_Range.txt"), h = T, sep = "\t")
                     pop_mean <- range %>% group_by(Year) %>% summarise(Abundance = mean(NInds), sd_Ab = sd(NInds),
                                                                        Occupancy = mean(NOccupCells), sd_Oc = sd(NOccupCells)) %>% add_column(Breadth = paste0("Breadth: ", width[b]))
                     pop <- readPop(s, path_input)
                     extProb_list <- Calc_ExtProb(pop, s) %>% add_column(Breadth = paste0("Breadth: ", width[b]))
                     
                     # Plot occurrences in landscape under cc -----------------------------------------------
                     
                     # Occurrences for individuals without long dispersal
                     pdf(paste0(path_input, "Output_Maps/occurrences_landscape_Breadth",width[b], "shortdisp_Rmax3_land3.pdf"))
                     
                     #Load specific pop data set
                     pop <- read.table(paste0(path_input, "Outputs/Batch", g, "_Sim0_Land1_Pop.txt"), header = T, sep = "\t")
                     
                     #remove unimportant columns
                     pop_short <- pop %>% dplyr::select(-c(RepSeason, Species))
                     # extract occurrences
                     occ_short <- subset(pop_short, pop_short$NInd >= 1)
                     #change column names
                     colnames(occ_short)[colnames(occ_short) == "x"] <- "X"
                     colnames(occ_short)[colnames(occ_short) == "y"] <- "Y"
                     # extract only the first replication
                     occ_Rep0 <- subset(occ_short, occ_short$Rep == 0)
                     # Plot the occurrences under climate change
                     for (i in 1:length(temp_rise)) {
                       tmp <-rast(paste0(path_input, "Inputs/habitat_per_land3_breadth", width[b], "_cc", temp_rise[i], ".asc"))
                       occ_sub <- subset(occ_Rep0, occ_Rep0$Year == i+99)
                       occ_sub$X <- occ_sub$X * 1000
                       occ_sub$Y <- occ_sub$Y * 1000
                       m <- vect(occ_sub, geom = c("X", "Y"))
                       if(length(m) >0){
                         plot(tmp)
                         plot(m, add = T)
                       } else {
                         plot(tmp)
                       }
                     }
                     dev.off()
                     
                     list(pop_mean, extProb_list)
                   }

stopCluster(cl)

saveRDS(results, file = paste0(path_input, "Outputs/results_nichebreadths_shortdisp_Rmax3_land3.rds"))

#clean all temporary files
gc()
rm(list=ls())