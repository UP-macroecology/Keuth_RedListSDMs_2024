# Ran simulation again to extract dispersal distance of individuals

# RangeShifter simulations for the 16 different scenarios for each landscape replica

# The different values for the different scenarios are
# niche position: cold (0.27), warm (0.5)
# niche breadth: narrow (0.025), wide (0.035)
# reproduction: slow (3), fast (5)
# dispersal: short (5000), long (15 000, 250 000, 0.95)

#define file path
sim_dir <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/02_Simulations/")
sdm_dir <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/03_SDMs/")

# Load packages
require(RangeShiftR)
library(dplyr)
require(foreach)
require(doParallel)
library(tibble)
library(ggplot2)
library(gridExtra)
library(scales)
library(data.table)
library(terra)

# Function for calculating extinction probability
source("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/Functions/Extinction_probability.R")

# create data frame with all parameter combinations
land_rep <- 1:3
optima <- c(0.27, 0.5)
breadth <- c(0.045, 0.055)
rmax <- c(3, 5)
dispersal <- c(5000, 15000)

sims <- expand.grid(land_rep = land_rep, optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims$BatchNum <- rep(1:16, each = 3)

ncores <- 12
cl <- makeCluster(ncores)
registerDoParallel(cl)

foreach(sim_nr=1:nrow(sims), .packages = c("RangeShiftR", "dplyr", "scales", "tibble", "scales", "ggplot2", "gridExtra", "terra")) %dopar% {
  # Extract parameter values
  rep_nr <- sims[sim_nr,]$land_rep
  optima <- sims[sim_nr,]$optima
  breadth <- sims[sim_nr,]$breadth
  rmax <- sims[sim_nr,]$rmax
  dispersal <- sims[sim_nr,]$dispersal
  BatchNum <- sims[sim_nr,]$BatchNum + 16
  
  # Set up the dynamic landscapes ------------------------------------------------------------------------------------
  # Numbers of spinup years in dynamic landscape
  spinup <- 100
  
  # Define Landscape module ------------------------------------------------------------------------------------
  
  land <- ImportedLandscape(LandscapeFile = paste0("land", rep_nr, "_optima",  optima, "_breadth", breadth, "_ccYear0.asc"),
                            Resolution = 1000,
                            HabPercent = TRUE,
                            K_or_DensDep = 0.05)
  
  # Define demography module ------------------------------------------------------------------------------------
  
  demo <- Demography(Rmax = rmax, ReproductionType = 0) # asexual model with no stage structure
  
  # Define dispersal module ------------------------------------------------------------------------------------
  if(dispersal == 5000){
    disp <-  Dispersal(
      # Emigration phase: constant emigration probability of 0.4
      Emigration = Emigration(EmigProb = 0.4),
      # Transfer phase: negative exponential dispersal kernel with mean dispersal distance dependent on scenarios
      Transfer = DispersalKernel(Distances = dispersal),
      # Settlement: if individual arrives in unsuitable cells, it can randomly chose a suitable neighbouring cell or will die
      Settlement = Settlement(Settle = 2)
    )
  } else {
    disp <-  Dispersal(
      # Emigration phase: stage 0 has constant emigration probability of 0.4
      Emigration = Emigration(EmigProb = 0.4),
      # Transfer phase: negative exponential dispersal kernel with mean dispersal distance of 8km
      Transfer = DispersalKernel(DoubleKernel = T, Distances = matrix(c(dispersal, 250000, 0.95), ncol = 3)),
      # Settlement: if individual arrives in unsuitable cells, it can randomly chose a suitable neighbouring cell or will die
      Settlement = Settlement(Settle = 2)
    )
  }
  
  # Define initial conditions for simulations ------------------------------------------------------------------------------------
  
  # Initialisation in all suitable cells at half carrying capacity
  init <- Initialise(InitType = 0, FreeType = 1, InitDens = 1)
  
  RepNb <- 100 #100 replicated runs
  sim <- Simulation(Simulation = rep_nr,
                    Replicates = RepNb,
                    Years = spinup,
                    OutIntInd = 1)
  
  s <- RSsim(batchnum = BatchNum, land = land, demog = demo, dispersal = disp, simul = sim, init = init)
  
  # Run simulations ------------------------------------------------------------------------------------
  
  RunRS(s, sim_dir)
  
}
stopCluster(cl)

gc()
rm(list=ls())