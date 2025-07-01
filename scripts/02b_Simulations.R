# Red List criteria underestimate climate-related extinction risk of range-shifting species


#-------------------------------------------------------------------------------

# -------------------------------------------------------------------------- #
#                         02b. Run RangeShifter Simulations                   #
# -------------------------------------------------------------------------- #


#-------------------------------------------------------------------------------

# RangeShifter simulations for the 16 different virtual species for each landscape replica
# Here the trait values growth rate and dispersal are adapted

# The different values for the different scenarios are
# niche position: marginal (0.27), central (0.5)
# niche breadth: narrow (0.025), wide (0.035)
# growth rate: slow (3), fast (5)
# dispersal: short (5000), long (15 000, 250 000, 0.95)


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

# define file paths
home_folder <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/") #needs to be adjusted based on own folder structure
sim_dir <- file.path(paste0(home_folder, "Simulations/"))
sdm_dir <- file.path(paste0(home_folder, "output_data/SDMs/"))

# Loading functions
source("scripts/00_functions.R")

# create data frame with all parameter combinations
land_rep <- 1:3
position <- c(0.27, 0.5)
breadth <- c(0.045, 0.055)
rmax <- c(3, 5)
dispersal <- c(5000, 15000)

sims <- expand.grid(land_rep = land_rep, position = position, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims$BatchNum <- rep(1:16, each = 3)

#prepare script to run on HPC
ncores <- 10
cl <- makeCluster(ncores)
registerDoParallel(cl)

foreach(sim_nr=1:nrow(sims), .packages = c("RangeShiftR", "dplyr", "scales", "tibble", "scales", "ggplot2", "gridExtra", "terra")) %dopar% {
  # Extract parameter values
  rep_nr <- sims[sim_nr,]$land_rep
  position <- sims[sim_nr,]$position
  breadth <- sims[sim_nr,]$breadth
  rmax <- sims[sim_nr,]$rmax
  dispersal <- sims[sim_nr,]$dispersal
  BatchNum <- sims[sim_nr,]$BatchNum

  # Set up the dynamic landscapes ------------------------------------------------------------------------------------
  # Numbers of spinup years in dynamic landscape
  spinup <- 100
      
  # Set dynamic landscape parameters
  landnames <- c()
  for (i in 0:89){
    landnames <- append(landnames, paste0("land", rep_nr, "_position",  position, "_breadth", breadth, "_ccYear", i, ".asc"))
  }
      
    #create vector for the years (landscapes start changing at year 101)
    years <- seq(spinup+1, length(landnames)+99,1)
    years <- c("0", years)
    years  <- as.numeric(years)
      
    # Define Landscape module ------------------------------------------------------------------------------------
      
    land <- ImportedLandscape(LandscapeFile = landnames,
                              DynamicLandYears = years,
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
    sim_years <- 90 #let simulation run for 90 years
    sim <- Simulation(Simulation = rep_nr,
                      Replicates = RepNb,
                      Years = spinup + sim_years,
                      OutIntPop = 1,
                      OutIntOcc = 1,
                      OutIntInd = 1)
      
    s <- RSsim(batchnum = BatchNum, land = land, demog = demo, dispersal = disp, simul = sim, init = init)
      
    # Run simulations ------------------------------------------------------------------------------------
      
    RunRS(s, sim_dir)
    
}
stopCluster(cl)

gc()
rm(list=ls())