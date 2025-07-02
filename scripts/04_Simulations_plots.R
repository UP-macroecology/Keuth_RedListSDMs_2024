# Red List criteria underestimate climate-related extinction risk of range-shifting species


#-------------------------------------------------------------------------------

# -------------------------------------------------------------------------- #
#                         04a. Plot results of Simulations                   #
# -------------------------------------------------------------------------- #


#-------------------------------------------------------------------------------

# Visualising the results of the RangeShifter Simulations

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

# create data frame with all parameter combinations
land_rep <- 1:3
position <- c(0.27, 0.5)
breadth <- c(0.045, 0.055)
rmax <- c(3, 5)
dispersal <- c(5000, 15000)

sims <- expand.grid(land_rep = land_rep, position = position, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims$BatchNum <- rep(1:16, each = 3)

ncores <- 5
cl <- makeCluster(ncores)
registerDoParallel(cl)

foreach(sim_nr=1:nrow(sims), .packages = c("RangeShiftR", "dplyr", "tibble", "scales", "ggplot2", "gridExtra", "terra", "data.table")) %dopar% {
  # Extract parameter values
  rep_nr <- sims[sim_nr,]$land_rep
  position <- sims[sim_nr,]$position
  breadth <- sims[sim_nr,]$breadth
  rmax <- sims[sim_nr,]$rmax
  dispersal <- sims[sim_nr,]$dispersal
  BatchNum <- sims[sim_nr,]$BatchNum
  
  # Create all submodules for the RangeShifter simulation for visual inspection of results ------------
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
                    OutIntOcc = 1)

  s <- RSsim(batchnum = BatchNum, land = land, demog = demo, dispersal = disp, simul = sim, init = init)
  
  # Load in required data ----------------------------
  pop <- fread(paste0(sim_dir, "Outputs/Batch", BatchNum, "_Sim", rep_nr, "_Land1_Pop.txt"))
  extProb <- readRDS(paste0(sim_dir, "Outputs/ExtProb_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  
  # plot outputs ---------------------------------------------------------------------------------
  pdf(paste0(sim_dir, "Output_Maps/Plots_Batch", BatchNum, "_Sim", rep_nr, ".pdf"))
  par(mfrow=c(2,2))
  plotAbundance(s, sim_dir)
  plotOccupancy(s, sim_dir)
  plot(NULL, type = "n", ylab = "Ext.Prob.", xlab = "Year", cex.lab = 1.5, cex.axis = 1, xlim = c(0,max(extProb$Year)), ylim = c(0,max(extProb$extProb)))
  lines(extProb$Year, extProb$extProb, type = "l", lwd = 1.8, col = "black")
  # Parameter values Plot
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.45, y = 0.7, paste0("ntemp:", position, "+", breadth),
       cex = 1, col = "black", family="serif", font=2, adj=0.5)
  text(x = 0.45, y = 0.6, paste0("npre:0.5+", breadth),
       cex = 1, col = "black", family="serif", font=2, adj=0.5)
  text(x = 0.45, y = 0.5, paste("K:0.05"),
       cex = 1, col = "black", family="serif", font=2, adj=0.5)
  text(x = 0.45, y = 0.4, paste0("Rmax:", rmax),
       cex = 1, col = "black", family="serif", font=2, adj=0.5)
  text(x = 0.45, y = 0.3, paste("EmigProb:0.4"),
       cex = 1, col = "black", family="serif", font=2, adj=0.5)
  if(dispersal == 5000){
    text(x = 0.45, y = 0.2, paste0("Dispersal:", dispersal),
         cex = 1, col = "black", family="serif", font=2, adj=0.5)
  } else {
    text(x = 0.45, y = 0.2, paste0("Dispersal:", dispersal, ", 250000, 0.95"),
         cex = 1, col = "black", family="serif", font=2, adj=0.5)
  }
  text(x = 0.45, y = 0.1, paste0("Land ", rep_nr),
       cex = 1, col = "black", family="serif", font=2, adj=0.5)
  par(mfrow=c(1,1))

  dev.off()
  
  # Plot occurrences in landscape under cc -----------------------------------------------
  
  # Occurrences for individuals without long dispersal
  pdf(paste0(sim_dir, "Output_Maps/occurrences_in_landscape_BatchNum", BatchNum, "_land", rep_nr, ".pdf"))
  
  #remove unimportant columns
  pop_short <- pop %>% dplyr::select(-c(RepSeason, Species))
  # extract occurrences
  occ_short <- subset(pop_short, pop_short$NInd >= 1)
  #change column names
  occ_short$x <- (occ_short$x + 0.5) * 1000
  occ_short$y <- (occ_short$y + 0.5) * 1000
  colnames(occ_short)[colnames(occ_short) == "x"] <- "X"
  colnames(occ_short)[colnames(occ_short) == "y"] <- "Y"
  # extract only the first replication
  occ_Rep0 <- subset(occ_short, occ_short$Rep == 0)
  
  # Plot the occurrences under climate change
  for (i in 0:89) {
    tmp <-terra::rast(paste0(sim_dir, "Inputs/land", rep_nr, "_position",  position, "_breadth", breadth, "_ccYear", i, ".asc"))
    occ_sub <- subset(occ_Rep0, occ_Rep0$Year == i+100)
    m <- terra::vect(occ_sub, geom = c("X", "Y"))
    if(length(m) >0){
      terra::plot(tmp, col = rev(grDevices::terrain.colors(50)))
      terra::plot(m, add = T)
    } else {
      terra::plot(tmp, col = rev(grDevices::terrain.colors(50)))
    }
  }
  dev.off()
  
}
stopCluster(cl)

gc()
rm(list=ls())