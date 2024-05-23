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

# Function for calculating extinction probability
source("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/02_Simulations/Functions/Extinction_probability.R")

# create data frame with all parameter combinations
land_rep <- 1:3
optima <- c(0.27, 0.5)
breadth <- c(0.045, 0.055)
rmax <- c(3, 5)
dispersal <- c(5000, 15000)

sims <- expand.grid(land_rep = land_rep, optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims$BatchNum <- rep(1:16, each = 3)

ncores <- 24
cl <- makeCluster(ncores)
registerDoParallel(cl)

foreach(sim_nr=1:nrow(sims), .packages = c("RangeShiftR", "dplyr", "scales", "tibble", "scales", "ggplot2", "gridExtra")) %dopar% {
  # Extract parameter values
  rep_nr <- sims[sim_nr,]$land_rep
  optima <- sims[sim_nr,]$optima
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
    landnames <- append(landnames, paste0("land", rep_nr, "_optima",  optima, "_breadth", breadth, "_ccYear", i, ".asc"))
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
      
    # Run simulations ------------------------------------------------------------------------------------
      
    RunRS(s, path_input)
      
    # Calculation of different values  -------------------------------------------------------------------
      
    # Calculate extinction probability
    pop <- readPop(s, path_input)
    extProb <- Calc_ExtProb(pop, s) 
    saveRDS(extProb, paste0(sim_dir, file = "Outputs/ExtProb_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
      
    # Calculate maximum number of individuals per replicate run and year
    sumInd_s <- pop %>% group_by(Rep,Year) %>% summarise(sumPop = sum(NInd), .groups='keep') 
      
    saveRDS(sumInd_s, file = paste0(sim_dir, "Outputs/SumInd_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
      
    # Obtaining occurrence points  -------------------------------------------------------------------
    # Obtain Occurrence points for year 100 (year before cc)
    pop_year100 <- subset(pop, pop$Year == 100)
    pop_year100 <- pop_year100 %>% dplyr::select(-c(RepSeason, Species, Year))
      
    # Extract coordinates where the number of individuals was above 1
    occ_year100 <- subset(pop_year100, pop_year100_short$NInd >= 1)
    colnames(occ_year100)[colnames(occ_year100) == "x"] <- "X"
    colnames(occ_year100)[colnames(occ_year100) == "y"] <- "Y"
      
    # store the occurrence points of every replicated run in a separate element of a list
    ls_Occ<- split(occ_year100, occ_year100$Rep)
    saveRDS(ls_Occ, paste0(sdm_dir, "data/occurrences/Occ_list_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
    
    # plot outputs ---------------------------------------------------------------------------------
    pdf(paste0(path_input, "Output_Maps/Plots_Batch", BatchNum, "_Sim", rep_nr, ".pdf"))
    par(mfrow=c(2,2))
    plotAbundance(s, path_input)
    plotOccupancy(s, path_input)
    plot(NULL, type = "n", ylab = "Ext.Prob.", xlab = "Year", cex.lab = 2, cex.axis = 1.5)
    lines(extProb$Year, extProb$extProb, type = "l", lwd = 1.8, col = "black")
    # Parameter values Plot
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.45, y = 0.6, paste0("ntemp:", optima, "+", breadth),
            cex = 1.5, col = "black", family="serif", font=2, adj=0.5)
    text(x = 0.45, y = 0.5, paste0("npre:0.5+", breadth),
           cex = 1.5, col = "black", family="serif", font=2, adj=0.5)
    text(x = 0.45, y = 0.4, paste("K:0.05"),
         cex = 1.5, col = "black", family="serif", font=2, adj=0.5)
    text(x = 0.45, y = 0.3, paste0("Rmax:", rmax),
          cex = 1.5, col = "black", family="serif", font=2, adj=0.5)
    text(x = 0.45, y = 0.2, paste("EmigProb:0.4"),
          cex = 1.5, col = "black", family="serif", font=2, adj=0.5)
    if(dispersal == 5000){
      text(x = 0.45, y = 0.1, paste0("Dispersal:", dispersal),
           cex = 1.5, col = "black", family="serif", font=2, adj=0.5)
    } else {
      text(x = 0.45, y = 0.1, paste0("Dispersal:", dispersal, ", 250000, 0.95"),
           cex = 1.5, col = "black", family="serif", font=2, adj=0.5)
    }
    text(x = 0.45, y = 0.2, paste0("Land ", rep_nr),
          cex = 1.5, col = "black", family="serif", font=2, adj=0.5)
    par(mfrow=c(1,1))
      
    dev.off()
      
    # Plot occurrences in landscape under cc -----------------------------------------------
      
    # Occurrences for individuals without long dispersal
    pdf(paste0(path_input, "Output_Maps/occurrences_in_landscape_BatchNum", BatchNum, "_land", rep_nr, ".pdf"))
      
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
    for (i in 0:89) {
      tmp <-rast(paste0(sim_dir, "Inputs/land", rep_nr, "_optima",  optima, "_breadth", breadth, "_ccYear", i, ".asc"))
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
}
stopCluster(cl)

gc()
rm(list=ls())