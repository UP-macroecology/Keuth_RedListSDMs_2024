# Red List criteria underestimate climate-related extinction risk of range-shifting species


#-------------------------------------------------------------------------------

# -------------------------------------------------------------------------- #
#                         03a. Data extraction from simulations              #
# -------------------------------------------------------------------------- #


#-------------------------------------------------------------------------------

# Extracting and calculating data based on simulation results

# Load packages
library(dplyr)
require(foreach)
require(doParallel)
library(tibble)
library(scales)
library(data.table)

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

#prepare HPC
ncores <- 5
cl <- makeCluster(ncores)
registerDoParallel(cl)

foreach(sim_nr=1:nrow(sims), .packages = c("dplyr", "tibble", "scales", "data.table")) %dopar% {
  # Extract parameter values
  rep_nr <- sims[sim_nr,]$land_rep
  position <- sims[sim_nr,]$position
  breadth <- sims[sim_nr,]$breadth
  rmax <- sims[sim_nr,]$rmax
  dispersal <- sims[sim_nr,]$dispersal
  BatchNum <- sims[sim_nr,]$BatchNum
  
  # Calculation of different values  -------------------------------------------------------------------

  # Calculate extinction probability
  pop <- fread(paste0(sim_dir, "Outputs/Batch", BatchNum, "_Sim", rep_nr, "_Land1_Pop.txt"))

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
  occ_year100 <- subset(pop_year100, pop_year100$NInd >= 1)

  # change coordinates to match climate coordinates (move coordinate to center and multiply with the resolution)
  occ_year100$x <- (occ_year100$x + 0.5) * 1000
  occ_year100$y <- (occ_year100$y + 0.5) * 1000
  colnames(occ_year100)[colnames(occ_year100) == "x"] <- "X"
  colnames(occ_year100)[colnames(occ_year100) == "y"] <- "Y"

  # store the occurrence points of every replicated run in a separate element of a list
  ls_Occ<- split(occ_year100, occ_year100$Rep)
  saveRDS(ls_Occ, paste0("output_data/occurrences/Occ_list_Batch", BatchNum, "_Sim", rep_nr, ".rds"))

}
stopCluster(cl)

gc()
rm(list=ls())