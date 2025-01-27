# Clean up cluster

# Loading packages
library(foreach)
library(doParallel)

#define file path
sim_dir <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/02_Simulations/")

# create data frame with all parameter combinations
land_rep <- 1:3
optima <- c(0.27, 0.5)
breadth <- c(0.045, 0.055)
rmax <- c(3, 5)
dispersal <- c(5000, 15000)

sims <- expand.grid(land_rep = land_rep, optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims$BatchNum <- rep(1:16, each = 3)

# obtain the 10 random replications from the 100
set.seed(8765)
replicates <- sample(0:99, 10)

# load in the dispersal assumption values
#load(paste0(sdm_dir, "data/values_dispersal_assumption.Rdata"))

#set up cluster
ncores <- 48
cl <- makeCluster(ncores)
registerDoParallel(cl)

# foreach loop through every single scenario
foreach(sim_nr=1:nrow(sims), .packages = c("dplyr", "data.table")) %dopar% {
  #for(sim_nr in 27){
  
  # Prepare variables --------------
  rep_nr <- sims[sim_nr,]$land_rep
  optima <- sims[sim_nr,]$optima
  breadth <- sims[sim_nr,]$breadth
  BatchNum <- sims[sim_nr,]$BatchNum
  dispersal <- sims[sim_nr,]$dispersal

  # Load all individual dispersal files
  df <- data.frame(Rep = NA, Year = NA, Natal_X = NA, Natal_Y = NA, X = NA, Y = NA, DistMoved = NA)
  for (replicate in 0:99) {
    BatchNum_disp <- BatchNum + 16
    tmp <- fread(paste0(sim_dir, "Outputs/Batch", BatchNum_disp, "_Sim", rep_nr, "_Land1_Rep", replicate, "_Inds.txt"))
    tmp <- tmp %>% select(., -c("RepSeason", "Species", "IndID", "Status"))
    df <- rbind(df, tmp)
  }

  df <-df[-1,]

  save(df, file = paste0(sim_dir, "Outputs/Batch", BatchNum_disp, "_Sim", rep_nr, "_Land1_Inds_large.Rdata"))

}

stopCluster(cl)

gc()
rm(list=ls())
