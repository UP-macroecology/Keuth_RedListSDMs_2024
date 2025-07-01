# Obtain number of replicated runs without a viable population size in the simulation for every year

library(foreach)
library(doParallel)
library(data.table)
library(dplyr)

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

#set up cluster
ncores <- 1
cl <- makeCluster(ncores)
registerDoParallel(cl)

# foreach loop through every single scenario
foreach(sim_nr=40, .packages = c("dplyr", "data.table")) %dopar% {
  
  # Extract parameter values
  rep_nr <- sims[sim_nr,]$land_rep
  BatchNum <- sims[sim_nr,]$BatchNum

  pop <- fread(paste0(sim_dir, "Outputs/Batch", BatchNum, "_Sim", rep_nr, "_Land1_Pop.txt"))

  pop_sum <- pop %>%
    group_by(Rep,Year) %>%
    # Sum individuals over all cells per year and replicate
    summarise(sumPop = sum(NInd), .groups='keep') %>% 
    group_by(Year) %>%
    summarise(replicate_runs = sum(sumPop>0, na.rm=T))
  
  # save data set
  saveRDS(pop_sum, file = paste0(sim_dir, "Outputs/Nr_replicates_year_Batch", BatchNum, "_Sim", rep_nr, ".rds"))

} #close foreach loop
#stopCluster(cl)

gc()
rm(list=ls())




