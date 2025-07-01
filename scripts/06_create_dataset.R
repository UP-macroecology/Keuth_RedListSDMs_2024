# Join the different outputs into one cohesive data set for all scenarios and landscape replications

# Load packages
library(data.table)
library(doParallel)
library(foreach)

# File path
sdm_dir <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/03_SDMs/")
sim_dir <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/02_Simulations/")

# Create the parameter table
land_rep <- 1:3
optima <- c(0.27, 0.5)
breadth <- c(0.045, 0.055)
rmax <- c(3, 5)
dispersal <- c(5000, 15000)

sims <- expand.grid(land_rep = land_rep, optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims$BatchNum <- rep(1:16, each = 3)

# obtain the 10 replicated runs
set.seed(8765)
replicates <- sample(0:99, 10)

#set up cluster
ncores <- 24
cl <- makeCluster(ncores)
registerDoParallel(cl)

# Loops for the SDM fitting
foreach(sim_nr=1:nrow(sims), .packages = c("data.table", "dplyr", "tidyr", "stringr")) %dopar% {
  
  # Prepare variables --------------
  rep_nr <- sims[sim_nr,]$land_rep
  BatchNum <- sims[sim_nr,]$BatchNum

  # Load data ----------
  #data_raw <- readRDS("4_Analysis/data/ExtProb_Batch10_Sim1.rds")
  data_raw <- readRDS(paste0(sim_dir, "Outputs/ExtProb_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  #pop <- readRDS("4_Analysis/data/SumInd_Batch10_Sim1.rds")
  pop <- readRDS(paste0(sim_dir, "Outputs/SumInd_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  #habitat <- readRDS("4_Analysis/data/habitat_suitability_SDM_Batch10_Sim1.rds")
  habitat <- readRDS(paste0(sdm_dir, "results/habitat_suitability_SDM_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  #range <- readRDS("4_Analysis/data/range_size_SDM_Batch10_Sim1.rds")
  range <- readRDS(paste0(sdm_dir, "results/range_size_SDM_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  
  #Extract population values from replicates I looked at
  pop <- subset(pop, pop$Rep %in% replicates)

  #add population value of each replicate as separate column
  for (i in 1:length(replicates)) {
    tmp <- subset(pop$sumPop, pop$Rep == replicates[i])
    data_raw$tmp <- 0
    data_raw$tmp[1:length(tmp)] <- tmp
    colnames(data_raw)[colnames(data_raw) == 'tmp'] <- paste0("pop_sum", replicates[i])
    rm(tmp)
  }

  # add columns for hs change (as many as number of replicated runs)
  for (i in 1:length(replicates)) {
    data_raw$tmp <- NA
    colnames(data_raw)[colnames(data_raw) == 'tmp'] <- paste0("hs_change", replicates[i])
  }

  # add habitat suitability to data set
  for (i in 1:length(replicates)) {
    data_raw[100:189,i+12] <- habitat[[i]]
  }

  # add range size to the data set
  for (i in 1:length(replicates)) {
    data_raw$tmp <- NA
    colnames(data_raw)[colnames(data_raw) == 'tmp'] <- paste0("range_change", replicates[i])
  }

  for (i in 1:length(replicates)) {
    data_raw[100:189,i+22] <- as.numeric(range[[i]])
  }
  
  #add column for BatchNum and landscape replicate
  data_raw$BatchNum <- BatchNum
  data_raw$land <- rep_nr

  # save data set
  #saveRDS(data_raw, paste0(sdm_dir, "results/data_analysis_raw_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  
  # append the single columns with each other
  data_long_pop <- pivot_longer(data_raw[, 1:12], cols = !c(Year, extProb), values_to = "pop_sum") %>% mutate(Rep = as.numeric(str_extract(name, "\\d+")))
  data_long_habitat <- pivot_longer(data_raw[, c(1:2,13:22)], cols = !c(Year, extProb), values_to = "hs_change") %>%  mutate(Rep = as.numeric(str_extract(name, "\\d+")))
  data_long_range <- pivot_longer(data_raw[, c(1:2,23:32)], cols = !c(Year, extProb), values_to = "range_change") %>%  mutate(Rep = as.numeric(str_extract(name, "\\d+")))
  
  # join the three different data sets
  data_long_raw <- full_join(data_long_pop, data_long_habitat, by = c("Year", "Rep"))
  data_long_raw <- full_join(data_long_raw, data_long_range, by = c("Year", "Rep"))
  
  # remove double columns
  data_long_raw <- data_long_raw[, -which(names(data_long_raw) %in% c("extProb.x", "extProb.y", "name", "name.x", "name.y"))]
  
  #rearrange columns
  data_long_raw <- data_long_raw %>% relocate(Rep, .before = Year) %>% relocate(extProb, .after = Year)
  
  # save data set
  saveRDS(data_long_raw, paste0(sdm_dir, "results/data_analysis_raw_long_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  
  # #add column for BatchNum and landscape replicate
  # data_long$BatchNum <- BatchNum
  # data_long$land <- rep_nr
  # 
  # # save data set
  # saveRDS(data_long, paste0(sdm_dir, "results/data_analysis_relative_long_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  # 
  # # remove additional rows of the spin up period to calculate the relative changes
  # data_rel <- data_raw[100:190,]
  # 
  # # calculate relative values
  # for (i in 3:32) {
  #   for (j in 2:nrow(data_rel)) {
  #     data_rel[j,i] <- data_rel[j,i]/data_rel[1,i]
  #   }
  #   data_rel[1,i] <- 1
  # }
  # 
  # #add column for BatchNum and landscape replicate
  # data_rel$BatchNum <- BatchNum
  # data_rel$land <- rep_nr
  # 
  # # save data set
  # saveRDS(data_rel, paste0(sdm_dir, "results/data_analysis_relative_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  # 
  # # append the single columns with each other
  # data_long_pop <- pivot_longer(data_rel[, 1:12], cols = !c(Year, extProb), values_to = "pop_sum") %>% mutate(Rep = as.numeric(str_extract(name, "\\d+")))
  # data_long_habitat <- pivot_longer(data_rel[, c(1:2,13:22)], cols = !c(Year, extProb), values_to = "hs_change") %>%  mutate(Rep = as.numeric(str_extract(name, "\\d+")))
  # data_long_range <- pivot_longer(data_rel[, c(1:2,23:32)], cols = !c(Year, extProb), values_to = "range_change") %>%  mutate(Rep = as.numeric(str_extract(name, "\\d+")))
  # 
  # # join the three different data sets
  # data_long <- full_join(data_long_pop, data_long_habitat, by = c("Year", "Rep"))
  # data_long <- full_join(data_long, data_long_range, by = c("Year", "Rep"))
  # 
  # # remove double columns
  # data_long <- data_long[, -which(names(data_long) %in% c("extProb.x", "extProb.y", "name", "name.x", "name.y"))]
  # 
  # #rearrange columns
  # data_long <- data_long %>% relocate(Rep, .before = Year) %>% relocate(extProb, .after = Year)
  # 
  # #add column for BatchNum and landscape replicate
  # data_long$BatchNum <- BatchNum
  # data_long$land <- rep_nr
  # 
  # # save data set
  # saveRDS(data_long, paste0(sdm_dir, "results/data_analysis_relative_long_Batch", BatchNum, "_Sim", rep_nr, ".rds"))

} #close foreach loop
stopCluster(cl)

gc()
rm(list=ls())
