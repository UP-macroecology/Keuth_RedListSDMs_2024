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


# unique trait values
optima <- c("range-contracting", "range-shifting")
breadth <- c("narrow", "wide")
rmax <- c("slow", "fast")
dispersal <- c("short", "long")
land_rep <- 1:3
BatchNum <- 1:16
set.seed(8765)
replicates <- sample(0:99, 10)

#create data frame with all trait combinations
sims_long <- expand.grid(optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims_long$BatchNum <- rep(1:16)

# prepare data sets
data <- vector("list", 16)
data_mean <- vector("list", 16)

for (i in 1:16){
  # data set for land replication 1
  tmp1 <- readRDS(paste0("4_Analysis/data/data_analysis_relative_long_Batch", i, "_Sim1.rds"))
  # data set for land replication 2
  tmp2 <- readRDS(paste0("4_Analysis/data/data_analysis_relative_long_Batch", i, "_Sim2.rds"))
  # data set for land replication 3
  tmp3 <- readRDS(paste0("4_Analysis/data/data_analysis_relative_long_Batch", i, "_Sim3.rds"))
  
  #rbind the different data sets
  data[[i]] <- rbind(tmp1, tmp2)
  data[[i]] <- rbind(data[[i]], tmp3)
  
  #add traits to data set
  data[[i]]$optima <- sims_long[which(sims_long$BatchNum == i),]$optima
  data[[i]]$breadth <- sims_long[which(sims_long$BatchNum == i),]$breadth
  data[[i]]$rmax <- sims_long[which(sims_long$BatchNum == i),]$rmax
  data[[i]]$dispersal <- sims_long[which(sims_long$BatchNum == i),]$dispersal
  
  #calculate habitat loss
  data[[i]]$hs_loss <- 1- data[[i]]$hs_change
  
  #set NAs to 1 (only occurr at the very end of the simulation time)
  data[[i]][which(is.na(data[[i]]$hs_loss)), "hs_loss"] <- 1
  
  #transform column
  data[[i]]$land <- as.character(data[[i]]$land)
  
  # calculate mean and standard deviation of population size, habitat size and extinction probability
  data_mean[[i]] <- data[[i]] %>% group_by(Year) %>% summarise(meanPop = mean(pop_sum), sdPop = sd(pop_sum), meanHS = mean(hs_change), sdHS = sd(hs_change),
                                                               meanExt = mean(extProb), sdExt = sd(extProb), .groups='keep')
  
  #add traits to data set
  data_mean[[i]]$optima <- sims_long[which(sims_long$BatchNum == i),]$optima
  data_mean[[i]]$breadth <- sims_long[which(sims_long$BatchNum == i),]$breadth
  data_mean[[i]]$rmax <- sims_long[which(sims_long$BatchNum == i),]$rmax
  data_mean[[i]]$dispersal <- sims_long[which(sims_long$BatchNum == i),]$dispersal
  
  # change time column
  data_mean[[i]]$Year <- data_mean[[i]]$Year - 100
}

# append data to large data frame
data_append <- do.call(rbind, data)

# remove values larger than 1 for pop_sum and hs_loss
data_adapted <- lapply(data, function(x){
  x[which(x$hs_loss < 0), "hs_loss"] <- 0; 
  x[which(x$pop_sum > 1), "pop_sum"] <- 1; 
  return(x)
})

data_adapted_long <- do.call(rbind, data_adapted)
data_adapted_long$predictions <- NA
data_adapted_long <- as.data.frame(data_adapted_long)

#save(data_adapted_long, file="4_Analysis/data/raw_data_longformat.RData")
#load("4_Analysis/data/raw_data_longformat.RData")
#data_adapted_long$hs_loss_squared <- I(data_adapted_long$hs_loss^2)
#save(data_adapted_long, file="4_Analysis/data/data_bayes_model.Rdata")

#Load model data
#load("4_Analysis/Model Results/Model_ordbeta_full.Rdata")

# optima <- c("range-contracting", "range-shifting")
# breadth <- c("narrow", "wide")
# rmax <- c("slow", "fast")
# dispersal <- c("short", "long")
# land_rep <- 1:3
# 
# #create data frame with all trait combinations
# sims_long <- expand.grid(optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal, land = land_rep)
# 
# #add new hs_loss predictions to it
# hs_loss <- seq(0,1,length=100)
# 
# # expand data set by length of vector
# new_data <- sims_long[rep(seq_len(nrow(sims_long)), length(hs_loss)), ]
# new_data$index <- as.numeric(row.names(new_data))
# new_data <- new_data[order(new_data$index), ]
# new_data <- cbind(new_data, hs_loss)
# new_data$land <- factor(new_data$land, levels = c("1", "2", "3"))
# 
# # predict to new data
# predictions_data_optima <- cbind(new_data, predict(model, newdata=new_data,  type = "response", se.fit=T))
# predictions_data_breadth <- cbind(new_data, predict(model, newdata=new_data,  type = "response", se.fit=T))
# predictions_data_rmax <- cbind(new_data, predict(model, newdata=new_data,  type = "response", se.fit=T))
# predictions_data_dispersal <- cbind(new_data, predict(model, newdata=new_data,  type = "response", se.fit=T))
# 
# save(predictions_data_optima, predictions_data_breadth, predictions_data_rmax, predictions_data_dispersal, file = "4_Analysis/data/Model_predictions_OBR_plot.Rdata")


# unique trait values
optima <- c("marginal", "central")
breadth <- c("narrow", "wide")
rmax <- c("slow", "fast")
dispersal <- c("short", "long")
land_rep <- 1:3
BatchNum <- 1:16
set.seed(8765)
replicates <- sample(0:99, 10)

#create data frame with all trait combinations
sims_long <- expand.grid(optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims_long$BatchNum <- rep(1:16)

# prepare data sets
data <- vector("list", 16)
data_mean <- vector("list", 16)

for (i in 1:16){
  # data set for land replication 1
  data[[i]] <- readRDS(paste0("4_Analysis/data/data_analysis_relative_long_Batch", i, "_Sim1.rds"))
  # data set for land replication 2
  #tmp2 <- readRDS(paste0("4_Analysis/data/data_analysis_relative_long_Batch", i, "_Sim2.rds"))
  # data set for land replication 3
  #tmp3 <- readRDS(paste0("4_Analysis/data/data_analysis_relative_long_Batch", i, "_Sim3.rds"))
  # the data sets include the abundance and the extinction probability from the simulation, and the habitat suitability predicted by the mean ensemble model of the three algorithms using a threshold value for 
  # calculating the sums of habitat suitability
  
  #rbind the different data sets
  #data[[i]] <- rbind(tmp1, tmp2)
  #data[[i]] <- rbind(data[[i]], tmp3)
  
  #add traits to data set
  data[[i]]$optima <- sims_long[which(sims_long$BatchNum == i),]$optima
  data[[i]]$breadth <- sims_long[which(sims_long$BatchNum == i),]$breadth
  data[[i]]$rmax <- sims_long[which(sims_long$BatchNum == i),]$rmax
  data[[i]]$dispersal <- sims_long[which(sims_long$BatchNum == i),]$dispersal
  
  #calculate habitat loss
  data[[i]]$hs_loss <- 1- data[[i]]$hs_change
  
  #set NAs to 1 (only occurr at the very end of the simulation time)
  data[[i]][which(is.na(data[[i]]$hs_loss)), "hs_loss"] <- 1
  
  #transform column
  #data[[i]]$land <- as.character(data[[i]]$land)
  
  # calculate mean and standard deviation of population size, habitat size and extinction probability
  data_mean[[i]] <- data[[i]] %>% group_by(Year) %>% summarise(meanPop = mean(pop_sum), sdPop = sd(pop_sum), meanHS = mean(hs_change), sdHS = sd(hs_change),
                                                               meanExt = mean(extProb), sdExt = sd(extProb), .groups='keep')
  
  #add traits to data set
  data_mean[[i]]$optima <- sims_long[which(sims_long$BatchNum == i),]$optima
  data_mean[[i]]$breadth <- sims_long[which(sims_long$BatchNum == i),]$breadth
  data_mean[[i]]$rmax <- sims_long[which(sims_long$BatchNum == i),]$rmax
  data_mean[[i]]$dispersal <- sims_long[which(sims_long$BatchNum == i),]$dispersal
  
  # change time column
  data_mean[[i]]$Year <- data_mean[[i]]$Year - 100
}



