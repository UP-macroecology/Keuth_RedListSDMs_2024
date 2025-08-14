# Red List criteria underestimate climate-related extinction risk of range-shifting species


#-------------------------------------------------------------------------------

# ------------------------------------------------------------ #
#              07b. Create data sets for analysis              #
# ------------------------------------------------------------ #


#-------------------------------------------------------------------------------

# Create the data sets required for the statistical analysis

# define file paths
home_folder <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/") #needs to be adjusted based on own folder structure
sim_dir <- file.path(paste0(home_folder, "Simulations/"))
sdm_dir <- file.path(paste0(home_folder, "output_data/SDMs/"))

# Load packages
library(data.table)

# unique trait values
position <- c("range-contracting", "range-shifting")
breadth <- c("narrow", "wide")
rmax <- c("slow", "fast")
dispersal <- c("short", "long")
land_rep <- 1:3
BatchNum <- 1:16
set.seed(8765)
replicates <- sample(0:99, 10)

#create data frame with all trait combinations
sims_long <- expand.grid(position = position, breadth = breadth, rmax = rmax, dispersal = dispersal)
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
  data[[i]]$position <- sims_long[which(sims_long$BatchNum == i),]$position
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
  data_mean[[i]]$position <- sims_long[which(sims_long$BatchNum == i),]$position
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

#save data set
save(data_adapted_long, file=paste0(home_folder, "analysis_data/data_mean_longformat.Rdata"))
save(data_mean, file=paste0(home_folder, "analysis_data/data_mean.Rdata"))
save(data, file=paste0(home_folder, "analysis_data/data_analysis.Rdata"))

# Create data set with all performance measures ------

# create data frame with all parameter combinations
land_rep <- 1:3
position <- c("marginal", "central")
breadth <- c("narrow", "wide")
rmax <- c("slow", "fast")
dispersal <- c("short", "long")

sims <- expand.grid(land_rep = land_rep, position = position, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims$BatchNum <- rep(1:16, each = 3)

# Load data sets
performance_measures <- c()

for (sim_nr in 1:nrow(sims)) {
  rep_nr <- sims[sim_nr,]$land_rep
  BatchNum <- sims[sim_nr,]$BatchNum
  tmp <- readRDS(paste0(sdm_dir, "evaluation/performance_measures/performance_mean_SDM_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  tmp <- do.call(rbind, tmp)
  # add scenario and land replication number
  tmp$scenario <- paste(BatchNum, rep_nr, sep = ".")
  tmp$BatchNum <- BatchNum
  tmp$landRep <- rep_nr
  tmp$position <- sims[sim_nr,]$position
  tmp$breadth <- sims[sim_nr,]$breadth
  tmp$rmax <- sims[sim_nr,]$rmax
  tmp$dispersal <- sims[sim_nr,]$dispersal
  performance_measures <- rbind(performance_measures, tmp)
}

save(performance_measures, file=paste0(home_folder, "analysis_data/performance_measures.Rdata"))