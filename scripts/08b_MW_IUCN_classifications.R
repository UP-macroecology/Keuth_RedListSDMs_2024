# Red List criteria underestimate climate-related extinction risk of range-shifting species


#-------------------------------------------------------------------------------

# --------------------------------------------------------------------- #
#              08b. Moving Window for classification times              #
# --------------------------------------------------------------------- #


#-------------------------------------------------------------------------------

# Prepare a moving window approach to obtain the classification time points of the different species for population size, habitat size and extinction probability

# define file paths
home_folder <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/") #needs to be adjusted based on own folder structure
sim_dir <- file.path(paste0(home_folder, "Simulations/"))
sdm_dir <- file.path(paste0(home_folder, "output_data/SDMs/"))

# Load packages
library(data.table)
library(dplyr)

# Loading functions
source("scripts/00_functions.R")

# create data frame with all parameter combinations for the IUCN classification time
land_rep <- 1:3
BatchNum <- 1:16
set.seed(8765)
replicates <- sample(0:99, 10)

IUCN_classification <- expand.grid(land_rep = land_rep, BatchNum = BatchNum, replicates = replicates)

# create the VU, EN, CR columns classification time for Population size, Habitat suitability, extinction probability
IUCN_classification$VU_Pop <- NA
IUCN_classification$EN_Pop <- NA
IUCN_classification$CR_Pop <- NA
IUCN_classification$VU_HS <- NA
IUCN_classification$EN_HS <- NA
IUCN_classification$CR_HS <- NA
IUCN_classification$VU_Ext <- NA
IUCN_classification$EN_Ext <- NA
IUCN_classification$CR_Ext <- NA

# loop to apply moving window approach to every scenario
for (i in 1:nrow(IUCN_classification)) {
  BatchNum <- IUCN_classification[i, "BatchNum"]
  land_rep <- IUCN_classification[i, "land_rep"]
  rep_nr <- IUCN_classification[i, "replicates"]
  
  # Load data
  dat <- readRDS(paste0(home_folder, "analysis_data/data_analysis_raw_long_Batch", BatchNum, "_Sim", land_rep, ".rds"))
  
  # remove rows till year 100
  dat <- dat[!dat$Year %in% c(0:99),]
  
  # Criterion A3 - Pop size
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.3, "VU", "Pop")
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.5, "EN", "Pop")
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.8, "CR", "Pop")
  
  # Criterion A3 - Habitat suitability
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.3, "VU", "HS")
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.5, "EN", "HS")
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.8, "CR", "HS")
  
  # Criterion E - Extinction probability
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 100, 0.1, "VU", "Ext")
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 20, 0.2, "EN", "Ext")
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.5, "CR", "Ext")
}

#create data frame with all trait combinations
position <- c("marginal", "central")
breadth <- c("narrow", "wide")
rmax <- c("slow", "fast")
dispersal <- c("short", "long")
land_rep <- 1:3
BatchNum <- 1:16

sims_long <- expand.grid(position = position, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims_long$BatchNum <- rep(1:16)

# merge data frame to add the trait values
IUCN_classification <- merge(IUCN_classification, sims_long, by = "BatchNum")

# save data set for plotting
save(IUCN_classification, file = paste0(home_folder, "analysis_data/IUCN_classification_times_allreplicates.RData"))

# Moving average approach for the dispersal assumptions ----

# create data frame with all parameter combinations for the IUCN classification time
land_rep <- 1:3
BatchNum <- 1:16
IUCN_classification <- expand.grid(land_rep = land_rep, BatchNum = BatchNum)

# add the columns for habitat suitability as well as the dispersal assumptions
IUCN_classification$VU_HS <- NA
IUCN_classification$EN_HS <- NA
IUCN_classification$CR_HS <- NA
IUCN_classification$VU_HS_disp <- NA
IUCN_classification$EN_HS_disp <- NA
IUCN_classification$CR_HS_disp <- NA

# loop to apply moving window approach to every scenario
for (i in 1:nrow(IUCN_classification)) {
  BatchNum <- IUCN_classification[i, "BatchNum"]
  land_rep <- IUCN_classification[i, "land_rep"]
  
  # Load data
  dat <- readRDS(paste0(home_folder, "analysis_data/data_analysis_raw_Batch", BatchNum, "_Sim", land_rep, ".rds"))
  
  # remove rows till year 100
  dat <- dat[!dat$Year %in% c(0:99),]
  
  # calculate mean hs
  dat$hs_mean <- rowMeans(dat[,13:22])
  
  # Criterion A3 - HS
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.3, "VU", "HS")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.5, "EN", "HS")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.8, "CR", "HS")
  
  # calculate time point of classification for using dispersal assumptions
  dat_disp <- readRDS(paste0(home_folder, "analysis_data/hs_loss_wide_SDM_dispersal_assumptions_Batch", BatchNum, "_Sim", land_rep, ".rds"))
  
  #calculate the mean hs_loss for every startYear
  dat_disp$hs_mean <- rowMeans(dat_disp[,2:11])

  # obtain the year when the criterion is fulfilled
  IUCN_classification[i, "VU_HS_disp"] <- head(dat_disp[which(dat_disp$hs_mean >= 30), "startYear"],1)
  IUCN_classification[i, "EN_HS_disp"] <- head(dat_disp[which(dat_disp$hs_mean >= 50), "startYear"],1)
  IUCN_classification[i, "CR_HS_disp"] <- head(dat_disp[which(dat_disp$hs_mean >= 80), "startYear"],1)
}

#create data frame with all trait combinations
position <- c("marginal", "central")
breadth <- c("narrow", "wide")
rmax <- c("slow", "fast")
dispersal <- c("short", "long")
land_rep <- 1:3
BatchNum <- 1:16

sims_long <- expand.grid(position = position, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims_long$BatchNum <- rep(1:16)

# merge data frame to add the trait values
IUCN_classification <- merge(IUCN_classification, sims_long, by = "BatchNum")

# save data set
save(IUCN_classification, file = paste0(home_folder, "analysis_data/IUCN_classification_times_dispersalassumptions.RData"))