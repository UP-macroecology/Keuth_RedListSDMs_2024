# Use Moving window approach to determine classification time point

# Load packages
library(data.table)
library(dplyr)

# Moving window approach using the values of every single replicate run ----------

# write moving window function for every single replicate
MW_replicates <- function(data, new_data, timehorizon, threshold, category, metric){
  
  # select the right column depending on the selected metric
  if(metric == "Pop"){
    column_data <- "pop_sum"
  } else if(metric == "HS") {
    column_data <- "hs_change"
  } else {
    column_data <- "extProb"
  }
  
  # reduce timehorizon if it exceeds the maximum number of years in the data set
  if(timehorizon > max(data$Year) - 100){
    timehorizon <- max(data$Year) - 100
    #print(timehorizon)
  }
  
  # for every year calculated the relative size to the size x years into the future
  for (i in 1:nrow(data)) {
    if(column_data == "extProb"){
      rel_loss <- data[data$Year == unique(data$Year)[i+timehorizon] & data$Rep == rep_nr, column_data]
      #print(rel_loss)
    } else {
      rel_loss <- 1 - (data[data$Year == unique(data$Year)[i+timehorizon] & data$Rep == rep_nr, column_data]/data[data$Year == unique(data$Year)[i] & data$Rep == rep_nr, column_data])
      #print(rel_loss)
    }
    
    # controls if the relative loss exceeds the threshold
    if(rel_loss >= threshold){
      #print(unique(data$Year)[i])
      new_data[new_data$BatchNum == BatchNum & new_data$land_rep == land_rep & new_data$replicates == rep_nr, paste(category, metric, sep = "_")] <- (unique(data$Year)[i])-100
      return(new_data)
      break
    }
  }
}

# create new data frame, which contains the years of reaching the criteria/ species being classified
land_rep <- 1:3
BatchNum <- 1:16
set.seed(8765)
replicates <- sample(0:99, 10)

# create data frame with all parameter combinations for the IUCN classification time
IUCN_classification <- expand.grid(land_rep = land_rep, BatchNum = BatchNum, replicates = replicates)

# create the VU, EN, CR columns classification time for all metrices (Pop, Range, HS, Ext.Prob)
IUCN_classification$VU_Pop <- NA
IUCN_classification$EN_Pop <- NA
IUCN_classification$CR_Pop <- NA
IUCN_classification$VU_HS <- NA
IUCN_classification$EN_HS <- NA
IUCN_classification$CR_HS <- NA
IUCN_classification$VU_Ext <- NA
IUCN_classification$EN_Ext <- NA
IUCN_classification$CR_Ext <- NA

# performs the same code for every scenario
for (i in 1:nrow(IUCN_classification)) {
  BatchNum <- IUCN_classification[i, "BatchNum"]
  land_rep <- IUCN_classification[i, "land_rep"]
  rep_nr <- IUCN_classification[i, "replicates"]
  
  # Load data
  #dat <- readRDS("4_Analysis/data/data_analysis_raw_long_Batch10_Sim1.rds")
  dat <- readRDS(paste0("4_Analysis/data/data_analysis_raw_long_Batch", BatchNum, "_Sim", land_rep, ".rds"))
  
  # remove rows till year 100
  dat <- dat[!dat$Year %in% c(0:99),]
  
  # Criterion A3 - Pop size
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.3, "VU", "Pop")
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.5, "EN", "Pop")
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.8, "CR", "Pop")
  
  # Criterion A3 - HS
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.3, "VU", "HS")
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.5, "EN", "HS")
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.8, "CR", "HS")
  
  # Criterion E - Extinction probability
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 100, 0.1, "VU", "Ext")
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 20, 0.2, "EN", "Ext")
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.5, "CR", "Ext")
  
}

# add the trait values to the data frame
optima <- c("marginal", "central")
breadth <- c("narrow", "wide")
rmax <- c("slow", "fast")
dispersal <- c("short", "long")
land_rep <- 1:3
BatchNum <- 1:16

#create data frame with all trait combinations
sims_long <- expand.grid(optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims_long$BatchNum <- rep(1:16)

# create data frame with all parameter combinations for the IUCN classification time
IUCN_classification <- merge(IUCN_classification, sims_long, by = "BatchNum")

save(IUCN_classification, file = "4_Analysis/data/IUCN_classification_times_allreplicates.RData")
write.csv(IUCN_classification, file = "4_Analysis/data/IUCN_classification_times_allreplicates.csv", row.names = F)

# Moving average approach using the mean ---------

# write moving window function for the mean of the hs and pop values
MW <- function(data, new_data, timehorizon, threshold, category, metric){
  #column_new_data <- paste(category, metric, sep = "_")
  #print(column_new_data)
  
  # select the right column depending on the selected metric
  if(metric == "Pop"){
    column_data <- "pop_mean"
  } else if(metric == "HS") {
    column_data <- "hs_mean"
  } else {
    column_data <- "extProb"
  }
  
  # reduce timehorizon if it exceeds the maximum number of years in the data set
  if(timehorizon > max(data$Year) - 100){
    timehorizon <- max(data$Year) - 100
    #print(timehorizon)
  }
  
  # for every year calculated the relative size to the size x years into the future
  for (i in 1:nrow(data)) {
    if(column_data == "extProb"){
      rel_loss <- data[data$Year == unique(data$Year)[i+timehorizon], column_data]
      #print(rel_loss)
    } else {
      rel_loss <- 1 - (data[data$Year == unique(data$Year)[i+timehorizon], column_data]/data[data$Year == unique(data$Year)[i], column_data])
      #print(rel_loss)
    }
    
    # controls if the relative loss exceeds the threshold
    if(rel_loss >= threshold){
      #print(unique(data$Year)[i])
      new_data[new_data$BatchNum == BatchNum & new_data$land_rep == land_rep, paste(category, metric, sep = "_")] <- (unique(data$Year)[i])-100
      return(new_data)
      break
    }
  }
}

# create new data frame, which contains the years of reaching the criteria/ species being classified
land_rep <- 1:3
BatchNum <- 1:16
IUCN_classification <- expand.grid(land_rep = land_rep, BatchNum = BatchNum)

# create the VU, EN, CR columns classification time for all metrices (Pop, Range, HS, Ext.Prob)
IUCN_classification$VU_Pop <- NA
IUCN_classification$EN_Pop <- NA
IUCN_classification$CR_Pop <- NA
IUCN_classification$VU_HS <- NA
IUCN_classification$EN_HS <- NA
IUCN_classification$CR_HS <- NA
IUCN_classification$VU_Ext <- NA
IUCN_classification$EN_Ext <- NA
IUCN_classification$CR_Ext <- NA

# performs the same code for every scenario
for (i in 1:nrow(IUCN_classification)) {
  BatchNum <- IUCN_classification[i, "BatchNum"]
  land_rep <- IUCN_classification[i, "land_rep"]
  
  # Load data
  #dat <- readRDS("4_Analysis/data/data_analysis_raw_Batch10_Sim1.rds")
  dat <- readRDS(paste0("4_Analysis/data/data_analysis_raw_Batch", BatchNum, "_Sim", land_rep, ".rds"))

  # remove rows till year 100
  dat <- dat[!dat$Year %in% c(0:99),]

  # calculate mean pop and mean hs
  dat$pop_mean <- rowMeans(dat[,3:12])
  dat$hs_mean <- rowMeans(dat[,13:22])

  # Criterion A3 - Pop size
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.3, "VU", "Pop")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.5, "EN", "Pop")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.8, "CR", "Pop")

  # Criterion A3 - HS
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.3, "VU", "HS")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.5, "EN", "HS")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.8, "CR", "HS")

  # Criterion E - Extinction probability
  IUCN_classification <- MW(dat, IUCN_classification, 100, 0.1, "VU", "Ext")
  IUCN_classification <- MW(dat, IUCN_classification, 20, 0.2, "EN", "Ext")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.5, "CR", "Ext")
}

# add the trait values to the data frame
optima <- c("marginal", "central")
breadth <- c("narrow", "wide")
rmax <- c("slow", "fast")
dispersal <- c("short", "long")
land_rep <- 1:3
BatchNum <- 1:16

#create data frame with all trait combinations
sims_long <- expand.grid(optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims_long$BatchNum <- rep(1:16)

# create data frame with all parameter combinations for the IUCN classification time
IUCN_classification <- merge(IUCN_classification, sims_long, by = "BatchNum")

save(IUCN_classification, file = "4_Analysis/data/IUCN_classification_times_mean.RData")
write.csv(IUCN_classification, file = "4_Analysis/data/IUCN_classification_times_mean.csv", row.names = F)

# Moving average approach using median ----

# create new data frame, which contains the years of reaching the criteria/ species being classified
land_rep <- 1:3
BatchNum <- 1:16
IUCN_classification <- expand.grid(land_rep = land_rep, BatchNum = BatchNum)

# create the VU, EN, CR columns classification time for all metrices (Pop, Range, HS, Ext.Prob)
IUCN_classification$VU_Pop <- NA
IUCN_classification$EN_Pop <- NA
IUCN_classification$CR_Pop <- NA
IUCN_classification$VU_HS <- NA
IUCN_classification$EN_HS <- NA
IUCN_classification$CR_HS <- NA
IUCN_classification$VU_Ext <- NA
IUCN_classification$EN_Ext <- NA
IUCN_classification$CR_Ext <- NA

for (i in 1:nrow(IUCN_classification)) {
  BatchNum <- IUCN_classification[i, "BatchNum"]
  land_rep <- IUCN_classification[i, "land_rep"]

  # Load data
  #dat <- readRDS("4_Analysis/data/data_analysis_raw_Batch10_Sim1.rds")
  dat <- readRDS(paste0("4_Analysis/data/data_analysis_raw_Batch", BatchNum, "_Sim", land_rep, ".rds"))

  # remove rows till year 100
  dat <- dat[!dat$Year %in% c(0:99),]

  #replace NAs with 0
  dat[is.na(dat)] <- 0

  # calculate mean pop and mean hs
  dat <- dat %>% rowwise() %>% mutate(pop_mean = median(c(pop_sum87:pop_sum70), na.rm = T))
  dat <- dat %>% rowwise() %>% mutate(hs_mean = median(c(hs_change87:hs_change70), na.rm = T))
  dat$extProb2 <- 1- dat$extProb

  # Criterion A3 - Pop size
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.3, "VU", "Pop")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.5, "EN", "Pop")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.8, "CR", "Pop")

  # Criterion A3 - HS
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.3, "VU", "HS")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.5, "EN", "HS")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.8, "CR", "HS")

  # Criterion E - Extinction probability
  IUCN_classification <- MW(dat, IUCN_classification, 100, 0.1, "VU", "Ext")
  IUCN_classification <- MW(dat, IUCN_classification, 20, 0.2, "EN", "Ext")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.5, "CR", "Ext")

}

# add the trait values to the data frame
optima <- c("marginal", "central")
breadth <- c("narrow", "wide")
rmax <- c("slow", "fast")
dispersal <- c("short", "long")
land_rep <- 1:3
BatchNum <- 1:16

#create data frame with all trait combinations
sims_long <- expand.grid(optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims_long$BatchNum <- rep(1:16)

# create data frame with all parameter combinations for the IUCN classification time
IUCN_classification <- merge(IUCN_classification, sims_long, by = "BatchNum")

save(IUCN_classification, file = "4_Analysis/data/IUCN_classification_times_median.RData")
write.csv(IUCN_classification, file = "4_Analysis/data/IUCN_classification_times_median.csv", row.names = F)

# Moving average approach for the dispersal assumptions ----

# create new data frame, which contains the years of reaching the criteria/ species being classified
land_rep <- 1:3
BatchNum <- 1:16
IUCN_classification <- expand.grid(land_rep = land_rep, BatchNum = BatchNum)

# add the correct columns
IUCN_classification$VU_HS <- NA
IUCN_classification$EN_HS <- NA
IUCN_classification$CR_HS <- NA
IUCN_classification$VU_HS_disp_median <- NA
IUCN_classification$EN_HS_disp_median <- NA
IUCN_classification$CR_HS_disp_median <- NA
IUCN_classification$VU_HS_disp_quant <- NA
IUCN_classification$EN_HS_disp_quant <- NA
IUCN_classification$CR_HS_disp_quant <- NA
IUCN_classification$VU_HS_disp_mean <- NA
IUCN_classification$EN_HS_disp_mean <- NA
IUCN_classification$CR_HS_disp_mean <- NA

# performs the same code for every scenario
for (i in 1:nrow(IUCN_classification)) {
  BatchNum <- IUCN_classification[i, "BatchNum"]
  land_rep <- IUCN_classification[i, "land_rep"]
  
  # Load data
  #dat <- readRDS("4_Analysis/data/data_analysis_raw_Batch10_Sim1.rds")
  dat <- readRDS(paste0("4_Analysis/data/data_analysis_raw_Batch", BatchNum, "_Sim", land_rep, ".rds"))
  
  # remove rows till year 100
  dat <- dat[!dat$Year %in% c(0:99),]
  
  # calculate mean hs
  dat$hs_mean <- rowMeans(dat[,13:22])
  
  # Criterion A3 - HS
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.3, "VU", "HS")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.5, "EN", "HS")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.8, "CR", "HS")
  
  # calculate time point of classification for using dispersal assumptions
  dat_median <- readRDS(paste0("4_Analysis/data/hs_loss_wide_SDM_dispersal_assumptions_median_Batch", BatchNum, "_Sim", land_rep, ".rds"))
  dat_quant <- readRDS(paste0("4_Analysis/data/hs_loss_wide_SDM_dispersal_assumptions_0.95quantile_Batch", BatchNum, "_Sim", land_rep, ".rds"))
  dat_mean <- readRDS(paste0("4_Analysis/data/hs_loss_wide_SDM_dispersal_assumptions_mean_Batch", BatchNum, "_Sim", land_rep, ".rds"))
  
  #calculate the mean hs_loss for every startYear
  dat_median$hs_mean <- rowMeans(dat_median[,2:11])
  dat_quant$hs_mean <- rowMeans(dat_quant[,2:11])
  dat_mean$hs_mean <- rowMeans(dat_mean[,2:11])
  
  # obtain the year when the criterion is fulfilled
  IUCN_classification[i, "VU_HS_disp_median"] <- head(dat_median[which(dat_median$hs_mean >= 30), "startYear"],1)
  IUCN_classification[i, "EN_HS_disp_median"] <- head(dat_median[which(dat_median$hs_mean >= 50), "startYear"],1)
  IUCN_classification[i, "CR_HS_disp_median"] <- head(dat_median[which(dat_median$hs_mean >= 80), "startYear"],1)
  
  IUCN_classification[i, "VU_HS_disp_quant"] <- head(dat_quant[which(dat_quant$hs_mean >= 30), "startYear"],1)
  IUCN_classification[i, "EN_HS_disp_quant"] <- head(dat_quant[which(dat_quant$hs_mean >= 50), "startYear"],1)
  IUCN_classification[i, "CR_HS_disp_quant"] <- head(dat_quant[which(dat_quant$hs_mean >= 80), "startYear"],1)
  
  IUCN_classification[i, "VU_HS_disp_mean"] <- head(dat_mean[which(dat_mean$hs_mean >= 30), "startYear"],1)
  IUCN_classification[i, "EN_HS_disp_mean"] <- head(dat_mean[which(dat_mean$hs_mean >= 50), "startYear"],1)
  IUCN_classification[i, "CR_HS_disp_mean"] <- head(dat_mean[which(dat_mean$hs_mean >= 80), "startYear"],1)
}

# add the trait values to the data frame
optima <- c("marginal", "central")
breadth <- c("narrow", "wide")
rmax <- c("slow", "fast")
dispersal <- c("short", "long")
land_rep <- 1:3
BatchNum <- 1:16

#create data frame with all trait combinations
sims_long <- expand.grid(optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims_long$BatchNum <- rep(1:16)

# create data frame with all parameter combinations for the IUCN classification time
IUCN_classification <- merge(IUCN_classification, sims_long, by = "BatchNum")

save(IUCN_classification, file = "4_Analysis/data/IUCN_classification_times_dispersalassumptions.RData")
write.csv(IUCN_classification, file = "4_Analysis/data/IUCN_classification_times_dispersalassumptions.csv", row.names = F)


test <- readRDS("4_Analysis/data/hs_loss_wide_SDM_dispersal_assumptions_empirical_Batch6_Sim1.rds")
test$hs_mean <- rowMeans(test[,2:11])
head(test[which(test$hs_mean >= 30), "startYear"],1)
head(test[which(test$hs_mean >= 50), "startYear"],1)
head(test[which(test$hs_mean >= 80), "startYear"],1)
