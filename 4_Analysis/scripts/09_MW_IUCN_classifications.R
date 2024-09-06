# Use Moving window approach to determine classification time point

# Load packages
library(data.table)

# write moving window function
MW <- function(data, new_data, timehorizon, threshold, category, metric){
  column_new_data <- paste(category, metric, sep = "_")
  #print(column_new_data)
  if(metric == "Pop"){
    column_data <- "pop_mean"
  } else if(metric == "HS") {
    column_data <- "hs_mean"
  } else {
    column_data <- "extProb2"
  }
  if(timehorizon > max(data$Year) - 100){
    timehorizon <- max(data$Year) - 100
    #print(timehorizon)
  }
  for (i in 1:nrow(data)) {
    rel_loss <- 1 - (data[data$Year == unique(data$Year)[i+timehorizon], column_data]/data[data$Year == unique(data$Year)[i], column_data])
    #print(rel_loss)
    if(rel_loss >= threshold){
      #print(unique(data$Year)[i])
      new_data[new_data$BatchNum == BatchNum & new_data$land_rep == land_rep, paste(category, metric, sep = "_")] <- unique(data$Year)[i]
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

#save(IUCN_classification, paste0(sdm_dir, "results/IUCN_classification_times.RData"))

# # Criterion A3 - pop size (VU)
# for (i in 1:nrow(dat)) {
#   rel_loss <- 1 - (dat[dat$Year == unique(dat$Year)[i+10],"pop_mean"]/dat[dat$Year == unique(dat$Year)[i],"pop_mean"])
#   if(rel_loss >= 0.8){
#     IUCN_classification[IUCN_classification$BatchNum == 10 & IUCN_classification$land_rep == 1 ,"CR_Pop"] <- unique(dat$Year)[i]
#     break
#   }
# }
