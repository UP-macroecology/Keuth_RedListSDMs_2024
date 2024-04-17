# Distribution of individuals in the landscapes of different niche breadths

# Load packages
library(RColorBrewer)
library(scales)
library(dplyr)
require(foreach)
require(doParallel)
library(terra)
library(ggplot2)
library(gridExtra)

#Load different niche breadths
width <- c(0.025, 0.035, 0.045, 0.055, 0.065, 0.075)

# Create temperature increase
t <- 1:90
alpha <- 0.5
beta <- 0.9 
theta <- 0.3
set.seed(5678)
ts <- alpha + beta * t + arima.sim(list(ma = theta), n = length(t))
x <- as.vector(ts)
temp_rise <- scales::rescale(x, c(0,0.9))

#create path loop
path_loop <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/01_TestingParameters/")

foreach(b=1:length(width), .packages = c("terra","dplyr", "scales", "RColorBrewer", "ggplot2", "gridExtra")) %dopar% {
          
  #Batch Number for simulations without long dispersal
  BatchNum <- 12 + b

  # Occurrences for individuals without long dispersal
  pdf(paste0(path_loop, "Output_Maps/occurrences_landscape_Breadth",width[b], "_wo_longdisp.pdf"))
  
  #Load specific pop data set
  pop <- read.table(paste0(path_loop, "Outputs/Batch", BatchNum, "_Sim0_Land1_Pop.txt"), header = T, sep = "\t")

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
  for (i in 1:length(temp_rise)) {
    tmp <-rast(paste0(path_loop, "Inputs/habitat_per_breadth", width[b], "_cc", temp_rise[i], ".asc"))
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

  #Occurrences for individuals with long dispersal
  # Extract specific Batch Number
  BatchNum <- 6 + b

  pdf(paste0(path_loop, "Output_Maps/occurrences_landscape_Breadth",width[b], ".pdf"))
  
  #Load specific pop data set
  pop <- read.table(paste0(path_loop, "Outputs/Batch", BatchNum, "_Sim0_Land1_Pop.txt"), header = T, sep = "\t")

  #remove unimportant columns
  pop_short <- pop %>% dplyr::select(-c(RepSeason, Species))
  # extract occurrences
  occ_short <- subset(pop_short, pop_short$NInd >= 1)
  #change column names
  colnames(occ_short)[colnames(occ_short) == "x"] <- "X"
  colnames(occ_short)[colnames(occ_short) == "y"] <- "Y"
  # extract only the first replication
  occ_Rep0 <- subset(occ_short, occ_short$Rep == 0)
  # Plot occurrences under climate change
  for (i in 1:length(temp_rise)) {
    tmp <-rast(paste0(path_loop, "Inputs/habitat_per_breadth", width[b], "_cc", temp_rise[i], "wo_longdisp.asc"))
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

#clean all temporary files
gc()
rm(list=ls())