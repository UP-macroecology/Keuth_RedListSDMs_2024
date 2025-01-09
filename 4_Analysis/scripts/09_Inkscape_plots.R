# Create plots for Inkscape

# load packages
library(ggplot2)
library(data.table)
library(dplyr)
library(terra)


# Load data

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
  tmp1 <- readRDS(paste0("4_Analysis/data/data_analysis_relative_long_Batch", i, "_Sim1.rds"))
  # data set for land replication 2
  tmp2 <- readRDS(paste0("4_Analysis/data/data_analysis_relative_long_Batch", i, "_Sim2.rds"))
  # data set for land replication 3
  tmp3 <- readRDS(paste0("4_Analysis/data/data_analysis_relative_long_Batch", i, "_Sim3.rds"))
  # the data sets include the abundance and the extinction probability from the simulation, and the habitat suitability predicted by the mean ensemble model of the three algorithms using a threshold value for 
  # calculating the sums of habitat suitability
  
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

# Create plots for figure 2

# Comparison graph of habitat loss, population size and extinction probability

ggplot(data_mean[[1]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(linewidth = 1.2, col = "gold")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_blank(), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.1,1.35))

ggsave("Inkscape/images/comparison_graph_Batch1.pdf")

ggplot(data_mean[[2]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(linewidth = 1.2, col = "gold")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_blank(), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.1,1.35))

ggsave("Inkscape/images/comparison_graph_Batch2.pdf")

# Abundance plots of the simulation

pop_Batch1 <- fread("")

# Habitat suitability predictions

load("4_Analysis/data/Predictions_curr_Batch1_Sim1_Replication4.RData")
r_current <- terra::rast(ens_preds[,1:3])

pdf("Inkscape/images/SDM_predictions_map_current_Batch1_Rep4_Land1.pdf")
terra::plot(r_current, legend = F, pax=list(side=0))
dev.off()

load("4_Analysis/data/Predictions_curr_Batch2_Sim1_Replication4.RData")
r_current <- terra::rast(ens_preds[,1:3])

pdf("Inkscape/images/SDM_predictions_map_current_Batch2_Rep4_Land1.pdf")
terra::plot(r_current, legend = F, pax=list(side=0))
dev.off()

load("4_Analysis/data/Predictions_fut_Batch1_Sim1_Replication4.RData")

# extract year 30
fut_preds_30 <- ens_fut_preds[[20]]
r_fut_30 <- terra::rast(fut_preds_30[,1:3])

pdf("Inkscape/images/SDM_predictions_map_current_Batch2_Rep4_Land1.pdf")
plot(r_fut_30, legend = F, pax=list(side=0))
dev.off()

load("4_Analysis/data/Predictions_fut_Batch2_Sim1_Replication4.RData")

# extract year 30
fut_preds_30 <- ens_fut_preds[[40]]
r_fut_30 <- terra::rast(fut_preds_30[,1:3])

pdf("Inkscape/images/SDM_predictions_map_current_Batch2_Rep4_Land1.pdf")
plot(r_fut_30, legend = F, pax=list(side=0))
dev.off()

# Plot landscapes
