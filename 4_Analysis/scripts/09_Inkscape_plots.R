# Create plots for Inkscape

# load packages
library(ggplot2)
library(data.table)
library(dplyr)
library(terra)

# Load functions
source("Functions/extract_legend.R")

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

# Create plots for figure 2

# Comparison graph of habitat loss, population size and extinction probability

ggplot(data_mean[[1]] %>% filter(), aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.3, fill = "#FF6A6A")+
  geom_line(linewidth = 1.2, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), alpha = 0.3, fill = "gold")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.3, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 25), legend.position = "")+
  ylab("simulated/ predicted value")+
  xlim(c(0,75))+
  ylim(c(-0.05,1.05))

ggsave("Inkscape/images/comparison_graph_Batch1.pdf")

ggplot(data_mean[[2]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.3, fill = "#FF6A6A")+
  geom_line(linewidth = 1.2, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.3, fill = "gold")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.3, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 25), legend.position = "")+
  ylab("simulated/ predicted value")+
  xlim(c(0,75))+
  ylim(c(-0.05,1.05))

ggsave("Inkscape/images/comparison_graph_Batch2.pdf")

# Extract legend for the plots

colors <- c("relative Population size" = "gold", "relative Habitat suitability" = "#FF6A6A", "Extinction probability" = "blue")

legend <- ggplot(data_mean[[16]], aes(x = Year, y = meanHS, color = "relative Habitat suitability"))+
  geom_line(linewidth = 1)+
  geom_line(aes(x = Year, y = meanPop, color = "relative Population size"), linewidth = 1)+
  geom_line(aes(x = Year, y = meanExt, color = "Extinction probability"), linewidth = 1)+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "right", , legend.title = element_blank(), 
        legend.text = element_text(size = 30), legend.key.size = unit(2, "cm"), legend.key.width = unit(2.5, "cm"))+
  scale_color_manual(values= colors, breaks = c("relative Habitat suitability", "relative Population size", "Extinction probability"))+
  guides(color = guide_legend(override.aes = list(linewidth = 3)))

shared_legend <- extract_legend(legend)

plot(shared_legend)

ggsave("Inkscape/images/legend_simulation_results.pdf")

# Habitat suitability predictions

load("4_Analysis/data/Predictions_curr_Batch1_Sim1_Replication4.RData")
r_current <- terra::rast(ens_preds[,1:3])

pdf("Inkscape/images/SDM_predictions_map_current_Batch1_Rep4_Land1.pdf")
plot(r_current, legend = F, axes = F, col = c("#F2F2F2", rev(c("#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529"))))
dev.off()

load("4_Analysis/data/Predictions_curr_Batch2_Sim1_Replication4.RData")
r_current <- terra::rast(ens_preds[,1:3])

pdf("Inkscape/images/SDM_predictions_map_current_Batch2_Rep4_Land1.pdf")
plot(r_current, legend = F, axes = F, col = c("#F2F2F2", rev(c("#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529"))))
dev.off()

load("4_Analysis/data/Predictions_fut_Batch1_Sim1_Replication4.RData")

# extract year 20 (for range-contracting)
fut_preds <- ens_fut_preds[[20]]
r_fut <- terra::rast(fut_preds[,1:3])

pdf("Inkscape/images/SDM_predictions_map_future_Batch1_Rep4_Land1.pdf")
plot(r_fut, legend = F, axes = F, col = c("#F2F2F2", rev(c("#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529"))))
dev.off()

load("4_Analysis/data/Predictions_fut_Batch2_Sim1_Replication4.RData")

# extract year 40 (for range-shifting)
fut_preds <- ens_fut_preds[[30]]
r_fut <- terra::rast(fut_preds[,1:3])

pdf("Inkscape/images/SDM_predictions_map_future_Batch2_Rep4_Land1.pdf")
plot(r_fut, legend = F, axes = F, col = c("#F2F2F2", rev(c("#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529"))))
dev.off()

# Extract legend
pdf("Inkscape/images/SDM_predictions_legend.pdf")
plot(r_current, axes = F, col = c("#F2F2F2", rev(c("#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529"))), range = c(0,100), plg = list(size = c(1,2), cex = 3), mar = c(1, 1, 1, 6))
dev.off()

# Abundance plots of the simulation
library(RColorBrewer)
# For the first species
pop_Batch1 <- readRDS("4_Analysis/data/Batch1_Sim1_Land1_Pop_Rep4.rds")
pop_Batch2 <- readRDS("4_Analysis/data/Batch2_Sim1_Land1_Pop_Rep4.rds")
load("4_Analysis/data/Predictions_curr_Batch1_Sim1_Replication4.RData")

# Abundance plot for year 0
pop_Batch1_current <- subset(pop_Batch1, pop_Batch1$Year == 100)
pop_Batch1_current_full <- merge(ens_preds[,c(1:2)], pop_Batch1_current[,c(4:5,7)], by = c("x","y"), all.x = T)
pop_Batch1_current_full[which(is.na(pop_Batch1_current_full$NInd)),"NInd"] <- 0
r_abu_current <- rast(as.data.frame(pop_Batch1_current_full[,c(1:3)]))

pdf("Inkscape/images/Abundances_map_current_Batch1_Rep4_Land1.pdf")
plot(r_abu_current, axes = F, range = c(0,11), legend = F, smooth = T, col = c("#F2F2F2", rev(brewer.pal(n = 11, name = "Spectral"))))
dev.off()

pop_Batch2_current <- subset(pop_Batch2, pop_Batch2$Year == 100)
pop_Batch2_current_full <- merge(ens_preds[,c(1:2)], pop_Batch2_current[,c(4:5,7)], by = c("x","y"), all.x = T)
pop_Batch2_current_full[which(is.na(pop_Batch2_current_full$NInd)),"NInd"] <- 0
r_abu_current <- rast(as.data.frame(pop_Batch2_current_full[,c(1:3)]))

pdf("Inkscape/images/Abundances_map_current_Batch2_Rep4_Land1.pdf")
plot(r_abu_current, axes = F, range = c(0,11), legend = F, smooth = T, col = c("#F2F2F2", rev(brewer.pal(n = 11, name = "Spectral"))))
dev.off()

# Plot future
pop_Batch1_future <- subset(pop_Batch1, pop_Batch1$Year == 120)
pop_Batch1_future_full <- merge(ens_preds[,c(1:2)], pop_Batch1_future[,c(4:5,7)], by = c("x","y"), all.x = T)
pop_Batch1_future_full[which(is.na(pop_Batch1_future_full$NInd)),"NInd"] <- 0
r_abu_future <- rast(as.data.frame(pop_Batch1_future_full[,c(1:3)]))

pdf("Inkscape/images/Abundances_map_future_Batch1_Rep4_Land1.pdf")
plot(r_abu_future, axes = F, range = c(0,11), legend = F, smooth = T, col = c("#F2F2F2", rev(brewer.pal(n = 11, name = "Spectral"))))
dev.off()

pop_Batch2_future <- subset(pop_Batch2, pop_Batch2$Year == 130)
pop_Batch2_future_full <- merge(ens_preds[,c(1:2)], pop_Batch2_future[,c(4:5,7)], by = c("x","y"), all.x = T)
pop_Batch2_future_full[which(is.na(pop_Batch2_future_full$NInd)),"NInd"] <- 0
r_abu_future <- rast(as.data.frame(pop_Batch2_future_full[,c(1:3)]))

pdf("Inkscape/images/Abundances_map_future_Batch2_Rep4_Land1.pdf")
plot(r_abu_future, axes = F, range = c(0,11), legend = F, col = c("#F2F2F2"))
dev.off()

pdf("Inkscape/images/Abundance_legend.pdf")
plot(r_abu_current, axes = F, range = c(0,11), col = c("#F2F2F2", rev(brewer.pal(n = 11, name = "Spectral"))), plg = list(size = c(1,2), cex = 3), mar = c(1, 1, 1, 6))
dev.off()
