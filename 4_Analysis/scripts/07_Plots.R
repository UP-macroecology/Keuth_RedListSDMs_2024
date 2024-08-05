# Plot results

# load packages
library(ggplot2)
library(gridExtra)
library(ggtext)
library(dplyr)
library(grid)

# Load functions
source("Functions/extract_legend.R")
source("2_Simulations/scripts/text_labels_plots.R")

# Prepare data

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

# create data frame with all parameter combinations for the IUCN classification time
IUCN_classification <- expand.grid(land_rep = land_rep, BatchNum = BatchNum, replicates = replicates)
IUCN_classification <- merge(IUCN_classification, sims_long, by = "BatchNum")

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

# create the VU, EN, CR columns classification time for all metrices (Pop, Range, HS, Ext.Prob)
IUCN_classification$VU_Pop <- NA
IUCN_classification$EN_Pop <- NA
IUCN_classification$CR_Pop <- NA
IUCN_classification$VU_HS <- NA
IUCN_classification$EN_HS <- NA
IUCN_classification$CR_HS <- NA
IUCN_classification$VU_Range <- NA
IUCN_classification$EN_Range <- NA
IUCN_classification$CR_Range <- NA
IUCN_classification$VU_Ext <- NA
IUCN_classification$EN_Ext <- NA
IUCN_classification$CR_Ext <- NA

# extract time point when the thresholds of the different criteria is surpassed for the different metrics
for (i in 1:nrow(IUCN_classification)) {
  # extract values
  land_nr <- IUCN_classification[i,"land_rep"]
  BatchNum <- IUCN_classification[i,"BatchNum"]
  rep_nr <- IUCN_classification[i,"replicates"]
  # Obtain the year when extinction probability exceeds stated thresholds
  # Vulnerable E (>=10%)
  IUCN_classification[i ,"VU_Ext"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & data[[BatchNum]]$extProb>=0.1, "Year"],1) - 100
  # Endangered E (>=20%)
  IUCN_classification[i ,"EN_Ext"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & data[[BatchNum]]$extProb>=0.2, "Year"],1) - 100
  # Critically endangered E (>=50%)
  IUCN_classification[i ,"CR_Ext"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & data[[BatchNum]]$extProb>=0.5, "Year"],1) - 100
  # Obtain the year when population size exceeds stated thresholds
  # Vulnerable A (>=30%)
  IUCN_classification[i ,"VU_Pop"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & (1-data[[BatchNum]]$pop_sum)>=0.3, "Year"],1) - 100
  # Endangered A (>=50%)
  IUCN_classification[i ,"EN_Pop"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & (1-data[[BatchNum]]$pop_sum)>=0.5, "Year"],1) - 100
  # Critically endangered A (>=80%)
  IUCN_classification[i ,"CR_Pop"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & (1-data[[BatchNum]]$pop_sum)>=0.8, "Year"],1) - 100
  # Obtain the year when habitat loss exceeds stated thresholds
  # Vulnerable A3 (>=30%)
  IUCN_classification[i ,"VU_HS"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & (1-data[[BatchNum]]$hs_change)>=0.3, "Year"],1) - 100
  # Endangered A3 (>=50%)
  IUCN_classification[i ,"EN_HS"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & (1-data[[BatchNum]]$hs_change)>=0.5, "Year"],1) - 100
  # Critically endangered A3 (>=80%)
  IUCN_classification[i ,"CR_HS"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & (1-data[[BatchNum]]$hs_change)>=0.8, "Year"],1) - 100
  # Obtain the year when range loss exceeds stated thresholds
  # Vulnerable A3 (>=30%)
  IUCN_classification[i ,"VU_Range"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & (1-data[[BatchNum]]$range_change)>=0.3, "Year"],1) - 100
  # Endangered A3 (>=50%)
  IUCN_classification[i ,"EN_Range"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & (1-data[[BatchNum]]$range_change)>=0.5, "Year"],1) - 100
  # Critically endangered A3 (>=80%)
  IUCN_classification[i ,"CR_Range"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & (1-data[[BatchNum]]$range_change)>=0.8, "Year"],1) - 100
}

# Plot the results -----

# Population size against habitat suitability (updated) ---------------

# create different data sets for the different models
data_optima <- data.frame(hs_loss=rep(seq(0,1,length=100),6))
data_optima$land <- rep(1:3, each = 100, times = 2)
data_optima$optima <- rep(c("central", "marginal"), each = 300)
data_optima$predictions <- NA
data_optima$land <- as.factor(data_optima$land)

data_breadth <- data.frame(hs_loss=rep(seq(0,1,length=100),6))
data_breadth$land <- rep(1:3, each = 100, times = 2)
data_breadth$breadth <- rep(c("narrow", "wide"), each = 300)
data_breadth$predictions <- NA
data_breadth$land <- as.factor(data_breadth$land)

data_rmax <-data.frame(hs_loss=rep(seq(0,1,length=100),6))
data_rmax$land <- rep(1:3, each = 100, times = 2)
data_rmax$rmax <- rep(c("slow", "fast"), each = 300)
data_rmax$predictions <- NA
data_rmax$land <- as.factor(data_rmax$land)

data_dispersal <- data.frame(hs_loss=rep(seq(0,1,length=100),6))
data_dispersal$land <- rep(1:3, each = 100, times = 2)
data_dispersal$dispersal <- rep(c("short", "long"), each = 300)
data_dispersal$predictions <- NA
data_dispersal$land <- as.factor(data_dispersal$land)

# create data frame with combinations of trait and land replication
land_rep <- 1:3
optima <- c("marginal", "central")

sims <- expand.grid(land = land_rep, optima = optima)
sims$optima <- as.character(sims$optima)
sims$breadth <- rep(c("narrow", "wide"), each = 3)
sims$rmax <- rep(c("slow", "fast"), each = 3)
sims$dispersal <- rep(c("short", "long"), each = 3)

sink("4_Analysis/Model Results/GLM_Popsize_HSloss.txt")
# calculate the different models
for (sim_nr in 1:nrow(sims)){
  # extract trait values
  optima_nr <- sims[sim_nr,]$optima
  breadth_nr <- sims[sim_nr,]$breadth
  rmax_nr <- sims[sim_nr,]$rmax
  dispersal_nr <- sims[sim_nr,]$dispersal
  land_nr <- sims[sim_nr,]$land
  
  # subset data set
  data_sub_optima <- subset(data_adapted_long, data_adapted_long$land == land_nr & data_adapted_long$optima == optima_nr)
  data_sub_breadth <- subset(data_adapted_long, data_adapted_long$land == land_nr & data_adapted_long$breadth == breadth_nr)
  data_sub_rmax <- subset(data_adapted_long, data_adapted_long$land == land_nr & data_adapted_long$rmax == rmax_nr)
  data_sub_dispersal <- subset(data_adapted_long, data_adapted_long$land == land_nr & data_adapted_long$dispersal == dispersal_nr)
  
  # calculate the different models
  model_optima <- glm(pop_sum ~ hs_loss, data=data_sub_optima, family = "binomial")
  model_breadth <- glm(pop_sum ~ hs_loss,  data=data_sub_breadth, family = "binomial")
  model_rmax <- glm(pop_sum ~ hs_loss,  data=data_sub_rmax, family = "binomial")
  model_dispersal <- glm(pop_sum ~ hs_loss,  data=data_sub_dispersal, family = "binomial")
  
  # save the model outputs
  print(paste0("land_nr: ", land_nr, " & optima: ", optima_nr))
  print(summary(model_optima))
  print(paste0("land_nr: ", land_nr, " & breadth: ", breadth_nr))
  print(summary(model_breadth))
  print(paste0("land_nr: ", land_nr, " & rmax: ", rmax_nr))
  print(summary(model_rmax))
  print(paste0("land_nr: ", land_nr, " & dispersal: ", dispersal_nr))
  print(summary(model_dispersal))
  
  #make predictions to data
  predictions_optima <- predict(model_optima, newdata=data.frame(hs_loss=seq(0,1,length=100)),  type = "response")
  data_optima[which(data_optima$land == land_nr & data_optima$optima == optima_nr),"predictions"] <- predictions_optima
  
  predictions_breadth <- predict(model_breadth, newdata=data.frame(hs_loss=seq(0,1,length=100)),  type = "response")
  data_breadth[which(data_breadth$land == land_nr & data_breadth$breadth == breadth_nr),"predictions"] <- predictions_breadth
  
  predictions_rmax <- predict(model_rmax, newdata=data.frame(hs_loss=seq(0,1,length=100)),  type = "response")
  data_rmax[which(data_rmax$land == land_nr & data_rmax$rmax == rmax_nr),"predictions"] <- predictions_rmax
  
  predictions_dispersal <- predict(model_dispersal, newdata=data.frame(hs_loss=seq(0,1,length=100)),  type = "response")
  data_dispersal[which(data_dispersal$land == land_nr & data_dispersal$dispersal == dispersal_nr),"predictions"] <- predictions_dispersal
}
sink()

p_pos <- ggplot(data_optima, aes(x=hs_loss, y = predictions, col = land, linetype = optima))+
  #geom_point()+
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("relative Population size")+
  geom_abline(intercept = 1, slope = -1, col = "black", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), plot.title = element_text(size = 18, face = "italic"), 
        legend.position = c(0.93, 0.92), axis.title.x = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 12), 
        legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_color_manual(values = c("#EB5E55", "#4BA3C3", "#FCB870"), guide = "none")
  ggtitle("Niche position")

p_breadth <- ggplot(data_breadth, aes(x=hs_loss, y = predictions, col = land, linetype = breadth))+
  #geom_point()+
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("relative Population size")+
  geom_abline(intercept = 1, slope = -1, col = "black", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  theme(axis.text = element_text(size = 12), plot.title = element_text(size = 18, face = "italic"), legend.position = c(0.94, 0.92),
        axis.title = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 12), legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_colour_discrete(guide = "none") +
  ggtitle("Niche breadth")

p_rmax <- ggplot(data_rmax, aes(x=hs_loss, y = predictions, col = land, linetype = rmax))+
  #geom_point()+
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("relative Population size")+
  geom_abline(intercept = 1, slope = -1, col = "black", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15),  plot.title = element_text(size = 18, face = "italic"), 
        legend.position = c(0.94, 0.9), legend.title = element_blank(), legend.text = element_text(size = 12), legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_colour_discrete(guide = "none") +
  ggtitle("Growth rate")

p_dispersal <- ggplot(data_dispersal, aes(x=hs_loss, y = predictions, col = land, linetype = dispersal))+
  #geom_point()+
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("relative Population size")+
  theme_bw()+
  geom_abline(intercept = 1, slope = -1, col = "black", linetype = "dashed", linewidth = 1)+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), plot.title = element_text(size = 18, face = "italic"), 
        legend.position = c(0.94, 0.9), axis.title.y = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 12), 
        legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_colour_discrete(guide = "none") +
  ggtitle("Dispersal")

legend <- ggplot(data_dispersal, aes(x=hs_loss, y = predictions, col = land, linetype = dispersal))+
  #geom_point()+
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("relative Population size")+
  theme_bw()+
  geom_abline(intercept = 1, slope = -1, col = "black", linetype = "dashed", linewidth = 1)+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), plot.title = element_text(size = 18, face = "italic"), 
        legend.title = element_text(size = 15), legend.text = element_text(size = 12), 
        legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  guides(linetype = "none", colour = guide_legend(title = "Landscape"))+
  ggtitle("Dispersal")

shared_legend <- extract_legend(legend)

grid.arrange(p_pos,p_breadth, p_rmax, p_dispersal, nrow=2, ncol = 2, heights = c(8,8), widths = c(8,8))
grid.arrange(arrangeGrob(p_pos,p_breadth, p_rmax, p_dispersal, nrow=2, ncol = 2, heights = c(8,8), widths = c(1,1)), shared_legend, ncol=2, nrow = 1, widths = c(10,0.8))


# IUCN classification time - updated plot -------

p_pos <- ggplot(IUCN_classification, aes(x = optima, y = VU_HS))+
  geom_boxplot(width = 0.06, col = "red", position = position_nudge(x = -0.42))+
  geom_boxplot(aes(x = optima, y = VU_Ext), position = position_nudge(x = -0.24), width = 0.06, col = "blue")+
  geom_boxplot(aes(x = optima, y = VU_Pop), position = position_nudge(x = - 0.33), width = 0.06, col = "orange")+
  geom_boxplot(aes(x = optima, y = EN_HS), width = 0.06, col = "red", position = position_nudge(x = -0.09))+
  geom_boxplot(aes(x = optima, y = EN_Ext), position = position_nudge(x = 0.09), width = 0.06, col = "blue")+
  geom_boxplot(aes(x = optima, y = EN_Pop), position = position_nudge(x = 0), width = 0.06, col = "orange")+
  geom_boxplot(aes(x = optima, y = CR_HS), width = 0.06, col = "red", position = position_nudge(x = 0.24))+
  geom_boxplot(aes(x = optima, y = CR_Ext), position = position_nudge(x = 0.43), width = 0.06, col = "blue")+
  geom_boxplot(aes(x = optima, y = CR_Pop), position = position_nudge(x = 0.33), width = 0.06, col = "orange")+
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 1.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 0.83, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 2.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 1.83, linetype = "dashed", color = "lightgrey")+
  annotate(geom="text", x=0.655, y=59, label="VU", color="black", size = 6)+
  annotate(geom="text", x=0.995, y=59, label="EN", color="black", size = 6)+
  annotate(geom="text", x=1.325, y=59, label="CR", color="black", size = 6)+
  annotate(geom="text", x=1.685, y=59, label="VU", color="black", size = 6)+
  annotate(geom="text", x=1.995, y=59, label="EN", color="black", size = 6)+
  annotate(geom="text", x=2.335, y=59, label="CR", color="black", size = 6)+
  scale_x_discrete(expand = c(0.25, 0.25)) +
  ggtitle("Niche position")+
  xlab("")+
  ylim(c(0,60))+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 18), legend.position = "", plot.title = element_text(size = 20, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))+
  ylab("Classification timepoint [years]")

p_breadth <- ggplot(IUCN_classification, aes(x = breadth, y = VU_HS))+
  geom_boxplot(width = 0.06, col = "red", position = position_nudge(x = -0.42))+
  geom_boxplot(aes(x = breadth, y = VU_Ext), position = position_nudge(x = -0.24), width = 0.06, col = "blue")+
  geom_boxplot(aes(x = breadth, y = VU_Pop), position = position_nudge(x = - 0.33), width = 0.06, col = "orange")+
  geom_boxplot(aes(x = breadth, y = EN_HS), width = 0.06, col = "red", position = position_nudge(x = -0.09))+
  geom_boxplot(aes(x = breadth, y = EN_Ext), position = position_nudge(x = 0.09), width = 0.06, col = "blue")+
  geom_boxplot(aes(x = breadth, y = EN_Pop), position = position_nudge(x = 0), width = 0.06, col = "orange")+
  geom_boxplot(aes(x = breadth, y = CR_HS), width = 0.06, col = "red", position = position_nudge(x = 0.24))+
  geom_boxplot(aes(x = breadth, y = CR_Ext), position = position_nudge(x = 0.43), width = 0.06, col = "blue")+
  geom_boxplot(aes(x = breadth, y = CR_Pop), position = position_nudge(x = 0.33), width = 0.06, col = "orange")+
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 1.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 0.83, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 2.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 1.83, linetype = "dashed", color = "lightgrey")+
  annotate(geom="text", x=0.655, y=59, label="VU", color="black", size = 6)+
  annotate(geom="text", x=0.995, y=59, label="EN", color="black", size = 6)+
  annotate(geom="text", x=1.325, y=59, label="CR", color="black", size = 6)+
  annotate(geom="text", x=1.685, y=59, label="VU", color="black", size = 6)+
  annotate(geom="text", x=1.995, y=59, label="EN", color="black", size = 6)+
  annotate(geom="text", x=2.335, y=59, label="CR", color="black", size = 6)+
  scale_x_discrete(expand = c(0.25, 0.25)) +
  ggtitle("Niche breadth")+
  ylim(c(0,60))+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_blank(), legend.position = "", plot.title = element_text(size = 20, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))

p_rmax <- ggplot(IUCN_classification, aes(x = rmax, y = VU_HS))+
  geom_boxplot(width = 0.06, col = "red", position = position_nudge(x = -0.42))+
  geom_boxplot(aes(x = rmax, y = VU_Ext), position = position_nudge(x = -0.24), width = 0.06, col = "blue")+
  geom_boxplot(aes(x = rmax, y = VU_Pop), position = position_nudge(x = - 0.33), width = 0.06, col = "orange")+
  geom_boxplot(aes(x = rmax, y = EN_HS), width = 0.06, col = "red", position = position_nudge(x = -0.09))+
  geom_boxplot(aes(x = rmax, y = EN_Ext), position = position_nudge(x = 0.09), width = 0.06, col = "blue")+
  geom_boxplot(aes(x = rmax, y = EN_Pop), position = position_nudge(x = 0), width = 0.06, col = "orange")+
  geom_boxplot(aes(x = rmax, y = CR_HS), width = 0.06, col = "red", position = position_nudge(x = 0.24))+
  geom_boxplot(aes(x = rmax, y = CR_Ext), position = position_nudge(x = 0.43), width = 0.06, col = "blue")+
  geom_boxplot(aes(x = rmax, y = CR_Pop), position = position_nudge(x = 0.33), width = 0.06, col = "orange")+
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 1.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 0.83, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 2.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 1.83, linetype = "dashed", color = "lightgrey")+
  annotate(geom="text", x=0.655, y=59, label="VU", color="black", size = 6)+
  annotate(geom="text", x=0.995, y=59, label="EN", color="black", size = 6)+
  annotate(geom="text", x=1.325, y=59, label="CR", color="black", size = 6)+
  annotate(geom="text", x=1.685, y=59, label="VU", color="black", size = 6)+
  annotate(geom="text", x=1.995, y=59, label="EN", color="black", size = 6)+
  annotate(geom="text", x=2.335, y=59, label="CR", color="black", size = 6)+
  scale_x_discrete(expand = c(0.25, 0.25)) +
  ggtitle("Growth rate")+
  xlab("")+
  ylim(c(0,60))+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 18), legend.position = "", plot.title = element_text(size = 20, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))+
  ylab("Classification timepoint [years]")

p_disp <- ggplot(IUCN_classification, aes(x = dispersal, y = VU_HS))+
  geom_boxplot(width = 0.06, col = "red", position = position_nudge(x = -0.42))+
  geom_boxplot(aes(x = dispersal, y = VU_Ext), position = position_nudge(x = -0.24), width = 0.06, col = "blue")+
  geom_boxplot(aes(x = dispersal, y = VU_Pop), position = position_nudge(x = - 0.33), width = 0.06, col = "orange")+
  geom_boxplot(aes(x = dispersal, y = EN_HS), width = 0.06, col = "red", position = position_nudge(x = -0.09))+
  geom_boxplot(aes(x = dispersal, y = EN_Ext), position = position_nudge(x = 0.11), width = 0.06, col = "blue")+
  geom_boxplot(aes(x = dispersal, y = EN_Pop), position = position_nudge(x = 0), width = 0.06, col = "orange")+
  geom_boxplot(aes(x = dispersal, y = CR_HS), width = 0.06, col = "red", position = position_nudge(x = 0.24))+
  geom_boxplot(aes(x = dispersal, y = CR_Ext), position = position_nudge(x = 0.43), width = 0.06, col = "blue")+
  geom_boxplot(aes(x = dispersal, y = CR_Pop), position = position_nudge(x = 0.33), width = 0.06, col = "orange")+
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 1.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 0.83, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 2.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 1.83, linetype = "dashed", color = "lightgrey")+
  annotate(geom="text", x=0.655, y=59, label="VU", color="black", size = 6)+
  annotate(geom="text", x=0.995, y=59, label="EN", color="black", size = 6)+
  annotate(geom="text", x=1.325, y=59, label="CR", color="black", size = 6)+
  annotate(geom="text", x=1.685, y=59, label="VU", color="black", size = 6)+
  annotate(geom="text", x=1.995, y=59, label="EN", color="black", size = 6)+
  annotate(geom="text", x=2.335, y=59, label="CR", color="black", size = 6)+
  scale_x_discrete(expand = c(0.25, 0.25)) +
  ggtitle("Dispersal")+
  ylim(c(0,60))+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_blank(), legend.position = "", plot.title = element_text(size = 20, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))

# create and extract common legend
colors <- c("Habitat suitability (A3)" = "red", "Population size (A3)" = "orange", "Extinction probability (E)" = "blue")

legend <- ggplot(IUCN_classification, aes(x = BatchNum, y = VU_Pop, color = "Population size (A3)"))+
  geom_boxplot()+
  geom_boxplot(aes(x = BatchNum, y = VU_HS, color = "Habitat suitability (A3)"), position = position_nudge(x = -0.25), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = VU_Ext, color = "Extinction probability (E)"), position = position_nudge(x = 0.25), width = 0.2)+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), plot.title = element_text(size = 25, face = "bold"), legend.title = element_blank(), legend.text = element_text(size = 15), legend.key.size = unit(1, "cm"),
        legend.position = "bottom")+
  ylab("Timepoint of classification")+
  scale_color_manual(values= colors, breaks = c("Habitat suitability (A3)", "Population size (A3)", "Extinction probability (E)"))

shared_legend <- extract_legend(legend)

#Plot large grid
grid.arrange(arrangeGrob(p_pos,p_breadth, p_rmax, p_disp, nrow=2, ncol = 2, heights = c(8,8), widths = c(1,1)), shared_legend, nrow=2, ncol = 1, heights = c(10,1))

# plot of HS, population size and extinction probability over time --------
p1 <- ggplot(data_mean[[1]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(linewidth = 1, col = "gold")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p2 <- ggplot(data_mean[[2]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(linewidth = 1, col = "gold")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p3 <- ggplot(data_mean[[3]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(linewidth = 1, col = "gold")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p4 <- ggplot(data_mean[[4]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(linewidth = 1, col = "gold")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p5 <- ggplot(data_mean[[5]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(linewidth = 1, col = "gold")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p6 <- ggplot(data_mean[[6]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(linewidth = 1, col = "gold")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p7 <- ggplot(data_mean[[7]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(linewidth = 1, col = "gold")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p8 <- ggplot(data_mean[[8]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(linewidth = 1, col = "gold")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p9 <- ggplot(data_mean[[9]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(linewidth = 1, col = "gold")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p10 <- ggplot(data_mean[[10]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(linewidth = 1, col = "gold")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p11 <- ggplot(data_mean[[11]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(linewidth = 1, col = "gold")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p12 <- ggplot(data_mean[[12]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(linewidth = 1, col = "gold")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p13 <- ggplot(data_mean[[13]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(linewidth = 1, col = "gold")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p14 <- ggplot(data_mean[[14]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(linewidth = 1, col = "gold")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p15 <- ggplot(data_mean[[15]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(linewidth = 1, col = "gold")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p16 <- ggplot(data_mean[[16]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(linewidth = 1, col = "gold")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

# create and extract common legend
colors <- c("Population size" = "#FF6A6A", "Habitat suitability" = "gold", "Extinction probability" = "blue")

legend <- ggplot(data_mean[[16]], aes(x = Year, y = meanHS, color = "Habitat suitability"))+
  geom_line(linewidth = 1)+
  geom_line(aes(x = Year, y = meanPop, color = "Population size"), linewidth = 1)+
  geom_line(aes(x = Year, y = meanExt, color = "Extinction probability"), linewidth = 1)+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "bottom", , legend.title = element_blank(), 
        legend.text = element_text(size = 15), legend.key.size = unit(1, "cm"))+
  scale_color_manual(values= colors)

shared_legend <- extract_legend(legend)

#Plot the large grid
grid.arrange(arrangeGrob(t0, t_cn, t_cw, t_wna, t_ww, t_ss, p1,p3,p2,p4, t_sl, p9,p11,p10,p12, t_fs, p5,p7,p6,p8, t_fl, p13,p15,p14,p16, nrow = 5, ncol = 5, heights= c(1,3.8,4,4,4.2), widths = c(1.5,5,5,5,5)),
             t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.2, 0.8), widths = c(11.7,0.3))

# Plot the results
# habitat loss compared to range loss ----------
p1 <- ggplot(data[[1]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p2 <-ggplot(data[[2]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p3 <- ggplot(data[[3]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p4 <- ggplot(data[[4]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p5 <- ggplot(data[[5]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p6 <- ggplot(data[[6]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p7 <- ggplot(data[[7]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p8 <- ggplot(data[[8]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p9 <- ggplot(data[[9]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p10 <- ggplot(data[[10]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p11 <- ggplot(data[[11]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p12 <-ggplot(data[[12]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p13 <- ggplot(data[[13]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "")+
  ylim(c(0,1))+
  xlim(c(0,1))

p14 <-ggplot(data[[14]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p15 <- ggplot(data[[15]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p16 <- ggplot(data[[16]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

#create and extract shared legend
legend <- ggplot(data[[16]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "bottom", legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), 
        legend.title = element_text(size = 15))

shared_legend <- extract_legend(legend)

#Plot large grid
grid.arrange(arrangeGrob(t0, t_cn, t_cw, t_wna, t_ww, t_ss, p1,p3,p2,p4, t_sl, p9,p11,p10,p12, t_fs, p5,p7,p6,p8, t_fl, p13,p15,p14,p16, nrow = 5, ncol = 5, heights= c(1,3.8,4,4,4.2), widths = c(1.5,5,5,5,5)),
             t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.2, 0.8), widths = c(11.7,0.3))

#-------
#-------
#-------
# Old Plots -----
# extinction probability - change in habitat suitability sums/ change in range size ------------
p1 <- ggplot(data[[1]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  # ggplot(data[[1]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  # xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p2 <-ggplot(data[[2]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  # ggplot(data[[2]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  # xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p3 <- ggplot(data[[3]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  ggplot(data[[3]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  # xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p4 <- ggplot(data[[4]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  # ggplot(data[[4]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  # xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p5 <- ggplot(data[[5]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  # ggplot(data[[5]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  # xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p6 <- ggplot(data[[6]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  # ggplot(data[[6]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  # xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p7 <- ggplot(data[[7]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  # ggplot(data[[7]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  # xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p8 <- ggplot(data[[8]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  # ggplot(data[[8]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  # xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p9 <- ggplot(data[[9]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  # ggplot(data[[9]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  # xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p10 <- ggplot(data[[10]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  # ggplot(data[[10]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  # xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p11 <- ggplot(data[[11]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  # ggplot(data[[11]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  # xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p12 <-ggplot(data[[12]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  # ggplot(data[[12]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  # xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p13 <- ggplot(data[[13]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  # ggplot(data[[13]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  # xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "")

p14 <-ggplot(data[[14]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  # ggplot(data[[14]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  # xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())

p15 <- ggplot(data[[15]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  #ggplot(data[[15]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  # xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())

p16 <- ggplot(data[[16]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  # ggplot(data[[16]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  # xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())

# create and extract shared legend
legend <- ggplot(data[[16]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "bottom", legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), 
        legend.title = element_text(size = 15))

shared_legend <- extract_legend(legend)

#Plot the large grid
grid.arrange(arrangeGrob(t0, t_cn, t_cw, t_wna, t_ww, t_ss, p1,p3,p2,p4, t_sl, p9,p11,p10,p12, t_fs, p5,p7,p6,p8, t_fl, p13,p15,p14,p16, nrow = 5, ncol = 5, heights= c(1,3.8,4,4,4.2), widths = c(1.5,5,5,5,5)),
             t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.2, 0.8), widths = c(11.7,0.3))

# population size - change in habitat suitability sums/ change in range size ------------
p1 <-# ggplot(data[[1]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  ggplot(data[[1]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  # xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p2 <-#ggplot(data[[2]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  ggplot(data[[2]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  # xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p3 <- #ggplot(data[[3]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  ggplot(data[[3]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  # xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p4 <- #ggplot(data[[4]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  ggplot(data[[4]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  # xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p5 <-# ggplot(data[[5]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  ggplot(data[[5]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  # xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p6 <-# ggplot(data[[6]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  ggplot(data[[6]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  # xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p7 <- #ggplot(data[[7]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  ggplot(data[[7]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  # xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p8 <- #ggplot(data[[8]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  ggplot(data[[8]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  # xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p9 <- #ggplot(data[[9]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  ggplot(data[[9]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  # xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p10 <-# ggplot(data[[10]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  ggplot(data[[10]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  # xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p11 <- #ggplot(data[[11]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  ggplot(data[[11]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  # xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p12 <-#ggplot(data[[12]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  ggplot(data[[12]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  # xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p13 <- #ggplot(data[[13]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  ggplot(data[[13]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  # xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "")+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p14 <-#ggplot(data[[14]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  ggplot(data[[14]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  # xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p15 <-# ggplot(data[[15]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  ggplot(data[[15]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  # xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p16 <- #ggplot(data[[16]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  ggplot(data[[16]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  # xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

#create and extract the shared legend
legend <- ggplot(data[[16]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "bottom", legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), 
        legend.title = element_text(size = 15))

shared_legend <- extract_legend(legend)

#Plot the large grid
grid.arrange(arrangeGrob(t0, t_cn, t_cw, t_wna, t_ww, t_ss, p1,p3,p2,p4, t_sl, p9,p11,p10,p12, t_fs, p5,p7,p6,p8, t_fl, p13,p15,p14,p16, nrow = 5, ncol = 5, heights= c(1,3.8,4,4,4.2), widths = c(1.5,5,5,5,5)),
             t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.2, 0.8), widths = c(11.7,0.3))

# IUCN classification time -----
# plot the classification time points for VU
VU1 <- ggplot(IUCN_classification %>% filter(land_rep == 1), aes(x= BatchNum, y = VU_Range))+
  geom_boxplot(width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = VU_Pop), col = "orange", position = position_nudge(x = 0.5), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = VU_HS), col = "red", position = position_nudge(x = -0.25), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = VU_Ext), position = position_nudge(x = 0.25), col = "blue", width = 0.2)+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")+
  scale_x_discrete(labels = c(
    "1" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "9" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "5" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "13" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "3" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "11" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "7" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "15" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "2" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "10" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "6" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "14" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "4" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "12" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "8" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "16" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>"
  ))+
  ggtitle("Land replication 1")+
  ylim(c(0,55))

VU2 <- ggplot(IUCN_classification %>% filter(land_rep == 2), aes(x= BatchNum, y = VU_Range))+
  geom_boxplot(width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = VU_Pop), col = "orange", position = position_nudge(x = 0.5), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = VU_HS), col = "red", position = position_nudge(x = -0.25), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = VU_Ext), position = position_nudge(x = 0.25), col = "blue", width = 0.2)+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")+
  scale_x_discrete(labels = c(
    "1" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "9" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "5" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "13" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "3" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "11" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "7" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "15" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "2" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "10" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "6" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "14" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "4" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "12" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "8" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "16" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>"
  ))+
  ggtitle("Land replication 2")+
  ylim(c(0,55))

VU3 <- ggplot(IUCN_classification %>% filter(land_rep == 3), aes(x= BatchNum, y = VU_Range))+
  geom_boxplot(width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = VU_Pop), col = "orange", position = position_nudge(x = 0.5), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = VU_HS), col = "red", position = position_nudge(x = -0.25), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = VU_Ext), position = position_nudge(x = 0.25), col = "blue", width = 0.2)+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")+
  scale_x_discrete(labels = c(
    "1" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "9" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "5" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "13" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "3" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "11" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "7" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "15" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "2" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "10" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "6" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "14" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "4" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "12" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "8" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "16" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>"
  ))+
  ggtitle("Land replication 3")+
  ylim(c(0,55))

# create and extract common legend
colors <- c("Range (A3)" = "black", "Extinction probability (E)" = "blue", "Habitat suitability (A3)" = "red", "Population (A3)" = "orange")

legend <- ggplot(IUCN_classification %>% filter(land_rep == 1), aes(x= BatchNum, y = VU_Range, color ="Range (A3)"))+
  geom_boxplot(width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = VU_Pop, color = "Population (A3)"), position = position_nudge(x = 0.5), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = VU_HS, color = "Habitat suitability (A3)"), position = position_nudge(x = -0.25), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = VU_Ext, color = "Extinction probability (E)"), position = position_nudge(x = 0.25), width = 0.2)+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), plot.title = element_text(size = 25, face = "bold"), legend.title = element_blank(), legend.text = element_text(size = 15), legend.key.size = unit(1, "cm"))+
  ylab("Timepoint of classification")+
  scale_color_manual(values= colors)

shared_legend <- extract_legend(legend)

#Plot large grid
grid.arrange(VU1,VU2, VU3, shared_legend, nrow=2, ncol = 2, heights = c(8,8), widths = c(8,8), top=textGrob("Vulnerable",gp=gpar(fontsize=25,font=2)))

# plot the classification time points for EN
EN1 <- ggplot(IUCN_classification %>% filter(land_rep == 1), aes(x= BatchNum, y = EN_Range))+
  geom_boxplot(width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = EN_Pop), col = "orange", position = position_nudge(x = 0.5), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = EN_HS), col = "red", position = position_nudge(x = -0.25), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = EN_Ext), position = position_nudge(x = 0.25), col = "blue", width = 0.2)+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")+
  scale_x_discrete(labels = c(
    "1" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "9" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "5" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "13" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "3" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "11" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "7" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "15" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "2" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "10" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "6" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "14" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "4" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "12" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "8" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "16" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>"
  ))+
  ggtitle("Land replication 1")+
  ylim(c(0,55))

EN2 <- ggplot(IUCN_classification %>% filter(land_rep == 2), aes(x= BatchNum, y = EN_Range))+
  geom_boxplot(width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = EN_Pop), col = "orange", position = position_nudge(x = 0.5), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = EN_HS), col = "red", position = position_nudge(x = -0.25), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = EN_Ext), position = position_nudge(x = 0.25), col = "blue", width = 0.2)+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")+
  scale_x_discrete(labels = c(
    "1" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "9" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "5" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "13" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "3" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "11" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "7" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "15" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "2" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "10" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "6" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "14" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "4" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "12" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "8" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "16" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>"
  ))+
  ggtitle("Land replication 2")+
  ylim(c(0,55))

EN3 <- ggplot(IUCN_classification %>% filter(land_rep == 3), aes(x= BatchNum, y = EN_Range))+
  geom_boxplot(width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = EN_Pop), col = "orange", position = position_nudge(x = 0.5), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = EN_HS), col = "red", position = position_nudge(x = -0.25), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = EN_Ext), position = position_nudge(x = 0.25), col = "blue", width = 0.2)+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")+
  scale_x_discrete(labels = c(
    "1" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "9" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "5" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "13" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "3" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "11" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "7" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "15" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "2" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "10" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "6" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "14" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "4" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "12" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "8" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "16" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>"
  ))+
  ggtitle("Land replication 3")+
  ylim(c(0,55))

#Plot large grid (with same legend as in the VU plot)
grid.arrange(EN1,EN2, EN3, shared_legend, nrow=2, ncol = 2, heights = c(8,8), widths = c(8,8), top=textGrob("Endangered",gp=gpar(fontsize=25,font=2)))

# plot the classification time points for CR
CR1 <- ggplot(IUCN_classification %>% filter(land_rep == 1), aes(x= BatchNum, y = CR_Range))+
  geom_boxplot(width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = CR_Pop), col = "orange", position = position_nudge(x = 0.5), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = CR_HS), col = "red", position = position_nudge(x = -0.25), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = CR_Ext), position = position_nudge(x = 0.25), col = "blue", width = 0.2)+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")+
  scale_x_discrete(labels = c(
    "1" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "9" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "5" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "13" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "3" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "11" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "7" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "15" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "2" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "10" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "6" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "14" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "4" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "12" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "8" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "16" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>"
  ))+
  ggtitle("Land replication 1")+
  ylim(c(0,55))

CR2 <- ggplot(IUCN_classification %>% filter(land_rep == 2), aes(x= BatchNum, y = CR_Range))+
  geom_boxplot(width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = CR_Pop), col = "orange", position = position_nudge(x = 0.5), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = CR_HS), col = "red", position = position_nudge(x = -0.25), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = CR_Ext), position = position_nudge(x = 0.25), col = "blue", width = 0.2)+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")+
  scale_x_discrete(labels = c(
    "1" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "9" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "5" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "13" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "3" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "11" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "7" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "15" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "2" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "10" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "6" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "14" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "4" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "12" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "8" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "16" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>"
  ))+
  ggtitle("Land replication 2")+
  ylim(c(0,55))

CR3 <- ggplot(IUCN_classification %>% filter(land_rep == 3), aes(x= BatchNum, y = CR_Range))+
  geom_boxplot(width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = CR_Pop), col = "orange", position = position_nudge(x = 0.5), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = CR_HS), col = "red", position = position_nudge(x = -0.25), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = CR_Ext), position = position_nudge(x = 0.25), col = "blue", width = 0.2)+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")+
  scale_x_discrete(labels = c(
    "1" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "9" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "5" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "13" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "3" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "11" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "7" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "15" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "2" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "10" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "6" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "14" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "4" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "12" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "8" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "16" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>"
  ))+
  ggtitle("Land replication 3")+
  ylim(c(0,55))

#Plot large grid (with same legend as in the VU plot)
grid.arrange(CR1,CR2, CR3, shared_legend, nrow=2, ncol = 2, heights = c(8,8), widths = c(8,8), top=textGrob("Critically endangered",gp=gpar(fontsize=25,font=2)))

# Population size over time ---------
p1 <- ggplot(data[[1]], aes(x= (Year-100), y = pop_sum, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("relative Population size")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())
  
p2 <- ggplot(data[[2]], aes(x= (Year-100), y = pop_sum, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("relative Population size")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p3 <- ggplot(data[[3]], aes(x= (Year-100), y = pop_sum, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("relative Population size")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p4 <- ggplot(data[[4]], aes(x= (Year-100), y = pop_sum, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("relative Population size")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p5 <- ggplot(data[[5]], aes(x= (Year-100), y = pop_sum, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("relative Population size")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p6 <- ggplot(data[[6]], aes(x= (Year-100), y = pop_sum, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("relative Population size")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p7 <- ggplot(data[[7]], aes(x= (Year-100), y = pop_sum, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("relative Population size")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p8 <- ggplot(data[[8]], aes(x= (Year-100), y = pop_sum, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("relative Population size")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p9 <- ggplot(data[[9]], aes(x= (Year-100), y = pop_sum, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("relative Population size")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p10 <- ggplot(data[[10]], aes(x= (Year-100), y = pop_sum, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("relative Population size")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p11 <- ggplot(data[[11]], aes(x= (Year-100), y = pop_sum, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("relative Population size")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p12 <- ggplot(data[[12]], aes(x= (Year-100), y = pop_sum, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("relative Population size")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p13 <- ggplot(data[[13]], aes(x= (Year-100), y = pop_sum, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("relative Population size")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p14 <- ggplot(data[[14]], aes(x= (Year-100), y = pop_sum, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("relative Population size")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p15 <- ggplot(data[[15]], aes(x= (Year-100), y = pop_sum, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("relative Population size")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p16 <- ggplot(data[[16]], aes(x= (Year-100), y = pop_sum, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("relative Population size")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

# create and extract shared legend
legend <-ggplot(data[[16]], aes(x= (Year-100), y = pop_sum, col = land))+
  geom_point()+
  geom_smooth()+
  ylab("relative Population size")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "bottom", legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), 
        legend.title = element_text(size = 15))

shared_legend <- extract_legend(legend)

#Plot the large grid
grid.arrange(arrangeGrob(t0, t_cn, t_cw, t_wna, t_ww, t_ss, p1,p3,p2,p4, t_sl, p9,p11,p10,p12, t_fs, p5,p7,p6,p8, t_fl, p13,p15,p14,p16, nrow = 5, ncol = 5, heights= c(1,3.8,4,4,4.2), widths = c(1.5,5,5,5,5)),
             t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.2, 0.8), widths = c(11.7,0.3))

# Extinction probability against population size ------
p1 <- ggplot(data[[1]], aes(x=(1-pop_sum), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Population loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p2 <- ggplot(data[[2]], aes(x=(1-pop_sum), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Population loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p3 <- ggplot(data[[3]], aes(x=(1-pop_sum), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Population loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p4 <- ggplot(data[[4]], aes(x=(1-pop_sum), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Population loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p5 <- ggplot(data[[5]], aes(x=(1-pop_sum), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Population loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p6 <- ggplot(data[[6]], aes(x=(1-pop_sum), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Population loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p7 <- ggplot(data[[7]], aes(x=(1-pop_sum), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Population loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p8 <- ggplot(data[[8]], aes(x=(1-pop_sum), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Population loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p9 <- ggplot(data[[9]], aes(x=(1-pop_sum), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Population loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p10 <- ggplot(data[[10]], aes(x=(1-pop_sum), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Population loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p11 <- ggplot(data[[11]], aes(x=(1-pop_sum), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Population loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p12 <-ggplot(data[[12]], aes(x=(1-pop_sum), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Population loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p13 <- ggplot(data[[13]], aes(x=(1-pop_sum), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Population loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "")

p14 <-ggplot(data[[14]], aes(x=(1-pop_sum), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Population loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())

p15 <- ggplot(data[[15]], aes(x=(1-pop_sum), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Population loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())

p16 <- ggplot(data[[16]], aes(x=(1-pop_sum), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Population loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())

# create and extract shared legend
legend <- ggplot(data[[16]], aes(x=(1-pop_sum), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "bottom", legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), 
        legend.title = element_text(size = 15))

shared_legend <- extract_legend(legend)

#Plot the large grid
grid.arrange(arrangeGrob(t0, t_cn, t_cw, t_wna, t_ww, t_ss, p1,p3,p2,p4, t_sl, p9,p11,p10,p12, t_fs, p5,p7,p6,p8, t_fl, p13,p15,p14,p16, nrow = 5, ncol = 5, heights= c(1,3.8,4,4,4.2), widths = c(1.5,5,5,5,5)),
             t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.2, 0.8), widths = c(11.7,0.3))

# Population size against change in habitat suitability ---------
p1 <- #ggplot(data_append %>% filter(land == 3), aes(x=(1-hs_change), y = pop_sum, col = optima))+
  #ggplot(data_append, aes(x=(1-hs_change), y = pop_sum, col = optima))+
  ggplot(data_append, aes(x=(1-hs_change), y = pop_sum, col = land, linetype = optima))+
  #geom_point()+
  xlim(c(0,1))+
  ylim(c(0,1))+
  geom_smooth(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("relative Population size")+
  geom_abline(intercept = 1, slope = -1, col = "black", linetype = "dashed", linewidth = 1)+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), plot.title = element_text(size = 18, face = "italic"))+
  theme_bw()+
  ggtitle("Niche optima")

p2 <-# ggplot(data_append %>% filter(land == 3), aes(x=(1-hs_change), y = pop_sum, col = breadth))+
 # ggplot(data_append, aes(x=(1-hs_change), y = pop_sum, col = breadth))+
  ggplot(data_append, aes(x=(1-hs_change), y = pop_sum, col = land, linetype = breadth))+
  #geom_point()+
  xlim(c(0,1))+
  ylim(c(0,1))+
  geom_smooth(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("relative Population size")+
  geom_abline(intercept = 1, slope = -1, col = "black", linetype = "dashed", linewidth = 1)+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), plot.title = element_text(size = 18, face = "italic"))+
  theme_bw()+
  ggtitle("Niche breadth")

p3 <- #ggplot(data_append %>% filter(land == 3), aes(x=(1-hs_change), y = pop_sum, col = rmax))+
  #ggplot(data_append, aes(x=(1-hs_change), y = pop_sum, col = rmax))+
  ggplot(data_append, aes(x=(1-hs_change), y = pop_sum, col = land, linetype = rmax))+
  #geom_point()+
  xlim(c(0,1))+
  ylim(c(0,1))+
  geom_smooth(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("relative Population size")+
  geom_abline(intercept = 1, slope = -1, col = "black", linetype = "dashed", linewidth = 1)+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15),  plot.title = element_text(size = 18, face = "italic"))+
  theme_bw()+
  ggtitle("Growth rate")

p4 <- #ggplot(data_append %>% filter(land == 3), aes(x=(1-hs_change), y = pop_sum, col = dispersal))+
 # ggplot(data_append, aes(x=(1-hs_change), y = pop_sum, col = dispersal))+
  ggplot(data_append, aes(x=(1-hs_change), y = pop_sum, col = land, linetype = dispersal))+
  #geom_point()+
  xlim(c(0,1))+
  ylim(c(0,1))+
  geom_smooth(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("relative Population size")+
  geom_abline(intercept = 1, slope = -1, col = "black", linetype = "dashed", linewidth = 1)+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), plot.title = element_text(size = 18, face = "italic"))+
  theme_bw()+
  ggtitle("Dispersal")

grid.arrange(p1,p2, p3, p4, nrow=2, ncol = 2, heights = c(8,8), widths = c(8,8), top=textGrob("Land replication 3",gp=gpar(fontsize=20,font=2)))
grid.arrange(p1,p2, p3, p4, nrow=2, ncol = 2, heights = c(8,8), widths = c(8,8))
 
# compare the relationships with the values larger than 1 and without
p1 <- ggplot(data_append, aes(x=hs_loss, y= pop_sum, col = land, linetype = optima))+
  geom_smooth()+
  geom_point()

p2 <- ggplot(data_append, aes(x=hs_loss, y= pop_sum, col = land, linetype = breadth))+
  geom_smooth()+
  geom_point()

p3 <- ggplot(data_append, aes(x=hs_loss, y= pop_sum, col = land, linetype = rmax))+
  geom_smooth()+
  geom_point()

p4 <- ggplot(data_append, aes(x=hs_loss, y= pop_sum, col = land, linetype = dispersal))+
  geom_smooth()+
  geom_point()

grid.arrange(p1,p2, p3, p4, nrow=2, ncol = 2, heights = c(8,8), widths = c(8,8))

# IUCN classification time --------

#transform BatchNum column
#IUCN_classification$BatchNum <- factor(IUCN_classification$BatchNum, levels = c("1", "9", "5", "13", "3", "11", "7", "15", "2", "10", "6", "14", "4", "12", "8","16"))
#IUCN_classification$BatchNum <- as.factor(IUCN_classification$BatchNum)

VU_pos <- #ggplot(IUCN_classification, aes(x = optima, y = VU_HS))+
  ggplot(IUCN_classification %>% filter(land_rep == 1), aes(x = optima, y = VU_HS))+
  geom_boxplot(width = 0.2, col = "red")+
  geom_boxplot(aes(x = optima, y = VU_Ext), position = position_nudge(x = -0.25), width = 0.2, col = "blue")+
  geom_boxplot(aes(x = optima, y = VU_Pop), position = position_nudge(x = 0.25), width = 0.2, col = "orange")+
  ggtitle("Niche position")+
  xlab("")+
  ylim(c(0,60))+
theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")

VU_breadth <- #ggplot(IUCN_classification, aes(x = breadth, y = VU_HS))+
  ggplot(IUCN_classification %>% filter(land_rep == 1), aes(x = breadth, y = VU_HS))+
  geom_boxplot(width = 0.2, col = "red")+
  geom_boxplot(aes(x = breadth, y = VU_Ext), position = position_nudge(x = -0.25), width = 0.2, col = "blue")+
  geom_boxplot(aes(x = breadth, y = VU_Pop), position = position_nudge(x = 0.25), width = 0.2, col = "orange")+
  ggtitle("Niche breadth")+
  xlab("")+
  ylim(c(0,60))+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")

VU_rmax <- #ggplot(IUCN_classification, aes(x = rmax, y = VU_HS))+
  ggplot(IUCN_classification %>% filter(land_rep == 1), aes(x = rmax, y = VU_HS))+
  geom_boxplot(width = 0.2, col = "red")+
  geom_boxplot(aes(x = rmax, y = VU_Ext), position = position_nudge(x = -0.25), width = 0.2, col = "blue")+
  geom_boxplot(aes(x = rmax, y = VU_Pop), position = position_nudge(x = 0.25), width = 0.2, col = "orange")+
  ggtitle("Growth rate")+
  xlab("")+
  ylim(c(0,60))+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")

VU_disp <-# ggplot(IUCN_classification, aes(x = dispersal, y = VU_HS))+
  ggplot(IUCN_classification %>% filter(land_rep == 1), aes(x = dispersal, y = VU_HS))+
  geom_boxplot(width = 0.2, col = "red")+
  geom_boxplot(aes(x = dispersal, y = VU_Ext), position = position_nudge(x = -0.25), width = 0.2, col = "blue")+
  geom_boxplot(aes(x = dispersal, y = VU_Pop), position = position_nudge(x = 0.25), width = 0.2, col = "orange")+
  ggtitle("Dispersal")+
  xlab("")+
  ylim(c(0,60))+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")

# create and extract common legend
colors <- c("Extinction probability (E)" = "blue", "Habitat suitability (A3)" = "red", "Population (A3)" = "orange")

legend <- ggplot(IUCN_classification %>% filter(land_rep == 1), aes(x = BatchNum, y = VU_Pop, color = "Population (A3)"))+
  geom_boxplot()+
  geom_boxplot(aes(x = BatchNum, y = VU_HS, color = "Habitat suitability (A3)"), position = position_nudge(x = -0.25), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = VU_Ext, color = "Extinction probability (E)"), position = position_nudge(x = 0.25), width = 0.2)+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), plot.title = element_text(size = 25, face = "bold"), legend.title = element_blank(), legend.text = element_text(size = 15), legend.key.size = unit(1, "cm"),
        legend.position = "bottom")+
  ylab("Timepoint of classification")+
  scale_color_manual(values= colors)

shared_legend <- extract_legend(legend)

#Plot large grid
grid.arrange(arrangeGrob(VU_pos,VU_breadth, VU_rmax, VU_disp, nrow=2, ncol = 2, heights = c(8,8), widths = c(8,8)), shared_legend, nrow=2, ncol = 1, heights = c(10,1),
             top=textGrob("Vulnerable",gp=gpar(fontsize=25,font=2)))
grid.arrange(arrangeGrob(VU_pos,VU_breadth, VU_rmax, VU_disp, nrow=2, ncol = 2, heights = c(8,8), widths = c(8,8)), shared_legend, nrow=2, ncol = 1, heights = c(10,1),
             top=textGrob("Vulnerable (land 1)",gp=gpar(fontsize=25,font=2)))

# Endangered
EN_pos <-# ggplot(IUCN_classification, aes(x = optima, y = EN_HS))+
  ggplot(IUCN_classification %>% filter(land_rep == 3), aes(x = optima, y = EN_HS))+
  geom_boxplot(width = 0.2, col = "red")+
  geom_boxplot(aes(x = optima, y = EN_Ext), position = position_nudge(x = -0.25), width = 0.2, col = "blue")+
  geom_boxplot(aes(x = optima, y = EN_Pop), position = position_nudge(x = 0.25), width = 0.2, col = "orange")+
  ggtitle("Niche position")+
  xlab("")+
  ylim(c(0,60))+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")

EN_breadth <- #ggplot(IUCN_classification, aes(x = breadth, y = EN_HS))+
  ggplot(IUCN_classification  %>% filter(land_rep == 3), aes(x = breadth, y = EN_HS))+
  geom_boxplot(width = 0.2, col = "red")+
  geom_boxplot(aes(x = breadth, y = EN_Ext), position = position_nudge(x = -0.25), width = 0.2, col = "blue")+
  geom_boxplot(aes(x = breadth, y = EN_Pop), position = position_nudge(x = 0.25), width = 0.2, col = "orange")+
  ggtitle("Niche breadth")+
  xlab("")+
  ylim(c(0,60))+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")

EN_rmax <-# ggplot(IUCN_classification, aes(x = rmax, y = EN_HS))+
  ggplot(IUCN_classification  %>% filter(land_rep == 3), aes(x = rmax, y = EN_HS))+
  geom_boxplot(width = 0.2, col = "red")+
  geom_boxplot(aes(x = rmax, y = EN_Ext), position = position_nudge(x = -0.25), width = 0.2, col = "blue")+
  geom_boxplot(aes(x = rmax, y = EN_Pop), position = position_nudge(x = 0.25), width = 0.2, col = "orange")+
  ggtitle("Growth rate")+
  xlab("")+
  ylim(c(0,60))+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")

EN_disp <-# ggplot(IUCN_classification, aes(x = dispersal, y = EN_HS))+
  ggplot(IUCN_classification %>% filter(land_rep == 3), aes(x = dispersal, y = EN_HS))+
  geom_boxplot(width = 0.2, col = "red")+
  geom_boxplot(aes(x = dispersal, y = EN_Ext), position = position_nudge(x = -0.25), width = 0.2, col = "blue")+
  geom_boxplot(aes(x = dispersal, y = EN_Pop), position = position_nudge(x = 0.25), width = 0.2, col = "orange")+
  ggtitle("Dispersal")+
  xlab("")+
  ylim(c(0,60))+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")

#Plot large grid
grid.arrange(arrangeGrob(EN_pos,EN_breadth, EN_rmax, EN_disp, nrow=2, ncol = 2, heights = c(8,8), widths = c(8,8)), shared_legend, nrow=2, ncol = 1, heights = c(10,1),
             top=textGrob("Endangered",gp=gpar(fontsize=25,font=2)))
grid.arrange(arrangeGrob(EN_pos,EN_breadth, EN_rmax, EN_disp, nrow=2, ncol = 2, heights = c(8,8), widths = c(8,8)), shared_legend, nrow=2, ncol = 1, heights = c(10,1),
             top=textGrob("Endangered (land 3)",gp=gpar(fontsize=25,font=2)))

# Critically Endangered
CR_pos <-# ggplot(IUCN_classification, aes(x = optima, y = CR_HS))+
  ggplot(IUCN_classification %>% filter(land_rep == 1), aes(x = optima, y = CR_HS))+
  geom_boxplot(width = 0.2, col = "red")+
  geom_boxplot(aes(x = optima, y = CR_Ext), position = position_nudge(x = -0.25), width = 0.2, col = "blue")+
  geom_boxplot(aes(x = optima, y = CR_Pop), position = position_nudge(x = 0.25), width = 0.2, col = "orange")+
  ggtitle("Niche position")+
  xlab("")+
  ylim(c(0,60))+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")

CR_breadth <- #ggplot(IUCN_classification, aes(x = breadth, y = CR_HS))+
  ggplot(IUCN_classification %>% filter(land_rep == 1), aes(x = breadth, y = CR_HS))+
  geom_boxplot(width = 0.2, col = "red")+
  geom_boxplot(aes(x = breadth, y = CR_Ext), position = position_nudge(x = -0.25), width = 0.2, col = "blue")+
  geom_boxplot(aes(x = breadth, y = CR_Pop), position = position_nudge(x = 0.25), width = 0.2, col = "orange")+
  ggtitle("Niche breadth")+
  xlab("")+
  ylim(c(0,60))+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")

CR_rmax <- #ggplot(IUCN_classification, aes(x = rmax, y = CR_HS))+
  ggplot(IUCN_classification %>% filter(land_rep == 1), aes(x = rmax, y = CR_HS))+
  geom_boxplot(width = 0.2, col = "red")+
  geom_boxplot(aes(x = rmax, y = CR_Ext), position = position_nudge(x = -0.25), width = 0.2, col = "blue")+
  geom_boxplot(aes(x = rmax, y = CR_Pop), position = position_nudge(x = 0.25), width = 0.2, col = "orange")+
  ggtitle("Growth rate")+
  xlab("")+
  ylim(c(0,60))+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")

CR_disp <-# ggplot(IUCN_classification, aes(x = dispersal, y = CR_HS))+
  ggplot(IUCN_classification %>% filter(land_rep == 1), aes(x = dispersal, y = CR_HS))+
  geom_boxplot(width = 0.2, col = "red")+
  geom_boxplot(aes(x = dispersal, y = CR_Ext), position = position_nudge(x = -0.25), width = 0.2, col = "blue")+
  geom_boxplot(aes(x = dispersal, y = CR_Pop), position = position_nudge(x = 0.25), width = 0.2, col = "orange")+
  ggtitle("Dispersal")+
  xlab("")+
  ylim(c(0,60))+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 20, face = "italic"))+
  ylab("Timepoint of classification")

#Plot large grid
grid.arrange(arrangeGrob(CR_pos,CR_breadth, CR_rmax, CR_disp, nrow=2, ncol = 2, heights = c(8,8), widths = c(8,8)), shared_legend, nrow=2, ncol = 1, heights = c(10,1),
             top=textGrob("Critically endangered",gp=gpar(fontsize=25,font=2)))
grid.arrange(arrangeGrob(CR_pos,CR_breadth, CR_rmax, CR_disp, nrow=2, ncol = 2, heights = c(8,8), widths = c(8,8)), shared_legend, nrow=2, ncol = 1, heights = c(10,1),
             top=textGrob("Critically endangered (land 1)",gp=gpar(fontsize=25,font=2)))

# habitat loss over time ---------
p1 <- ggplot(data[[1]], aes(x= (Year-100), y = hs_change, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("Habitat size")+
  xlab("Year")+
  ylim(c(0,1.4))+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p2 <- ggplot(data[[2]], aes(x= (Year-100), y = hs_change, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("Habitat size")+
  xlab("Year")+
  ylim(c(0,1.4))+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p3 <- ggplot(data[[3]], aes(x= (Year-100), y = hs_change, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("Habitat size")+
  xlab("Year")+
  ylim(c(0,1.4))+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p4 <- ggplot(data[[4]], aes(x= (Year-100), y = hs_change, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("Habitat size")+
  xlab("Year")+
  ylim(c(0,1.4))+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p5 <- ggplot(data[[5]], aes(x= (Year-100), y = hs_change, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("Habitat size")+
  xlab("Year")+
  ylim(c(0,1.4))+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p6 <- ggplot(data[[6]], aes(x= (Year-100), y = hs_change, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("Habitat size")+
  xlab("Year")+
  ylim(c(0,1.4))+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p7 <- ggplot(data[[7]], aes(x= (Year-100), y = hs_change, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("Habitat size")+
  xlab("Year")+
  ylim(c(0,1.4))+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p8 <- ggplot(data[[8]], aes(x= (Year-100), y = hs_change, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("Habitat size")+
  xlab("Year")+
  ylim(c(0,1.4))+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p9 <- ggplot(data[[9]], aes(x= (Year-100), y = hs_change, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("Habitat size")+
  xlab("Year")+
  ylim(c(0,1.4))+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p10 <- ggplot(data[[10]], aes(x= (Year-100), y = hs_change, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("Habitat size")+
  xlab("Year")+
  ylim(c(0,1.4))+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p11 <- ggplot(data[[11]], aes(x= (Year-100), y = hs_change, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("Habitat size")+
  xlab("Year")+
  ylim(c(0,1.4))+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p12 <- ggplot(data[[12]], aes(x= (Year-100), y = hs_change, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("Habitat size")+
  xlab("Year")+
  ylim(c(0,1.4))+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p13 <- ggplot(data[[13]], aes(x= (Year-100), y = hs_change, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("Habitat size")+
  xlab("Year")+
  ylim(c(0,1.4))+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p14 <- ggplot(data[[14]], aes(x= (Year-100), y = hs_change, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("Habitat size")+
  xlab("Year")+
  ylim(c(0,1.4))+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p15 <- ggplot(data[[15]], aes(x= (Year-100), y = hs_change, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("Habitat size")+
  xlab("Year")+
  ylim(c(0,1.4))+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p16 <- ggplot(data[[16]], aes(x= (Year-100), y = hs_change, col = land))+
  geom_point(size = 0.75)+
  #geom_smooth()+
  ylab("Habitat size")+
  xlab("Year")+
  ylim(c(0,1.4))+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

# create and extract shared legend
legend <-ggplot(data[[16]], aes(x= (Year-100), y = pop_sum, col = land))+
  geom_point()+
  geom_smooth()+
  ylab("relative Population size")+
  xlab("Year")+
  ylab(c(0,1.4))+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "bottom", legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), 
        legend.title = element_text(size = 15))

shared_legend <- extract_legend(legend)

#Plot the large grid
grid.arrange(arrangeGrob(t0, t_cn, t_cw, t_wna, t_ww, t_ss, p1,p3,p2,p4, t_sl, p9,p11,p10,p12, t_fs, p5,p7,p6,p8, t_fl, p13,p15,p14,p16, nrow = 5, ncol = 5, heights= c(1,3.8,4,4,4.2), widths = c(1.5,5,5,5,5)),
             t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.2, 0.8), widths = c(11.7,0.3))


