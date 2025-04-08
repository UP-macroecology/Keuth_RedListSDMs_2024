# Plot results

# load packages
library(ggplot2)
library(gridExtra)
library(dplyr)
library(grid)

# Load functions
source("Functions/extract_legend.R")
source("2_Simulations/scripts/misc/text_labels_plots.R")

# Prepare data

load("4_Analysis/data/IUCN_classification_times_allreplicates.RData")

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

# Plot the results -----

# Population size against habitat suitability (updated) ---------------

# create different data sets for the different models
data_optima <- data.frame(hs_loss=rep(seq(0,1,length=100),6))
data_optima$land <- rep(1:3, each = 100, times = 2)
data_optima$optima <- rep(c("range-shifting", "range-contracting"), each = 300)
data_optima$predictions <- NA
data_optima$land <- as.factor(data_optima$land)

data_breadth <- data.frame(hs_loss=rep(seq(0,1,length=100),6))
data_breadth$land <- rep(1:3, each = 100, times = 2)
data_breadth$breadth <- rep(c("narrow", "wide"), each = 300)
data_breadth$breadth <- factor(data_breadth$breadth, levels = c("wide", "narrow"))
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
optima <- c("range-contracting", "range-shifting")

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
  model_optima <- glm(pop_sum ~ hs_loss + I(hs_loss^2), data=data_sub_optima, family = "binomial")
  model_breadth <- glm(pop_sum ~ hs_loss + I(hs_loss^2), data=data_sub_breadth, family = "binomial")
  model_rmax <- glm(pop_sum ~ hs_loss + I(hs_loss^2), data=data_sub_rmax, family = "binomial")
  model_dispersal <- glm(pop_sum ~ hs_loss + I(hs_loss^2), data=data_sub_dispersal, family = "binomial")
  
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
  predictions_optima <- predict(model_optima, newdata=data.frame(hs_loss=seq(0,1,length=100)),  type = "response", se.fit=T)
  data_optima[which(data_optima$land == land_nr & data_optima$optima == optima_nr),"predictions"] <- predictions_optima$fit
  data_optima[which(data_optima$land == land_nr & data_optima$optima == optima_nr),"lower95"] <- predictions_optima$fit - 1.96*predictions_optima$se.fit
  data_optima[which(data_optima$land == land_nr & data_optima$optima == optima_nr),"upper95"] <- predictions_optima$fit + 1.96*predictions_optima$se.fit
  
  predictions_breadth <- predict(model_breadth, newdata=data.frame(hs_loss=seq(0,1,length=100)),  type = "response", se.fit=T)
  data_breadth[which(data_breadth$land == land_nr & data_breadth$breadth == breadth_nr),"predictions"] <- predictions_breadth$fit
  data_breadth[which(data_breadth$land == land_nr & data_breadth$breadth == breadth_nr),"lower95"] <- predictions_breadth$fit - 1.96*predictions_breadth$se.fit
  data_breadth[which(data_breadth$land == land_nr & data_breadth$breadth == breadth_nr),"upper95"] <- predictions_breadth$fit + 1.96*predictions_breadth$se.fit
  
  predictions_rmax <- predict(model_rmax, newdata=data.frame(hs_loss=seq(0,1,length=100)),  type = "response", se.fit=T)
  data_rmax[which(data_rmax$land == land_nr & data_rmax$rmax == rmax_nr),"predictions"] <- predictions_rmax$fit
  data_rmax[which(data_rmax$land == land_nr & data_rmax$rmax == rmax_nr),"lower95"] <- predictions_rmax$fit - 1.96*predictions_rmax$se.fit
  data_rmax[which(data_rmax$land == land_nr & data_rmax$rmax == rmax_nr),"upper95"] <- predictions_rmax$fit + 1.96*predictions_rmax$se.fit
  
  predictions_dispersal <- predict(model_dispersal, newdata=data.frame(hs_loss=seq(0,1,length=100)),  type = "response", se.fit=T)
  data_dispersal[which(data_dispersal$land == land_nr & data_dispersal$dispersal == dispersal_nr),"predictions"] <- predictions_dispersal$fit
  data_dispersal[which(data_dispersal$land == land_nr & data_dispersal$dispersal == dispersal_nr),"lower95"] <- predictions_dispersal$fit - 1.96*predictions_dispersal$se.fit
  data_dispersal[which(data_dispersal$land == land_nr & data_dispersal$dispersal == dispersal_nr),"upper95"] <- predictions_dispersal$fit + 1.96*predictions_dispersal$se.fit
}
sink()

save(data_optima, data_breadth, data_rmax, data_dispersal, file = "4_Analysis/data/data_model_poploss_hsloss.Rdata")

ggplot(data_optima, aes(x=hs_loss, y = predictions, col = land, linetype = optima))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha=0.1, fill='steelblue4') +
  #geom_point()+
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 23), plot.title = element_text(size = 28, face = "italic"),
        legend.position = c(0.91, 0.85),  legend.title = element_text(size = 23), legend.text = element_text(size = 23),
        legend.key.size = unit(2,"line"))+ #axis.title.x = element_blank(),
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"))+
  ggtitle("Range dynamics")+
  labs(colour = "Landscape", linetype = NULL)+
  guides(linetype = guide_legend(order = 1))

#grid.arrange(p_pos, shared_legend, ncol=2, nrow = 1, widths = c(10,1))


p_pos <- ggplot(data_optima, aes(x=hs_loss, y = predictions, col = land, linetype = optima))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha=0.1, fill='steelblue4') +
  #geom_point()+
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 18), plot.title = element_text(size = 23, face = "italic"), 
        legend.position = c(0.85, 0.89),  legend.title = element_blank(), legend.text = element_text(size = 18), 
        legend.key.size = unit(2,"line"))+ #axis.title.x = element_blank(),
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"), guide = "none")+
  ggtitle("Range dynamics")

p_breadth <- ggplot(data_breadth, aes(x=hs_loss, y = predictions, col = land, linetype = breadth))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha=0.1, fill='steelblue4') +
  #geom_point()+
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), plot.title = element_text(size = 23, face = "italic"), legend.position = c(0.92, 0.90),
        axis.title = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 18), legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"), guide = "none")+
  ggtitle("Niche breadth")

p_rmax <- ggplot(data_rmax, aes(x=hs_loss, y = predictions, col = land, linetype = rmax))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha=0.1, fill='steelblue4') +
  #geom_point()+
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 18),  plot.title = element_text(size = 23, face = "italic"), 
        legend.position = c(0.92, 0.89), legend.title = element_blank(), legend.text = element_text(size = 18), legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"), guide = "none")+
  ggtitle("Growth rate")

p_dispersal <- ggplot(data_dispersal, aes(x=hs_loss, y = predictions, col = land, linetype = dispersal))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha=0.1, fill='steelblue4') +
  #geom_point()+
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 18), plot.title = element_text(size = 23, face = "italic"), 
        legend.position = c(0.91, 0.89), axis.title.y = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 18), 
        legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"), guide = "none")+
  ggtitle("Dispersal")

legend <- ggplot(data_dispersal, aes(x=hs_loss, y = predictions, col = land, linetype = dispersal))+
  #geom_point()+
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "dashed", linewidth = 1)+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), plot.title = element_text(size = 23, face = "italic"), 
        legend.title = element_text(size = 23), legend.text = element_text(size = 18), 
        legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  guides(linetype = "none", colour = guide_legend(title = "Landscape"))+
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"))+
  ggtitle("Dispersal")

shared_legend <- extract_legend(legend)

grid.arrange(arrangeGrob(p_pos,p_breadth, p_rmax, p_dispersal, nrow=2, ncol = 2, heights = c(8,8), widths = c(1,1)), shared_legend, ncol=2, nrow = 1, widths = c(10,1))

#Same plot but using loess instead of the model
data_adapted_long$breadth <- factor(data_adapted_long$breadth, levels = c("wide", "narrow"))
data_adapted_long$rmax <- factor(data_adapted_long$rmax, levels = c("fast", "slow"))
data_adapted_long$dispersal <- factor(data_adapted_long$dispersal, levels = c("long", "short"))

ggplot(data_adapted_long, aes(x=hs_loss, y = pop_sum, col = land, linetype = optima))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  #geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha=0.1, fill='steelblue4') +
  #geom_point()+
  geom_smooth(method = "gam", alpha=0.1, fill='steelblue4', linewidth = 1.5)+
  stat_smooth(method = "gam", geom = "ribbon", fill = NA, show.legend = F)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 23), plot.title = element_text(size = 28, face = "italic"),
        legend.position = c(0.91, 0.85),  legend.title = element_text(size = 23), legend.text = element_text(size = 23),
        legend.key.size = unit(2,"line"), )+ #axis.title.x = element_blank(),
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"))+
  ggtitle("Range dynamics")+
  labs(colour = "Landscape", linetype = NULL)+
  guides(linetype = guide_legend(order = 1, override.aes = list(color = "black")))

#grid.arrange(p_pos, shared_legend, ncol=2, nrow = 1, widths = c(10,1))


p_pos <- ggplot(data_adapted_long, aes(x=hs_loss, y = pop_sum, col = land, linetype = optima))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_smooth(method = "gam", alpha=0.1, fill='steelblue4', linewidth = 1.5)+
  stat_smooth(method = "gam", geom = "ribbon", fill = NA, show.legend = F)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 18), plot.title = element_text(size = 23, face = "italic"), 
        legend.position = c(0.85, 0.89),  legend.title = element_blank(), legend.text = element_text(size = 18), 
        legend.key.size = unit(2,"line"))+ #axis.title.x = element_blank(),
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"), guide = "none")+
  ggtitle("Range dynamics")+
  guides(linetype = guide_legend(order = 1, override.aes = list(color = "black")))

p_breadth <- ggplot(data_adapted_long, aes(x=hs_loss, y = pop_sum, col = land, linetype = breadth))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_smooth(method = "gam", alpha=0.1, fill='steelblue4', linewidth = 1.5)+
  stat_smooth(method = "gam", geom = "ribbon", fill = NA, show.legend = F)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), plot.title = element_text(size = 23, face = "italic"), legend.position = c(0.92, 0.90),
        axis.title = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 18), legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"), guide = "none")+
  ggtitle("Niche breadth")+
  guides(linetype = guide_legend(order = 1, override.aes = list(color = "black")))

p_rmax <- ggplot(data_adapted_long, aes(x=hs_loss, y = pop_sum, col = land, linetype = rmax))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_smooth(method = "gam", alpha=0.1, fill='steelblue4', linewidth = 1.5)+
  stat_smooth(method = "gam", geom = "ribbon", fill = NA, show.legend = F)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 18),  plot.title = element_text(size = 23, face = "italic"), 
        legend.position = c(0.92, 0.89), legend.title = element_blank(), legend.text = element_text(size = 18), legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"), guide = "none")+
  ggtitle("Growth rate")+
  guides(linetype = guide_legend(order = 1, override.aes = list(color = "black")))

p_dispersal <- ggplot(data_adapted_long, aes(x=hs_loss, y = pop_sum, col = land, linetype = dispersal))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_smooth(method = "gam", alpha=0.1, fill='steelblue4', linewidth = 1.5)+
  stat_smooth(method = "gam", geom = "ribbon", fill = NA, show.legend = F)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 18), plot.title = element_text(size = 23, face = "italic"), 
        legend.position = c(0.91, 0.89), axis.title.y = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 18), 
        legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"), guide = "none")+
  ggtitle("Dispersal")+
  guides(linetype = guide_legend(order = 1, override.aes = list(color = "black")))

legend <- ggplot(data_adapted_long, aes(x=hs_loss, y = pop_sum, col = land, linetype = dispersal))+
  #geom_point()+
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "dashed", linewidth = 1)+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), plot.title = element_text(size = 23, face = "italic"), 
        legend.title = element_text(size = 23), legend.text = element_text(size = 18), 
        legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  guides(linetype = "none", colour = guide_legend(title = "Landscape"))+
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"))+
  ggtitle("Dispersal")

shared_legend <- extract_legend(legend)

grid.arrange(arrangeGrob(p_pos,p_breadth, p_rmax, p_dispersal, nrow=2, ncol = 2, heights = c(8,8), widths = c(1,1)), shared_legend, ncol=2, nrow = 1, widths = c(10,1))


# IUCN classification time - updated plot -------
load("4_Analysis/data/IUCN_classification_times_allreplicates.RData")
IUCN_classification$optima <- factor(IUCN_classification$optima, levels = c("marginal", "central"))

p_pos <- ggplot(IUCN_classification, aes(x = optima, y = VU_HS))+
  geom_boxplot(width = 0.06, fill = "#EE2C2C", position = position_nudge(x = -0.42))+
  geom_boxplot(aes(x = optima, y = VU_Ext), position = position_nudge(x = -0.24), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(x = optima, y = VU_Pop), position = position_nudge(x = - 0.33), width = 0.06, fill = "orange")+
  geom_boxplot(aes(x = optima, y = EN_HS), width = 0.06, fill = "#EE2C2C", position = position_nudge(x = -0.09))+
  geom_boxplot(aes(x = optima, y = EN_Ext), position = position_nudge(x = 0.09), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(x = optima, y = EN_Pop), position = position_nudge(x = 0), width = 0.06, fill = "orange")+
  geom_boxplot(aes(x = optima, y = CR_HS), width = 0.06, fill = "#EE2C2C", position = position_nudge(x = 0.24))+
  geom_boxplot(aes(x = optima, y = CR_Ext), position = position_nudge(x = 0.43), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(x = optima, y = CR_Pop), position = position_nudge(x = 0.33), width = 0.06, fill = "orange")+
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
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 20),
        axis.title = element_text(size = 22), legend.position = "", plot.title = element_text(size = 24, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))+
  ylab("Classification time [years]")

p_breadth <- ggplot(IUCN_classification, aes(x = breadth, y = VU_HS))+
  geom_boxplot(width = 0.06, fill = "#EE2C2C", position = position_nudge(x = -0.42))+
  geom_boxplot(aes(x = breadth, y = VU_Ext), position = position_nudge(x = -0.24), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(x = breadth, y = VU_Pop), position = position_nudge(x = - 0.33), width = 0.06, fill = "orange")+
  geom_boxplot(aes(x = breadth, y = EN_HS), width = 0.06, fill = "#EE2C2C", position = position_nudge(x = -0.09))+
  geom_boxplot(aes(x = breadth, y = EN_Ext), position = position_nudge(x = 0.09), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(x = breadth, y = EN_Pop), position = position_nudge(x = 0), width = 0.06, fill = "orange")+
  geom_boxplot(aes(x = breadth, y = CR_HS), width = 0.06, fill = "#EE2C2C", position = position_nudge(x = 0.24))+
  geom_boxplot(aes(x = breadth, y = CR_Ext), position = position_nudge(x = 0.43), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(x = breadth, y = CR_Pop), position = position_nudge(x = 0.33), width = 0.06, fill = "orange")+
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
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 20),
        axis.title = element_blank(), legend.position = "", plot.title = element_text(size = 24, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))

p_rmax <- ggplot(IUCN_classification, aes(x = rmax, y = VU_HS))+
  geom_boxplot(width = 0.06, fill = "#EE2C2C", position = position_nudge(x = -0.42))+
  geom_boxplot(aes(x = rmax, y = VU_Ext), position = position_nudge(x = -0.24), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(x = rmax, y = VU_Pop), position = position_nudge(x = - 0.33), width = 0.06, fill = "orange")+
  geom_boxplot(aes(x = rmax, y = EN_HS), width = 0.06, fill = "#EE2C2C", position = position_nudge(x = -0.09))+
  geom_boxplot(aes(x = rmax, y = EN_Ext), position = position_nudge(x = 0.09), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(x = rmax, y = EN_Pop), position = position_nudge(x = 0), width = 0.06, fill = "orange")+
  geom_boxplot(aes(x = rmax, y = CR_HS), width = 0.06, fill = "#EE2C2C", position = position_nudge(x = 0.24))+
  geom_boxplot(aes(x = rmax, y = CR_Ext), position = position_nudge(x = 0.43), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(x = rmax, y = CR_Pop), position = position_nudge(x = 0.33), width = 0.06, fill = "orange")+
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
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 20),
        axis.title = element_text(size = 22), legend.position = "", plot.title = element_text(size = 24, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))+
  ylab("Classification time [years]")

p_disp <- ggplot(IUCN_classification, aes(x = dispersal, y = VU_HS))+
  geom_boxplot(width = 0.06, fill = "#EE2C2C", position = position_nudge(x = -0.42))+
  geom_boxplot(aes(x = dispersal, y = VU_Ext), position = position_nudge(x = -0.24), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(x = dispersal, y = VU_Pop), position = position_nudge(x = - 0.33), width = 0.06, fill = "orange")+
  geom_boxplot(aes(x = dispersal, y = EN_HS), width = 0.06, fill = "#EE2C2C", position = position_nudge(x = -0.09))+
  geom_boxplot(aes(x = dispersal, y = EN_Ext), position = position_nudge(x = 0.11), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(x = dispersal, y = EN_Pop), position = position_nudge(x = 0), width = 0.06, fill = "orange")+
  geom_boxplot(aes(x = dispersal, y = CR_HS), width = 0.06, fill = "#EE2C2C", position = position_nudge(x = 0.24))+
  geom_boxplot(aes(x = dispersal, y = CR_Ext), position = position_nudge(x = 0.43), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(x = dispersal, y = CR_Pop), position = position_nudge(x = 0.33), width = 0.06, fill = "orange")+
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
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 20),
        axis.title = element_blank(), legend.position = "", plot.title = element_text(size = 24, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))

# create and extract common legend
colors <- c("Habitat suitability (A3)" = "#EE2C2C", "Population size (A3)" = "orange", "Extinction probability (E)" = "#1C86EE")

legend <- ggplot(IUCN_classification, aes(x = BatchNum, y = VU_Pop, fill = "Population size (A3)"))+
  geom_boxplot()+
  geom_boxplot(aes(x = BatchNum, y = VU_HS, fill = "Habitat suitability (A3)"), position = position_nudge(x = -0.25), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = VU_HS, fill = "Extinction probability (E)"), position = position_nudge(x = 0.25), width = 0.2)+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), plot.title = element_text(size = 25, face = "bold"), legend.title = element_blank(), legend.text = element_text(size = 22), legend.key.size = unit(1.5, "cm"),
        legend.position = "bottom")+
  ylab("Timepoint of classification")+
  scale_fill_manual(values= colors, breaks = c("Habitat suitability (A3)", "Population size (A3)", "Extinction probability (E)"))

shared_legend <- extract_legend(legend)

grid.arrange(arrangeGrob(p_pos,p_breadth, p_rmax, p_disp, nrow=2, ncol = 2, heights = c(8,8), widths = c(1,1)), shared_legend, nrow=2, ncol = 1, heights = c(10,1))

# single plot
IUCN_classification$optima <- as.character(IUCN_classification$optima)
IUCN_classification[which(IUCN_classification$optima == "marginal"), "optima"] <- "range-contracting"
IUCN_classification[which(IUCN_classification$optima == "central"), "optima"] <- "range-shifting"

p_pos1 <- ggplot(IUCN_classification, aes(x = optima, y = VU_HS))+
  geom_boxplot(width = 0.06, fill = "#EE2C2C", position = position_nudge(x = -0.42))+
  geom_boxplot(aes(x = optima, y = VU_Ext), position = position_nudge(x = -0.24), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(x = optima, y = VU_Pop), position = position_nudge(x = - 0.33), width = 0.06, fill = "orange")+
  geom_boxplot(aes(x = optima, y = EN_HS), width = 0.06, fill = "#EE2C2C", position = position_nudge(x = -0.09))+
  geom_boxplot(aes(x = optima, y = EN_Ext), position = position_nudge(x = 0.09), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(x = optima, y = EN_Pop), position = position_nudge(x = 0), width = 0.06, fill = "orange")+
  geom_boxplot(aes(x = optima, y = CR_HS), width = 0.06, fill = "#EE2C2C", position = position_nudge(x = 0.24))+
  geom_boxplot(aes(x = optima, y = CR_Ext), position = position_nudge(x = 0.43), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(x = optima, y = CR_Pop), position = position_nudge(x = 0.33), width = 0.06, fill = "orange")+
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 1.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 0.83, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 2.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 1.83, linetype = "dashed", color = "lightgrey")+
  annotate(geom="text", x=0.655, y=59, label="VU", color="black", size = 8)+
  annotate(geom="text", x=0.995, y=59, label="EN", color="black", size = 8)+
  annotate(geom="text", x=1.325, y=59, label="CR", color="black", size = 8)+
  annotate(geom="text", x=1.685, y=59, label="VU", color="black", size = 8)+
  annotate(geom="text", x=1.995, y=59, label="EN", color="black", size = 8)+
  annotate(geom="text", x=2.335, y=59, label="CR", color="black", size = 8)+
  scale_x_discrete(expand = c(0.25, 0.25)) +
  xlab("")+
  ylim(c(0,60))+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 22),
        axis.title = element_text(size = 24), legend.position = "", 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))+
  ylab("Classification time [years]")


#Plot large grid
grid.arrange(p_pos1, shared_legend, nrow=2, ncol = 1, heights = c(10,1))


# plot of HS, population size and extinction probability over time --------
# p1 <- ggplot(data_mean[[1]], aes(x = Year, y = meanHS))+
#   geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "#FF6A6A")+
#   geom_line(linewidth = 1.2, col = "#FF6A6A")+
#   geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "gold")+
#   geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
#   geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
#   geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
#   theme_bw()+
#   theme(axis.text = element_text(size = 18), axis.title.y = element_blank(), legend.position = "", plot.title = element_text(size = 20, face = "italic"), axis.title.x = element_text(size = 22))+
#   xlim(c(0,75))+
#   ylim(c(-0.08,1.4))+
#   labs(title = bquote(bold('range-contracting;') ~ 'narrow niche; slow growth rate; short dispersal'))
#   ggtitle("range-contracting; narrow niche\nslow growth rate; short dispersal")
# 
# p2 <- ggplot(data_mean[[2]], aes(x = Year, y = meanHS))+
#   geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "#FF6A6A")+
#   geom_line(linewidth = 1.2, col = "#FF6A6A")+
#   geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "gold")+
#   geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
#   geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
#   geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
#   theme_bw()+
#   theme(axis.text = element_text(size = 18), axis.title.y = element_blank(), legend.position = "", plot.title = element_text(size = 20, face = "italic"), axis.title.x = element_text(size = 22))+
#   xlim(c(0,75))+
#   ylim(c(-0.08,1.4))+
#   labs(title = bquote(bold('range-shifting;') ~ 'narrow niche; slow growth rate; short dispersal'))
#   ggtitle("range-shifting; narrow niche\nslow growth rate; short dispersal")
#   
#   grid.arrange(p1, p2, t0, t0, nrow=2, ncol = 2, heights = c(10,5))
  
p1 <- ggplot(data_mean[[1]], aes(x = Year, y = meanHS))+
    geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "#FF6A6A")+
    geom_line(linewidth = 1.2, col = "#FF6A6A")+
    geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "gold")+
    geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
    geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
    geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
    theme_bw()+
    theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
    xlim(c(0,75))+
    ylim(c(-0.15,1.4))
  
p2 <- ggplot(data_mean[[2]], aes(x = Year, y = meanHS))+
    geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "#FF6A6A")+
    geom_line(linewidth = 1.2, col = "#FF6A6A")+
    geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "gold")+
    geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
    geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
    geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
    theme_bw()+
    theme(axis.text = element_text(size = 16), axis.title = element_blank(), legend.position = "")+
    xlim(c(0,75))+
    ylim(c(-0.15,1.4))

p3 <- ggplot(data_mean[[3]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(linewidth = 1.2, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_blank(), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p4 <- ggplot(data_mean[[4]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(linewidth = 1.2, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_blank(), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p5 <- ggplot(data_mean[[5]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(linewidth = 1.2, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p6 <- ggplot(data_mean[[6]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(linewidth = 1.2, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_blank(), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p7 <- ggplot(data_mean[[7]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(linewidth = 1.2, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_blank(), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p8 <- ggplot(data_mean[[8]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(linewidth = 1.2, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_blank(), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p9 <- ggplot(data_mean[[9]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(linewidth = 1.2, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p10 <- ggplot(data_mean[[10]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(linewidth = 1.2, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_blank(), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p11 <- ggplot(data_mean[[11]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(linewidth = 1.2, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_blank(), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p12 <- ggplot(data_mean[[12]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(linewidth = 1.2, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_blank(), legend.position = "")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p13 <- ggplot(data_mean[[13]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(linewidth = 1.2, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p14 <- ggplot(data_mean[[14]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(linewidth = 1.2, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), legend.position = "", axis.title.y = element_blank())+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p15 <- ggplot(data_mean[[15]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(linewidth = 1.2, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), legend.position = "", axis.title.y = element_blank())+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

p16 <- ggplot(data_mean[[16]], aes(x = Year, y = meanHS))+
  geom_ribbon(aes(ymin = meanHS - sdHS, ymax = meanHS + sdHS), col = NA, alpha = 0.15, fill = "#FF6A6A")+
  geom_line(linewidth = 1.2, col = "#FF6A6A")+
  geom_ribbon(aes(ymin = meanPop - sdPop, ymax = meanPop + sdPop), col = NA, alpha = 0.15, fill = "gold")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 1.2, col = "gold")+
  geom_ribbon(aes(ymin = meanExt - sdExt, ymax = meanExt + sdExt), col = NA, alpha = 0.15, fill = "blue")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 1.2, col = "blue")+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), legend.position = "", axis.title.y = element_blank())+
  xlim(c(0,75))+
  ylim(c(-0.15,1.4))

# create and extract common legend
#colors <- c("relative Population size" = "#018571", "relative Habitat suitability" = "#DFC27D", "Extinction probability" = "#9460A5")
colors <- c("Relative population size" = "gold", "Relative habitat suitability" = "#FF6A6A", "Extinction probability" = "blue")

legend <- ggplot(data_mean[[16]], aes(x = Year, y = meanHS, color = "Relative habitat suitability"))+
  geom_line(linewidth = 1)+
  geom_line(aes(x = Year, y = meanPop, color = "Relative population size"), linewidth = 1)+
  geom_line(aes(x = Year, y = meanExt, color = "Extinction probability"), linewidth = 1)+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "bottom", , legend.title = element_blank(), 
        legend.text = element_text(size = 20), legend.key.size = unit(1.5, "cm"))+
  scale_color_manual(values= colors, breaks = c("Relative habitat suitability", "Relative population size", "Extinction probability"))

shared_legend <- extract_legend(legend)

#Plot the large grid
grid.arrange(arrangeGrob(t0, t_cn, t_cw, t_wna, t_ww, t_ss, p1,p3,p2,p4, t_sl, p9,p11,p10,p12, t_fs, p5,p7,p6,p8, t_fl, p13,p15,p14,p16, nrow = 5, ncol = 5, heights= c(1,3.8,4,4,4.2), widths = c(2,5,5,5,5)),
             t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.2, 0.8), widths = c(11.7,0.3))
