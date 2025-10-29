# Red List criteria underestimate climate-related extinction risk of range-shifting species


#-------------------------------------------------------------------------------

# --------------------------------------------------------------------- #
#                         09a. Visualisation of Results                 #
# --------------------------------------------------------------------- #


#-------------------------------------------------------------------------------

# Plots for the main text

# define file paths
home_folder <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/") #needs to be adjusted based on own folder structure
sim_dir <- file.path(paste0(home_folder, "Simulations/"))
sdm_dir <- file.path(paste0(home_folder, "output_data/SDMs/"))

# load packages
library(ggplot2)
library(gridExtra)
library(dplyr)
library(grid)
library(RColorBrewer)
library(terra)

# Loading functions
source("scripts/00_functions.R")
source("scripts/text_labels_plots.R")

# Load data
load(paste0(home_folder, "analysis_data/IUCN_classification_times_allreplicates.RData"))
load(paste0(home_folder, "analysis_data/data_mean.Rdata"))

load("4_Analysis/data/IUCN_classification_times_allreplicates.RData")
load("4_Analysis/data/data_bayes_model.Rdata")

# Plots for Fig. 2 --------------
# plot of HS, population size and extinction probability over time --------

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

ggsave(paste0(home_folder, "final_plots/comparison_graph_Batch1.pdf"))

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

ggsave(paste0(home_folder, "final_plots/comparison_graph_Batch2.pdf"))

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

ggsave(paste0(home_folder, "final_plots/legend_simulation_results.pdf"))

# Habitat suitability predictions

load(paste0(home_folder, "data_analysis/Predictions_curr_Batch1_Sim1_Replication4.RData"))
r_current <- rast(ens_preds[,1:3])

pdf(paste0(home_folder, "final_plots/SDM_predictions_map_current_Batch1_Rep4_Land1.pdf"))
plot(r_current, legend = F, axes = F, col = c("#F2F2F2", rev(c("#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529"))))
dev.off()

load(paste0(home_folder, "data_analysis/Predictions_curr_Batch2_Sim1_Replication4.RData"))
r_current <- rast(ens_preds[,1:3])

pdf(paste0(home_folder, "final_plots/SDM_predictions_map_current_Batch2_Rep4_Land1.pdf"))
plot(r_current, legend = F, axes = F, col = c("#F2F2F2", rev(c("#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529"))))
dev.off()

load(paste0(home_folder, "data_analysis/Predictions_fut_Batch1_Sim1_Replication4.RData"))

# extract year 20 (for range-contracting)
fut_preds <- ens_fut_preds[[20]]
r_fut <- rast(fut_preds[,1:3])

pdf(paste0(home_folder, "final_plots/SDM_predictions_map_future_Batch1_Rep4_Land1.pdf"))
plot(r_fut, legend = F, axes = F, col = c("#F2F2F2", rev(c("#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529"))))
dev.off()

load(paste0(home_folder, "data_analysis/Predictions_fut_Batch2_Sim1_Replication4.RData"))

# extract year 40 (for range-shifting)
fut_preds <- ens_fut_preds[[30]]
r_fut <- rast(fut_preds[,1:3])

pdf(paste0(home_folder, "final_plots/SDM_predictions_map_future_Batch2_Rep4_Land1.pdf"))
plot(r_fut, legend = F, axes = F, col = c("#F2F2F2", rev(c("#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529"))))
dev.off()

# Extract legend
pdf(paste0(home_folder, "final_plots/SDM_predictions_legend.pdf"))
plot(r_current, axes = F, col = c("#F2F2F2", rev(c("#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529"))), range = c(0,100), plg = list(size = c(1,2), cex = 3), mar = c(1, 1, 1, 6))
dev.off()

# Abundance plots of the simulation

# Load required data
pop_Batch1 <- readRDS(paste0(home_folder, "data_analysis/Batch1_Sim1_Land1_Pop_Rep4.rds"))
pop_Batch2 <- readRDS(paste0(home_folder, "data_analysis/Batch2_Sim1_Land1_Pop_Rep4.rds"))
load(paste0(home_folder, "data_analysis/Predictions_curr_Batch1_Sim1_Replication4.RData"))

# Abundance plot for year 0
pop_Batch1_current <- subset(pop_Batch1, pop_Batch1$Year == 100)
pop_Batch1_current_full <- merge(ens_preds[,c(1:2)], pop_Batch1_current[,c(4:5,7)], by = c("x","y"), all.x = T)
pop_Batch1_current_full[which(is.na(pop_Batch1_current_full$NInd)),"NInd"] <- 0
r_abu_current <- rast(as.data.frame(pop_Batch1_current_full[,c(1:3)]))

pdf(paste0(home_folder, "final_plots/Abundances_map_current_Batch1_Rep4_Land1.pdf"))
plot(r_abu_current, axes = F, range = c(0,11), legend = F, smooth = T, col = c("#F2F2F2", rev(brewer.pal(n = 11, name = "Spectral"))))
dev.off()

pop_Batch2_current <- subset(pop_Batch2, pop_Batch2$Year == 100)
pop_Batch2_current_full <- merge(ens_preds[,c(1:2)], pop_Batch2_current[,c(4:5,7)], by = c("x","y"), all.x = T)
pop_Batch2_current_full[which(is.na(pop_Batch2_current_full$NInd)),"NInd"] <- 0
r_abu_current <- rast(as.data.frame(pop_Batch2_current_full[,c(1:3)]))

pdf(paste0(home_folder, "final_plots/Abundances_map_current_Batch2_Rep4_Land1.pdf"))
plot(r_abu_current, axes = F, range = c(0,11), legend = F, smooth = T, col = c("#F2F2F2", rev(brewer.pal(n = 11, name = "Spectral"))))
dev.off()

# Plot future
pop_Batch1_future <- subset(pop_Batch1, pop_Batch1$Year == 120)
pop_Batch1_future_full <- merge(ens_preds[,c(1:2)], pop_Batch1_future[,c(4:5,7)], by = c("x","y"), all.x = T)
pop_Batch1_future_full[which(is.na(pop_Batch1_future_full$NInd)),"NInd"] <- 0
r_abu_future <- rast(as.data.frame(pop_Batch1_future_full[,c(1:3)]))

pdf(paste0(home_folder, "final_plots/Abundances_map_future_Batch1_Rep4_Land1.pdf"))
plot(r_abu_future, axes = F, range = c(0,11), legend = F, smooth = T, col = c("#F2F2F2", rev(brewer.pal(n = 11, name = "Spectral"))))
dev.off()

pop_Batch2_future <- subset(pop_Batch2, pop_Batch2$Year == 130)
pop_Batch2_future_full <- merge(ens_preds[,c(1:2)], pop_Batch2_future[,c(4:5,7)], by = c("x","y"), all.x = T)
pop_Batch2_future_full[which(is.na(pop_Batch2_future_full$NInd)),"NInd"] <- 0
r_abu_future <- rast(as.data.frame(pop_Batch2_future_full[,c(1:3)]))

pdf(paste0(home_folder, "final_plots/Abundances_map_future_Batch2_Rep4_Land1.pdf"))
plot(r_abu_future, axes = F, range = c(0,11), legend = F, col = c("#F2F2F2"))
dev.off()

pdf(paste0(home_folder, "final_plots/Abundance_legend.pdf"))
plot(r_abu_current, axes = F, range = c(0,11), col = c("#F2F2F2", rev(brewer.pal(n = 11, name = "Spectral"))), plg = list(size = c(1,2), cex = 3), mar = c(1, 1, 1, 6))
dev.off()


# Plots for Fig. 3 -------------------
# Population size against habitat suitability ---------------
#Load model data
load(paste0(home_folder, "model_results/Model Results/Model_ordbeta_randomintercept.Rdata"))

# get all trait values
position <- c("range-contracting", "range-shifting")
breadth <- c("narrow", "wide")
rmax <- c("slow", "fast")
dispersal <- c("short", "long")
land_rep <- 1:3

#create data frame with all trait combinations
sims_long <- expand.grid(position = position, breadth = breadth, rmax = rmax, dispersal = dispersal, land = land_rep)

#add new hs_loss predictions to it
hs_loss <- seq(0,1,length=100)

# expand data set by length of vector
new_data <- sims_long[rep(seq_len(nrow(sims_long)), length(hs_loss)), ]
new_data$index <- as.numeric(row.names(new_data))
new_data <- new_data[order(new_data$index), ]
new_data <- cbind(new_data, hs_loss)
new_data$land <- factor(new_data$land, levels = c("1", "2", "3"))

# predict to new data
predictions_data_position <- cbind(new_data, predict(model, newdata=new_data,  type = "response", se.fit=T))
predictions_data_breadth <- cbind(new_data, predict(model, newdata=new_data,  type = "response", se.fit=T))
predictions_data_rmax <- cbind(new_data, predict(model, newdata=new_data,  type = "response", se.fit=T))
predictions_data_dispersal <- cbind(new_data, predict(model, newdata=new_data,  type = "response", se.fit=T))

save(predictions_data_position, predictions_data_breadth, predictions_data_rmax, predictions_data_dispersal, file = paste0(home_folder, "analysis_data/Model_predictions_OBR_plot.Rdata"))

# Since the predictions can take a lot of time, I save the results and can load them in afterwards
#load(paste0(home_folder, "analysis_data/Model_predictions_OBR_plot.Rdata"))

# create mean predictions per variable and land
predictions_mean_position <- predictions_data_position %>%
  group_by(position, land, hs_loss) %>%
  summarise(mean = mean(Estimate), sd = sd(Estimate))

predictions_mean_breadth <- predictions_data_breadth %>%
  group_by(breadth, land, hs_loss) %>%
  summarise(mean = mean(Estimate), sd = sd(Estimate))

predictions_mean_rmax <- predictions_data_rmax %>%
  group_by(rmax, land, hs_loss) %>%
  summarise(mean = mean(Estimate), sd = sd(Estimate))

predictions_mean_dispersal <- predictions_data_dispersal %>%
  group_by(dispersal, land, hs_loss) %>%
  summarise(mean = mean(Estimate), sd = sd(Estimate))

# plot predictions
ggplot(predictions_mean_position, aes(x=hs_loss, y = mean, col = land, linetype = position))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_ribbon(aes(ymin = (mean-1.96*sd), ymax = (mean+1.96*sd), fill = land), alpha=0.05, col = "lightgrey") +
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 22), axis.title = element_text(size = 24),
        legend.position = c(0.91, 0.85),  legend.title = element_text(size = 23), legend.text = element_text(size = 22),
        legend.key.size = unit(2,"line"))+ #axis.title.x = element_blank(),
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"))+
  labs(colour = "Landscape", linetype = NULL)+
  guides(linetype = guide_legend(order = 1), fill = "none")

# Plots for Fig. 4 ------------------
# IUCN classification time -------

IUCN_classification$position <- as.character(IUCN_classification$position)
IUCN_classification[which(IUCN_classification$position == "marginal"), "position"] <- "range-contracting"
IUCN_classification[which(IUCN_classification$position == "central"), "position"] <- "range-shifting"

# calculate mean Extinction time and standard deviation for position trait
hline_df <- data.frame(IUCN_classification %>% group_by(position) %>% summarise(meanExt = mean(Ext_Time),
                                                                                       sdExt = sd(Ext_Time)))
# Create mapping from x levels to numeric positions
x_positions <- setNames(1:length(levels(hline_df$position)), levels(hline_df$position))

# Add x numeric positions to hline_df
hline_df$x_num <- x_positions[as.character(hline_df$position)]

# plot
p_pos1 <- ggplot(IUCN_classification, aes(x = position, y = VU_HS))+
  geom_segment(data = hline_df,
               aes(x = x_num - 0.45, xend = x_num + 0.45, y = meanExt, yend = meanExt),
               inherit.aes = FALSE,
               color = "gray55")+
  geom_rect(data = hline_df,
            aes(xmin = x_num - 0.45, xmax = x_num + 0.45, ymin = meanExt - sdExt, ymax = meanExt + sdExt),
            inherit.aes = FALSE,
            fill = "gray55", alpha = 0.2) +
  geom_boxplot(width = 0.06, fill = "#EE2C2C", position = position_nudge(x = -0.42))+
  geom_boxplot(aes(x = position, y = VU_Ext), position = position_nudge(x = -0.24), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(x = position, y = VU_Pop), position = position_nudge(x = - 0.33), width = 0.06, fill = "orange")+
  geom_boxplot(aes(x = position, y = EN_HS), width = 0.06, fill = "#EE2C2C", position = position_nudge(x = -0.09))+
  geom_boxplot(aes(x = position, y = EN_Ext), position = position_nudge(x = 0.09), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(x = position, y = EN_Pop), position = position_nudge(x = 0), width = 0.06, fill = "orange")+
  geom_boxplot(aes(x = position, y = CR_HS), width = 0.06, fill = "#EE2C2C", position = position_nudge(x = 0.24))+
  geom_boxplot(aes(x = position, y = CR_Ext), position = position_nudge(x = 0.43), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(x = position, y = CR_Pop), position = position_nudge(x = 0.33), width = 0.06, fill = "orange")+
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 1.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 0.83, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 2.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 1.83, linetype = "dashed", color = "lightgrey")+
  annotate(geom="text", x=0.655, y=68, label="VU", color="black", size = 8)+
  annotate(geom="text", x=0.995, y=68, label="EN", color="black", size = 8)+
  annotate(geom="text", x=1.325, y=68, label="CR", color="black", size = 8)+
  annotate(geom="text", x=1.685, y=68, label="VU", color="black", size = 8)+
  annotate(geom="text", x=1.995, y=68, label="EN", color="black", size = 8)+
  annotate(geom="text", x=2.335, y=68, label="CR", color="black", size = 8)+
  scale_x_discrete(expand = c(0.25, 0.25)) +
  xlab("")+
  ylim(c(0,70))+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 22),
        axis.title = element_text(size = 24), legend.position = "", 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))+
  ylab("Classification time [years]")

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

#Plot large grid
grid.arrange(p_pos1, shared_legend, nrow=2, ncol = 1, heights = c(10,1))