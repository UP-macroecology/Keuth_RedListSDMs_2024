# Models used for Red List assessments underestimate climate-related extinction risk of range-shifting species


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
source("scripts/00_text_labels_plots.R")

# Load data
load(paste0(home_folder, "analysis_data/IUCN_classification_times_allreplicates.RData"))
load(paste0(home_folder, "analysis_data/data_mean.Rdata"))

# Plots for Fig. 2 --------------
# plot of HS, population size and extinction probability over time --------

ggplot(data_mean[[1]] %>% filter(), aes(x = Year, y = meanHS))+
  geom_line(linewidth = 2.3, col = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 2.3, col = "gold")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 2.3, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 35), axis.title = element_text(size = 38), legend.position = "")+
  ylab("Simulated/ predicted value")+
  xlim(c(0,75))+
  ylim(c(-0.05,1.08))

ggsave(paste0(home_folder, "final_plots/comparison_graph_Batch1.pdf"))

ggplot(data_mean[[2]], aes(x = Year, y = meanHS))+
  geom_line(linewidth = 2.3, col = "#FF6A6A")+
  geom_line(aes(x = Year, y = meanPop), linewidth = 2.3, col = "gold")+
  geom_line(aes(x = Year, y = meanExt), linewidth = 2.3, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 35), axis.title = element_text(size = 38), legend.position = "")+
  ylab("Simulated/ predicted value")+
  xlim(c(0,75))+
  ylim(c(-0.05,1.08))

ggsave(paste0(home_folder, "final_plots/comparison_graph_Batch2.pdf"))

# Extract legend for the plots

colors <- c("Relative population size" = "gold", "Relative habitat suitability" = "#FF6A6A", "Extinction probability" = "blue")

legend <- ggplot(data_mean[[16]], aes(x = Year, y = meanHS, color = "Relative habitat suitability"))+
  geom_line(linewidth = 1)+
  geom_line(aes(x = Year, y = meanPop, color = "Relative population size"), linewidth = 1)+
  geom_line(aes(x = Year, y = meanExt, color = "Extinction probability"), linewidth = 1)+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "right", , legend.title = element_blank(), 
        legend.text = element_text(size = 30), legend.key.size = unit(2, "cm"), legend.key.width = unit(2.5, "cm"))+
  scale_color_manual(values= colors, breaks = c("Relative habitat suitability", "Relative population size", "Extinction probability"))+
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

# extract year 30 (for range-shifting)
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
#pop_Batch1 <- readRDS("4_Analysis/data/Batch1_Sim1_Land1_Pop_Rep4.rds")
pop_Batch2 <- readRDS(paste0(home_folder, "data_analysis/Batch2_Sim1_Land1_Pop_Rep4.rds"))
#pop_Batch2 <- readRDS("4_Analysis/data/Batch2_Sim1_Land1_Pop_Rep4.rds")
load(paste0(home_folder, "data_analysis/Predictions_curr_Batch1_Sim1_Replication4.RData"))
#load("4_Analysis/data/Predictions_curr_Batch1_Sim1_Replication4.RData")

# Abundance plot for year 0
pop_Batch1_current <- subset(pop_Batch1, pop_Batch1$Year == 100)
pop_Batch1_current_full <- merge(ens_preds[,c(1:2)], pop_Batch1_current[,c(4:5,7)], by = c("x","y"), all.x = T)
pop_Batch1_current_full[which(is.na(pop_Batch1_current_full$NInd)),"NInd"] <- 0
r_abu_current <- rast(as.data.frame(pop_Batch1_current_full[,c(1:3)]))

pdf(paste0(home_folder, "final_plots/Abundances_map_current_Batch1_Rep4_Land1.pdf"))
#pdf("Inkscape/images/Abundances_map_current_Batch1_Rep4_Land1.pdf")
plot(r_abu_current, axes = F, range = c(0,11), legend = F, smooth = T, col = c("#F2F2F2", c("#264D81", "#2A5B93", "#3165A6", "#3E6FB8", "#557CCA", "#7087D7", "#8A94E1", "#A19FE9", "#B1ACEF", "#BDB7F1", "#C7C2F4", "#D2CEF5", "#DDDAF7", "#E8E6FA")))
dev.off()

pop_Batch2_current <- subset(pop_Batch2, pop_Batch2$Year == 100)
pop_Batch2_current_full <- merge(ens_preds[,c(1:2)], pop_Batch2_current[,c(4:5,7)], by = c("x","y"), all.x = T)
pop_Batch2_current_full[which(is.na(pop_Batch2_current_full$NInd)),"NInd"] <- 0
r_abu_current <- rast(as.data.frame(pop_Batch2_current_full[,c(1:3)]))

pdf(paste0(home_folder, "final_plots/Abundances_map_current_Batch2_Rep4_Land1.pdf"))
plot(r_abu_current, axes = F, range = c(0,11), legend = F, smooth = T, col = c("#F2F2F2", c("#264D81", "#2A5B93", "#3165A6", "#3E6FB8", "#557CCA", "#7087D7", "#8A94E1", "#A19FE9", "#B1ACEF", "#BDB7F1", "#C7C2F4", "#D2CEF5", "#DDDAF7", "#E8E6FA")))
dev.off()

# Plot future
pop_Batch1_future <- subset(pop_Batch1, pop_Batch1$Year == 120)
pop_Batch1_future_full <- merge(ens_preds[,c(1:2)], pop_Batch1_future[,c(4:5,7)], by = c("x","y"), all.x = T)
pop_Batch1_future_full[which(is.na(pop_Batch1_future_full$NInd)),"NInd"] <- 0
r_abu_future <- rast(as.data.frame(pop_Batch1_future_full[,c(1:3)]))

pdf(paste0(home_folder, "final_plots/Abundances_map_future_Batch1_Rep4_Land1.pdf"))
plot(r_abu_future, axes = F, range = c(0,11), legend = F, smooth = T, col = c("#F2F2F2", c("#264D81", "#2A5B93", "#3165A6", "#3E6FB8", "#557CCA", "#7087D7", "#8A94E1", "#A19FE9", "#B1ACEF", "#BDB7F1", "#C7C2F4", "#D2CEF5", "#DDDAF7", "#E8E6FA")))
dev.off()

pop_Batch2_future <- subset(pop_Batch2, pop_Batch2$Year == 130)
pop_Batch2_future_full <- merge(ens_preds[,c(1:2)], pop_Batch2_future[,c(4:5,7)], by = c("x","y"), all.x = T)
pop_Batch2_future_full[which(is.na(pop_Batch2_future_full$NInd)),"NInd"] <- 0
r_abu_future <- rast(as.data.frame(pop_Batch2_future_full[,c(1:3)]))

pdf(paste0(home_folder, "final_plots/Abundances_map_future_Batch2_Rep4_Land1.pdf"))
plot(r_abu_future, axes = F, range = c(0,11), legend = F, col = c("#F2F2F2", c("#264D81", "#2A5B93", "#3165A6", "#3E6FB8", "#557CCA", "#7087D7", "#8A94E1", "#A19FE9", "#B1ACEF", "#BDB7F1", "#C7C2F4", "#D2CEF5", "#DDDAF7", "#E8E6FA")))
dev.off()

pdf(paste0(home_folder, "final_plots/Abundance_legend_side.pdf"))
pdf("4_Analysis/plots/Paper/Abundance_legend_side.pdf")
plot(r_abu_current, axes = F, range = c(0,11), col = c("#F2F2F2", c("#264D81", "#2A5B93", "#3165A6", "#3E6FB8", "#557CCA", "#7087D7", "#8A94E1", "#A19FE9", "#B1ACEF", "#BDB7F1", "#C7C2F4", "#D2CEF5", "#DDDAF7", "#E8E6FA")), plg = list(size = c(1,2), cex = 3), mar = c(1, 1, 1, 6))
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
load(paste0(home_folder, "analysis_data/Model_predictions_OBR_plot.Rdata"))
#load("4_Analysis/data/Model_predictions_OBR_plot.Rdata")

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
#pdf("4_Analysis/plots/Final submission/Fig.3.pdf", width = 3.4, height = 2)
pdf(paste0(home_folder, "final_plots/Fig.3.pdf"), width = 3.4, height = 2)
ggplot(predictions_mean_position, aes(x=hs_loss, y = mean, col = land, linetype = position))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 0.4)+
  geom_ribbon(aes(ymin = (mean-1.96*sd), ymax = (mean+1.96*sd), fill = land), alpha=0.05, col = "lightgrey") +
  geom_line(linewidth = 0.6)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw(base_size=3)+
  theme(axis.text = element_text(size = 5), axis.title = element_text(size = 5),
        legend.position = c(0.87, 0.78),  legend.title = element_text(size = 4), legend.text = element_text(size = 4),
        legend.key.size = unit(0.5,"line"))+ #axis.title.x = element_blank(),
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_color_manual(values = c("#80cdc1", "#dfc27d", "#a6611a"))+
  scale_linetype_discrete(labels = c("Range-contracting\n(Marginal niche position)", "Range-shifting\n(Central niche position)"))+
  labs(colour = "Landscape", linetype = NULL)+
  guides(linetype = guide_legend(order = 1,
                                 theme = theme(
                                   legend.key.spacing.y = unit(0.1, "cm")  # larger spacing for linetype
                                 )), fill = "none")
dev.off()

# Plots for Fig. 4 ------------------
# IUCN classification time -------
#names(IUCN_classification)[names(IUCN_classification) == 'optima'] <- 'position'
IUCN_classification$position <- as.character(IUCN_classification$position)
IUCN_classification[which(IUCN_classification$position == "marginal"), "position"] <- "range-contracting"
IUCN_classification[which(IUCN_classification$position == "central"), "position"] <- "range-shifting"

# calculate mean Extinction time and standard deviation for position trait
hline_df <- data.frame(IUCN_classification %>% group_by(position) %>% summarise(meanExt = mean(Ext_Time),
                                                                                       sdExt = sd(Ext_Time)))

hline_df$position <- factor(hline_df$position, levels = c("range-contracting", "range-shifting"))
# Create mapping from x levels to numeric positions
x_positions <- setNames(1:length(levels(hline_df$position)), levels(hline_df$position))

# Add x numeric positions to hline_df
hline_df$x_num <- x_positions[as.character(hline_df$position)]

# plot
p_pos1 <- ggplot(IUCN_classification, aes(x = position, y = VU_HS))+
  geom_segment(data = hline_df_position,
               aes(x = x_num - 0.45, xend = x_num + 0.45, y = meanExt, yend = meanExt),
               inherit.aes = FALSE,
               color = "gray55", linewidth = 0.2)+
  geom_rect(data = hline_df_position,
            aes(xmin = x_num - 0.45, xmax = x_num + 0.45, ymin = meanExt - 1.96 * sdExt, ymax = meanExt + 1.96 * sdExt),
            inherit.aes = FALSE,
            fill = "gray55", alpha = 0.2) +
  geom_boxplot(width = 0.06, fill = "#EE2C2C", position = position_nudge(x = -0.42), linewidth = 0.2, fatten = 1.5, outlier.size = 0.1)+
  geom_boxplot(aes(x = position, y = VU_Ext), position = position_nudge(x = -0.24), width = 0.06, fill = "#1C86EE", linewidth = 0.2, fatten = 1.5, outlier.size = 0.1)+
  geom_boxplot(aes(x = position, y = VU_Pop), position = position_nudge(x = - 0.33), width = 0.06, fill = "orange", linewidth = 0.2, fatten = 1.5, outlier.size = 0.1)+
  geom_boxplot(aes(x = position, y = EN_HS), width = 0.06, fill = "#EE2C2C", position = position_nudge(x = -0.09), linewidth = 0.2, fatten = 1.5, outlier.size = 0.1)+
  geom_boxplot(aes(x = position, y = EN_Ext), position = position_nudge(x = 0.09), width = 0.06, fill = "#1C86EE", linewidth = 0.2, fatten = 1.5, outlier.size = 0.1)+
  geom_boxplot(aes(x = position, y = EN_Pop), position = position_nudge(x = 0), width = 0.06, fill = "orange", linewidth = 0.2, fatten = 1.5, outlier.size = 0.1)+
  geom_boxplot(aes(x = position, y = CR_HS), width = 0.06, fill = "#EE2C2C", position = position_nudge(x = 0.24), linewidth = 0.2, fatten = 1.5, outlier.size = 0.1)+
  geom_boxplot(aes(x = position, y = CR_Ext), position = position_nudge(x = 0.43), width = 0.06, fill = "#1C86EE", linewidth = 0.2, fatten = 1.5, outlier.size = 0.1)+
  geom_boxplot(aes(x = position, y = CR_Pop), position = position_nudge(x = 0.33), width = 0.06, fill = "orange", linewidth = 0.2, fatten = 1.5, outlier.size = 0.1)+
  geom_vline(xintercept = 1.5, linewidth = 0.2)+
  geom_vline(xintercept = 1.16, linetype = "dashed", color = "lightgrey", linewidth = 0.2)+
  geom_vline(xintercept = 0.83, linetype = "dashed", color = "lightgrey", linewidth = 0.2)+
  geom_vline(xintercept = 2.16, linetype = "dashed", color = "lightgrey", linewidth = 0.2)+
  geom_vline(xintercept = 1.83, linetype = "dashed", color = "lightgrey", linewidth = 0.2)+
  annotate(geom="text", x=0.655, y=79, label="VU", color="black", size = 2.5)+
  annotate(geom="text", x=0.995, y=79, label="EN", color="black", size = 2.5)+
  annotate(geom="text", x=1.325, y=79, label="CR", color="black", size = 2.5)+
  annotate(geom="text", x=1.685, y=79, label="VU", color="black", size = 2.5)+
  annotate(geom="text", x=1.995, y=79, label="EN", color="black", size = 2.5)+
  annotate(geom="text", x=2.335, y=79, label="CR", color="black", size = 2.5)+
  annotate(geom="text", x=2.42, y=70, label="\u2020", color="black", size = 3)+
  annotate(geom="text", x=1.42, y=59, label="\u2020", color="black", size = 3)+
  scale_x_discrete(expand = c(0.25, 0.25), labels = c("Range-contracting \n (Marginal niche position)", "Range-shifting \n (Central niche position)")) +
  xlab("")+
  ylim(c(0,80))+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 7),
        axis.title = element_text(size = 7), legend.position = "", axis.ticks = element_line(linewidth = 0.3), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.3))+
  ylab("Classification time [years]")

# create and extract common legend
colors <- c("Habitat suitability (A3)" = "#EE2C2C", "Population size (A3)" = "orange", "Extinction probability (E)" = "#1C86EE")

legend <- ggplot(IUCN_classification, aes(x = BatchNum, y = VU_Pop, fill = "Population size (A3)"))+
  geom_boxplot()+
  geom_boxplot(aes(x = BatchNum, y = VU_HS, fill = "Habitat suitability (A3)"), position = position_nudge(x = -0.25), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = VU_HS, fill = "Extinction probability (E)"), position = position_nudge(x = 0.25), width = 0.2)+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), plot.title = element_text(size = 25, face = "bold"), legend.title = element_blank(), legend.text = element_text(size = 7), 
        legend.key.size = unit(0.4, "cm"),
        legend.position = "bottom")+
  guides(
    fill = guide_legend(
      override.aes = list(
        size = 0.3,
        linewidth = 0.2  # controls the box outline thickness
      )
    )
  )+
  ylab("Timepoint of classification")+
  scale_fill_manual(values= colors, breaks = c("Habitat suitability (A3)", "Population size (A3)", "Extinction probability (E)"))


shared_legend <- extract_legend(legend)

#Plot large grid
#cairo_pdf("4_Analysis/plots/Final submission/Fig.4.pdf", height = 4)
cairo_pdf(paste0(home_folder, "final_plots/Fig.4.pdf"), height = 4) # to keep the unicode character in the plot
grid.arrange(p_pos1, shared_legend, nrow=2, ncol = 1, heights = c(9,0.5))
dev.off()
