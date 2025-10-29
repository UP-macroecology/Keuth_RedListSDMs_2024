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
library(raster)

# Loading functions
source("scripts/00_functions.R")
source("scripts/text_labels_plots.R")

# Load data
load(paste0(home_folder, "model_results/Model Results/Model_ordbeta_randomintercept.Rdata"))
load(paste0(home_folder, "analysis_data/Model_predictions_OBR_plot.Rdata"))
load(paste0(home_folder, "analysis_data/data_mean_longformat.Rdata"))
load(paste0(home_folder, "analysis_data/IUCN_classification_times_dispersalassumptions.RData"))
IUCN_classification_dispersal <- IUCN_classification
load(paste0(home_folder, "analysis_data/IUCN_classification_times_allreplicates.RData"))
load(paste0(home_folder, "analysis_data/data_mean.Rdata"))
load(paste0(home_folder, "analysis_data/performance_measures.Rdata"))

# Plots for Fig. S1 --------------

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

# Plots for Fig. S2 --------------
#convert specific columns
performance$Algorithm <- factor(performance$Algorithm, levels = c("GLM", "RF", "Maxent", "mean_prob"))
performance$BatchNum <- factor(performance$BatchNum, levels = c("1", "9", "5", "13", "3", "11", "7", "15", "2", "10", "6", "14", "4", "12", "8","16"))

performance$position <- factor(performance$position, levels = c("marginal", "central"))

p_pos <- ggplot(performance, aes(x= position, y = mean_AUC, fill = Algorithm))+
  geom_boxplot()+
  ylab("AUC")+
  ylim(c(0.85,1))+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 20),
        axis.title = element_text(size = 22), legend.position = "", plot.title = element_text(size = 24, face = "italic"))+
  scale_fill_manual(values = c("#9460A5", "#C2A5CF", "#A6DBA0", "#388E5A"))+
  ggtitle("Niche position")

p_breadth <- ggplot(performance, aes(x= breadth, y = mean_AUC, fill = Algorithm))+
  geom_boxplot()+
  ylab("AUC")+
  ylim(c(0.85,1))+
  theme_bw()+
  theme(axis.title = element_blank(), axis.text = element_text(size = 20), legend.position = "", plot.title = element_text(size = 24, face = "italic"))+
  scale_fill_manual(values = c("#9460A5", "#C2A5CF", "#A6DBA0", "#388E5A"))+
  ggtitle("Niche breadth")

p_rmax <- ggplot(performance, aes(x= rmax, y = mean_AUC, fill = Algorithm))+
  geom_boxplot()+
  ylab("AUC")+
  ylim(c(0.85,1))+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 20),
        axis.title = element_text(size = 22), legend.position = "", plot.title = element_text(size = 24, face = "italic"))+
  scale_fill_manual(values = c("#9460A5", "#C2A5CF", "#A6DBA0", "#388E5A"))+
  ggtitle("Growth rate")

p_disp <- ggplot(performance, aes(x= dispersal, y = mean_AUC, fill = Algorithm))+
  geom_boxplot()+
  ylab("AUC")+
  ylim(c(0.85,1))+
  theme_bw()+
  theme(axis.title = element_blank(), axis.text = element_text(size = 20), legend.position = "", plot.title = element_text(size = 24, face = "italic"))+
  scale_fill_manual(values = c("#9460A5", "#C2A5CF", "#A6DBA0", "#388E5A"))+
  ggtitle("Dispersal")

# extract and create legend
legend <- ggplot(performance %>% filter(landRep == 1), aes(x= dispersal, y = mean_TSS, fill = Algorithm))+
  geom_boxplot()+
  ylab("AUC")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 20),
        axis.title = element_text(size = 20), plot.title = element_text(size = 25, face = "bold"), legend.key.size = unit(1.5, 'cm'), 
        legend.title = element_text(size=22, face = "bold"), #change legend title font size
        legend.text = element_text(size=20), legend.position = "bottom")+
  scale_fill_manual(values = c("#9460A5", "#C2A5CF", "#A6DBA0", "#388E5A"), labels = c('GLM', 'Random Forest', "MaxEnt", "Ensemble"))

shared_legend <- extract_legend(legend)

#Plot for main text
grid.arrange(arrangeGrob(p_pos,p_breadth, p_rmax, p_disp, nrow=2, ncol = 2, heights = c(8,8), widths = c(8,8)), shared_legend, nrow=2, ncol = 1, heights = c(10,1))

# Plots for Fig. S3 --------------
#convert specific columns
performance$Algorithm <- factor(performance$Algorithm, levels = c("GLM", "RF", "Maxent", "mean_prob"))
performance$BatchNum <- factor(performance$BatchNum, levels = c("1", "9", "5", "13", "3", "11", "7", "15", "2", "10", "6", "14", "4", "12", "8","16"))

performance$position <- factor(performance$position, levels = c("marginal", "central"))

p_pos <- ggplot(performance, aes(x= position, y = mean_TSS, fill = Algorithm))+
  geom_boxplot()+
  ylab("TSS")+
  ylim(c(0.6,1))+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 20),
        axis.title = element_text(size = 22), legend.position = "", plot.title = element_text(size = 24, face = "italic"))+
  scale_fill_manual(values = c("#9460A5", "#C2A5CF", "#A6DBA0", "#388E5A"))+
  ggtitle("Niche position")

p_breadth <- ggplot(performance, aes(x= breadth, y = mean_TSS, fill = Algorithm))+
  geom_boxplot()+
  ylab("TSS")+
  ylim(c(0.6,1))+
  theme_bw()+
  theme(axis.title = element_blank(), axis.text = element_text(size = 20), legend.position = "", plot.title = element_text(size = 24, face = "italic"))+
  scale_fill_manual(values = c("#9460A5", "#C2A5CF", "#A6DBA0", "#388E5A"))+
  ggtitle("Niche breadth")

p_rmax <- ggplot(performance, aes(x= rmax, y = mean_TSS, fill = Algorithm))+
  geom_boxplot()+
  ylab("TSS")+
  ylim(c(0.6,1))+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 20),
        axis.title = element_text(size = 22), legend.position = "", plot.title = element_text(size = 24, face = "italic"))+
  scale_fill_manual(values = c("#9460A5", "#C2A5CF", "#A6DBA0", "#388E5A"))+
  ggtitle("Growth rate")

p_disp <- ggplot(performance, aes(x= dispersal, y = mean_TSS, fill = Algorithm))+
  geom_boxplot()+
  ylab("TSS")+
  ylim(c(0.6,1))+
  theme_bw()+
  theme(axis.title = element_blank(), axis.text = element_text(size = 20), legend.position = "", plot.title = element_text(size = 24, face = "italic"))+
  scale_fill_manual(values = c("#9460A5", "#C2A5CF", "#A6DBA0", "#388E5A"))+
  ggtitle("Dispersal")

# extract and create legend
legend <- ggplot(performance %>% filter(landRep == 1), aes(x= dispersal, y = mean_TSS, fill = Algorithm))+
  geom_boxplot()+
  ylab("AUC")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 20),
        axis.title = element_text(size = 20), plot.title = element_text(size = 25, face = "bold"), legend.key.size = unit(1.5, 'cm'), 
        legend.title = element_text(size=22, face = "bold"), #change legend title font size
        legend.text = element_text(size=20), legend.position = "bottom")+
  scale_fill_manual(values = c("#9460A5", "#C2A5CF", "#A6DBA0", "#388E5A"), labels = c('GLM', 'Random Forest', "MaxEnt", "Ensemble"))

shared_legend <- extract_legend(legend)

#Plot for main text
grid.arrange(arrangeGrob(p_pos,p_breadth, p_rmax, p_disp, nrow=2, ncol = 2, heights = c(8,8), widths = c(8,8)), shared_legend, nrow=2, ncol = 1, heights = c(10,1))

# Plots for Fig. S4 --------------
predictions_mean_position$position <- as.character(predictions_mean_position$position)
predictions_mean_position[which(predictions_mean_position$position == "range-contracting"), "position"] <- "marginal"
predictions_mean_position[which(predictions_mean_position$position == "range-shifting"), "position"] <- "central"
predictions_mean_position$position <- factor(predictions_mean_position$position, levels = c("marginal", "central"))
predictions_mean_breadth$breadth <- factor(predictions_mean_breadth$breadth, levels = c("wide", "narrow"))
predictions_mean_rmax$rmax <- factor(predictions_mean_rmax$rmax, levels = c("fast", "slow"))
predictions_mean_dispersal$dispersal <- factor(predictions_mean_dispersal$dispersal, levels = c("long", "short"))

p_pos <- ggplot(predictions_mean_position, aes(x=hs_loss, y = mean, col = land, linetype = position))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_ribbon(aes(ymin = (mean-1.96*sd), ymax = (mean+1.96*sd), fill = land), alpha=0.05, col='lightgrey') +
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 18), plot.title = element_text(size = 23, face = "italic"), 
        legend.position = c(0.9, 0.89),  legend.title = element_blank(), legend.text = element_text(size = 18), 
        legend.key.size = unit(2,"line"))+ #axis.title.x = element_blank(),
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(-0.1,1.25), expand = c(0.015, 0.015), breaks = c(0,0.25,0.5,0.75,1.0)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"), guide = "none")+
  ggtitle("Niche position")+
  guides(fill = "none")

p_breadth <- ggplot(predictions_mean_breadth, aes(x=hs_loss, y = mean, col = land, linetype = breadth))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_ribbon(aes(ymin = (mean-1.96*sd), ymax = (mean+1.96*sd), fill = land), alpha=0.05, col='lightgrey') +
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), plot.title = element_text(size = 23, face = "italic"), legend.position = c(0.92, 0.90),
        axis.title = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 18), legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(-0.1,1.25), expand = c(0.015, 0.015), breaks = c(0,0.25,0.5,0.75,1.0)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"), guide = "none")+
  ggtitle("Niche breadth")+
  guides(fill = "none")

p_rmax <- ggplot(predictions_mean_rmax, aes(x=hs_loss, y = mean, col = land, linetype = rmax))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_ribbon(aes(ymin = (mean-1.96*sd), ymax = (mean+1.96*sd), fill = land), alpha=0.05, col='lightgrey') +
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 18),  plot.title = element_text(size = 23, face = "italic"), 
        legend.position = c(0.93, 0.89), legend.title = element_blank(), legend.text = element_text(size = 18), legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(-0.1,1.25), expand = c(0.015, 0.015), breaks = c(0,0.25,0.5,0.75,1.0)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"), guide = "none")+
  ggtitle("Growth rate")+
  guides(fill = "none")

p_dispersal <- ggplot(predictions_mean_dispersal, aes(x=hs_loss, y = mean, col = land, linetype = dispersal))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_ribbon(aes(ymin = (mean-1.96*sd), ymax = (mean+1.96*sd), fill = land), alpha=0.05, col='lightgrey') +
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 18), plot.title = element_text(size = 23, face = "italic"), 
        legend.position = c(0.93, 0.89), axis.title.y = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 18), 
        legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(-0.1,1.25), expand = c(0.015, 0.015), breaks = c(0,0.25,0.5,0.75,1.0)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"), guide = "none")+
  ggtitle("Dispersal")+
  guides(fill = "none")

legend <- ggplot(predictions_mean_dispersal, aes(x=hs_loss, y = mean, col = land, linetype = dispersal))+
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

# Plots for Fig. S5 --------------

#Prepare data
data_adapted_long$breadth <- factor(data_adapted_long$breadth, levels = c("wide", "narrow"))
data_adapted_long$rmax <- factor(data_adapted_long$rmax, levels = c("fast", "slow"))
data_adapted_long$dispersal <- factor(data_adapted_long$dispersal, levels = c("long", "short"))
data_adapted_long$position <- as.character(data_adapted_long$position)
data_adapted_long[which(data_adapted_long$position == "range-contracting"), "position"] <- "marginal"
data_adapted_long[which(data_adapted_long$position == "range-shifting"), "position"] <- "central"
data_adapted_long$position <- factor(data_adapted_long$position, levels = c("marginal", "central"))

ggplot(data_adapted_long, aes(x=hs_loss, y = pop_sum, col = land, linetype = position))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
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

p_pos <- ggplot(data_adapted_long, aes(x=hs_loss, y = pop_sum, col = land, linetype = position))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_smooth(method = "gam", alpha=0.1, fill='steelblue4', linewidth = 1.5)+
  stat_smooth(method = "gam", geom = "ribbon", fill = NA, show.legend = F)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 18), plot.title = element_text(size = 23, face = "italic"), 
        legend.position = c(0.9, 0.89),  legend.title = element_blank(), legend.text = element_text(size = 18), 
        legend.key.size = unit(2,"line"))+ #axis.title.x = element_blank(),
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"), guide = "none")+
  ggtitle("Niche position")+
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
        legend.position = c(0.93, 0.89), legend.title = element_blank(), legend.text = element_text(size = 18), legend.key.size = unit(2,"line"))+
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
        legend.position = c(0.92, 0.89), axis.title.y = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 18), 
        legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"), guide = "none")+
  ggtitle("Dispersal")+
  guides(linetype = guide_legend(order = 1, override.aes = list(color = "black")))

legend <- ggplot(data_adapted_long, aes(x=hs_loss, y = pop_sum, col = land, linetype = dispersal))+
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

# Plots for Fig. S6 --------------

p_pos <- ggplot(IUCN_classification_dispersal, aes(x = position, y = VU_HS))+
  geom_boxplot(width = 0.1, fill = "#F36868", position = position_nudge(x = -0.40))+
  geom_boxplot(aes(x = position, y = VU_HS_disp), position = position_nudge(x = -0.26), width = 0.1, fill = "#3EA39F")+
  geom_boxplot(aes(x = position, y = EN_HS), width = 0.1, fill = "#F36868", position = position_nudge(x = -0.07))+
  geom_boxplot(aes(x = position, y = EN_HS_disp), position = position_nudge(x = 0.07), width = 0.1, fill = "#3EA39F")+
  geom_boxplot(aes(x = position, y = CR_HS), width = 0.1, fill = "#F36868", position = position_nudge(x = 0.26))+
  geom_boxplot(aes(x = position, y = CR_HS_disp), position = position_nudge(x = 0.4), width = 0.1, fill = "#3EA39F")+
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


p_breadth <- ggplot(IUCN_classification_dispersal, aes(x = breadth, y = VU_HS))+
  geom_boxplot(width = 0.1, fill = "#F36868", position = position_nudge(x = -0.4))+
  geom_boxplot(aes(x = breadth, y = VU_HS_disp), position = position_nudge(x = -0.26), width = 0.1, fill = "#3EA39F")+
  geom_boxplot(aes(x = breadth, y = EN_HS), width = 0.1, fill = "#F36868", position = position_nudge(x = -0.07))+
  geom_boxplot(aes(x = breadth, y = EN_HS_disp), position = position_nudge(x = 0.07), width = 0.1, fill = "#3EA39F")+
  geom_boxplot(aes(x = breadth, y = CR_HS), width = 0.1, fill = "#F36868", position = position_nudge(x = 0.26))+
  geom_boxplot(aes(x = breadth, y = CR_HS_disp), position = position_nudge(x = 0.4), width = 0.1, fill = "#3EA39F")+
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

p_rmax <- ggplot(IUCN_classification_dispersal, aes(x = rmax, y = VU_HS))+
  geom_boxplot(width = 0.1, fill = "#F36868", position = position_nudge(x = -0.4))+
  geom_boxplot(aes(x = rmax, y = VU_HS_disp), position = position_nudge(x = -0.26), width = 0.1, fill = "#3EA39F")+
  geom_boxplot(aes(x = rmax, y = EN_HS), width = 0.1, fill = "#F36868", position = position_nudge(x = -0.07))+
  geom_boxplot(aes(x = rmax, y = EN_HS_disp), position = position_nudge(x = 0.07), width = 0.1, fill = "#3EA39F")+
  geom_boxplot(aes(x = rmax, y = CR_HS), width = 0.1, fill = "#F36868", position = position_nudge(x = 0.26))+
  geom_boxplot(aes(x = rmax, y = CR_HS_disp), position = position_nudge(x = 0.4), width = 0.1, fill = "#3EA39F")+
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

p_disp <- ggplot(IUCN_classification_dispersal, aes(x = dispersal, y = VU_HS))+
  geom_boxplot(width = 0.1, fill = "#F36868", position = position_nudge(x = -0.4))+
  geom_boxplot(aes(x = dispersal, y = VU_HS_disp), position = position_nudge(x = -0.26), width = 0.1, fill = "#3EA39F")+
  geom_boxplot(aes(x = dispersal, y = EN_HS), width = 0.1, fill = "#F36868", position = position_nudge(x = -0.07))+
  geom_boxplot(aes(x = dispersal, y = EN_HS_disp), position = position_nudge(x = 0.07), width = 0.1, fill = "#3EA39F")+
  geom_boxplot(aes(x = dispersal, y = CR_HS), width = 0.1, fill = "#F36868", position = position_nudge(x = 0.26))+
  geom_boxplot(aes(x = dispersal, y = CR_HS_disp), position = position_nudge(x = 0.4), width = 0.1, fill = "#3EA39F")+
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
colors <- c("Habitat suitability (A3)" = "#F36868", "Habitat suitability with dispersal assumption (A3)" = "#3EA39F")

legend <- ggplot(IUCN_classification, aes(x = BatchNum, y = VU_HS, fill = "Habitat suitability (A3)"))+
  geom_boxplot()+
  geom_boxplot(aes(x = BatchNum, y = VU_HS, fill = "Habitat suitability with dispersal assumption (A3)"), position = position_nudge(x = 0.25), width = 0.2)+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), plot.title = element_text(size = 25, face = "bold"), legend.title = element_blank(), legend.text = element_text(size = 22), legend.key.size = unit(1.5, "cm"),
        legend.position = "bottom")+
  ylab("Timepoint of classification")+
  scale_fill_manual(values= colors, breaks = c("Habitat suitability (A3)", "Habitat suitability with dispersal assumption (A3)"))

shared_legend <- extract_legend(legend)

#Plot large grid
grid.arrange(arrangeGrob(p_pos,p_breadth, p_rmax, p_disp, nrow=2, ncol = 2, heights = c(8,8), widths = c(1,1)), shared_legend, nrow=2, ncol = 1, heights = c(10,1))

# Plots for Fig. S7 --------------
IUCN_classification$position <- factor(IUCN_classification$position, levels = c("marginal", "central"))

# calculate mean and sd Extinction time
hline_df_position <- data.frame(IUCN_classification %>% group_by(position) %>% summarise(meanExt = mean(Ext_Time),
                                                                                       sdExt = sd(Ext_Time)))
hline_df_breadth <- data.frame(IUCN_classification %>% group_by(breadth) %>% summarise(meanExt = mean(Ext_Time),
                                                                                       sdExt = sd(Ext_Time)))
hline_df_rmax <- data.frame(IUCN_classification %>% group_by(rmax) %>% summarise(meanExt = mean(Ext_Time),
                                                                                 sdExt = sd(Ext_Time)))
hline_df_dispersal <- data.frame(IUCN_classification %>% group_by(dispersal) %>% summarise(meanExt = mean(Ext_Time),
                                                                                           sdExt = sd(Ext_Time)))

# Create mapping from x levels to numeric positions
x_positions_position <- setNames(1:length(levels(hline_df_position$position)), levels(hline_df_position$position))
x_positions_breadth <- setNames(1:length(levels(hline_df_breadth$breadth)), levels(hline_df_breadth$breadth))
x_positions_rmax <- setNames(1:length(levels(hline_df_rmax$rmax)), levels(hline_df_rmax$rmax))
x_positions_dispersal <- setNames(1:length(levels(hline_df_dispersal$dispersal)), levels(hline_df_dispersal$dispersal))

# Add x numeric positions to hline_df
hline_df_position$x_num <- x_positions_position[as.character(hline_df_position$position)]
hline_df_breadth$x_num <- x_positions_breadth[as.character(hline_df_breadth$breadth)]
hline_df_rmax$x_num <- x_positions_rmax[as.character(hline_df_rmax$rmax)]
hline_df_dispersal$x_num <- x_positions_dispersal[as.character(hline_df_dispersal$dispersal)]

# Plot
p_pos <- ggplot(IUCN_classification, aes(x = position, y = VU_HS))+
  geom_segment(data = hline_df_position,
               aes(x = x_num - 0.45, xend = x_num + 0.45, y = meanExt, yend = meanExt),
               inherit.aes = FALSE,
               color = "gray55")+
  geom_rect(data = hline_df_position,
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
  annotate(geom="text", x=0.655, y=68, label="VU", color="black", size = 6)+
  annotate(geom="text", x=0.995, y=68, label="EN", color="black", size = 6)+
  annotate(geom="text", x=1.325, y=68, label="CR", color="black", size = 6)+
  annotate(geom="text", x=1.685, y=68, label="VU", color="black", size = 6)+
  annotate(geom="text", x=1.995, y=68, label="EN", color="black", size = 6)+
  annotate(geom="text", x=2.335, y=68, label="CR", color="black", size = 6)+
  scale_x_discrete(expand = c(0.25, 0.25)) +
  ggtitle("Niche position")+
  xlab("")+
  ylim(c(0,70))+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 20),
        axis.title = element_text(size = 22), legend.position = "", plot.title = element_text(size = 24, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))+
  ylab("Classification time [years]")

p_breadth <- ggplot(IUCN_classification, aes(x = breadth, y = VU_HS))+
  geom_segment(data = hline_df_breadth,
               aes(x = x_num - 0.45, xend = x_num + 0.45, y = meanExt, yend = meanExt),
               inherit.aes = FALSE,
               color = "gray55")+
  geom_rect(data = hline_df_breadth,
            aes(xmin = x_num - 0.45, xmax = x_num + 0.45, ymin = meanExt - sdExt, ymax = meanExt + sdExt),
            inherit.aes = FALSE,
            fill = "gray55", alpha = 0.2) +
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
  annotate(geom="text", x=0.655, y=68, label="VU", color="black", size = 6)+
  annotate(geom="text", x=0.995, y=68, label="EN", color="black", size = 6)+
  annotate(geom="text", x=1.325, y=68, label="CR", color="black", size = 6)+
  annotate(geom="text", x=1.685, y=68, label="VU", color="black", size = 6)+
  annotate(geom="text", x=1.995, y=68, label="EN", color="black", size = 6)+
  annotate(geom="text", x=2.335, y=68, label="CR", color="black", size = 6)+
  scale_x_discrete(expand = c(0.25, 0.25)) +
  ggtitle("Niche breadth")+
  ylim(c(0,70))+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 20),
        axis.title = element_blank(), legend.position = "", plot.title = element_text(size = 24, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))

p_rmax <- ggplot(IUCN_classification, aes(x = rmax, y = VU_HS))+
  geom_segment(data = hline_df_rmax,
               aes(x = x_num - 0.45, xend = x_num + 0.45, y = meanExt, yend = meanExt),
               inherit.aes = FALSE,
               color = "gray55")+
  geom_rect(data = hline_df_rmax,
            aes(xmin = x_num - 0.45, xmax = x_num + 0.45, ymin = meanExt - sdExt, ymax = meanExt + sdExt),
            inherit.aes = FALSE,
            fill = "gray55", alpha = 0.2) +
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
  annotate(geom="text", x=0.655, y=68, label="VU", color="black", size = 6)+
  annotate(geom="text", x=0.995, y=68, label="EN", color="black", size = 6)+
  annotate(geom="text", x=1.325, y=68, label="CR", color="black", size = 6)+
  annotate(geom="text", x=1.685, y=68, label="VU", color="black", size = 6)+
  annotate(geom="text", x=1.995, y=68, label="EN", color="black", size = 6)+
  annotate(geom="text", x=2.335, y=68, label="CR", color="black", size = 6)+
  scale_x_discrete(expand = c(0.25, 0.25)) +
  ggtitle("Growth rate")+
  xlab("")+
  ylim(c(0,70))+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 20),
        axis.title = element_text(size = 22), legend.position = "", plot.title = element_text(size = 24, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))+
  ylab("Classification time [years]")

p_disp <- ggplot(IUCN_classification, aes(x = dispersal, y = VU_HS))+
  geom_segment(data = hline_df_dispersal,
               aes(x = x_num - 0.45, xend = x_num + 0.45, y = meanExt, yend = meanExt),
               inherit.aes = FALSE,
               color = "gray55")+
  geom_rect(data = hline_df_dispersal,
            aes(xmin = x_num - 0.45, xmax = x_num + 0.45, ymin = meanExt - sdExt, ymax = meanExt + sdExt),
            inherit.aes = FALSE,
            fill = "gray55", alpha = 0.2) +
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
  annotate(geom="text", x=0.655, y=68, label="VU", color="black", size = 6)+
  annotate(geom="text", x=0.995, y=68, label="EN", color="black", size = 6)+
  annotate(geom="text", x=1.325, y=68, label="CR", color="black", size = 6)+
  annotate(geom="text", x=1.685, y=68, label="VU", color="black", size = 6)+
  annotate(geom="text", x=1.995, y=68, label="EN", color="black", size = 6)+
  annotate(geom="text", x=2.335, y=68, label="CR", color="black", size = 6)+
  scale_x_discrete(expand = c(0.25, 0.25)) +
  ggtitle("Dispersal")+
  ylim(c(0,70))+
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

# Plots for Fig. S8 --------------

#Landscape 1
load(paste0(home_folder, "landscapes/land1_temp_pre.Rdata"))

png(paste0(home_folder, 'plots/land1_pre.png', height=2*nrow(l_pre), width=2*ncol(l_pre)))
plot(l_pre, maxpixels=2*ncell(l_pre), cex.axis = 1.5, col = colorRampPalette(c("royalblue3", "yellow", "brown1"))(255), xaxt='n', yaxt = "n", legend = F)
dev.off()

png(paste0(home_folder, 'plots/land1_temp.png', height=2*nrow(l_tn), width=2*ncol(l_tn)))
plot(l_tn, maxpixels=2*ncell(l_tn), cex.axis = 1.5, col = colorRampPalette(c("royalblue3", "yellow", "brown1"))(255), xaxt='n', yaxt = "n", legend = F)
dev.off()

# Landscape 2
load(paste0(home_folder, "landscapes/land2_temp_pre.Rdata"))

png(paste0(home_folder, 'plots/land2_pre.png', height=2*nrow(l_pre), width=2*ncol(l_pre)))
plot(l_pre, maxpixels=2*ncell(l_pre), cex.axis = 1.5, col = colorRampPalette(c("royalblue3", "yellow", "brown1"))(255), xaxt='n', yaxt = "n", legend = F)
dev.off()

png(paste0(home_folder, 'plots/land2_temp.png', height=2*nrow(l_tn), width=2*ncol(l_tn)))
plot(l_tn, maxpixels=2*ncell(l_tn), cex.axis = 1.5, col = colorRampPalette(c("royalblue3", "yellow", "brown1"))(255), xaxt='n', yaxt = "n", legend = F)
dev.off()

#Landscape 3
load(paste0(home_folder, "landscapes/land3_temp_pre.Rdata"))

png(paste0(home_folder, 'plots/land3_pre.png', height=2*nrow(l_pre), width=2*ncol(l_pre)))
plot(l_pre, maxpixels=2*ncell(l_pre), cex.axis = 1.5, col = colorRampPalette(c("royalblue3", "yellow", "brown1"))(255), xaxt='n', yaxt = "n", legend = F)
dev.off()

png(paste0(home_folder, 'plots/land3_temp.png', height=2*nrow(l_tn), width=2*ncol(l_tn)))
plot(l_tn, maxpixels=2*ncell(l_tn), cex.axis = 1.5, col = colorRampPalette(c("royalblue3", "yellow", "brown1"))(255), xaxt='n', yaxt = "n", legend = F)
dev.off()

# Plot legend
png(paste0(home_folder, 'plots/land_legend.png', height=nrow(l_tn), width=ncol(l_tn)))
plot(l_tn, maxpixels=2*ncell(l_tn), cex.axis = 1.5, col = colorRampPalette(c("royalblue3", "yellow", "brown1"))(255), xaxt='n', yaxt = "n", legend.only = T, legend.width = 3, legend.shrink = 1, axis.args = list(cex.axis = 3))
dev.off()

# plot niches
land <- raster(paste0(home_folder, "landscapes/land1_position0.5_breadth0.055_ccYear0.asc"))

png(paste0(home_folder, 'plots/land1_niche.png', height=2*nrow(land), width=2*ncol(land)))
plot(land, maxpixels=2*ncell(land), cex.axis = 1.5, xaxt='n', yaxt = "n", legend = F, col = c("#F2F2F2", rev(c("#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529"))))
dev.off()

land <- raster(paste0(home_folder, "landscapes/land2_position0.5_breadth0.055_ccYear0.asc"))

png(paste0(home_folder, 'plots/land2_niche.png', height=2*nrow(land), width=2*ncol(land)))
plot(land, maxpixels=2*ncell(land), cex.axis = 1.5, xaxt='n', yaxt = "n", legend = F, col = c("#F2F2F2", rev(c("#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529"))))
dev.off()

land <- raster(paste0(home_folder, "landscapes/land3_position0.5_breadth0.055_ccYear0.asc"))

png(paste0(home_folder, 'plots/land3_niche.png', height=2*nrow(land), width=2*ncol(land)))
plot(land, maxpixels=2*ncell(land), cex.axis = 1.5, xaxt='n', yaxt = "n", legend = F, col = c("#F2F2F2", rev(c("#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529"))))
dev.off()

# Legend
png(paste0(home_folder, 'plots/niche_legend.png', height=2*nrow(land), width=2*ncol(land)))
plot(land, maxpixels=2*ncell(land), cex.axis = 1.5, xaxt='n', yaxt = "n", legend.only = T, legend.width = 3, legend.shrink = 1, axis.args = list(cex.axis = 3), mar = c(1, 1, 1, 6), col = c("#F2F2F2", rev(c("#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529"))))

dev.off()

# Plots for Tab. S1 --------------
summary(model_intercept)
