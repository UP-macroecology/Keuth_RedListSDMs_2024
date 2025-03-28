# The different performance measures of the SDMs are plotted for all three algorithms as well as the ensemble model

# Load packages
library(data.table)
library(ggplot2)
library(dplyr)
library(gridExtra)
#library(ggtext)
library(grid)

# Load in function
source("Functions/extract_legend.R")

# Prepare simulation data
# create data frame with all parameter combinations
land_rep <- 1:3
optima <- c("marginal", "central")
breadth <- c("narrow", "wide")
rmax <- c("slow", "fast")
dispersal <- c("short", "long")

sims <- expand.grid(land_rep = land_rep, optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims$BatchNum <- rep(1:16, each = 3)

# Load data sets
performance <- c()

for (sim_nr in 1:nrow(sims)) {
  rep_nr <- sims[sim_nr,]$land_rep
  BatchNum <- sims[sim_nr,]$BatchNum
  tmp <- readRDS(paste0("4_Analysis/data/performance_mean_SDM_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  tmp <- do.call(rbind, tmp)
  # add scenario and land replication number
  tmp$scenario <- paste(BatchNum, rep_nr, sep = ".")
  tmp$BatchNum <- BatchNum
  tmp$landRep <- rep_nr
  tmp$optima <- sims[sim_nr,]$optima
  tmp$breadth <- sims[sim_nr,]$breadth
  tmp$rmax <- sims[sim_nr,]$rmax
  tmp$dispersal <- sims[sim_nr,]$dispersal
  performance <- rbind(performance, tmp)
}

# calculate mean + sd for ensemble model
ensemble <- subset(performance, performance$Algorithm == "mean_prob")
mean(ensemble$mean_AUC)
sd(ensemble$mean_AUC)
mean(ensemble$mean_TSS)
sd(ensemble$mean_TSS)

#convert specific columns
performance$Algorithm <- factor(performance$Algorithm, levels = c("GLM", "RF", "Maxent", "mean_prob"))
performance$BatchNum <- factor(performance$BatchNum, levels = c("1", "9", "5", "13", "3", "11", "7", "15", "2", "10", "6", "14", "4", "12", "8","16"))

performance$optima <- factor(performance$optima, levels = c("marginal", "central"))
# performance$optima <- as.character(performance$optima)
# performance[which(performance$optima == "marginal"), "optima"] <- "range-contracting"
# performance[which(performance$optima == "central"), "optima"] <- "range-shifting"

# Plot performance -----------
# Plot performance in small plots ----------
p_pos <- #ggplot(performance %>% filter(landRep == 3), aes(x= optima, y = mean_AUC, color = Algorithm))+
  ggplot(performance, aes(x= optima, y = mean_AUC, fill = Algorithm))+
  geom_boxplot()+
  ylab("AUC")+
  ylim(c(0.85,1))+
  # ylab("TSS")+
  # ylim(c(0.6,1))+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 20),
        axis.title = element_text(size = 22), legend.position = "", plot.title = element_text(size = 24, face = "italic"))+
  #scale_fill_brewer(palette = "PRGn")+
  scale_fill_manual(values = c("#9460A5", "#C2A5CF", "#A6DBA0", "#388E5A"))+
  ggtitle("Niche position")

p_breadth <- #ggplot(performance %>% filter(landRep == 3), aes(x= breadth, y = mean_AUC, color = Algorithm))+
  ggplot(performance, aes(x= breadth, y = mean_AUC, fill = Algorithm))+
  geom_boxplot()+
  ylab("AUC")+
  ylim(c(0.85,1))+
  # ylab("TSS")+
  # ylim(c(0.6,1))+
  theme_bw()+
  theme(axis.title = element_blank(), axis.text = element_text(size = 20), legend.position = "", plot.title = element_text(size = 24, face = "italic"))+
  #scale_fill_brewer(palette = "PRGn")+
  scale_fill_manual(values = c("#9460A5", "#C2A5CF", "#A6DBA0", "#388E5A"))+
  ggtitle("Niche breadth")

p_rmax <-# ggplot(performance %>% filter(landRep == 3), aes(x= rmax, y = mean_AUC, color = Algorithm))+
  ggplot(performance, aes(x= rmax, y = mean_AUC, fill = Algorithm))+
  geom_boxplot()+
  ylab("AUC")+
  ylim(c(0.85,1))+
  # ylab("TSS")+
  # ylim(c(0.6,1))+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 20),
        axis.title = element_text(size = 22), legend.position = "", plot.title = element_text(size = 24, face = "italic"))+
  #scale_fill_brewer(palette = "PRGn")+
  scale_fill_manual(values = c("#9460A5", "#C2A5CF", "#A6DBA0", "#388E5A"))+
  ggtitle("Growth rate")

p_disp <- #ggplot(performance %>% filter(landRep == 3), aes(x= dispersal, y = mean_AUC, color = Algorithm))+
  ggplot(performance, aes(x= dispersal, y = mean_AUC, fill = Algorithm))+
  geom_boxplot()+
  ylab("AUC")+
  ylim(c(0.85,1))+
  # ylab("TSS")+
  # ylim(c(0.6,1))+
  theme_bw()+
  theme(axis.title = element_blank(), axis.text = element_text(size = 20), legend.position = "", plot.title = element_text(size = 24, face = "italic"))+
  #scale_fill_brewer(palette = "PRGn")+
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

# old plots ----

p1 <- ggplot(performance %>% filter(landRep == 1), aes(x = factor(BatchNum), y = mean_Spec))+
  geom_boxplot(data= performance %>% filter(landRep == 1), aes(color = Algorithm), position = position_dodge(width=1))+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 25, face = "bold"))+
  scale_color_brewer(palette = "PRGn")+
  # ylim(c(0.8,1))+
  # ylab("AUC")+
  # ylim(c(0.5,1))+
  # ylab("TSS")+
  # ylim(c(0.7,1))+
  # ylab("Sensitivity")+
  ylim(c(0.7,1))+
  ylab("Specificity")+
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
  ggtitle("Land replication 1")

p2 <- ggplot(performance %>% filter(landRep == 2), aes(x = factor(BatchNum), y = mean_Spec))+
  geom_boxplot(data= performance %>% filter(landRep == 2), aes(color = Algorithm), position = position_dodge(width=1))+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 25, face = "bold"))+
  scale_color_brewer(palette = "PRGn")+
  # ylim(c(0.8,1))+
  # ylab("AUC")+
  # ylim(c(0.5,1))+
  # ylab("TSS")+
  # ylim(c(0.7,1))+
  # ylab("Sensitivity")+
  ylim(c(0.7,1))+
  ylab("Specificity")+
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
  ggtitle("Land replication 2")

p3 <- ggplot(performance %>% filter(landRep == 3), aes(x = factor(BatchNum), y = mean_Spec))+
  geom_boxplot(data= performance %>% filter(landRep == 3), aes(color = Algorithm), position = position_dodge(width=1))+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 25, face = "bold"))+
  scale_color_brewer(palette = "PRGn")+
  # ylim(c(0.8,1))+
  # ylab("AUC")+
  # ylim(c(0.5,1))+
  # ylab("TSS")+
  # ylim(c(0.7,1))+
  # ylab("Sensitivity")+
  ylim(c(0.7,1))+
  ylab("Specificity")+
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
  ggtitle("Land replication 3")

# create and extract shared legend
legend <- ggplot(performance %>% filter(landRep == 3), aes(x = factor(BatchNum), y = mean_AUC))+
  geom_boxplot(data= performance %>% filter(landRep == 3), aes(color = Algorithm), position = position_dodge(width=1.01))+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), plot.title = element_text(size = 25, face = "bold"), legend.key.size = unit(1.5, 'cm'), 
        legend.title = element_text(size=18, face = "bold"), #change legend title font size
        legend.text = element_text(size=13))+
  ylim(c(0.8,1))+
  ylab("AUC")+
  scale_x_discrete(labels = c(
    "1" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "2" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "3" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "4" = 
      "<span style='color:black'>sd</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "5" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "6" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "7" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "8" = 
      "<span style='color:black'>sd</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "9" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "10" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "11" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "12" = 
      "<span style='color:black'>ld</span><br><span style='color:orchid3'>sg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>",
    "13" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#0D21A1'>c</span>",
    "14" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:#199F51'>nn</span><br><span style='color:#DD5560'>w</span>",
    "15" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#0D21A1'>c</span>",
    "16" = 
      "<span style='color:black'>ld</span><br><span style='color:turquoise3'>fg</span><br><span style='color:goldenrod3'>wn</span><br><span style='color:#DD5560'>w</span>"
  ))+
  scale_color_brewer(palette = "PRGn", labels = c('GLM', 'Random Forest', "MaxEnt", "Ensemble"))

shared_legend <- extract_legend(legend)

#Plot for main text
grid.arrange(p1,p2, p3, shared_legend, nrow=2, ncol = 2, heights = c(8,8), widths = c(8,8))


