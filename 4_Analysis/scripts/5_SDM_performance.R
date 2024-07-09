# The different performance measures of the SDMs are plotted for all three algorithms as well as the ensemble model

# Load packages
library(data.table)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggtext)

# Load in function
source("4_Analysis/functions/extract_legend.R")

# Prepare simulation data
# create data frame with all parameter combinations
land_rep <- 1:3
optima <- c(0.27, 0.5)
breadth <- c(0.045, 0.055)
rmax <- c(3, 5)
dispersal <- c(5000, 15000)

sims <- expand.grid(land_rep = land_rep, optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims$BatchNum <- rep(1:16, each = 3)

# Load data sets
performance <- c()

for (sim_nr in 1:nrow(sims)) {
  rep_nr <- sims[sim_nr,]$land_rep
  BatchNum <- sims[sim_nr,]$BatchNum
  tmp <- readRDS(paste0("4_Analysis/data/performance_mean_SDM_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  tmp <- do.call(rbind, tmp)
  tmp$scenario <- paste(BatchNum, rep_nr, sep = ".")
  tmp$BatchNum <- BatchNum
  tmp$landRep <- rep_nr
  performance <- rbind(performance, tmp)
}

performance$Algorithm <- factor(performance$Algorithm, levels = c("GLM", "RF", "Maxent", "mean_prob"))

# Plot performance

p1 <- ggplot(performance %>% filter(landRep == 1), aes(x = factor(BatchNum), y = mean_Spec))+
  geom_boxplot(data= performance %>% filter(landRep == 1), aes(color = Algorithm), position = position_dodge(width=1))+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 25, face = "bold"))+
  scale_color_brewer(palette = "PRGn")+
  #ylim(c(0.8,1))+
  #ylab("AUC")+
  # ylim(c(0.5,1))+
  # ylab("TSS")+
  # ylim(c(0.7,1))+
  # ylab("Sensitivity")+
  ylim(c(0.7,1))+
  ylab("Specificity")+
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
  ggtitle("Land replication 1")

p2 <- ggplot(performance %>% filter(landRep == 2), aes(x = factor(BatchNum), y = mean_Spec))+
  geom_boxplot(data= performance %>% filter(landRep == 2), aes(color = Algorithm), position = position_dodge(width=1))+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 25, face = "bold"))+
  scale_color_brewer(palette = "PRGn")+
  #ylim(c(0.8,1))+
  #ylab("AUC")+
  # ylim(c(0.5,1))+
  # ylab("TSS")+
  # ylim(c(0.7,1))+
  # ylab("Sensitivity")+
  ylim(c(0.7,1))+
  ylab("Specificity")+
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
  ggtitle("Land replication 2")

p3 <- ggplot(performance %>% filter(landRep == 3), aes(x = factor(BatchNum), y = mean_Spec))+
  geom_boxplot(data= performance %>% filter(landRep == 3), aes(color = Algorithm), position = position_dodge(width=1))+
  theme_bw()+
  theme(axis.text.x = element_markdown(), axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), legend.position = "", plot.title = element_text(size = 25, face = "bold"))+
  scale_color_brewer(palette = "PRGn")+
  #ylim(c(0.8,1))+
  #ylab("AUC")+
  # ylim(c(0.5,1))+
  # ylab("TSS")+
  # ylim(c(0.7,1))+
  # ylab("Sensitivity")+
  ylim(c(0.7,1))+
  ylab("Specificity")+
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
  ggtitle("Land replication 3")

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

