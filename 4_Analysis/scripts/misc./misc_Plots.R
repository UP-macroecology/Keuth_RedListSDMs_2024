# Miscellanous plots

library(ggplot2)
library(gridExtra)
source("Functions/extract_legend.R")
# Plot dispersal assumptions in comparison

load("4_Analysis/data/IUCN_classification_times_dispersalassumptions.RData")

#check for differences between using dispersal assumptions and not using dispersal assumptions
which(IUCN_classification$CR_HS != IUCN_classification$CR_HS_disp)
which(IUCN_classification$CR_HS != IUCN_classification$CR_HS_disp)
which(IUCN_classification$CR_HS != IUCN_classification$CR_HS_disp)

IUCN_classification$optima <- as.character(IUCN_classification$optima)
IUCN_classification[which(IUCN_classification$optima == "marginal"), "optima"] <- "range-contracting"
IUCN_classification[which(IUCN_classification$optima == "central"), "optima"] <- "range-shifting"

p_pos <- ggplot(IUCN_classification, aes(x = optima, y = VU_HS))+
  geom_boxplot(width = 0.1, fill = "red", position = position_nudge(x = -0.40))+
  geom_boxplot(aes(x = optima, y = VU_HS_disp), position = position_nudge(x = -0.26), width = 0.1, fill = "blue")+
  geom_boxplot(aes(x = optima, y = EN_HS), width = 0.1, fill = "red", position = position_nudge(x = -0.07))+
  geom_boxplot(aes(x = optima, y = EN_HS_disp), position = position_nudge(x = 0.07), width = 0.1, fill = "blue")+
  geom_boxplot(aes(x = optima, y = CR_HS), width = 0.1, fill = "red", position = position_nudge(x = 0.26))+
  geom_boxplot(aes(x = optima, y = CR_HS_disp), position = position_nudge(x = 0.4), width = 0.1, fill = "blue")+
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
  ggtitle("Range dynamics")+
  xlab("")+
  ylim(c(0,60))+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 22),
        axis.title = element_text(size = 24), legend.position = "", plot.title = element_text(size = 20, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))+
  ylab("Classification timepoint [years]")


p_breadth <- ggplot(IUCN_classification, aes(x = breadth, y = VU_HS))+
  geom_boxplot(width = 0.1, fill = "red", position = position_nudge(x = -0.4))+
  geom_boxplot(aes(x = breadth, y = VU_HS_disp), position = position_nudge(x = -0.26), width = 0.1, fill = "blue")+
  geom_boxplot(aes(x = breadth, y = EN_HS), width = 0.1, fill = "red", position = position_nudge(x = -0.07))+
  geom_boxplot(aes(x = breadth, y = EN_HS_disp), position = position_nudge(x = 0.07), width = 0.1, fill = "blue")+
  geom_boxplot(aes(x = breadth, y = CR_HS), width = 0.1, fill = "red", position = position_nudge(x = 0.26))+
  geom_boxplot(aes(x = breadth, y = CR_HS_disp), position = position_nudge(x = 0.4), width = 0.1, fill = "blue")+
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
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 22),
        axis.title = element_blank(), legend.position = "", plot.title = element_text(size = 24, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))

p_rmax <- ggplot(IUCN_classification, aes(x = rmax, y = VU_HS))+
  geom_boxplot(width = 0.1, fill = "red", position = position_nudge(x = -0.4))+
  geom_boxplot(aes(x = rmax, y = VU_HS_disp), position = position_nudge(x = -0.26), width = 0.1, fill = "blue")+
  geom_boxplot(aes(x = rmax, y = EN_HS), width = 0.1, fill = "red", position = position_nudge(x = -0.07))+
  geom_boxplot(aes(x = rmax, y = EN_HS_disp), position = position_nudge(x = 0.07), width = 0.1, fill = "blue")+
  geom_boxplot(aes(x = rmax, y = CR_HS), width = 0.1, fill = "red", position = position_nudge(x = 0.26))+
  geom_boxplot(aes(x = rmax, y = CR_HS_disp), position = position_nudge(x = 0.4), width = 0.1, fill = "blue")+
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
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 22),
        axis.title = element_text(size = 24), legend.position = "", plot.title = element_text(size = 24, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))+
  ylab("Classification timepoint [years]")

p_disp <- ggplot(IUCN_classification, aes(x = dispersal, y = VU_HS))+
  geom_boxplot(width = 0.1, fill = "red", position = position_nudge(x = -0.4))+
  geom_boxplot(aes(x = dispersal, y = VU_HS_disp), position = position_nudge(x = -0.26), width = 0.1, fill = "blue")+
  geom_boxplot(aes(x = dispersal, y = EN_HS), width = 0.1, fill = "red", position = position_nudge(x = -0.07))+
  geom_boxplot(aes(x = dispersal, y = EN_HS_disp), position = position_nudge(x = 0.07), width = 0.1, fill = "blue")+
  geom_boxplot(aes(x = dispersal, y = CR_HS), width = 0.1, fill = "red", position = position_nudge(x = 0.26))+
  geom_boxplot(aes(x = dispersal, y = CR_HS_disp), position = position_nudge(x = 0.4), width = 0.1, fill = "blue")+
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
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 22),
        axis.title = element_blank(), legend.position = "", plot.title = element_text(size = 24, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))

# create and extract common legend
colors <- c("Habitat suitability (A3)" = "red", "Habitat suitability (dispersal assumption) (A3)" = "blue")

legend <- ggplot(IUCN_classification, aes(x = BatchNum, y = VU_HS, fill = "Habitat suitability (A3)"))+
  geom_boxplot()+
  geom_boxplot(aes(x = BatchNum, y = VU_HS, fill = "Habitat suitability (dispersal assumption) (A3)"), position = position_nudge(x = 0.25), width = 0.2)+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), plot.title = element_text(size = 25, face = "bold"), legend.title = element_blank(), legend.text = element_text(size = 24), legend.key.size = unit(1.5, "cm"),
        legend.position = "bottom")+
  ylab("Timepoint of classification")+
  scale_fill_manual(values= colors, breaks = c("Habitat suitability (A3)", "Habitat suitability (dispersal assumption) (A3)"))

shared_legend <- extract_legend(legend)

#Plot large grid
grid.arrange(arrangeGrob(p_pos,p_breadth, p_rmax, p_disp, nrow=2, ncol = 2, heights = c(8,8), widths = c(1,1)), shared_legend, nrow=2, ncol = 1, heights = c(10,1))






# create data frame with all parameter combinations for the IUCN classification time
IUCN_classification <- expand.grid(land_rep = land_rep, BatchNum = BatchNum, replicates = replicates)
IUCN_classification <- merge(IUCN_classification, sims_long, by = "BatchNum")

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

IUCN_warning_time <- expand.grid(land_rep = land_rep, BatchNum = BatchNum, replicates = replicates)
IUCN_warning_time <- merge(IUCN_warning_time, sims_long, by = "BatchNum")

IUCN_warning_time$VU_Pop <- NA
IUCN_warning_time$EN_Pop <- NA
IUCN_warning_time$CR_Pop <- NA
IUCN_warning_time$VU_HS <- NA
IUCN_warning_time$EN_HS <- NA
IUCN_warning_time$CR_HS <- NA
IUCN_warning_time$VU_Ext <- NA
IUCN_warning_time$EN_Ext <- NA
IUCN_warning_time$CR_Ext <- NA
IUCN_warning_time$Extinction <- NA

# extract time point when the thresholds of the different criteria is surpassed for the different metrics
for (i in 1:nrow(IUCN_warning_time)) {
  # extract values
  land_nr <- IUCN_warning_time[i,"land_rep"]
  BatchNum <- IUCN_warning_time[i,"BatchNum"]
  rep_nr <- IUCN_warning_time[i,"replicates"]
  # Obtain the year when extinction probability exceeds stated thresholds
  # Vulnerable E (>=10%)
  IUCN_warning_time[i ,"VU_Ext"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & data[[BatchNum]]$extProb>=0.1, "Year"],1) - 100
  # Endangered E (>=20%)
  IUCN_warning_time[i ,"EN_Ext"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & data[[BatchNum]]$extProb>=0.2, "Year"],1) - 100
  # Critically endangered E (>=50%)
  IUCN_warning_time[i ,"CR_Ext"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & data[[BatchNum]]$extProb>=0.5, "Year"],1) - 100
  # Obtain the year when population size exceeds stated thresholds
  # Vulnerable A (>=30%)
  IUCN_warning_time[i ,"VU_Pop"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & (1-data[[BatchNum]]$pop_sum)>=0.3, "Year"],1) - 100
  # Endangered A (>=50%)
  IUCN_warning_time[i ,"EN_Pop"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & (1-data[[BatchNum]]$pop_sum)>=0.5, "Year"],1) - 100
  # Critically endangered A (>=80%)
  IUCN_warning_time[i ,"CR_Pop"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & (1-data[[BatchNum]]$pop_sum)>=0.8, "Year"],1) - 100
  # Obtain the year when habitat loss exceeds stated thresholds
  # Vulnerable A3 (>=30%)
  IUCN_warning_time[i ,"VU_HS"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & (1-data[[BatchNum]]$hs_change)>=0.3, "Year"],1) - 100
  # Endangered A3 (>=50%)
  IUCN_warning_time[i ,"EN_HS"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & (1-data[[BatchNum]]$hs_change)>=0.5, "Year"],1) - 100
  # Critically endangered A3 (>=80%)
  IUCN_warning_time[i ,"CR_HS"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & (1-data[[BatchNum]]$hs_change)>=0.8, "Year"],1) - 100
  # Obtain the year when extinction of population happens
  IUCN_warning_time[i ,"Extinction"] <- head(data[[BatchNum]][data[[BatchNum]]$Rep == rep_nr & data[[BatchNum]]$land == land_nr & data[[BatchNum]]$pop_sum==0, "Year"],1) - 100
}

#obtain warning time for every step
for (i in 1:nrow(IUCN_warning_time)){
  for (k in 8:16){
    IUCN_warning_time[i,k] <- IUCN_warning_time[i,17] - IUCN_warning_time[i,k]
  }
}


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




# Plot the results

library(ggplot2)
library(gridExtra)

load("4_Analysis/data/IUCN_classification_times_mean.RData")

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
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 18),
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
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 18),
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
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 18),
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
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_blank(), legend.position = "", plot.title = element_text(size = 20, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))

# create and extract common legend
colors <- c("Habitat suitability (A3)" = "red", "Population size (A3)" = "orange", "Extinction probability (E)" = "blue")

legend <- ggplot(IUCN_classification, aes(x = BatchNum, y = VU_Pop, color = "Population size (A3)"))+
  geom_boxplot()+
  geom_boxplot(aes(x = BatchNum, y = VU_HS, color = "Habitat suitability (A3)"), position = position_nudge(x = -0.25), width = 0.2)+
  geom_boxplot(aes(x = BatchNum, y = VU_Ext, color = "Extinction probability (E)"), position = position_nudge(x = 0.25), width = 0.2)+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), plot.title = element_text(size = 25, face = "bold"), legend.title = element_blank(), legend.text = element_text(size = 15), legend.key.size = unit(1, "cm"),
        legend.position = "bottom")+
  ylab("Timepoint of classification")+
  scale_color_manual(values= colors, breaks = c("Habitat suitability (A3)", "Population size (A3)", "Extinction probability (E)"))

shared_legend <- extract_legend(legend)

#Plot large grid
grid.arrange(arrangeGrob(p_pos,p_breadth, p_rmax, p_disp, nrow=2, ncol = 2, heights = c(8,8), widths = c(1,1)), shared_legend, nrow=2, ncol = 1, heights = c(10,1))


# habitat loss compared to range loss ----------
p1 <- ggplot(data[[1]], aes(x=hs_loss, y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "black", linetype = "dashed", linewidth = 1.2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())+
  scale_color_manual(values = c("#C7E9C0", "#31A354", "#006629"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) 

p2 <-ggplot(data[[2]], aes(x=hs_loss, y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "black", linetype = "dashed", linewidth = 1.2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  scale_color_manual(values = c("#C7E9C0", "#31A354", "#006629"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) 

p3 <- ggplot(data[[3]], aes(x=hs_loss, y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "black", linetype = "dashed", linewidth = 1.2)+
  scale_color_manual(values = c("#C7E9C0", "#31A354", "#006629"))+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) 

p4 <- ggplot(data[[4]], aes(x=hs_loss, y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "black", linetype = "dashed", linewidth = 1.2)+
  scale_color_manual(values = c("#C7E9C0", "#31A354", "#006629"))+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) 

p5 <- ggplot(data[[5]], aes(x=hs_loss, y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "black", linetype = "dashed", linewidth = 1.2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())+
  scale_color_manual(values = c("#C7E9C0", "#31A354", "#006629"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) 

p6 <- ggplot(data[[6]], aes(x=hs_loss, y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "black", linetype = "dashed", linewidth = 1.2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  scale_color_manual(values = c("#C7E9C0", "#31A354", "#006629"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) 

p7 <- ggplot(data[[7]], aes(x=hs_loss, y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "black", linetype = "dashed", linewidth = 1.2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  scale_color_manual(values = c("#C7E9C0", "#31A354", "#006629"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) 

p8 <- ggplot(data[[8]], aes(x=hs_loss, y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "black", linetype = "dashed", linewidth = 1.2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  scale_color_manual(values = c("#C7E9C0", "#31A354", "#006629"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) 

p9 <- ggplot(data[[9]], aes(x=hs_loss, y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "black", linetype = "dashed", linewidth = 1.2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())+
  scale_color_manual(values = c("#C7E9C0", "#31A354", "#006629"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) 

p10 <- ggplot(data[[10]], aes(x=hs_loss, y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "black", linetype = "dashed", linewidth = 1.2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  scale_color_manual(values = c("#C7E9C0", "#31A354", "#006629"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) 

p11 <- ggplot(data[[11]], aes(x=hs_loss, y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "black", linetype = "dashed", linewidth = 1.2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  scale_color_manual(values = c("#C7E9C0", "#31A354", "#006629"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) 

p12 <-ggplot(data[[12]], aes(x=hs_loss, y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "black", linetype = "dashed", linewidth = 1.2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  scale_color_manual(values = c("#C7E9C0", "#31A354", "#006629"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) 

p13 <- ggplot(data[[13]], aes(x=hs_loss, y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "black", linetype = "dashed", linewidth = 1.2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "")+
  scale_color_manual(values = c("#C7E9C0", "#31A354", "#006629"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) 

p14 <-ggplot(data[[14]], aes(x=hs_loss, y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "black", linetype = "dashed", linewidth = 1.2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())+
  scale_color_manual(values = c("#C7E9C0", "#31A354", "#006629"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) 

p15 <- ggplot(data[[15]], aes(x=hs_loss, y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "black", linetype = "dashed", linewidth = 1.2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())+
  scale_color_manual(values = c("#C7E9C0", "#31A354", "#006629"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) 

p16 <- ggplot(data[[16]], aes(x=hs_loss, y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "black", linetype = "dashed", linewidth = 1.2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())+
  scale_color_manual(values = c("#C7E9C0", "#31A354", "#006629"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) 

#create and extract shared legend
legend <- ggplot(data[[16]], aes(x=hs_loss, y=pop_sum, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "bottom", legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), 
        legend.title = element_text(size = 15))+
  scale_color_manual(values = c("#C7E9C0", "#31A354", "#006629"))+
  guides(colour = guide_legend(title = "Landscape"))

shared_legend <- extract_legend(legend)

#Plot large grid
grid.arrange(arrangeGrob(t0, t_cn, t_cw, t_wna, t_ww, t_ss, p1,p3,p2,p4, t_sl, p9,p11,p10,p12, t_fs, p5,p7,p6,p8, t_fl, p13,p15,p14,p16, nrow = 5, ncol = 5, heights= c(1,3.8,4,4,4.2), widths = c(1.5,5,5,5,5)),
             t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.2, 0.8), widths = c(11.7,0.3))

# try the correct criteria plot

# create the VU, EN, CR columns classification time for all metrices (Pop, Range, HS, Ext.Prob)
IUCN_classification$VU_Pop_cri <- NA
IUCN_classification$EN_Pop_cri <- NA
IUCN_classification$CR_Pop_cri <- NA
IUCN_classification$VU_HS_cri <- NA
IUCN_classification$EN_HS_cri <- NA
IUCN_classification$CR_HS_cri <- NA
IUCN_classification$VU_Ext_cri <- NA
IUCN_classification$EN_Ext_cri <- NA
IUCN_classification$CR_Ext_cri <- NA

# extract time point when the thresholds of the different criteria is surpassed for the different metrics
for (i in 1:nrow(IUCN_classification)) {
  for (k in 8:13) {
    ifelse(IUCN_classification[i, k] < 10, IUCN_classification[i, k+12] <- "Y", IUCN_classification[i, k+12] <- "N")
  }
  #criterion E
  ifelse(IUCN_classification[i, "VU_Ext"] < 100, IUCN_classification[i, "VU_Ext_cri"] <- "Y", IUCN_classification[i, "VU_Ext_cri"] <- "N")
  ifelse(IUCN_classification[i, "EN_Ext"] < 20, IUCN_classification[i, "EN_Ext_cri"] <- "Y", IUCN_classification[i, "EN_Ext_cri"] <- "N")
  ifelse(IUCN_classification[i, "CR_Ext"] < 10, IUCN_classification[i, "CR_Ext_cri"] <- "Y", IUCN_classification[i, "CR_Ext_cri"] <- "N")
  
}


p_pos <- ggplot(IUCN_classification %>% group_by(optima) %>% count(VU_Pop_cri) %>% mutate(per = n/240) %>% filter(VU_Pop_cri == "Y"), aes(x = optima, y = per))+
  geom_bar(stat = "identity",  width = 0.06, fill = "orange", position = position_nudge(x = - 0.33))+
  geom_bar(data = IUCN_classification %>% group_by(optima) %>% count(VU_HS_cri) %>% mutate(per = n/240) %>% filter(VU_HS_cri == "Y"), aes(x = optima, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = -0.42), fill = "red")+
  geom_bar(data = IUCN_classification %>% group_by(optima) %>% count(VU_Ext_cri) %>% mutate(per = n/240) %>% filter(VU_Ext_cri == "Y"), aes(x = optima, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = -0.24), fill = "blue")+
  geom_bar(data = IUCN_classification %>% group_by(optima) %>% count(EN_Pop_cri) %>% mutate(per = n/240) %>% filter(EN_Pop_cri == "Y"), stat = "identity",  
           width = 0.06, fill = "orange", position = position_nudge(x = 0))+
  geom_bar(data = IUCN_classification %>% group_by(optima) %>% count(EN_HS_cri) %>% mutate(per = n/240) %>% filter(EN_HS_cri == "Y"), aes(x = optima, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = -0.09), fill = "red")+
  geom_bar(data = IUCN_classification %>% group_by(optima) %>% count(EN_Ext_cri) %>% mutate(per = n/240) %>% filter(EN_Ext_cri == "Y"), aes(x = optima, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = 0.09), fill = "blue")+
  geom_bar(data = IUCN_classification %>% group_by(optima) %>% count(CR_Pop_cri) %>% mutate(per = n/240) %>% filter(CR_Pop_cri == "Y"), stat = "identity",  
           width = 0.06, fill = "orange", position = position_nudge(x = 0.33))+
  geom_bar(data = IUCN_classification %>% group_by(optima) %>% count(CR_HS_cri) %>% mutate(per = n/240) %>% filter(CR_HS_cri == "Y"), aes(x = optima, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = 0.24), fill = "red")+
  geom_bar(data = IUCN_classification %>% group_by(optima) %>% count(CR_Ext_cri) %>% mutate(per = n/240) %>% filter(CR_Ext_cri == "Y"), aes(x = optima, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = 0.43), fill = "blue")+
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 1.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 0.83, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 2.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 1.83, linetype = "dashed", color = "lightgrey")+
  annotate(geom="text", x=0.655, y=1.08, label="VU", color="black", size = 6)+
  annotate(geom="text", x=0.995, y=1.08, label="EN", color="black", size = 6)+
  annotate(geom="text", x=1.325, y=1.08, label="CR", color="black", size = 6)+
  annotate(geom="text", x=1.685, y=1.08, label="VU", color="black", size = 6)+
  annotate(geom="text", x=1.995, y=1.08, label="EN", color="black", size = 6)+
  annotate(geom="text", x=2.335, y=1.08, label="CR", color="black", size = 6)+
  scale_x_discrete(expand = c(0.25, 0.25)) +
  ggtitle("Niche position")+
  xlab("")+
  ylab("")+
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.15))+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 18), legend.position = "", plot.title = element_text(size = 20, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))

p_breadth <- ggplot(IUCN_classification %>% group_by(breadth) %>% count(VU_Pop_cri) %>% mutate(per = n/240) %>% filter(VU_Pop_cri == "Y"), aes(x = breadth, y = per))+
  geom_bar(stat = "identity",  width = 0.06, fill = "orange", position = position_nudge(x = - 0.33))+
  geom_bar(data = IUCN_classification %>% group_by(breadth) %>% count(VU_HS_cri) %>% mutate(per = n/240) %>% filter(VU_HS_cri == "Y"), aes(x = breadth, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = -0.42), fill = "red")+
  geom_bar(data = IUCN_classification %>% group_by(breadth) %>% count(VU_Ext_cri) %>% mutate(per = n/240) %>% filter(VU_Ext_cri == "Y"), aes(x = breadth, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = -0.24), fill = "blue")+
  geom_bar(data = IUCN_classification %>% group_by(breadth) %>% count(EN_Pop_cri) %>% mutate(per = n/240) %>% filter(EN_Pop_cri == "Y"), stat = "identity",  
           width = 0.06, fill = "orange", position = position_nudge(x = 0))+
  geom_bar(data = IUCN_classification %>% group_by(breadth) %>% count(EN_HS_cri) %>% mutate(per = n/240) %>% filter(EN_HS_cri == "Y"), aes(x = breadth, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = -0.09), fill = "red")+
  geom_bar(data = IUCN_classification %>% group_by(breadth) %>% count(EN_Ext_cri) %>% mutate(per = n/240) %>% filter(EN_Ext_cri == "Y"), aes(x = breadth, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = 0.09), fill = "blue")+
  geom_bar(data = IUCN_classification %>% group_by(breadth) %>% count(CR_Pop_cri) %>% mutate(per = n/240) %>% filter(CR_Pop_cri == "Y"), stat = "identity",  
           width = 0.06, fill = "orange", position = position_nudge(x = 0.33))+
  geom_bar(data = IUCN_classification %>% group_by(breadth) %>% count(CR_HS_cri) %>% mutate(per = n/240) %>% filter(CR_HS_cri == "Y"), aes(x = breadth, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = 0.24), fill = "red")+
  geom_bar(data = IUCN_classification %>% group_by(breadth) %>% count(CR_Ext_cri) %>% mutate(per = n/240) %>% filter(CR_Ext_cri == "Y"), aes(x = breadth, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = 0.43), fill = "blue")+
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 1.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 0.83, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 2.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 1.83, linetype = "dashed", color = "lightgrey")+
  annotate(geom="text", x=0.655, y=1.08, label="VU", color="black", size = 6)+
  annotate(geom="text", x=0.995, y=1.08, label="EN", color="black", size = 6)+
  annotate(geom="text", x=1.325, y=1.08, label="CR", color="black", size = 6)+
  annotate(geom="text", x=1.685, y=1.08, label="VU", color="black", size = 6)+
  annotate(geom="text", x=1.995, y=1.08, label="EN", color="black", size = 6)+
  annotate(geom="text", x=2.335, y=1.08, label="CR", color="black", size = 6)+
  scale_x_discrete(expand = c(0.25, 0.25)) +
  ggtitle("Niche breadth")+
  xlab("")+
  ylab("")+
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.15))+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 18), legend.position = "", plot.title = element_text(size = 20, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))

p_rmax <- ggplot(IUCN_classification %>% group_by(rmax) %>% count(VU_Pop_cri) %>% mutate(per = n/240) %>% filter(VU_Pop_cri == "Y"), aes(x = rmax, y = per))+
  geom_bar(stat = "identity",  width = 0.06, fill = "orange", position = position_nudge(x = - 0.33))+
  geom_bar(data = IUCN_classification %>% group_by(rmax) %>% count(VU_HS_cri) %>% mutate(per = n/240) %>% filter(VU_HS_cri == "Y"), aes(x = rmax, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = -0.42), fill = "red")+
  geom_bar(data = IUCN_classification %>% group_by(rmax) %>% count(VU_Ext_cri) %>% mutate(per = n/240) %>% filter(VU_Ext_cri == "Y"), aes(x = rmax, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = -0.24), fill = "blue")+
  geom_bar(data = IUCN_classification %>% group_by(rmax) %>% count(EN_Pop_cri) %>% mutate(per = n/240) %>% filter(EN_Pop_cri == "Y"), stat = "identity",  
           width = 0.06, fill = "orange", position = position_nudge(x = 0))+
  geom_bar(data = IUCN_classification %>% group_by(rmax) %>% count(EN_HS_cri) %>% mutate(per = n/240) %>% filter(EN_HS_cri == "Y"), aes(x = rmax, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = -0.09), fill = "red")+
  geom_bar(data = IUCN_classification %>% group_by(rmax) %>% count(EN_Ext_cri) %>% mutate(per = n/240) %>% filter(EN_Ext_cri == "Y"), aes(x = rmax, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = 0.09), fill = "blue")+
  geom_bar(data = IUCN_classification %>% group_by(rmax) %>% count(CR_Pop_cri) %>% mutate(per = n/240) %>% filter(CR_Pop_cri == "Y"), stat = "identity",  
           width = 0.06, fill = "orange", position = position_nudge(x = 0.33))+
  geom_bar(data = IUCN_classification %>% group_by(rmax) %>% count(CR_HS_cri) %>% mutate(per = n/240) %>% filter(CR_HS_cri == "Y"), aes(x = rmax, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = 0.24), fill = "red")+
  geom_bar(data = IUCN_classification %>% group_by(rmax) %>% count(CR_Ext_cri) %>% mutate(per = n/240) %>% filter(CR_Ext_cri == "Y"), aes(x = rmax, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = 0.43), fill = "blue")+
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 1.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 0.83, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 2.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 1.83, linetype = "dashed", color = "lightgrey")+
  annotate(geom="text", x=0.655, y=1.08, label="VU", color="black", size = 6)+
  annotate(geom="text", x=0.995, y=1.08, label="EN", color="black", size = 6)+
  annotate(geom="text", x=1.325, y=1.08, label="CR", color="black", size = 6)+
  annotate(geom="text", x=1.685, y=1.08, label="VU", color="black", size = 6)+
  annotate(geom="text", x=1.995, y=1.08, label="EN", color="black", size = 6)+
  annotate(geom="text", x=2.335, y=1.08, label="CR", color="black", size = 6)+
  scale_x_discrete(expand = c(0.25, 0.25)) +
  ggtitle("Growth rate")+
  xlab("")+
  ylab("")+
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.15))+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 18), legend.position = "", plot.title = element_text(size = 20, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))

p_disp <- ggplot(IUCN_classification %>% group_by(dispersal) %>% count(VU_Pop_cri) %>% mutate(per = n/240) %>% filter(VU_Pop_cri == "Y"), aes(x = dispersal, y = per))+
  geom_bar(stat = "identity",  width = 0.06, fill = "orange", position = position_nudge(x = - 0.33))+
  geom_bar(data = IUCN_classification %>% group_by(dispersal) %>% count(VU_HS_cri) %>% mutate(per = n/240) %>% filter(VU_HS_cri == "Y"), aes(x = dispersal, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = -0.42), fill = "red")+
  geom_bar(data = IUCN_classification %>% group_by(dispersal) %>% count(VU_Ext_cri) %>% mutate(per = n/240) %>% filter(VU_Ext_cri == "Y"), aes(x = dispersal, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = -0.24), fill = "blue")+
  geom_bar(data = IUCN_classification %>% group_by(dispersal) %>% count(EN_Pop_cri) %>% mutate(per = n/240) %>% filter(EN_Pop_cri == "Y"), stat = "identity",  
           width = 0.06, fill = "orange", position = position_nudge(x = 0))+
  geom_bar(data = IUCN_classification %>% group_by(dispersal) %>% count(EN_HS_cri) %>% mutate(per = n/240) %>% filter(EN_HS_cri == "Y"), aes(x = dispersal, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = -0.09), fill = "red")+
  geom_bar(data = IUCN_classification %>% group_by(dispersal) %>% count(EN_Ext_cri) %>% mutate(per = n/240) %>% filter(EN_Ext_cri == "Y"), aes(x = dispersal, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = 0.09), fill = "blue")+
  geom_bar(data = IUCN_classification %>% group_by(dispersal) %>% count(CR_Pop_cri) %>% mutate(per = n/240) %>% filter(CR_Pop_cri == "Y"), stat = "identity",  
           width = 0.06, fill = "orange", position = position_nudge(x = 0.33))+
  geom_bar(data = IUCN_classification %>% group_by(dispersal) %>% count(CR_HS_cri) %>% mutate(per = n/240) %>% filter(CR_HS_cri == "Y"), aes(x = dispersal, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = 0.24), fill = "red")+
  geom_bar(data = IUCN_classification %>% group_by(dispersal) %>% count(CR_Ext_cri) %>% mutate(per = n/240) %>% filter(CR_Ext_cri == "Y"), aes(x = dispersal, y = per), 
           stat = "identity",  width = 0.06, position = position_nudge(x = 0.43), fill = "blue")+
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 1.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 0.83, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 2.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 1.83, linetype = "dashed", color = "lightgrey")+
  annotate(geom="text", x=0.655, y=1.08, label="VU", color="black", size = 6)+
  annotate(geom="text", x=0.995, y=1.08, label="EN", color="black", size = 6)+
  annotate(geom="text", x=1.325, y=1.08, label="CR", color="black", size = 6)+
  annotate(geom="text", x=1.685, y=1.08, label="VU", color="black", size = 6)+
  annotate(geom="text", x=1.995, y=1.08, label="EN", color="black", size = 6)+
  annotate(geom="text", x=2.335, y=1.08, label="CR", color="black", size = 6)+
  scale_x_discrete(expand = c(0.25, 0.25)) +
  ggtitle("Dispersal")+
  xlab("")+
  ylab("")+
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.15))+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 18), legend.position = "", plot.title = element_text(size = 20, face = "italic"), 
        panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))

# create and extract common legend
colors <- c("Habitat suitability (A3)" = "red", "Population size (A3)" = "orange", "Extinction probability (E)" = "blue")

legend <- ggplot(IUCN_classification %>% group_by(dispersal) %>% count(VU_Pop_cri) %>% mutate(per = n/240) %>% filter(VU_Pop_cri == "Y"), 
                 aes(x = dispersal, y = per, fill = "Population size (A3)"))+
  geom_bar(stat = "identity",  width = 0.06, position = position_nudge(x = - 0.33))+
  geom_bar(data = IUCN_classification %>% group_by(dispersal) %>% count(VU_HS_cri) %>% mutate(per = n/240) %>% filter(VU_HS_cri == "Y"), 
           aes(x = dispersal, y = per, fill = "Habitat suitability (A3)"), stat = "identity",  width = 0.06, position = position_nudge(x = -0.42))+
  geom_bar(data = IUCN_classification %>% group_by(dispersal) %>% count(VU_Ext_cri) %>% mutate(per = n/240) %>% filter(VU_Ext_cri == "Y"), 
           aes(x = dispersal, y = per,  fill = "Extinction probability (E)"), 
           stat = "identity",  width = 0.06, position = position_nudge(x = -0.24))+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), plot.title = element_text(size = 25, face = "bold"), legend.title = element_blank(), legend.text = element_text(size = 15), legend.key.size = unit(1, "cm"),
        legend.position = "bottom")+
  scale_fill_manual(values= colors, breaks = c("Habitat suitability (A3)", "Population size (A3)", "Extinction probability (E)"))

shared_legend <- extract_legend(legend)

#Plot large grid
grid.arrange(arrangeGrob(p_pos,p_breadth, p_rmax, p_disp, nrow=2, ncol = 2, heights = c(8,8), widths = c(1,1)), shared_legend, nrow=2, ncol = 1, heights = c(10,1))


#IUCN warning time

p_pos <- ggplot(IUCN_warning_time, aes(x = optima, y = VU_HS))+
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
  ylab("Warning time [years]")

p_breadth <- ggplot(IUCN_warning_time, aes(x = breadth, y = VU_HS))+
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

p_rmax <- ggplot(IUCN_warning_time, aes(x = rmax, y = VU_HS))+
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
  ylab("Warning time [years]")

p_disp <- ggplot(IUCN_warning_time, aes(x = dispersal, y = VU_HS))+
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

legend <- ggplot(IUCN_warning_time, aes(x = BatchNum, y = VU_Pop, color = "Population size (A3)"))+
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

# test including time horizon


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
  annotate("segment", x = 0.55, xend = 0.7, y = 10, yend = 10, colour = "purple3", linetype = "dotdash")+
  annotate("segment", x = 0.88, xend = 1.03, y = 10, yend = 10, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 1.2, xend = 1.45, y = 10, yend = 10, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 1.05, xend = 1.15, y = 20, yend = 20, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 1.55, xend = 1.7, y = 10, yend = 10, colour = "purple3", linetype = "dotdash")+
  annotate("segment", x = 1.88, xend = 2.03, y = 10, yend = 10, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 2.2, xend = 2.45, y = 10, yend = 10, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 2.05, xend = 2.15, y = 20, yend = 20, colour = "purple3", linetype="dotdash")+
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
  annotate("segment", x = 0.55, xend = 0.7, y = 10, yend = 10, colour = "purple3", linetype = "dotdash")+
  annotate("segment", x = 0.88, xend = 1.03, y = 10, yend = 10, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 1.2, xend = 1.45, y = 10, yend = 10, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 1.05, xend = 1.15, y = 20, yend = 20, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 1.55, xend = 1.7, y = 10, yend = 10, colour = "purple3", linetype = "dotdash")+
  annotate("segment", x = 1.88, xend = 2.03, y = 10, yend = 10, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 2.2, xend = 2.45, y = 10, yend = 10, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 2.05, xend = 2.15, y = 20, yend = 20, colour = "purple3", linetype="dotdash")+
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
  annotate("segment", x = 0.55, xend = 0.7, y = 10, yend = 10, colour = "purple3", linetype = "dotdash")+
  annotate("segment", x = 0.88, xend = 1.03, y = 10, yend = 10, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 1.2, xend = 1.45, y = 10, yend = 10, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 1.05, xend = 1.15, y = 20, yend = 20, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 1.55, xend = 1.7, y = 10, yend = 10, colour = "purple3", linetype = "dotdash")+
  annotate("segment", x = 1.88, xend = 2.03, y = 10, yend = 10, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 2.2, xend = 2.45, y = 10, yend = 10, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 2.05, xend = 2.15, y = 20, yend = 20, colour = "purple3", linetype="dotdash")+
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
  annotate("segment", x = 0.55, xend = 0.7, y = 10, yend = 10, colour = "purple3", linetype = "dotdash")+
  annotate("segment", x = 0.88, xend = 1.03, y = 10, yend = 10, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 1.2, xend = 1.45, y = 10, yend = 10, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 1.05, xend = 1.15, y = 20, yend = 20, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 1.55, xend = 1.7, y = 10, yend = 10, colour = "purple3", linetype = "dotdash")+
  annotate("segment", x = 1.88, xend = 2.03, y = 10, yend = 10, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 2.2, xend = 2.45, y = 10, yend = 10, colour = "purple3", linetype="dotdash")+
  annotate("segment", x = 2.05, xend = 2.15, y = 20, yend = 20, colour = "purple3", linetype="dotdash")+
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

