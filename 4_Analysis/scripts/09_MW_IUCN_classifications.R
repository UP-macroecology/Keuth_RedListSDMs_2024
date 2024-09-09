# Use Moving window approach to determine classification time point

# Load packages
library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)

source("Functions/extract_legend.R")

# write moving window function
MW <- function(data, new_data, timehorizon, threshold, category, metric){
  #column_new_data <- paste(category, metric, sep = "_")
  #print(column_new_data)
  
  # select the right column depending on the selected metric
  if(metric == "Pop"){
    column_data <- "pop_mean"
  } else if(metric == "HS") {
    column_data <- "hs_mean"
  } else {
    column_data <- "extProb2"
  }
  
  # reduce timehorizon if it exceeds the maximum number of years in the data set
  if(timehorizon > max(data$Year) - 100){
    timehorizon <- max(data$Year) - 100
    #print(timehorizon)
  }
  
  # for every year calculated the relative size to the size x years into the future
  for (i in 1:nrow(data)) {
    rel_loss <- 1 - (data[data$Year == unique(data$Year)[i+timehorizon], column_data]/data[data$Year == unique(data$Year)[i], column_data])
    #print(rel_loss)
    
    # controls if the relative loss exceeds the threshold
    if(rel_loss >= threshold){
      #print(unique(data$Year)[i])
      new_data[new_data$BatchNum == BatchNum & new_data$land_rep == land_rep, paste(category, metric, sep = "_")] <- (unique(data$Year)[i])-100
      return(new_data)
      break
    }
  }
}

# create new data frame, which contains the years of reaching the criteria/ species being classified
land_rep <- 1:3
BatchNum <- 1:16
IUCN_classification <- expand.grid(land_rep = land_rep, BatchNum = BatchNum)

# create the VU, EN, CR columns classification time for all metrices (Pop, Range, HS, Ext.Prob)
IUCN_classification$VU_Pop <- NA
IUCN_classification$EN_Pop <- NA
IUCN_classification$CR_Pop <- NA
IUCN_classification$VU_HS <- NA
IUCN_classification$EN_HS <- NA
IUCN_classification$CR_HS <- NA
IUCN_classification$VU_Ext <- NA
IUCN_classification$EN_Ext <- NA
IUCN_classification$CR_Ext <- NA

# performs the same code for every scenario
for (i in 1:nrow(IUCN_classification)) {
  BatchNum <- IUCN_classification[i, "BatchNum"]
  land_rep <- IUCN_classification[i, "land_rep"]
  
  # Load data
  #dat <- readRDS("4_Analysis/data/data_analysis_raw_Batch10_Sim1.rds")
  dat <- readRDS(paste0("4_Analysis/data/data_analysis_raw_Batch", BatchNum, "_Sim", land_rep, ".rds"))

  # remove rows till year 100
  dat <- dat[!dat$Year %in% c(0:99),]

  # calculate mean pop and mean hs
  dat$pop_mean <- rowMeans(dat[,3:12])
  dat$hs_mean <- rowMeans(dat[,13:22])
  dat$extProb2 <- 1- dat$extProb

  # Criterion A3 - Pop size
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.3, "VU", "Pop")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.5, "EN", "Pop")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.8, "CR", "Pop")

  # Criterion A3 - HS
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.3, "VU", "HS")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.5, "EN", "HS")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.8, "CR", "HS")

  # Criterion E - Extinction probability
  IUCN_classification <- MW(dat, IUCN_classification, 100, 0.1, "VU", "Ext")
  IUCN_classification <- MW(dat, IUCN_classification, 20, 0.2, "EN", "Ext")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.5, "CR", "Ext")

}

save(IUCN_classification, file = "4_Analysis/data/IUCN_classification_times_mean.RData")

# performs the same code for every scenario
for (i in 1:nrow(IUCN_classification)) {
  BatchNum <- IUCN_classification[i, "BatchNum"]
  land_rep <- IUCN_classification[i, "land_rep"]
  
  # Load data
  #dat <- readRDS("4_Analysis/data/data_analysis_raw_Batch10_Sim1.rds")
  dat <- readRDS(paste0("4_Analysis/data/data_analysis_raw_Batch", BatchNum, "_Sim", land_rep, ".rds"))
  
  # remove rows till year 100
  dat <- dat[!dat$Year %in% c(0:99),]
  
  #replace NAs with 0
  dat[is.na(dat)] <- 0
  
  # calculate mean pop and mean hs
  dat <- dat %>% rowwise() %>% mutate(pop_mean = median(c(pop_sum87:pop_sum70), na.rm = T))
  dat <- dat %>% rowwise() %>% mutate(hs_mean = median(c(hs_change87:hs_change70), na.rm = T))
  dat$extProb2 <- 1- dat$extProb
  
  # Criterion A3 - Pop size
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.3, "VU", "Pop")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.5, "EN", "Pop")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.8, "CR", "Pop")
  
  # Criterion A3 - HS
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.3, "VU", "HS")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.5, "EN", "HS")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.8, "CR", "HS")
  
  # Criterion E - Extinction probability
  IUCN_classification <- MW(dat, IUCN_classification, 100, 0.1, "VU", "Ext")
  IUCN_classification <- MW(dat, IUCN_classification, 20, 0.2, "EN", "Ext")
  IUCN_classification <- MW(dat, IUCN_classification, 10, 0.5, "CR", "Ext")
  
}

save(IUCN_classification, file = "4_Analysis/data/IUCN_classification_times_median.RData")

# Plot the results

library(ggplot2)
library(gridExtra)

load("4_Analysis/data/IUCN_classification_times_mean.RData")

# add the trait values to the data frame
optima <- c("marginal", "central")
breadth <- c("narrow", "wide")
rmax <- c("slow", "fast")
dispersal <- c("short", "long")
land_rep <- 1:3
BatchNum <- 1:16

#create data frame with all trait combinations
sims_long <- expand.grid(optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims_long$BatchNum <- rep(1:16)

# create data frame with all parameter combinations for the IUCN classification time
IUCN_classification <- merge(IUCN_classification, sims_long, by = "BatchNum")

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

# make the same but for every replicate

# write moving window function
MW_replicates <- function(data, new_data, timehorizon, threshold, category, metric){
  #column_new_data <- paste(category, metric, sep = "_")
  #print(column_new_data)
  
  # select the right column depending on the selected metric
  if(metric == "Pop"){
    column_data <- "pop_sum"
  } else if(metric == "HS") {
    column_data <- "hs_change"
  } else {
    column_data <- "extProb2"
  }
  
  # reduce timehorizon if it exceeds the maximum number of years in the data set
  if(timehorizon > max(data$Year) - 100){
    timehorizon <- max(data$Year) - 100
    #print(timehorizon)
  }
  
  # for every year calculated the relative size to the size x years into the future
  for (i in 1:nrow(data)) {
    rel_loss <- 1 - (data[data$Year == unique(data$Year)[i+timehorizon] & data$Rep == rep_nr, column_data]/data[data$Year == unique(data$Year)[i]  & data$Rep == rep_nr, column_data])
    #print(rel_loss)
    
    # controls if the relative loss exceeds the threshold
    if(rel_loss >= threshold){
      #print(unique(data$Year)[i])
      new_data[new_data$BatchNum == BatchNum & new_data$land_rep == land_rep & new_data$replicates == rep_nr, paste(category, metric, sep = "_")] <- (unique(data$Year)[i])-100
      return(new_data)
      break
    }
  }
}

# create new data frame, which contains the years of reaching the criteria/ species being classified
land_rep <- 1:3
BatchNum <- 1:16
set.seed(8765)
replicates <- sample(0:99, 10)

# create data frame with all parameter combinations for the IUCN classification time
IUCN_classification <- expand.grid(land_rep = land_rep, BatchNum = BatchNum, replicates = replicates)

# create the VU, EN, CR columns classification time for all metrices (Pop, Range, HS, Ext.Prob)
IUCN_classification$VU_Pop <- NA
IUCN_classification$EN_Pop <- NA
IUCN_classification$CR_Pop <- NA
IUCN_classification$VU_HS <- NA
IUCN_classification$EN_HS <- NA
IUCN_classification$CR_HS <- NA
IUCN_classification$VU_Ext <- NA
IUCN_classification$EN_Ext <- NA
IUCN_classification$CR_Ext <- NA

# performs the same code for every scenario
for (i in 1:nrow(IUCN_classification)) {
  BatchNum <- IUCN_classification[i, "BatchNum"]
  land_rep <- IUCN_classification[i, "land_rep"]
  rep_nr <- IUCN_classification[i, "replicates"]
  
  # Load data
  #dat <- readRDS("4_Analysis/data/data_analysis_raw_Batch10_Sim1.rds")
  dat <- readRDS(paste0("4_Analysis/data/data_analysis_raw_long_Batch", BatchNum, "_Sim", land_rep, ".rds"))
  
  # remove rows till year 100
  dat <- dat[!dat$Year %in% c(0:99),]
  
  dat$extProb2 <- 1- dat$extProb
  
  # Criterion A3 - Pop size
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.3, "VU", "Pop")
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.5, "EN", "Pop")
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.8, "CR", "Pop")
  
  # Criterion A3 - HS
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.3, "VU", "HS")
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.5, "EN", "HS")
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.8, "CR", "HS")
  
  # Criterion E - Extinction probability
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 100, 0.1, "VU", "Ext")
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 20, 0.2, "EN", "Ext")
  IUCN_classification <- MW_replicates(dat, IUCN_classification, 10, 0.5, "CR", "Ext")
  
}

save(IUCN_classification, file = "4_Analysis/data/IUCN_classification_times_allreplicates.RData")

# add the trait values to the data frame
optima <- c("marginal", "central")
breadth <- c("narrow", "wide")
rmax <- c("slow", "fast")
dispersal <- c("short", "long")
land_rep <- 1:3
BatchNum <- 1:16

#create data frame with all trait combinations
sims_long <- expand.grid(optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims_long$BatchNum <- rep(1:16)

# create data frame with all parameter combinations for the IUCN classification time
IUCN_classification <- merge(IUCN_classification, sims_long, by = "BatchNum")

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

