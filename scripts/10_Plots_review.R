# Reviewer comment on providing details on initialisation and equilibrium conditions
# individuals, occupancy, range?

library(data.table)
library(ggplot2)
library(gridExtra)
library(lme4)
library(ggtext)

source("functions/extract_legend.R")
source("scripts/text_labels_plots.R")
# Create the parameter table
land_rep <- 1:3
optima <- c(0.27, 0.5)
breadth <- c(0.045, 0.055)
rmax <- c(3, 5)
dispersal <- c(5000, 15000)
set.seed(8765)
replicates <- sample(0:99, 10)

sims <- expand.grid(land_rep = land_rep, optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims$BatchNum <- rep(1:16, each = 3)

land_rep <- 1:3
BatchNum <- 1:16
set.seed(8765)
replicates <- sample(0:99, 10)

data_simulations <- expand.grid(land_rep = land_rep, BatchNum = BatchNum, replicates = replicates)
data_simulations$pop0 <- NA
data_simulations$pop100 <- NA
data_simulations$occ0 <- NA
data_simulations$occ100 <- NA

for (sim_nr in 1:nrow(sims)) {
  rep_nr <- sims[sim_nr,]$land_rep
  BatchNum <- sims[sim_nr,]$BatchNum
  
  # Load in data
  pop <- readRDS(paste0("4_Analysis/data/SumInd_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  occ <- fread(paste0("4_Analysis/data/Batch", BatchNum, "_Sim", rep_nr, "_Land1_Range.txt"))
  
  # extract the replicate runs
  pop_sub <- subset(pop, pop$Rep %in% replicates)
  occ_sub <- subset(occ, occ$Rep %in% replicates)
  
  # extract the year 0 and 100
  pop_sub2 <- subset(pop_sub, pop_sub$Year %in% c(0,100))
  occ_sub2 <- subset(occ_sub, occ_sub$Year %in% c(0,100))
  
  for (i in 1:length(replicates)) {
    data_simulations[data_simulations$BatchNum == BatchNum & data_simulations$land_rep == rep_nr &
                       data_simulations$replicates == replicates[i], "pop0"] <- pop_sub2[pop_sub2$Rep == replicates[i] & pop_sub2$Year == 0, "sumPop"]
    data_simulations[data_simulations$BatchNum == BatchNum & data_simulations$land_rep == rep_nr &
                       data_simulations$replicates == replicates[i], "occ0"] <- occ_sub2[occ_sub2$Rep == replicates[i] & occ_sub2$Year == 0, "NOccupCells"]
    
    data_simulations[data_simulations$BatchNum == BatchNum & data_simulations$land_rep == rep_nr &
                       data_simulations$replicates == replicates[i], "pop100"] <- pop_sub2[pop_sub2$Rep == replicates[i] & pop_sub2$Year == 100, "sumPop"]
    data_simulations[data_simulations$BatchNum == BatchNum & data_simulations$land_rep == rep_nr &
                       data_simulations$replicates == replicates[i], "occ100"] <- occ_sub2[occ_sub2$Rep == replicates[i] & occ_sub2$Year == 100, "NOccupCells"]
  }
}

# transform column
data_simulations$BatchNum <- as.character(data_simulations$BatchNum)

# Plot results as boxplots

p1 <- ggplot(subset(data_simulations, data_simulations$BatchNum == 1), aes(y=pop0, x =BatchNum))+
  geom_boxplot(position = position_nudge(x = -0.3), width = 0.15, fill = "#646F58")+
  geom_boxplot(aes(y=pop100, x = BatchNum), position = position_nudge(x = -0.1), width = 0.15, fill = "#B38D97")+
  geom_boxplot(aes(y=occ0, x = BatchNum), position = position_nudge(x = 0.1), width = 0.15, fill = "#4F94CD")+
  geom_boxplot(aes(y=occ100, x = BatchNum), position = position_nudge(x = 0.3), width = 0.15, fill = "#CDAD00")+
  ylim(c(0,70000))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  ylab("simulated value")

p2 <- ggplot(subset(data_simulations, data_simulations$BatchNum == 2), aes(y=pop0, x =BatchNum))+
  geom_boxplot(position = position_nudge(x = -0.3), width = 0.15, fill = "#646F58")+
  geom_boxplot(aes(y=pop100, x = BatchNum), position = position_nudge(x = -0.1), width = 0.15, fill = "#B38D97")+
  geom_boxplot(aes(y=occ0, x = BatchNum), position = position_nudge(x = 0.1), width = 0.15, fill = "#4F94CD")+
  geom_boxplot(aes(y=occ100, x = BatchNum), position = position_nudge(x = 0.3), width = 0.15, fill = "#CDAD00")+
  ylim(c(0,70000))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16), axis.title = element_blank(), legend.position = "",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

p3 <- ggplot(subset(data_simulations, data_simulations$BatchNum == 3), aes(y=pop0, x =BatchNum))+
  geom_boxplot(position = position_nudge(x = -0.3), width = 0.15, fill = "#646F58")+
  geom_boxplot(aes(y=pop100, x = BatchNum), position = position_nudge(x = -0.1), width = 0.15, fill = "#B38D97")+
  geom_boxplot(aes(y=occ0, x = BatchNum), position = position_nudge(x = 0.1), width = 0.15, fill = "#4F94CD")+
  geom_boxplot(aes(y=occ100, x = BatchNum), position = position_nudge(x = 0.3), width = 0.15, fill = "#CDAD00")+
  ylim(c(0,70000))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16), axis.title = element_blank(), legend.position = "",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

p4 <- ggplot(subset(data_simulations, data_simulations$BatchNum == 4), aes(y=pop0, x =BatchNum))+
  geom_boxplot(position = position_nudge(x = -0.3), width = 0.15, fill = "#646F58")+
  geom_boxplot(aes(y=pop100, x = BatchNum), position = position_nudge(x = -0.1), width = 0.15, fill = "#B38D97")+
  geom_boxplot(aes(y=occ0, x = BatchNum), position = position_nudge(x = 0.1), width = 0.15, fill = "#4F94CD")+
  geom_boxplot(aes(y=occ100, x = BatchNum), position = position_nudge(x = 0.3), width = 0.15, fill = "#CDAD00")+
  ylim(c(0,70000))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16), axis.title = element_blank(), legend.position = "",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

p5 <- ggplot(subset(data_simulations, data_simulations$BatchNum == 5), aes(y=pop0, x =BatchNum))+
  geom_boxplot(position = position_nudge(x = -0.3), width = 0.15, fill = "#646F58")+
  geom_boxplot(aes(y=pop100, x = BatchNum), position = position_nudge(x = -0.1), width = 0.15, fill = "#B38D97")+
  geom_boxplot(aes(y=occ0, x = BatchNum), position = position_nudge(x = 0.1), width = 0.15, fill = "#4F94CD")+
  geom_boxplot(aes(y=occ100, x = BatchNum), position = position_nudge(x = 0.3), width = 0.15, fill = "#CDAD00")+
  ylim(c(0,70000))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  ylab("simulated value")

p6 <- ggplot(subset(data_simulations, data_simulations$BatchNum == 6), aes(y=pop0, x =BatchNum))+
  geom_boxplot(position = position_nudge(x = -0.3), width = 0.15, fill = "#646F58")+
  geom_boxplot(aes(y=pop100, x = BatchNum), position = position_nudge(x = -0.1), width = 0.15, fill = "#B38D97")+
  geom_boxplot(aes(y=occ0, x = BatchNum), position = position_nudge(x = 0.1), width = 0.15, fill = "#4F94CD")+
  geom_boxplot(aes(y=occ100, x = BatchNum), position = position_nudge(x = 0.3), width = 0.15, fill = "#CDAD00")+
  ylim(c(0,70000))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16), axis.title = element_blank(), legend.position = "",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

p7 <- ggplot(subset(data_simulations, data_simulations$BatchNum == 7), aes(y=pop0, x =BatchNum))+
  geom_boxplot(position = position_nudge(x = -0.3), width = 0.15, fill = "#646F58")+
  geom_boxplot(aes(y=pop100, x = BatchNum), position = position_nudge(x = -0.1), width = 0.15, fill = "#B38D97")+
  geom_boxplot(aes(y=occ0, x = BatchNum), position = position_nudge(x = 0.1), width = 0.15, fill = "#4F94CD")+
  geom_boxplot(aes(y=occ100, x = BatchNum), position = position_nudge(x = 0.3), width = 0.15, fill = "#CDAD00")+
  ylim(c(0,70000))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16),axis.title = element_blank(), legend.position = "",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

p8 <- ggplot(subset(data_simulations, data_simulations$BatchNum == 8), aes(y=pop0, x =BatchNum))+
  geom_boxplot(position = position_nudge(x = -0.3), width = 0.15, fill = "#646F58")+
  geom_boxplot(aes(y=pop100, x = BatchNum), position = position_nudge(x = -0.1), width = 0.15, fill = "#B38D97")+
  geom_boxplot(aes(y=occ0, x = BatchNum), position = position_nudge(x = 0.1), width = 0.15, fill = "#4F94CD")+
  geom_boxplot(aes(y=occ100, x = BatchNum), position = position_nudge(x = 0.3), width = 0.15, fill = "#CDAD00")+
  ylim(c(0,70000))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16), axis.title = element_blank(), legend.position = "",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

p9 <- ggplot(subset(data_simulations, data_simulations$BatchNum == 9), aes(y=pop0, x =BatchNum))+
  geom_boxplot(position = position_nudge(x = -0.3), width = 0.15, fill = "#646F58")+
  geom_boxplot(aes(y=pop100, x = BatchNum), position = position_nudge(x = -0.1), width = 0.15, fill = "#B38D97")+
  geom_boxplot(aes(y=occ0, x = BatchNum), position = position_nudge(x = 0.1), width = 0.15, fill = "#4F94CD")+
  geom_boxplot(aes(y=occ100, x = BatchNum), position = position_nudge(x = 0.3), width = 0.15, fill = "#CDAD00")+
  ylim(c(0,70000))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  ylab("simulated value")

p10 <- ggplot(subset(data_simulations, data_simulations$BatchNum == 10), aes(y=pop0, x =BatchNum))+
  geom_boxplot(position = position_nudge(x = -0.3), width = 0.15, fill = "#646F58")+
  geom_boxplot(aes(y=pop100, x = BatchNum), position = position_nudge(x = -0.1), width = 0.15, fill = "#B38D97")+
  geom_boxplot(aes(y=occ0, x = BatchNum), position = position_nudge(x = 0.1), width = 0.15, fill = "#4F94CD")+
  geom_boxplot(aes(y=occ100, x = BatchNum), position = position_nudge(x = 0.3), width = 0.15, fill = "#CDAD00")+
  ylim(c(0,70000))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16), axis.title = element_blank(), legend.position = "",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

p11 <- ggplot(subset(data_simulations, data_simulations$BatchNum == 11), aes(y=pop0, x =BatchNum))+
  geom_boxplot(position = position_nudge(x = -0.3), width = 0.15, fill = "#646F58")+
  geom_boxplot(aes(y=pop100, x = BatchNum), position = position_nudge(x = -0.1), width = 0.15, fill = "#B38D97")+
  geom_boxplot(aes(y=occ0, x = BatchNum), position = position_nudge(x = 0.1), width = 0.15, fill = "#4F94CD")+
  geom_boxplot(aes(y=occ100, x = BatchNum), position = position_nudge(x = 0.3), width = 0.15, fill = "#CDAD00")+
  ylim(c(0,70000))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16),axis.title = element_blank(), legend.position = "",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

p12 <- ggplot(subset(data_simulations, data_simulations$BatchNum == 12), aes(y=pop0, x =BatchNum))+
  geom_boxplot(position = position_nudge(x = -0.3), width = 0.15, fill = "#646F58")+
  geom_boxplot(aes(y=pop100, x = BatchNum), position = position_nudge(x = -0.1), width = 0.15, fill = "#B38D97")+
  geom_boxplot(aes(y=occ0, x = BatchNum), position = position_nudge(x = 0.1), width = 0.15, fill = "#4F94CD")+
  geom_boxplot(aes(y=occ100, x = BatchNum), position = position_nudge(x = 0.3), width = 0.15, fill = "#CDAD00")+
  ylim(c(0,70000))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16), axis.title = element_blank(), legend.position = "",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

p13 <- ggplot(subset(data_simulations, data_simulations$BatchNum == 13), aes(y=pop0, x =BatchNum))+
  geom_boxplot(position = position_nudge(x = -0.3), width = 0.15, fill = "#646F58")+
  geom_boxplot(aes(y=pop100, x = BatchNum), position = position_nudge(x = -0.1), width = 0.15, fill = "#B38D97")+
  geom_boxplot(aes(y=occ0, x = BatchNum), position = position_nudge(x = 0.1), width = 0.15, fill = "#4F94CD")+
  geom_boxplot(aes(y=occ100, x = BatchNum), position = position_nudge(x = 0.3), width = 0.15, fill = "#CDAD00")+
  ylim(c(0,70000))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  ylab("simulated value")

p14 <- ggplot(subset(data_simulations, data_simulations$BatchNum == 14), aes(y=pop0, x =BatchNum))+
  geom_boxplot(position = position_nudge(x = -0.3), width = 0.15, fill = "#646F58")+
  geom_boxplot(aes(y=pop100, x = BatchNum), position = position_nudge(x = -0.1), width = 0.15, fill = "#B38D97")+
  geom_boxplot(aes(y=occ0, x = BatchNum), position = position_nudge(x = 0.1), width = 0.15, fill = "#4F94CD")+
  geom_boxplot(aes(y=occ100, x = BatchNum), position = position_nudge(x = 0.3), width = 0.15, fill = "#CDAD00")+
  ylim(c(0,70000))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16), axis.title = element_blank(), legend.position = "",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

p15 <- ggplot(subset(data_simulations, data_simulations$BatchNum == 15), aes(y=pop0, x =BatchNum))+
  geom_boxplot(position = position_nudge(x = -0.3), width = 0.15, fill = "#646F58")+
  geom_boxplot(aes(y=pop100, x = BatchNum), position = position_nudge(x = -0.1), width = 0.15, fill = "#B38D97")+
  geom_boxplot(aes(y=occ0, x = BatchNum), position = position_nudge(x = 0.1), width = 0.15, fill = "#4F94CD")+
  geom_boxplot(aes(y=occ100, x = BatchNum), position = position_nudge(x = 0.3), width = 0.15, fill = "#CDAD00")+
  ylim(c(0,70000))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16), axis.title = element_blank(), legend.position = "",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

p16 <- ggplot(subset(data_simulations, data_simulations$BatchNum == 16), aes(y=pop0, x =BatchNum))+
  geom_boxplot(position = position_nudge(x = -0.3), width = 0.15, fill = "#646F58")+
  geom_boxplot(aes(y=pop100, x = BatchNum), position = position_nudge(x = -0.1), width = 0.15, fill = "#B38D97")+
  geom_boxplot(aes(y=occ0, x = BatchNum), position = position_nudge(x = 0.1), width = 0.15, fill = "#4F94CD")+
  geom_boxplot(aes(y=occ100, x = BatchNum), position = position_nudge(x = 0.3), width = 0.15, fill = "#CDAD00")+
  ylim(c(0,70000))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16), axis.title = element_blank(), legend.position = "",
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

# create and extract common legend
colors <- c("Population Size Year 0" = "#646F58", "Population Size Year 100" = "#B38D97", "Occupancy Year 0" = "#4F94CD", "Occupancy Year 100" = "#CDAD00")

legend <- ggplot(subset(data_simulations, data_simulations$BatchNum == 16), aes(y=pop0, x =BatchNum, fill = "Population Size Year 0"))+
  geom_boxplot(position = position_nudge(x = 0.1), width = 0.15)+
  geom_boxplot(aes(y=pop100, x = BatchNum, fill = "Population Size Year 100"), position = position_nudge(x = -0.1), width = 0.15)+
  geom_boxplot(aes(y=occ0, x = BatchNum, fill = "Occupancy Year 0"), position = position_nudge(x = 0.3), width = 0.15)+
  geom_boxplot(aes(y=occ100, x = BatchNum, fill = "Occupancy Year 100"), position = position_nudge(x = -0.3), width = 0.15)+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 18),
        axis.title = element_text(size = 20), plot.title = element_text(size = 25, face = "bold"), legend.title = element_blank(), legend.text = element_text(size = 22), legend.key.size = unit(1.5, "cm"),
        legend.position = "bottom")+
  scale_fill_manual(values= colors, breaks = c("Population Size Year 0", "Population Size Year 100", "Occupancy Year 0", "Occupancy Year 100"))
  
shared_legend <- extract_legend(legend)

#Plot the large grid
grid.arrange(arrangeGrob(t0, t_cn, t_cw, t_wna, t_ww, t_ss, p1,p3,p2,p4, t_sl, p9,p11,p10,p12, t_fs, p5,p7,p6,p8, t_fl, p13,p15,p14,p16, nrow = 5, ncol = 5, heights= c(1,3.8,4,4,4.2), widths = c(2,5,5,5,5)),
             t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.2, 0.8), widths = c(11.7,0.3))


# compare real habitat, SDM predictions, actual occupancy

# prepare data sets and values for the for-loop
data <- vector("list", 16)
k <- 1
tmp_dataset <- c()

for (sim_nr in 1:nrow(sims)) {
  # Extract values of the for-loop
  rep_nr <- sims[sim_nr,]$land_rep
  BatchNum <- sims[sim_nr,]$BatchNum
  position <- sims[sim_nr,]$optima
  breadth <- sims[sim_nr,]$breadth
  
  # Load in data
  habitat_SDM <- readRDS(paste0("4_Analysis/data/habitat_suitability_SDM_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  occ <- fread(paste0("4_Analysis/data/Batch", BatchNum, "_Sim", rep_nr, "_Land1_Range.txt"))
  habitat <- readRDS(paste0("4_Analysis/data/real_habitatloss_land", rep_nr, "_optima",  position, "_breadth", breadth, ".rds"))
  
  # extract replicate runs
  occ_sub <- subset(occ, occ$Rep %in% replicates)
  
  # extract the years
  occ_sub2 <- subset(occ_sub, occ_sub$Year > 100)
  
  # join the different data sets to one large one
  for (i in 1:length(habitat_SDM)) {
    # create a temporary column
    habitat$tmp <- 0
    # add the respective SDM habitat value to the column
    habitat$tmp[1:length(habitat_SDM[[i]])] <- habitat_SDM[[i]]
    # change column name
    colnames(habitat)[colnames(habitat) == 'tmp'] <- paste0("habitat_SDM", replicates[i])
    
    # extract the occupancy data of the specific replicate run
    tmp <- subset(occ_sub2, occ_sub2$Rep == replicates[i])
    
    # add extra column
    habitat$tmp <- 0
    # add values occupancy
    habitat$tmp[1:nrow(tmp)] <- tmp$NOccupCells
    # change name of the column
    colnames(habitat)[colnames(habitat) == 'tmp'] <- paste0("occ", replicates[i])
  }
  
  # bind the data set to a temporary one (in this way I can have all data of all three landscapes per virtual species)
  tmp_dataset <- rbind(tmp_dataset, habitat)
  
  # after every third number, add the data set to a list and change the number and create an empty data set again
  if (sim_nr %% 3 == 0){
    data[[k]] <- tmp_dataset
    tmp_dataset <- c()
    k <- k + 1
  }
}

# aggregate over all replicate landscapes
data <- lapply(data, function(x){
  aggregate(x, by = list(x$Year), FUN = mean)
})

# calculate row means for SDM habitat and occupancy
data_mean <- vector("list", 16)
for (i in 1:length(data_mean)) {
  # add Year and the real habitat size and loss
  data_mean[[i]] <- data[[i]][,c(2:4)]
  # add the mean of predicted habitat loss
  data_mean[[i]] <- cbind(data_mean[[i]], rowMeans(data[[i]][, seq(5, ncol(data[[i]]), by = 2)]))
  colnames(data_mean[[i]])[4] <- "mean_habitat_SDM"
  # add sd of predicted habitat loss
  data_mean[[i]] <- cbind(data_mean[[i]], apply(data[[i]][, seq(5, ncol(data[[i]]), by = 2)], 1, sd))
  colnames(data_mean[[i]])[5] <- "sd_habitat_SDM"
  # add the mean of occupancy
  data_mean[[i]] <- cbind(data_mean[[i]], rowMeans(data[[i]][, seq(6, ncol(data[[i]]), by = 2)]))
  colnames(data_mean[[i]])[6] <- "mean_occupancy"
  # add sd of occupancy
  data_mean[[i]] <- cbind(data_mean[[i]], apply(data[[i]][, seq(6, ncol(data[[i]]), by = 2)], 1, sd))
  colnames(data_mean[[i]])[7] <- "sd_occupancy"
}


# Plot results
p1 <- ggplot(data_mean[[1]], aes(x = Year, y = habitat_size))+
  geom_ribbon(aes(ymin = mean_habitat_SDM - sd_habitat_SDM, ymax = mean_habitat_SDM + sd_habitat_SDM), col = NA, alpha = 0.15, fill = "skyblue3")+
  geom_line(aes(x = Year, y = mean_habitat_SDM), linewidth = 1.2, col = "skyblue3")+
  geom_line(linewidth = 1.2, col = "green4")+
  #geom_ribbon(aes(ymin = mean_occupancy - sd_occupancy, ymax = mean_occupancy + sd_occupancy), col = NA, alpha = 0.15, fill = "blue")+
  #geom_line(aes(x = Year, y = mean_occupancy), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,55))+
  ylim(c(0,25000))

p2 <- ggplot(data_mean[[2]], aes(x = Year, y = habitat_size))+
  geom_ribbon(aes(ymin = mean_habitat_SDM - sd_habitat_SDM, ymax = mean_habitat_SDM + sd_habitat_SDM), col = NA, alpha = 0.15, fill = "skyblue3")+
  geom_line(aes(x = Year, y = mean_habitat_SDM), linewidth = 1.2, col = "skyblue3")+
  geom_line(linewidth = 1.2, col = "green4")+
  #geom_ribbon(aes(ymin = mean_occupancy - sd_occupancy, ymax = mean_occupancy + sd_occupancy), col = NA, alpha = 0.15, fill = "blue")+
  #geom_line(aes(x = Year, y = mean_occupancy), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,55))+
  ylim(c(0,25000))

p3 <- ggplot(data_mean[[3]], aes(x = Year, y = habitat_size))+
  geom_ribbon(aes(ymin = mean_habitat_SDM - sd_habitat_SDM, ymax = mean_habitat_SDM + sd_habitat_SDM), col = NA, alpha = 0.15, fill = "skyblue3")+
  geom_line(aes(x = Year, y = mean_habitat_SDM), linewidth = 1.2, col = "skyblue3")+
  geom_line(linewidth = 1.2, col = "green4")+
  #geom_ribbon(aes(ymin = mean_occupancy - sd_occupancy, ymax = mean_occupancy + sd_occupancy), col = NA, alpha = 0.15, fill = "blue")+
  #geom_line(aes(x = Year, y = mean_occupancy), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,55))+
  ylim(c(0,25000))

p4 <- ggplot(data_mean[[4]], aes(x = Year, y = habitat_size))+
  geom_ribbon(aes(ymin = mean_habitat_SDM - sd_habitat_SDM, ymax = mean_habitat_SDM + sd_habitat_SDM), col = NA, alpha = 0.15, fill = "skyblue3")+
  geom_line(aes(x = Year, y = mean_habitat_SDM), linewidth = 1.2, col = "skyblue3")+
  geom_line(linewidth = 1.2, col = "green4")+
  #geom_ribbon(aes(ymin = mean_occupancy - sd_occupancy, ymax = mean_occupancy + sd_occupancy), col = NA, alpha = 0.15, fill = "blue")+
  #geom_line(aes(x = Year, y = mean_occupancy), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,55))+
  ylim(c(0,25000))

p5 <- ggplot(data_mean[[5]], aes(x = Year, y = habitat_size))+
  geom_ribbon(aes(ymin = mean_habitat_SDM - sd_habitat_SDM, ymax = mean_habitat_SDM + sd_habitat_SDM), col = NA, alpha = 0.15, fill = "skyblue3")+
  geom_line(aes(x = Year, y = mean_habitat_SDM), linewidth = 1.2, col = "skyblue3")+
  geom_line(linewidth = 1.2, col = "green4")+
  #geom_ribbon(aes(ymin = mean_occupancy - sd_occupancy, ymax = mean_occupancy + sd_occupancy), col = NA, alpha = 0.15, fill = "blue")+
  #geom_line(aes(x = Year, y = mean_occupancy), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,55))+
  ylim(c(0,25000))

p6 <- ggplot(data_mean[[6]], aes(x = Year, y = habitat_size))+
  geom_ribbon(aes(ymin = mean_habitat_SDM - sd_habitat_SDM, ymax = mean_habitat_SDM + sd_habitat_SDM), col = NA, alpha = 0.15, fill = "skyblue3")+
  geom_line(aes(x = Year, y = mean_habitat_SDM), linewidth = 1.2, col = "skyblue3")+
  geom_line(linewidth = 1.2, col = "green4")+
  #geom_ribbon(aes(ymin = mean_occupancy - sd_occupancy, ymax = mean_occupancy + sd_occupancy), col = NA, alpha = 0.15, fill = "blue")+
  #geom_line(aes(x = Year, y = mean_occupancy), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,55))+
  ylim(c(0,25000))

p7 <- ggplot(data_mean[[7]], aes(x = Year, y = habitat_size))+
  geom_ribbon(aes(ymin = mean_habitat_SDM - sd_habitat_SDM, ymax = mean_habitat_SDM + sd_habitat_SDM), col = NA, alpha = 0.15, fill = "skyblue3")+
  geom_line(aes(x = Year, y = mean_habitat_SDM), linewidth = 1.2, col = "skyblue3")+
  geom_line(linewidth = 1.2, col = "green4")+
  #geom_ribbon(aes(ymin = mean_occupancy - sd_occupancy, ymax = mean_occupancy + sd_occupancy), col = NA, alpha = 0.15, fill = "blue")+
  #geom_line(aes(x = Year, y = mean_occupancy), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,55))+
  ylim(c(0,25000))

p8 <- ggplot(data_mean[[8]], aes(x = Year, y = habitat_size))+
  geom_ribbon(aes(ymin = mean_habitat_SDM - sd_habitat_SDM, ymax = mean_habitat_SDM + sd_habitat_SDM), col = NA, alpha = 0.15, fill = "skyblue3")+
  geom_line(aes(x = Year, y = mean_habitat_SDM), linewidth = 1.2, col = "skyblue3")+
  geom_line(linewidth = 1.2, col = "green4")+
  #geom_ribbon(aes(ymin = mean_occupancy - sd_occupancy, ymax = mean_occupancy + sd_occupancy), col = NA, alpha = 0.15, fill = "blue")+
  #geom_line(aes(x = Year, y = mean_occupancy), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,55))+
  ylim(c(0,25000))

p9 <- ggplot(data_mean[[9]], aes(x = Year, y = habitat_size))+
  geom_ribbon(aes(ymin = mean_habitat_SDM - sd_habitat_SDM, ymax = mean_habitat_SDM + sd_habitat_SDM), col = NA, alpha = 0.15, fill = "skyblue3")+
  geom_line(aes(x = Year, y = mean_habitat_SDM), linewidth = 1.2, col = "skyblue3")+
  geom_line(linewidth = 1.2, col = "green4")+
  #geom_ribbon(aes(ymin = mean_occupancy - sd_occupancy, ymax = mean_occupancy + sd_occupancy), col = NA, alpha = 0.15, fill = "blue")+
  #geom_line(aes(x = Year, y = mean_occupancy), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,55))+
  ylim(c(0,25000))

p10 <- ggplot(data_mean[[10]], aes(x = Year, y = habitat_size))+
  geom_ribbon(aes(ymin = mean_habitat_SDM - sd_habitat_SDM, ymax = mean_habitat_SDM + sd_habitat_SDM), col = NA, alpha = 0.15, fill = "skyblue3")+
  geom_line(aes(x = Year, y = mean_habitat_SDM), linewidth = 1.2, col = "skyblue3")+
  geom_line(linewidth = 1.2, col = "green4")+
  #geom_ribbon(aes(ymin = mean_occupancy - sd_occupancy, ymax = mean_occupancy + sd_occupancy), col = NA, alpha = 0.15, fill = "blue")+
  #geom_line(aes(x = Year, y = mean_occupancy), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,55))+
  ylim(c(0,25000))

p11 <- ggplot(data_mean[[11]], aes(x = Year, y = habitat_size))+
  geom_ribbon(aes(ymin = mean_habitat_SDM - sd_habitat_SDM, ymax = mean_habitat_SDM + sd_habitat_SDM), col = NA, alpha = 0.15, fill = "skyblue3")+
  geom_line(aes(x = Year, y = mean_habitat_SDM), linewidth = 1.2, col = "skyblue3")+
  geom_line(linewidth = 1.2, col = "green4")+
  #geom_ribbon(aes(ymin = mean_occupancy - sd_occupancy, ymax = mean_occupancy + sd_occupancy), col = NA, alpha = 0.15, fill = "blue")+
  #geom_line(aes(x = Year, y = mean_occupancy), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,55))+
  ylim(c(0,25000))

p12 <- ggplot(data_mean[[12]], aes(x = Year, y = habitat_size))+
  geom_ribbon(aes(ymin = mean_habitat_SDM - sd_habitat_SDM, ymax = mean_habitat_SDM + sd_habitat_SDM), col = NA, alpha = 0.15, fill = "skyblue3")+
  geom_line(aes(x = Year, y = mean_habitat_SDM), linewidth = 1.2, col = "skyblue3")+
  geom_line(linewidth = 1.2, col = "green4")+
  #geom_ribbon(aes(ymin = mean_occupancy - sd_occupancy, ymax = mean_occupancy + sd_occupancy), col = NA, alpha = 0.15, fill = "blue")+
  #geom_line(aes(x = Year, y = mean_occupancy), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,55))+
  ylim(c(0,25000))

p13 <- ggplot(data_mean[[13]], aes(x = Year, y = habitat_size))+
  geom_ribbon(aes(ymin = mean_habitat_SDM - sd_habitat_SDM, ymax = mean_habitat_SDM + sd_habitat_SDM), col = NA, alpha = 0.15, fill = "skyblue3")+
  geom_line(aes(x = Year, y = mean_habitat_SDM), linewidth = 1.2, col = "skyblue3")+
  geom_line(linewidth = 1.2, col = "green4")+
  #geom_ribbon(aes(ymin = mean_occupancy - sd_occupancy, ymax = mean_occupancy + sd_occupancy), col = NA, alpha = 0.15, fill = "blue")+
  #geom_line(aes(x = Year, y = mean_occupancy), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,55))+
  ylim(c(0,25000))

p14 <- ggplot(data_mean[[14]], aes(x = Year, y = habitat_size))+
  geom_ribbon(aes(ymin = mean_habitat_SDM - sd_habitat_SDM, ymax = mean_habitat_SDM + sd_habitat_SDM), col = NA, alpha = 0.15, fill = "skyblue3")+
  geom_line(aes(x = Year, y = mean_habitat_SDM), linewidth = 1.2, col = "skyblue3")+
  geom_line(linewidth = 1.2, col = "green4")+
  #geom_ribbon(aes(ymin = mean_occupancy - sd_occupancy, ymax = mean_occupancy + sd_occupancy), col = NA, alpha = 0.15, fill = "blue")+
  #geom_line(aes(x = Year, y = mean_occupancy), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,55))+
  ylim(c(0,25000))

p15 <- ggplot(data_mean[[15]], aes(x = Year, y = habitat_size))+
  geom_ribbon(aes(ymin = mean_habitat_SDM - sd_habitat_SDM, ymax = mean_habitat_SDM + sd_habitat_SDM), col = NA, alpha = 0.15, fill = "skyblue3")+
  geom_line(aes(x = Year, y = mean_habitat_SDM), linewidth = 1.2, col = "skyblue3")+
  geom_line(linewidth = 1.2, col = "green4")+
  #geom_ribbon(aes(ymin = mean_occupancy - sd_occupancy, ymax = mean_occupancy + sd_occupancy), col = NA, alpha = 0.15, fill = "blue")+
  #geom_line(aes(x = Year, y = mean_occupancy), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,55))+
  ylim(c(0,25000))

p16 <- ggplot(data_mean[[16]], aes(x = Year, y = habitat_size))+
  geom_ribbon(aes(ymin = mean_habitat_SDM - sd_habitat_SDM, ymax = mean_habitat_SDM + sd_habitat_SDM), col = NA, alpha = 0.15, fill = "skyblue3")+
  geom_line(aes(x = Year, y = mean_habitat_SDM), linewidth = 1.2, col = "skyblue3")+
  geom_line(linewidth = 1.2, col = "green4")+
  #geom_ribbon(aes(ymin = mean_occupancy - sd_occupancy, ymax = mean_occupancy + sd_occupancy), col = NA, alpha = 0.15, fill = "blue")+
  #geom_line(aes(x = Year, y = mean_occupancy), linewidth = 1.2, col = "blue")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), axis.title.x = element_blank(), legend.position = "")+
  ylab("simulated/ predicted \n value")+
  xlim(c(0,55))+
  ylim(c(0,25000))

colors <- c("Real habitat size" = "skyblue3", "Predicted habitat size (SDM)" = "green4")#, "simulated Occupancy" = "blue")

legend <- ggplot(data_mean[[16]], aes(x = Year, y = habitat_size, color = "Real habitat size"))+
  geom_line(linewidth = 1)+
  geom_line(aes(x = Year, y = mean_habitat_SDM, color = "Predicted habitat size (SDM)"), linewidth = 1)+
  #geom_line(aes(x = Year, y = mean_occupancy, color = "simulated Occupancy"), linewidth = 1)+
  ylab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 18), legend.position = "bottom", , legend.title = element_blank(), 
        legend.text = element_text(size = 20), legend.key.size = unit(1.5, "cm"))+
  scale_color_manual(values= colors, breaks = c("Real habitat size", "Predicted habitat size (SDM)"))#, "simulated Occupancy"))

shared_legend <- extract_legend(legend)

#Plot the large grid
grid.arrange(arrangeGrob(t0, t_cn, t_cw, t_wna, t_ww, t_ss, p1,p3,p2,p4, t_sl, p9,p11,p10,p12, t_fs, p5,p7,p6,p8, t_fl, p13,p15,p14,p16, nrow = 5, ncol = 5, heights= c(1,3.8,4,4,4.2), widths = c(2,5,5,5,5)),
             t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.2, 0.8), widths = c(11.7,0.3))


# Example response curves of the different species (the four different niche combinations)

# create data set
clim <- seq(0,1,0.001)
response_curves_df <- data.frame(clim = seq(0,1,0.001),
                                    mn_temp = dnorm(clim,mean = 0.27, sd = 0.045),
                                    mn_pre = dnorm(clim,mean = 0.5, sd = 0.045),
                                    mw_temp = dnorm(clim,mean = 0.27, sd = 0.055),
                                    mw_pre = dnorm(clim,mean = 0.5, sd = 0.055),
                                    cn_temp = dnorm(clim,mean = 0.5, sd = 0.045),
                                    cn_pre = dnorm(clim,mean = 0.505, sd = 0.045),
                                    cw_temp = dnorm(clim,mean = 0.5, sd = 0.055),
                                    cw_pre = dnorm(clim, mean = 0.505, sd = 0.055))
                                    
# marginal & narrow
p1 <- ggplot(response_curves_df, aes(x = clim, y = mn_temp))+
  geom_line(linewidth = 1.1)+
  geom_line(aes(y = mn_pre), linetype = "dashed", linewidth = 1.1)+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 23), axis.title.x = element_blank())+
  xlab("Climate ranges")+
  ylab("Occurrence probability")+
  scale_y_continuous(
    breaks = c(0, 2.5, 5.0, 7.5),
    labels = c("0", "0.25", "0.5", "0.75")
  )

# marginal & wide
p2 <- ggplot(response_curves_df, aes(x = clim, y = mw_temp))+
  geom_line(linewidth = 1.1)+
  geom_line(aes(y = mw_pre), linetype = "dashed", linewidth = 1.1)+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_blank())+
  xlab("Climate ranges")+
  scale_y_continuous(
    breaks = c(0, 2.5, 5.0, 7.5),
    labels = c("0", "0.25", "0.5", "0.75")
  )

# central & narrow
p3 <- ggplot(response_curves_df, aes(x = clim, y = cn_temp))+
  geom_line(linewidth = 1.1)+
  geom_line(aes(y = cn_pre), linetype = "dashed", linewidth = 1.1)+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 23))+
  xlab("Climate ranges")+
  ylab("Occurrence probability")+
  scale_y_continuous(
    breaks = c(0, 2.5, 5.0, 7.5),
    labels = c("0", "0.25", "0.5", "0.75")
  )

# central & wide
p4 <- ggplot(response_curves_df, aes(x = clim, y = cw_temp))+
  geom_line(linewidth = 1.1)+
  geom_line(aes(y = cw_pre), linetype = "dashed", linewidth = 1.1)+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 23), axis.title.y = element_blank())+
  xlab("Climate ranges")+
  scale_y_continuous(
    breaks = c(0, 2.5, 5.0, 7.5),
    labels = c("0", "0.25", "0.5", "0.75")
  )

legend <- ggplot(response_curves_df, aes(x = clim, y = cw_temp))+
  geom_line(aes(linetype = "Temperature"), linewidth = 1.1)+
  geom_line(aes(y = cw_pre, linetype = "Precipitation"), linewidth = 1.1)+
  theme_bw()+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 23), legend.position = "bottom", legend.text = element_text(size = 23),
        legend.key.size = unit(2,"line"), legend.title=element_blank())+
  xlab("Climate ranges")+
  ylab("Occurrence probability")+
  scale_linetype_manual(
    values = c("Temperature" = "solid", "Precipitation" = "dashed")
  )

shared_legend <- extract_legend(legend)

#Plot the large grid
grid.arrange(arrangeGrob(t0, t_nn, t_wn, t_c_medium, p1,p2, t_w_medium, p3,p4, nrow = 3, ncol = 3, heights= c(1,4,4), widths = c(2,5,5)),
             t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.2, 0.8), widths = c(11.7,0.3))

# create plot that lists the warning time for all range-shifting species separately 
load("4_Analysis/data/IUCN_classification_times_allreplicates.RData")

IUCN_sub <- subset(IUCN_classification, IUCN_classification$optima == "central")
IUCN_sub$BatchNum <- factor(IUCN_sub$BatchNum, levels = c("2", "10", "6", "14", "4", "12", "8", "16"))


p_pos1 <- ggplot(IUCN_sub, aes(x = BatchNum, y = VU_HS))+
  geom_boxplot(width = 0.06, fill = "#EE2C2C", position = position_nudge(x = -0.42))+
  geom_boxplot(aes(y = VU_Ext), position = position_nudge(x = -0.24), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(y = VU_Pop), position = position_nudge(x = - 0.33), width = 0.06, fill = "orange")+
  geom_boxplot(aes(y = EN_HS), width = 0.06, fill = "#EE2C2C", position = position_nudge(x = -0.09))+
  geom_boxplot(aes(y = EN_Ext), position = position_nudge(x = 0.09), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(y = EN_Pop), position = position_nudge(x = 0), width = 0.06, fill = "orange")+
  geom_boxplot(aes(y = CR_HS), width = 0.06, fill = "#EE2C2C", position = position_nudge(x = 0.24))+
  geom_boxplot(aes(y = CR_Ext), position = position_nudge(x = 0.43), width = 0.06, fill = "#1C86EE")+
  geom_boxplot(aes(y = CR_Pop), position = position_nudge(x = 0.33), width = 0.06, fill = "orange")+
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 2.5)+
  geom_vline(xintercept = 3.5)+
  geom_vline(xintercept = 4.5)+
  geom_vline(xintercept = 5.5)+
  geom_vline(xintercept = 6.5)+
  geom_vline(xintercept = 7.5)+
  geom_vline(xintercept = 1.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 0.83, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 2.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 1.83, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 3.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 2.83, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 4.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 3.83, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 5.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 4.83, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 6.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 5.83, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 7.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 6.83, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 8.16, linetype = "dashed", color = "lightgrey")+
  geom_vline(xintercept = 7.83, linetype = "dashed", color = "lightgrey")+
  annotate(geom="text", x=0.655, y=48, label="VU", color="black", size = 8)+
  annotate(geom="text", x=0.995, y=48, label="EN", color="black", size = 8)+
  annotate(geom="text", x=1.325, y=48, label="CR", color="black", size = 8)+
  annotate(geom="text", x=1.655, y=48, label="VU", color="black", size = 8)+
  annotate(geom="text", x=1.995, y=48, label="EN", color="black", size = 8)+
  annotate(geom="text", x=2.325, y=48, label="CR", color="black", size = 8)+
  annotate(geom="text", x=2.655, y=48, label="VU", color="black", size = 8)+
  annotate(geom="text", x=2.995, y=48, label="EN", color="black", size = 8)+
  annotate(geom="text", x=3.325, y=48, label="CR", color="black", size = 8)+
  annotate(geom="text", x=3.655, y=48, label="VU", color="black", size = 8)+
  annotate(geom="text", x=3.995, y=48, label="EN", color="black", size = 8)+
  annotate(geom="text", x=4.325, y=48, label="CR", color="black", size = 8)+
  annotate(geom="text", x=4.655, y=48, label="VU", color="black", size = 8)+
  annotate(geom="text", x=4.995, y=48, label="EN", color="black", size = 8)+
  annotate(geom="text", x=5.325, y=48, label="CR", color="black", size = 8)+
  annotate(geom="text", x=5.655, y=48, label="VU", color="black", size = 8)+
  annotate(geom="text", x=5.995, y=48, label="EN", color="black", size = 8)+
  annotate(geom="text", x=6.325, y=48, label="CR", color="black", size = 8)+
  annotate(geom="text", x=6.655, y=48, label="VU", color="black", size = 8)+
  annotate(geom="text", x=6.995, y=48, label="EN", color="black", size = 8)+
  annotate(geom="text", x=7.325, y=48, label="CR", color="black", size = 8)+
  annotate(geom="text", x=7.655, y=48, label="VU", color="black", size = 8)+
  annotate(geom="text", x=7.995, y=48, label="EN", color="black", size = 8)+
  annotate(geom="text", x=8.325, y=48, label="CR", color="black", size = 8)+
  scale_x_discrete(expand = c(0.065, 0.065), label = c("narrow niche<br>slow growth rate<br>short dispersal", "narrow niche<br>slow growth rate<br>long dispersal",
                                                       "narrow niche<br>fast growth rate<br>short dispersal", "narrow niche<br>fast growth rate<br>long dispersal",
                                                       "wide niche<br>slow growth rate<br>short dispersal", "**wide niche<br>slow growth rate<br>long dispersal**",
                                                       "wide niche<br>fast growth rate<br>short dispersal", "**wide niche<br>fast growth rate<br>long dispersal**")) +
  xlab("")+
  ylim(c(0,50))+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 22), axis.text.x = ggtext::element_markdown(),
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


# # Fitting a linear model to the data with a random intercept
# #model_lm <- glmer(pop_sum ~ hs_loss + optima + breadth + rmax + dispersal + hs_loss:optima + hs_loss:breadth + hs_loss:rmax + 
# #                        hs_loss:dispersal + (1|land), data = data_adapted_long,control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), family = "binomial")
# 
# #save(model_lm, file = "4_Analysis/Model Results/Model_lm.Rdata")
# 
# load("4_Analysis/Model Results/Model_ordbeta_full.Rdata")
# load("4_Analysis/Model Results/Model_lm.Rdata")
# 
# # fit hsloss and poploss curve with linear regression
# load("4_Analysis/data/data_bayes_model.Rdata")
# 
# data_adapted_long$breadth <- factor(data_adapted_long$breadth, levels = c("wide", "narrow"))
# data_adapted_long$rmax <- factor(data_adapted_long$rmax, levels = c("fast", "slow"))
# data_adapted_long$dispersal <- factor(data_adapted_long$dispersal, levels = c("long", "short"))
# data_adapted_long$optima <- as.character(data_adapted_long$optima)
# data_adapted_long[which(data_adapted_long$optima == "range-contracting"), "optima"] <- "marginal"
# data_adapted_long[which(data_adapted_long$optima == "range-shifting"), "optima"] <- "central"
# data_adapted_long$optima <- factor(data_adapted_long$optima, levels = c("marginal", "central"))
# 
# ggplot(data_adapted_long, aes(x=hs_loss, y = pop_sum, col = land, linetype = optima))+
#   geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
#   geom_point(data = data_adapted_long[seq(1, nrow(data_adapted_long),2),], aes(shape = optima))+
#   geom_smooth(method = lm)+
#   xlab("Habitat loss")+
#   ylab("Relative population size")+
#   theme_bw()+
#   theme(axis.text = element_text(size = 18), axis.title = element_text(size = 23), plot.title = element_text(size = 28, face = "italic"),
#         legend.position = c(0.91, 0.85),  legend.title = element_text(size = 23), legend.text = element_text(size = 23),
#         legend.key.size = unit(2,"line"), )+ #axis.title.x = element_blank(),
#   scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
#   scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
#   scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"))+
#   ggtitle("Range dynamics")+
#   labs(colour = "Landscape", linetype = NULL)+
#   guides(linetype = guide_legend(order = 1, override.aes = list(color = "black")))
