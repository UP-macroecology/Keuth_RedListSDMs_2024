# Testing effect of rare long distance dispersal for wide and narrow niche
# In this script I try different values for the emigration probability as well as the mean dispersal distance and the probability for using 
#the rare long dispersal

# Load packages
library(RangeShiftR)
library(scales)
library(dplyr)
library(gridExtra)
library(tibble)
library(ggplot2)

#File path
path_input <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/01_TestingParameters/")

#obtain number for temperature increase
t <- 1:90
alpha <- 0.5
beta <- 0.9 
theta <- 0.3
set.seed(5678)
ts <- alpha + beta * t + arima.sim(list(ma = theta), n = length(t))
x <- as.vector(ts)
temp_rise <- scales::rescale(x, c(0,0.9))

# Set up the dynamic landscapes ------------------------------------------------------------------------------------
# Numbers of spinup years in dynamic landscape
spinup <- 100

# Set dynamic landscape parameters
landnames <- c()
for (i in 1:length(temp_rise)){
  landnames <- append(landnames, paste0("habitat_per_cc", temp_rise[i], ".asc"))
}

#create vector for the years (landscapes start changing at year 101)
years <- seq(spinup+1, length(landnames)+99,1)
years <- c("0", years)
years  <- as.numeric(years)

# Define Landscape module ------------------------------------------------------------------------------------

land <- ImportedLandscape(LandscapeFile = landnames,
                          DynamicLandYears = years,
                          Resolution = 1000,
                          HabPercent = TRUE,
                          K_or_DensDep = 0.05)

# Define demography module ------------------------------------------------------------------------------------

demo <- Demography(Rmax = 5, ReproductionType = 0) # asexual model with no stage structure

# Define dispersal module ------------------------------------------------------------------------------------

#Neutral model
disp <-  Dispersal(
  # Emigration phase: constant emigration probability of 0.4
  Emigration = Emigration(EmigProb = 0.4),
  # Transfer phase: negative exponential dispersal kernel with mean dispersal distance dependent on scenarios
  Transfer = DispersalKernel(DoubleKernel = T, Distances = matrix(c(15000, 450000, 0.9), ncol = 3)),
  #Fandos et al. (2023) report long distance as the 95 percentile, so I would specify it as occurring having a probability of 5 %, 
  # Settlement: if individual arrives in unsuitable cells, it can randomly chose a suitable neighbouring cell or will die
  Settlement = Settlement(Settle = 2)
)

#Emigration probability -5
disp_e_m5 <-  Dispersal(
  # Emigration phase: constant emigration probability of 0.4
  Emigration = Emigration(EmigProb = 0.35),
  # Transfer phase: negative exponential dispersal kernel with mean dispersal distance dependent on scenarios
  Transfer = DispersalKernel(DoubleKernel = T, Distances = matrix(c(15000, 450000, 0.9), ncol = 3)),
  #Fandos et al. (2023) report long distance as the 95 percentile, so I would specify it as occurring having a probability of 5 %, 
  # Settlement: if individual arrives in unsuitable cells, it can randomly chose a suitable neighbouring cell or will die
  Settlement = Settlement(Settle = 2)
)

#Emigration probability +5
disp_e_p5 <-  Dispersal(
  # Emigration phase: constant emigration probability of 0.4
  Emigration = Emigration(EmigProb = 0.45),
  # Transfer phase: negative exponential dispersal kernel with mean dispersal distance dependent on scenarios
  Transfer = DispersalKernel(DoubleKernel = T, Distances = matrix(c(15000, 450000, 0.9), ncol = 3)),
  #Fandos et al. (2023) report long distance as the 95 percentile, so I would specify it as occurring having a probability of 5 %, 
  # Settlement: if individual arrives in unsuitable cells, it can randomly chose a suitable neighbouring cell or will die
  Settlement = Settlement(Settle = 2)
)

#Dispersal probability -5
disp_dp_m5 <-  Dispersal(
  # Emigration phase: constant emigration probability of 0.4
  Emigration = Emigration(EmigProb = 0.4),
  # Transfer phase: negative exponential dispersal kernel with mean dispersal distance dependent on scenarios
  Transfer = DispersalKernel(DoubleKernel = T, Distances = matrix(c(15000, 450000, 0.85), ncol = 3)),
  #Fandos et al. (2023) report long distance as the 95 percentile, so I would specify it as occurring having a probability of 5 %, 
  # Settlement: if individual arrives in unsuitable cells, it can randomly chose a suitable neighbouring cell or will die
  Settlement = Settlement(Settle = 2)
)

#Dispersal probability +5
disp_dp_p5 <-  Dispersal(
  # Emigration phase: constant emigration probability of 0.4
  Emigration = Emigration(EmigProb = 0.4),
  # Transfer phase: negative exponential dispersal kernel with mean dispersal distance dependent on scenarios
  Transfer = DispersalKernel(DoubleKernel = T, Distances = matrix(c(15000, 450000, 0.95), ncol = 3)),
  #Fandos et al. (2023) report long distance as the 95 percentile, so I would specify it as occurring having a probability of 5 %, 
  # Settlement: if individual arrives in unsuitable cells, it can randomly chose a suitable neighbouring cell or will die
  Settlement = Settlement(Settle = 2)
)

#Dispersal distance
disp_dd_m5 <-  Dispersal(
  # Emigration phase: constant emigration probability of 0.4
  Emigration = Emigration(EmigProb = 0.4),
  # Transfer phase: negative exponential dispersal kernel with mean dispersal distance dependent on scenarios
  Transfer = DispersalKernel(DoubleKernel = T, Distances = matrix(c(15000, 400000, 0.9), ncol = 3)),
  #Fandos et al. (2023) report long distance as the 95 percentile, so I would specify it as occurring having a probability of 5 %, 
  # Settlement: if individual arrives in unsuitable cells, it can randomly chose a suitable neighbouring cell or will die
  Settlement = Settlement(Settle = 2)
)

#Dispersal distance
disp_dd_p5 <-  Dispersal(
  # Emigration phase: constant emigration probability of 0.4
  Emigration = Emigration(EmigProb = 0.4),
  # Transfer phase: negative exponential dispersal kernel with mean dispersal distance dependent on scenarios
  Transfer = DispersalKernel(DoubleKernel = T, Distances = matrix(c(15000, 500000, 0.9), ncol = 3)),
  #Fandos et al. (2023) report long distance as the 95 percentile, so I would specify it as occurring having a probability of 5 %, 
  # Settlement: if individual arrives in unsuitable cells, it can randomly chose a suitable neighbouring cell or will die
  Settlement = Settlement(Settle = 2)
)

# Define initial conditions for simulations ------------------------------------------------------------------------------------

# Initialisation in all suitable cells at half carrying capacity
init <- Initialise(InitType = 0, FreeType = 1, InitDens = 1)

RepNb <- 10 #100 replicated runs
sim_years <- 89 #let simulation run for 89 years
sim <- Simulation(Simulation = 0,
                  Replicates = RepNb,
                  Years = spinup + sim_years,
                  OutIntPop = 1,
                  OutIntOcc = 1)

s <- RSsim(batchnum = 1, land = land, demog = demo, dispersal = disp, simul = sim, init = init)
#s_e_m5 <- RSsim(batchnum = 2, land = land, demog = demo, dispersal = disp_e_m5, simul = sim, init = init)
#s_e_p5 <- RSsim(batchnum = 3, land = land, demog = demo, dispersal = disp_e_p5, simul = sim, init = init)
s_dp_m5 <- RSsim(batchnum = 4, land = land, demog = demo, dispersal = disp_dp_m5, simul = sim, init = init)
s_dp_p5 <- RSsim(batchnum = 5, land = land, demog = demo, dispersal = disp_dp_p5, simul = sim, init = init)
s_dd_m5 <- RSsim(batchnum = 6, land = land, demog = demo, dispersal = disp_dd_m5, simul = sim, init = init)
s_dd_p5 <- RSsim(batchnum = 7, land = land, demog = demo, dispersal = disp_dd_p5, simul = sim, init = init)

# Run simulations ------------------------------------------------------------------------------------

RunRS(s, path_input)
#RunRS(s_e_m5, path_input)
#RunRS(s_e_p5, path_input)
RunRS(s_dp_m5, path_input)
RunRS(s_dp_p5, path_input)
RunRS(s_dd_m5, path_input)
RunRS(s_dd_p5, path_input)

# Plot changes in abundances
####  Load abundances ####
# Join mean (and sd) of abundances over all static scenarios in a single data frame
abund_sens <- bind_rows(
  # Default:
  # The function readRange() runs in the background of plotAbundance(). Here, we extract abundances per scenario by hand
  readRange(s,path_input) %>%
    group_by(Year) %>%
    summarise(Abundance = mean(NInds), sd = sd(NInds)) %>% add_column(Scenario = "0 - Default"),
  # Sensitivity 3
  readRange(s_dp_m5,path_input) %>%
    group_by(Year) %>%
    summarise(Abundance = mean(NInds), sd = sd(NInds)) %>% add_column(Scenario = "1 - Disp. Prob. -5%"),
  # Sensitivity 4
  readRange(s_dp_p5,path_input) %>%
    group_by(Year) %>%
    summarise(Abundance = mean(NInds), sd = sd(NInds)) %>% add_column(Scenario = "2 - Disp. Prob. +5%"),
  # Sensitivity 5
  readRange(s_dd_m5,path_input) %>%
    group_by(Year) %>%
    summarise(Abundance = mean(NInds), sd = sd(NInds)) %>% add_column(Scenario = "3 - Disp. Dist. -5%"),
  # Sensitivity 6
  readRange(s_dd_p5,path_input) %>%
    group_by(Year) %>%
    summarise(Abundance = mean(NInds), sd = sd(NInds)) %>% add_column(Scenario = "4 - Disp. Dist. +5%"),
)
#### Plotting ####
# Set color palette for scenarios excluding "Scenario 0 - Default"
scenario_colors <- rainbow(length(unique(abund_sens$Scenario)) - 1)
# Set color for "Scenario 0 - Default" to black
scenario_colors <- c("black", scenario_colors)
# Define the order of scenarios
scenario_order <- c("0 - Default",
                    "1 - Disp. Prob. -5%",
                    "2 - Disp. Prob. +5%",
                    "3 - Disp. Dist. -5%",
                    "4 - Disp. Dist. +5%")
# Convert Scenario to factor with desired levels
abund_sens$Scenario <- factor(abund_sens$Scenario, levels = scenario_order)
# Remove rows with NA in the Scenario column
#abund_sens <- abund_sens[!is.na(abund_sens$Scenario), ]
pdf(paste0(path_input, "Outputs/Plots_Parametertesting_wn_longDisp.pdf"))
p1 <- ggplot(data = abund_sens, mapping = aes(x = Year, y = Abundance, color = Scenario)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = Abundance - sd, ymax = Abundance + sd), linetype = 2, alpha = 0.1) +
  scale_color_manual(values = scenario_colors) +
  scale_fill_discrete(breaks = abund_sens$Scenario) +
  xlim(c(50,120))+
  ylim(c(0,2500))
grid.arrange(p1)
dev.off()



# # Calculate maximum number of individuals per replicate run and year
# pop <- readPop(s, path_input)
# sumInd <- pop %>% group_by(Rep,Year) %>% summarise(sumPop = sum(NInd), .groups='keep') 
# 
# write.table(sumInd, paste0(path_input, "Outputs/SumInd_Batch1.txt"), sep = ",", col.names = T, row.names = F)
# 
# # Plot results of Double Dispersalkernel (Abundance, Occupancy)
# 
# pdf(paste0(path_input, "Outputs/Plots_Batch1.pdf"))
# par(mfrow=c(2,2))
# plotAbundance(s, path_input, ylim=c(0,3000))
# plotOccupancy(s, path_input)
# plot(NULL, type = "n", ylab = "Abundance", xlab = "Year", xlim=c(60, 110), ylim=c(0,1500), cex.lab = 2.8, cex.axis = 2)
# for (i in 0:9) {
#   tmp <- sumInd %>% filter(Rep == i)
#   lines(tmp$Year, tmp$sumPop, type = "l", lwd = 0.8, col = "grey")
# }
# meanInd <- data.frame(Year = unique(sumInd$Year))
# meanInd <- sumInd %>% group_by(Year) %>% summarise(meanPop = mean(sumPop), 
#                                                    .groups = "keep")
# 
# lines(meanInd$Year, meanInd$meanPop, type = "l", lwd = 3, col = "red")
# dev.off()



#sumInd <- read.table("Dokumente/Arbeit - Zurell/SDM_Extinctions/Plots/SumInd_Batch1.txt", sep = ",", header = T)


# Code for plotting the occurrences in the landscape over time

# pop <- readPop(s, path_input)
# 
# #remove unimportant columns
# pop_short <- pop %>% dplyr::select(-c(RepSeason, Species))
# # extract occurrences
# occ_short <- subset(pop_short, pop_short$NInd >= 1)
# #change column names
# colnames(occ_short)[colnames(occ_short) == "x"] <- "X"
# colnames(occ_short)[colnames(occ_short) == "y"] <- "Y"
# # extract only the first replication
# occ_Rep0 <- subset(occ_short, occ_short$Rep == 0)
# for (i in 1:40) {
#   tmp <- rast(paste0(path_loop, "Inputs/habitat_per_cc", temp_rise[i], ".asc"))
# occ_sub <- subset(occ_Rep0, occ_Rep0$Year == i+99)
# occ_sub$X <- occ_sub$X * 1000
# occ_sub$Y <- occ_sub$Y * 1000
# m <- vect(occ_sub, geom = c("X", "Y"))
# if(length(m) >0){
# plot(tmp)
# plot(m, add = T)
# } else {
#   plot(tmp)
# }
# }

#clean all temporary files
gc()
rm(list=ls())