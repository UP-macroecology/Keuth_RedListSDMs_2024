# Evaluating simulation results
# Looking at the abundances and extinctions of the different scenarios and landscapes

# Load packages
library(dplyr)
library(data.table)
library(ggplot2)
library(gridExtra)

# Load functions
source("2_Simulations/scripts/text_labels_plots.R")
source("Functions/extract_legend.R")

# create traits overview (same table as for the simulation)
sims <- expand.grid(optima = c("cold", "warm"), breadth = c("narrow", "wide"), rmax = c("slow", "fast"), dispersal = c("short", "long"))
sims$scenario_name <- paste(sims$optima, sims$breadth, sims$rmax, sims$dispersal, " ")

# Load and prepare data ------------
# The following code needs to be repeated for all three landscape replications. For this the number after "Sim" needs to be replaced every time a data set is loaded

# Load extinction data
extinction <- vector("list", 16)
for (i in 1:16) {
  tmp <- readRDS(paste0("2_Simulations/data/ExtProb_Batch", i, "_Sim3.rds"))
  #add scenario information
  tmp$scenario <- as.factor(i) 
  tmp$Year <- tmp$Year - 100
  tmp <- tmp[98:nrow(tmp),]
  tmp$optima <- sims[i,]$optima
  tmp$breadth <- sims[i,]$breadth
  tmp$rmax <- sims[i,]$rmax
  tmp$dispersal <- sims[i,]$dispersal
  extinction[[i]] <- tmp
  rm(tmp)
}

# Load abundance data
abundance <- vector("list", 16)
for (i in 1:16) {
  tmp <- readRDS(paste0("2_Simulations/data/SumInd_Batch", i, "_Sim3.rds"))
  #add scenario information
  tmp$scenario <- as.factor(i)
  tmp$optima <- sims[i,]$optima
  tmp$breadth <- sims[i,]$breadth
  tmp$rmax <- sims[i,]$rmax
  tmp$dispersal <- sims[i,]$dispersal
  tmp$scenario_name <- as.factor(sims[i,]$scenario_name)
  abundance[[i]] <- tmp
  rm(tmp)
}

#remove years of spin up and adjust year counting; calculate mean and sd abundance; calculate relative population size
years <- data.frame(Year = c(-2:max(extinction[[1]]$Year)))
abundance <- lapply(abundance, function(x){x <- x[!x$Year %in% c(0:97),]; x$Year <- x$Year - 100;
                                          x <- x %>% group_by(Year, scenario, optima, breadth, rmax, dispersal, scenario_name) %>% summarise(meanPop = mean(sumPop), sdPop = sd(sumPop), .groups = "keep");
                                          x <- as.data.frame(x); x$rel_pop <- NA;
                                          for (i in 4:nrow(x)){
                                            x[i, 10] <- x[i, 8] / x[3,8]
                                          };
                                          x[3,10] <- 1; 
                                          x <- left_join(years, x, by = "Year"); 
                                          x$scenario <- unique(na.omit(x$scenario)); 
                                          x$optima <- unique(na.omit(x$optima)); 
                                          x$breadth <- unique(na.omit(x$breadth)); 
                                          x$rmax <- unique(na.omit(x$rmax));
                                          x$dispersal <- unique(na.omit(x$dispersal));
                                          x$scenario_name <- unique(na.omit(x$scenario_name)); 
                                          x[is.na(x)] <- 0;
                                          x[1:2,10] <- NA;
                                          return(x)})

# Load habitat loss values
habitat_loss <- vector("list", 4)

habitat_loss[[1]] <- readRDS("2_Simulations/data/real_habitatloss_land3_optima0.27_breadth0.045.rds")
habitat_loss[[2]] <- readRDS("2_Simulations/data/real_habitatloss_land3_optima0.27_breadth0.055.rds")
habitat_loss[[3]] <- readRDS("2_Simulations/data/real_habitatloss_land3_optima0.5_breadth0.045.rds")
habitat_loss[[4]] <- readRDS("2_Simulations/data/real_habitatloss_land3_optima0.5_breadth0.055.rds")

habitat_loss <- lapply(habitat_loss, function(x){x <- left_join(years, x, by = "Year"); x[is.na(x)] <- 0; x[1:2,3] <- NA; return(x)})

#cbind the different data sets (extinctions, abundance, habitat loss)
df_simulations <- vector("list", 16)
for (i in 1:16) {
  df_simulations[[i]] <- abundance[[i]]
  df_simulations[[i]] <- cbind(df_simulations[[i]], extProb = extinction[[i]]$extProb)
}

#add habitat loss to comparison data
for (i in c(1,5,9,13)) {
  df_simulations[[i]] <- cbind(df_simulations[[i]], habitat_loss = habitat_loss[[1]]$habitat_loss)
}
for (i in c(2,6,10,14)) {
  df_simulations[[i]] <- cbind(df_simulations[[i]], habitat_loss = habitat_loss[[2]]$habitat_loss)
}
for (i in c(3,7,11,15)) {
  df_simulations[[i]] <- cbind(df_simulations[[i]], habitat_loss = habitat_loss[[3]]$habitat_loss)
}
for (i in c(4,8,12,16)) {
  df_simulations[[i]] <- cbind(df_simulations[[i]], habitat_loss = habitat_loss[[4]]$habitat_loss)
}

df_simulations_all <- do.call(rbind, df_simulations)


# Plot the abundances ----
a1 <- ggplot(df_simulations[[1]], aes(x = Year, y = meanPop, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = df_simulations[[5]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[9]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[13]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("mean Abundance"))+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA), 
        legend.position = "", axis.title.x = element_blank(), axis.title = element_text(size = 23), axis.text = element_text(size = 20))

a2 <- ggplot(df_simulations[[3]], aes(x = Year, y = meanPop, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = df_simulations[[7]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[11]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[15]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("mean Abundance"))+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA), 
        legend.position = "", axis.title.x = element_blank(), axis.title = element_text(size = 23), axis.text = element_text(size = 20))

a3 <- ggplot(df_simulations[[2]], aes(x = Year, y = meanPop, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = df_simulations[[6]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[10]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[14]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("mean Abundance"))+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA), 
        legend.position = "", axis.title.x = element_blank(), axis.title = element_text(size = 23), axis.text = element_text(size = 20))

a4 <- ggplot(df_simulations[[4]], aes(x = Year, y = meanPop, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = df_simulations[[8]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[12]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[16]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("mean Abundance"))+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA), 
        legend.position = "", axis.title.x = element_blank(), axis.title = element_text(size = 23), axis.text = element_text(size = 20))

legend <- ggplot(df_simulations[[4]], aes(x = Year, y = meanPop, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = df_simulations[[8]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[12]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[16]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("mean Abundance"))+
  theme(legend.spacing.y = unit(0.5, 'cm'), legend.title=element_text(size = 23), legend.text = element_text(size = 19), legend.position = "bottom", 
        legend.key.size = unit(1, "cm"))

shared_legend <- extract_legend(legend)

# Draw plots with shared legend
g <- grid.arrange(arrangeGrob(t0,t_c_medium, t_w_medium, t_nn, a1, a3, t_wn, a2, a4, ncol = 3, nrow = 3, heights= c(1,8,8), widths = c(1,5.5,5.5)),
                  t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.2, 0.8), widths = c(11.7,0.3))

# all abundances in single plots
ggplot(df_simulations_all, aes(x = Year, y = meanPop))+
  geom_line(linewidth = 1)+
  theme(strip.text = element_text(size = 14))+
  facet_wrap(~scenario_name)

# Extinction plots ---------
e1 <- ggplot(df_simulations[[1]], aes(x = Year, y = extProb, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = df_simulations[[5]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[9]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[13]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("Extinction probability"))+
  xlim(-3,80)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA), 
        legend.position = "", axis.title.x = element_blank(), axis.title = element_text(size = 23), axis.text = element_text(size = 20))

e2 <- ggplot(df_simulations[[3]], aes(x = Year, y = extProb, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = df_simulations[[7]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[11]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[15]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("Extinction probability"))+
  xlim(-3,80)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA), 
        legend.position = "", axis.title.x = element_blank(), axis.title = element_text(size = 23), axis.text = element_text(size = 20))

e3 <- ggplot(df_simulations[[2]], aes(x = Year, y = extProb, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = df_simulations[[6]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[10]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[14]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("Extinction probability"))+
  xlim(-3,80)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA), 
        legend.position = "", axis.title.x = element_blank(), axis.title = element_text(size = 23), axis.text = element_text(size = 20))

e4 <- ggplot(df_simulations[[4]], aes(x = Year, y = extProb, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = df_simulations[[8]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[12]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[16]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("Extinction probability"))+
  xlim(-3,80)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA), 
        legend.position = "", axis.title.x = element_blank(), axis.title = element_text(size = 23), axis.text = element_text(size = 20))

legend <- ggplot(df_simulations[[4]], aes(x = Year, y = extProb, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = df_simulations[[8]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[12]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = df_simulations[[16]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("mean extinction"))+
  theme(legend.spacing.y = unit(0.5, 'cm'), legend.title=element_text(size = 23), legend.text = element_text(size = 19), legend.position = "bottom", 
        legend.key.size = unit(1, "cm"))

shared_legend <- extract_legend(legend)

# Draw plots with shared legend
g <- grid.arrange(arrangeGrob(t0,t_c_medium, t_w_medium, t_nn, e1, e3, t_wn, e2, e4, ncol = 3, nrow = 3, heights= c(1,8,8), widths = c(1,5.5,5.5)),
                  t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.2, 0.8), widths = c(11.7,0.3))

# Plot huge comparison graph ----------
ggplot(df_simulations_all, aes(x= Year, y = rel_pop, colour = "Pop"))+
  geom_line(linewidth = 1)+
  geom_line(aes(x= Year, y= habitat_loss, colour = "Habitat"), linewidth = 1)+
  geom_line(aes(x = Year, y = extProb, colour = "Ext"), linewidth = 1)+
  theme(strip.text = element_text(size = 14), legend.text = element_text(size = 20), legend.key.size = unit(1, "cm"), legend.title = element_blank())+
  scale_color_manual(values = c("Pop" = "#FF6A6A", "Habitat" = "gold", "Ext" = "blue"), labels=c( "Extinction probability", "Population size", "Habitat size"))+
  ylab("Rate")+
  facet_wrap(~scenario_name)
