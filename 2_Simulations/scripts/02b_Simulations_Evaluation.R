# Evaluating simulation results

# Load packages
library(dplyr)
library(data.table)
library(ggplot2)
library(gridExtra)

# Load functions
source("2_Simulations/scripts/text_labels_plots.R")
source("Functions/extract_legend.R")

optima <- c("cold", "warm")
breadth <- c("narrow", "wide")
rmax <- c("slow", "fast")
dispersal <- c("short", "long")

sims <- expand.grid(optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)

# Plot abundance for the different landscapes and scenarios
abundance <- vector("list", 16)
for (i in 1:16) {
  tmp <- readRDS(paste0("2_Simulations/data/SumInd_Batch", i, "_Sim3.rds"))
  tmp$scenario <- i #add scenario to respective data set
  tmp$optima <- sims[i,]$optima
  tmp$breadth <- sims[i,]$breadth
  tmp$rmax <- sims[i,]$rmax
  tmp$dispersal <- sims[i,]$dispersal
  abundance[[i]] <- tmp
  rm(tmp)
}

#remove years of spin up and without population
abundance <- lapply(abundance, function(x){x <- x[!x$Year %in% c(0:97),];
                    x$scenario <- as.factor(x$scenario);
                    x$optima <- as.factor(x$optima);
                    x$breadth <- as.factor(x$breadth);
                    x$rmax <- as.factor(x$rmax);
                    x$dispersal <- as.factor(x$dispersal);
                    return(x)})

#calculate mean and sd over all replications, reduce Year so year 100 is 0
abundance <- lapply(abundance, function(x){x <- x %>% group_by(Year, scenario, optima, breadth, rmax, dispersal) %>% summarise(meanPop = mean(sumPop), sdPop = sd(sumPop), 
                                                                                       .groups = "keep");
                                      x <- as.data.frame(x); 
                                      x$Year <- x$Year - 100; return(x)})

a1 <- ggplot(abundance[[1]], aes(x = Year, y = meanPop, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = abundance[[5]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = abundance[[9]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = abundance[[13]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("mean Abundance"))+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA), 
        legend.position = "", axis.title.x = element_blank(), axis.title = element_text(size = 23), axis.text = element_text(size = 20))

a2 <- ggplot(abundance[[3]], aes(x = Year, y = meanPop, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = abundance[[7]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = abundance[[11]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = abundance[[15]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("mean Abundance"))+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA), 
        legend.position = "", axis.title.x = element_blank(), axis.title = element_text(size = 23), axis.text = element_text(size = 20))

a3 <- ggplot(abundance[[2]], aes(x = Year, y = meanPop, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = abundance[[6]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = abundance[[10]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = abundance[[14]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("mean Abundance"))+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA), 
        legend.position = "", axis.title.x = element_blank(), axis.title = element_text(size = 23), axis.text = element_text(size = 20))

a4 <- ggplot(abundance[[4]], aes(x = Year, y = meanPop, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = abundance[[8]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = abundance[[12]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = abundance[[16]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("mean Abundance"))+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA), 
        legend.position = "", axis.title.x = element_blank(), axis.title = element_text(size = 23), axis.text = element_text(size = 20))

legend <- ggplot(abundance[[4]], aes(x = Year, y = meanPop, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = abundance[[8]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = abundance[[12]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = abundance[[16]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("mean Abundance"))+
  theme(legend.spacing.y = unit(0.5, 'cm'), legend.title=element_text(size = 23), legend.text = element_text(size = 19), legend.position = "bottom", 
        legend.key.size = unit(1, "cm"))

shared_legend <- extract_legend(legend)

# Draw plots with shared legend
g <- grid.arrange(arrangeGrob(t0,t_c_medium, t_w_medium, t_nn, a1, a3, t_wn, a2, a4, ncol = 3, nrow = 3, heights= c(1,8,8), widths = c(1,5.5,5.5)),
                  t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.2, 0.8), widths = c(11.7,0.3))

# Extinction plots
extinction <- vector("list", 16)
for (i in 1:16) {
  tmp <- readRDS(paste0("2_Simulations/data/ExtProb_Batch", i, "_Sim3.rds"))
  tmp$scenario <- i #add scenario to the data set
  tmp$scenario <- as.factor(tmp$scenario)
  tmp$Year <- tmp$Year -100
  tmp$optima <- sims[i,]$optima
  tmp$breadth <- sims[i,]$breadth
  tmp$rmax <- sims[i,]$rmax
  tmp$dispersal <- sims[i,]$dispersal
  extinction[[i]] <- tmp
  rm(tmp)
}


e1 <- ggplot(extinction[[1]], aes(x = Year, y = extProb, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = extinction[[5]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = extinction[[9]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = extinction[[13]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("Extinction probability"))+
  xlim(-3,80)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA), 
        legend.position = "", axis.title.x = element_blank(), axis.title = element_text(size = 23), axis.text = element_text(size = 20))

e2 <- ggplot(extinction[[3]], aes(x = Year, y = extProb, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = extinction[[7]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = extinction[[11]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = extinction[[15]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("Extinction probability"))+
  xlim(-3,80)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA), 
        legend.position = "", axis.title.x = element_blank(), axis.title = element_text(size = 23), axis.text = element_text(size = 20))

e3 <- ggplot(extinction[[2]], aes(x = Year, y = extProb, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = extinction[[6]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = extinction[[10]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = extinction[[14]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("Extinction probability"))+
  xlim(-3,80)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA), 
        legend.position = "", axis.title.x = element_blank(), axis.title = element_text(size = 23), axis.text = element_text(size = 20))

e4 <- ggplot(extinction[[4]], aes(x = Year, y = extProb, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = extinction[[8]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = extinction[[12]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = extinction[[16]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("Extinction probability"))+
  xlim(-3,80)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA), 
        legend.position = "", axis.title.x = element_blank(), axis.title = element_text(size = 23), axis.text = element_text(size = 20))

legend <- ggplot(extinction[[4]], aes(x = Year, y = extProb, linetype = rmax, colour = dispersal))+
  geom_line(linewidth = 1.8)+
  geom_line(data = extinction[[8]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = extinction[[12]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  geom_line(data = extinction[[16]], aes(linetype = rmax, colour = dispersal), linewidth = 1.8)+
  ylab(c("mean extinction"))+
  theme(legend.spacing.y = unit(0.5, 'cm'), legend.title=element_text(size = 23), legend.text = element_text(size = 19), legend.position = "bottom", 
        legend.key.size = unit(1, "cm"))

shared_legend <- extract_legend(legend)

# Draw plots with shared legend
g <- grid.arrange(arrangeGrob(t0,t_c_medium, t_w_medium, t_nn, e1, e3, t_wn, e2, e4, ncol = 3, nrow = 3, heights= c(1,8,8), widths = c(1,5.5,5.5)),
                  t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.2, 0.8), widths = c(11.7,0.3))
