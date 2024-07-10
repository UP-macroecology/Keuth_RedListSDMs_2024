# Plot probability of occupancy

#Load packages
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(data.table)

# Text label plots
source("2_Simulations/scripts/text_labels_plots.R")

#colour palette
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0.01,0.8))

#Prepare lists for the different plots
o1 <- vector("list", length = 3)
o2 <- vector("list", length = 3)
o3 <- vector("list", length = 3)
o4 <- vector("list", length = 3)
o5 <- vector("list", length = 3)
o6 <- vector("list", length = 3)
o7 <- vector("list", length = 3)
o8 <- vector("list", length = 3)
o9 <- vector("list", length = 3)
o10 <- vector("list", length = 3)
o11 <- vector("list", length = 3)
o12 <- vector("list", length = 3)
o13 <- vector("list", length = 3)
o14 <- vector("list", length = 3)
o15 <- vector("list", length = 3)
o16 <- vector("list", length = 3)

#Plots
#Loading data - Scenario 1
occ <- fread("4_Analysis/data/Batch1_Sim1_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o1[[1]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch1_Sim2_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o1[[2]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch1_Sim3_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o1[[3]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))


#Loading data - Scenario 2
occ <- fread("4_Analysis/data/Batch2_Sim1_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o2[[1]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch2_Sim2_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o2[[2]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch2_Sim3_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o2[[3]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

#Loading data - Scenario 3
occ <- fread("4_Analysis/data/Batch3_Sim1_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o3[[1]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch3_Sim2_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o3[[2]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch3_Sim3_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o3[[3]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

#Loading data - Scenario 4
occ <- fread("4_Analysis/data/Batch4_Sim1_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o4[[1]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch4_Sim2_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o4[[2]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch4_Sim3_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o4[[3]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

#Loading data - Scenario 5
occ <- fread("4_Analysis/data/Batch5_Sim1_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o5[[1]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch5_Sim2_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o5[[2]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch5_Sim3_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o5[[3]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

#Loading data - Scenario 6
occ <- fread("4_Analysis/data/Batch6_Sim1_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o6[[1]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch6_Sim2_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o6[[2]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch6_Sim3_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o6[[3]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

#Loading data - Scenario 7
occ <- fread("4_Analysis/data/Batch7_Sim1_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o7[[1]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch7_Sim2_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o7[[2]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch7_Sim3_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o7[[3]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

#Loading data - Scenario 8
occ <- fread("4_Analysis/data/Batch8_Sim1_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o8[[1]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch8_Sim2_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o8[[2]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch8_Sim3_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o8[[3]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

#Loading data - Scenario 9
occ <- fread("4_Analysis/data/Batch9_Sim1_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o9[[1]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch9_Sim2_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o9[[2]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch9_Sim3_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o9[[3]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

#Loading data - Scenario 10
occ <- fread("4_Analysis/data/Batch10_Sim1_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o10[[1]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch10_Sim2_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o10[[2]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch10_Sim3_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o10[[3]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

#Loading data - Scenario 11
occ <- fread("4_Analysis/data/Batch11_Sim1_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o11[[1]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch11_Sim2_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o11[[2]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch11_Sim3_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o11[[3]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

#Loading data - Scenario 2
occ <- fread("4_Analysis/data/Batch12_Sim1_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o12[[1]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch12_Sim2_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o12[[2]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch12_Sim3_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o12[[3]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

#Loading data - Scenario 13
occ <- fread("4_Analysis/data/Batch13_Sim1_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o13[[1]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch13_Sim2_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o13[[2]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch13_Sim3_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o13[[3]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

#Loading data - Scenario 14
occ <- fread("4_Analysis/data/Batch14_Sim1_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o14[[1]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch14_Sim2_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o14[[2]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch14_Sim3_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o14[[3]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

#Loading data - Scenario 15
occ <- fread("4_Analysis/data/Batch15_Sim1_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o15[[1]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch15_Sim2_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o15[[2]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch15_Sim3_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o15[[3]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

#Loading data - Scenario 16
occ <- fread("4_Analysis/data/Batch16_Sim1_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o16[[1]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch16_Sim2_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o16[[2]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))

occ <- fread("4_Analysis/data/Batch16_Sim3_Land1_Occupancy.txt")
occ <- occ[,c("X", "Y", "Year_100")]
occ <- subset(occ, occ$Year_100 > 0)

o16[[3]] <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "none", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 15))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))


#extract common legend
legend <- ggplot(occ, aes(x=X, y= Y, color = Year_100))+
  geom_point()+
  sc+
  theme(legend.position = "bottom", axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA), axis.text = element_text(size = 12), legend.title = element_text(size = 20), 
        legend.text = element_text(size =17), legend.key.size = unit(0.6, "cm"), legend.key.width = unit(1, "cm"))+
  scale_x_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  scale_y_continuous(expand = c(0,01,0,01), limits = c(0,511))+
  labs(color = "Occupancy Probability")

shared_legend <- extract_legend(legend)

# create final plot
# plots for cold-adapted species
g3 <- grid.arrange(arrangeGrob(t0, t_l1, t_l2, t_l3,
                               l1, o1[[1]], o1[[2]], o1[[3]],
                               l2, o9[[1]], o9[[2]], o9[[3]],
                               l3, o5[[1]], o5[[2]], o5[[3]],
                               l4, o13[[1]], o13[[2]], o13[[3]],
                               l5, o3[[1]], o3[[2]], o3[[3]],
                               l6, o11[[1]], o11[[2]], o11[[3]],
                               l7, o7[[1]], o7[[2]], o7[[3]],
                               l8, o15[[1]], o15[[2]], o15[[3]],
                               ncol = 4, nrow = 9, heights= c(1,rep(5,8)), widths= c(2.3,4.9,4.9,4.9)), 
                   t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.5, 0.5), widths = c(11.7,0.3))

ggsave("4_Analysis/plots/Occurrence_cold.png", g3, height = 17, width = 10)

g4 <- grid.arrange(arrangeGrob(t0, t_l1, t_l2, t_l3,
                               l9, o2[[1]], o2[[2]], o2[[3]],
                               l10, o10[[1]], o10[[2]], o10[[3]],
                               l11, o6[[1]], o6[[2]], o6[[3]],
                               l12, o14[[1]], o14[[2]], o14[[3]],
                               l13, o4[[1]], o4[[2]], o4[[3]],
                               l14, o12[[1]], o12[[2]], o12[[3]],
                               l15, o8[[1]], o8[[2]], o8[[3]],
                               l16, o16[[1]], o16[[2]], o16[[3]],
                               ncol = 4, nrow = 9, heights= c(1,rep(5,8)), widths= c(2.3,4.9,4.9,4.9)), 
                   t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.5, 0.5), widths = c(11.7,0.3))



ggsave("4_Analysis/plots/Occurrence_warm.png", g4, height = 17, width = 10)
