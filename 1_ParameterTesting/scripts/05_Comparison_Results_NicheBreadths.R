# Testing the results of different niche breadths
# Goal: Plot the habitat size, population size and extinction probability in one plot for the different niche breadths to see if a dispersal limitation is present (timely)
# Questions: Does the double kernel has an effect? Can I remove the dispersal limitations with increasing the niche breadths and by this the population size?

# repeat the same for the long dispersal with with different probabilities and the short dispersal

# Load packages
library(ggplot2)
library(gridExtra)

# Load functions
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

# different niche breadths ---------
#Load data
results_wo_long <- readRDS("1_ParameterTesting/data/results_nichebreadths_wo_longdisp.rds")

#Exract single data sets
pop_mean_wo_long <- results_wo_long[[1]]
extProb_list_wo_long <- results_wo_long[[2]]
real_rangechange_wo_long <- results_wo_long[[3]]

#correct years
real_rangechange_wo_long <- lapply(real_rangechange_wo_long, function(x){x$Year <- x$Year + 100; return(x)})

#calculate relative abundance to plot with other values
pop_mean_wo_long <- lapply(pop_mean_wo_long, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

# Plot combined plot
p1 <- ggplot(extProb_list_wo_long[[1]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data = real_rangechange_wo_long[[1]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_mean_wo_long[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  ylab("Rate")+
  ggtitle(unique(extProb_list_wo_long[[1]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p2 <- ggplot(extProb_list_wo_long[[2]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data = real_rangechange_wo_long[[2]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_mean_wo_long[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  ylab("Rate")+
  ggtitle(unique(extProb_list_wo_long[[2]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p3 <- ggplot(extProb_list_wo_long[[3]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data = real_rangechange_wo_long[[3]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_mean_wo_long[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  ylab("Rate")+
  ggtitle(unique(extProb_list_wo_long[[3]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p4 <- ggplot(extProb_list_wo_long[[4]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data = real_rangechange_wo_long[[4]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_mean_wo_long[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  ylab("Rate")+
  ggtitle(unique(extProb_list_wo_long[[4]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p5 <- ggplot(extProb_list_wo_long[[5]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data = real_rangechange_wo_long[[5]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_mean_wo_long[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  ylab("Rate")+
  ggtitle(unique(extProb_list_wo_long[[5]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p6 <- ggplot(extProb_list_wo_long[[6]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data = real_rangechange_wo_long[[6]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_mean_wo_long[[6]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  ylab("Rate")+
  ggtitle(unique(extProb_list_wo_long[[6]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p7 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = paste0("ntemp:0.25+variable; npre:0.5+variable\nK:0.05; Rmax:3; EmigProb:0.4; Dispersal:15000")) +
  theme_void()

legend <- ggplot(extProb_list[[1]], aes(x = Year, y = extProb, colour = "Ext"))+
  geom_line(linewidth = 1)+
  geom_line(data = real_rangechange[[1]], aes(x = Year, y = diff, colour = "Habitat"), linewidth = 1)+
  geom_line(data = pop_mean[[1]], aes(x = Year, y = rel_pop, colour = "Pop"), linewidth = 1)+
  ylab("Rate")+
  ggtitle(unique(extProb_list[[1]]$Breadth))+
  xlim(c(90, 170))+
  scale_color_manual(values = c("Pop" = "#FF6A6A", "Habitat" = "gold", "Ext" = "blue"), labels=c( "Extinction probability", "Population size", "Habitat size"))+
  labs(color=NULL)+
  theme(legend.text = element_text(size = 20), legend.key.size = unit(1, "cm"))

shared_legend <- extract_legend(legend)

# Draw plots with shared legend
grid.arrange(arrangeGrob(p1,p2,p3,p4,p5,p6, ncol = 3, nrow = 2),p7,shared_legend, ncol = 1, nrow = 3, heights = c(10,1.5,1.5))

# Comparison when including long dispersal
results <- readRDS("1_ParameterTesting/data/results_nichebreadths.rds")

pop_mean <- results[[1]]
extProb_list <- results[[2]]
real_rangechange <- results[[3]]

real_rangechange <- lapply(real_rangechange, function(x){x$Year <- x$Year + 100; return(x)})

#calculate relative abundance to plot within the plot
pop_mean <- lapply(pop_mean, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

p1.1 <- ggplot(extProb_list[[1]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data = real_rangechange[[1]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_mean[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  ylab("Rate")+
  ggtitle(unique(extProb_list[[1]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p2.1 <- ggplot(extProb_list[[2]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data = real_rangechange[[2]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_mean[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  ylab("Rate")+
  ggtitle(unique(extProb_list[[2]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p3.1 <- ggplot(extProb_list[[3]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data = real_rangechange[[3]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_mean[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  ylab("Rate")+
  ggtitle(unique(extProb_list[[3]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p4.1 <- ggplot(extProb_list[[4]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data = real_rangechange[[4]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_mean[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  ylab("Rate")+
  ggtitle(unique(extProb_list[[4]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p5.1 <- ggplot(extProb_list[[5]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data = real_rangechange[[5]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_mean[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  ylab("Rate")+
  ggtitle(unique(extProb_list[[5]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p6.1 <- ggplot(extProb_list[[6]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data = real_rangechange[[6]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_mean[[6]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  ylab("Rate")+
  ggtitle(unique(extProb_list[[6]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p7.1 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = paste0("ntemp:0.25+variable; npre:0.5+variable\nK:0.05; Rmax:3; EmigProb:0.4; Dispersal:15000, 250000, 0.95")) +
  theme_void()

grid.arrange(arrangeGrob(p1.1,p2.1,p3.1,p4.1,p5.1,p6.1, ncol = 3, nrow = 2),p7.1,shared_legend, ncol = 1, nrow = 3, heights = c(10,1.5,1.5))

# short and long dispersal with different probabilities ------

# Comparison when including long dispersal
results_shortdisp <- readRDS("1_ParameterTesting/data/results_nichebreadths_shortdisp.rds")
results_longdisp <- readRDS("1_ParameterTesting/data/results_nichebreadths_longdisp0.95.rds")
results_longdisp0.8 <- readRDS("1_ParameterTesting/data/results_nichebreadths_longdisp0.8.rds")
results_longdisp0.85 <- readRDS("1_ParameterTesting/data/results_nichebreadths_longdisp0.85.rds")
results_longdisp0.9 <- readRDS("1_ParameterTesting/data/results_nichebreadths_longdisp0.9.rds")
habitatsize <- readRDS("1_ParameterTesting/data/habitatloss_nichebreadths.rds")

# extract the single lists
pop_shortdisp <- results_shortdisp[[1]]
extProb_shortdisp <- results_shortdisp[[2]]

pop_longdisp <- results_longdisp[[1]]
extProb_longdisp <- results_longdisp[[2]]

pop_longdisp0.8 <- results_longdisp0.8[[1]]
extProb_longdisp0.8 <- results_longdisp0.8[[2]]

pop_longdisp0.85 <- results_longdisp0.85[[1]]
extProb_longdisp0.85 <- results_longdisp0.85[[2]]

pop_longdisp0.9 <- results_longdisp0.9[[1]]
extProb_longdisp0.9 <- results_longdisp0.9[[2]]

habitatsize <- lapply(habitatsize, function(x){x$Year <- x$Year + 100; return(x)})

#calculate relative abundance to plot within the plot
pop_shortdisp <- lapply(pop_shortdisp, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

pop_longdisp <- lapply(pop_longdisp, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

pop_longdisp0.8 <- lapply(pop_longdisp0.8, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

pop_longdisp0.85 <- lapply(pop_longdisp0.85, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

pop_longdisp0.9 <- lapply(pop_longdisp0.9, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

# Plot the results
p1 <- ggplot(extProb_shortdisp[[1]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[1]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[1]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[1]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p2 <- ggplot(extProb_shortdisp[[2]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[2]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[2]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[2]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p3 <- ggplot(extProb_shortdisp[[3]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[3]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[3]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[3]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p4 <- ggplot(extProb_shortdisp[[4]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[4]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[4]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[4]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p5 <- ggplot(extProb_shortdisp[[5]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[5]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[5]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[5]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p7 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = paste0("ntemp:0.25+variable; npre:0.5+variable\nK:0.05; Rmax:3; EmigProb:0.4; Dispersal:5000,15000, 250000, 0.95\nlandscape 1")) +
  theme_void()

legend <- ggplot(extProb_shortdisp[[1]], aes(x = Year, y = extProb, colour = "Ext"))+
  geom_line(linewidth = 1)+
  geom_line(data = habitatsize[[1]], aes(x = Year, y = diff, colour = "Habitat"), linewidth = 1)+
  geom_line(data = pop_shortdisp[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "short"), linewidth = 1)+
  geom_line(data = pop_longdisp[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long"), linewidth = 1)+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[1]]$Breadth))+
  xlim(c(90, 170))+
  scale_color_manual(values = c("Pop" = "#FF6A6A", "Habitat" = "gold", "Ext" = "blue"), labels=c( "Extinction probability", "Population size", "Habitat size"))+
  scale_linetype_manual(values = c("long" = "dashed", "short" = "solid"), labels = c("long dispersal", "short dispersal"))+
  labs(color=NULL, linetype = NULL)+
  theme(legend.text = element_text(size = 20), legend.key.size = unit(1, "cm"), legend.position = "bottom")
  

shared_legend <- extract_legend(legend)

grid.arrange(arrangeGrob(p1,p2,p3,p4,p5, ncol = 3, nrow = 2),p7,shared_legend, ncol = 1, nrow = 3, heights = c(10,1.5,1.5))

# different probabilities for long dispersal

p1.1 <- ggplot(extProb_longdisp0.8[[1]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[1]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[1]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[1]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[1]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[1]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p2.1 <- ggplot(extProb_longdisp0.8[[2]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[2]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[2]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[2]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[2]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[2]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p3.1 <- ggplot(extProb_longdisp0.8[[3]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[3]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[3]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[3]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[3]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[3]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p4.1 <- ggplot(extProb_longdisp0.8[[4]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[4]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[4]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[4]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[4]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[4]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p5.1 <- ggplot(extProb_longdisp0.8[[5]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[5]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[5]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[5]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[5]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[5]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p7.1 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = paste0("ntemp:0.25+variable; npre:0.5+variable\nK:0.05; Rmax:3; EmigProb:0.4; Dispersal:15000, 250000, variable\nlandscape 1")) +
  theme_void()

legend <- ggplot(extProb_longdisp0.8[[1]], aes(x = Year, y = extProb, colour = "Ext"))+
  geom_line(linewidth = 1)+
  geom_line(data = habitatsize[[1]], aes(x = Year, y = diff, colour = "Habitat"), linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long0.8"), linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long0.85"), linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long0.9"), linewidth = 1)+
  geom_line(data = pop_longdisp[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long"), linewidth = 1)+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[1]]$Breadth))+
  xlim(c(90, 170))+
  scale_color_manual(values = c("Pop" = "#FF6A6A", "Habitat" = "gold", "Ext" = "blue"), labels=c( "Extinction probability", "Population size", "Habitat size"))+
  scale_linetype_manual(values = c("long0.8" = "solid", "long0.85" = "dashed", "long0.9" = "dotted", "long" = "twodash"), labels = c("0.95", "0.8", "0.85", "0.9"))+
  labs(color=NULL, linetype = "Dispersal probability")+
  theme(legend.text = element_text(size = 20), legend.key.size = unit(1, "cm"), legend.position = "bottom", legend.title = element_text(size = 18))


shared_legend <- extract_legend(legend)

grid.arrange(arrangeGrob(p1.1,p2.1,p3.1,p4.1,p5.1, ncol = 3, nrow = 2),p7.1,shared_legend, ncol = 1, nrow = 3, heights = c(10,1.5,1.5))

# look at the real abundances and occupancy values

p1.2 <- ggplot(pop_longdisp[[1]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[1]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[1]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[1]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[1]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[1]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

p2.2 <- ggplot(pop_longdisp[[2]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[2]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[2]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[2]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[2]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[2]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

p3.2 <- ggplot(pop_longdisp[[3]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[3]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[3]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[3]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[3]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[3]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

p4.2 <- ggplot(pop_longdisp[[4]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[4]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[4]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[4]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[4]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[4]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

p5.2 <- ggplot(pop_longdisp[[5]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[5]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[5]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[5]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[5]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[5]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

legend <- ggplot(pop_longdisp[[5]], aes(x= Year, y = Abundance, color = "0.95"))+
  geom_line(linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[5]], aes(x=Year, y= Abundance, color = "0.8"), linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[5]], aes(x=Year, y= Abundance, color = "0.85"), linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[5]], aes(x=Year, y= Abundance, color = "0.9"), linewidth = 1)+
  geom_line(data = pop_shortdisp[[5]], aes(x=Year, y= Abundance, color = "short"), linewidth = 1)+
  ggtitle(unique(pop_longdisp[[5]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))+
  scale_color_manual(values = c("0.95" = "#FF0000", "0.8" = "#00FF66", "0.85" = "#0066FF", "0.9" = "#CC00FF", "short" = "black"), labels=c( "0.8", "0.85", "0.9", "0.95", "short dispersal"))+
  theme(legend.text = element_text(size = 20), legend.key.size = unit(1, "cm"), legend.position = "bottom", legend.title = element_text(size = 18))+
  labs(color="dispersal probability")

shared_legend <- extract_legend(legend)

grid.arrange(arrangeGrob(p1.2,p2.2,p3.2,p4.2,p5.2, ncol = 3, nrow = 2),shared_legend, ncol = 1, nrow = 2, heights = c(11,1.5))

# short and long dispersal with different probabilities for Rmax 5 ------

# Comparison when including long dispersal
results_shortdisp <- readRDS("1_ParameterTesting/data/results_nichebreadths_shortdisp_Rmax5.rds")
results_longdisp <- readRDS("1_ParameterTesting/data/results_nichebreadths_longdisp0.95_Rmax5.rds")
results_longdisp0.8 <- readRDS("1_ParameterTesting/data/results_nichebreadths_longdisp0.8_Rmax5.rds")
results_longdisp0.85 <- readRDS("1_ParameterTesting/data/results_nichebreadths_longdisp0.85_Rmax5.rds")
results_longdisp0.9 <- readRDS("1_ParameterTesting/data/results_nichebreadths_longdisp0.9_Rmax5.rds")
habitatsize <- readRDS("1_ParameterTesting/data/habitatloss_nichebreadths.rds")

# extract the single lists
pop_shortdisp <- results_shortdisp[[1]]
extProb_shortdisp <- results_shortdisp[[2]]

pop_longdisp <- results_longdisp[[1]]
extProb_longdisp <- results_longdisp[[2]]

pop_longdisp0.8 <- results_longdisp0.8[[1]]
extProb_longdisp0.8 <- results_longdisp0.8[[2]]

pop_longdisp0.85 <- results_longdisp0.85[[1]]
extProb_longdisp0.85 <- results_longdisp0.85[[2]]

pop_longdisp0.9 <- results_longdisp0.9[[1]]
extProb_longdisp0.9 <- results_longdisp0.9[[2]]

habitatsize <- lapply(habitatsize, function(x){x$Year <- x$Year + 100; return(x)})

#calculate relative abundance to plot within the plot
pop_shortdisp <- lapply(pop_shortdisp, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

pop_longdisp <- lapply(pop_longdisp, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

pop_longdisp0.8 <- lapply(pop_longdisp0.8, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

pop_longdisp0.85 <- lapply(pop_longdisp0.85, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

pop_longdisp0.9 <- lapply(pop_longdisp0.9, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

# Plot the results
p1 <- ggplot(extProb_shortdisp[[1]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[1]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[1]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[1]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p2 <- ggplot(extProb_shortdisp[[2]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[2]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[2]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[2]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p3 <- ggplot(extProb_shortdisp[[3]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[3]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[3]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[3]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p4 <- ggplot(extProb_shortdisp[[4]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[4]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[4]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[4]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p5 <- ggplot(extProb_shortdisp[[5]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[5]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[5]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[5]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p7 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = paste0("ntemp:0.25+variable; npre:0.5+variable\nK:0.05; Rmax:5; EmigProb:0.4; Dispersal:15000, 250000, 0.95")) +
  theme_void()

legend <- ggplot(extProb_shortdisp[[1]], aes(x = Year, y = extProb, colour = "Ext"))+
  geom_line(linewidth = 1)+
  geom_line(data = habitatsize[[1]], aes(x = Year, y = diff, colour = "Habitat"), linewidth = 1)+
  geom_line(data = pop_shortdisp[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "short"), linewidth = 1)+
  geom_line(data = pop_longdisp[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long"), linewidth = 1)+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[1]]$Breadth))+
  xlim(c(90, 170))+
  scale_color_manual(values = c("Pop" = "#FF6A6A", "Habitat" = "gold", "Ext" = "blue"), labels=c( "Extinction probability", "Population size", "Habitat size"))+
  scale_linetype_manual(values = c("long" = "dashed", "short" = "solid"), labels = c("long dispersal", "short dispersal"))+
  labs(color=NULL, linetype = NULL)+
  theme(legend.text = element_text(size = 20), legend.key.size = unit(1, "cm"), legend.position = "bottom")


shared_legend <- extract_legend(legend)

grid.arrange(arrangeGrob(p1,p2,p3,p4,p5, ncol = 3, nrow = 2),p7,shared_legend, ncol = 1, nrow = 3, heights = c(10,1.5,1.5))

# different probabilities for long dispersal

p1.1 <- ggplot(extProb_longdisp0.8[[1]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[1]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[1]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[1]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[1]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[1]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p2.1 <- ggplot(extProb_longdisp0.8[[2]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[2]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[2]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[2]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[2]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[2]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p3.1 <- ggplot(extProb_longdisp0.8[[3]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[3]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[3]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[3]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[3]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[3]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p4.1 <- ggplot(extProb_longdisp0.8[[4]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[4]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[4]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[4]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[4]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[4]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p5.1 <- ggplot(extProb_longdisp0.8[[5]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[5]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[5]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[5]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[5]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[5]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))+
  theme(plot.title = element_text(size = 20))

p7.1 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = paste0("ntemp:0.25+variable; npre:0.5+variable\nK:0.05; Rmax:5; EmigProb:0.4; Dispersal:15000, 250000, variable")) +
  theme_void()

legend <- ggplot(extProb_longdisp0.8[[1]], aes(x = Year, y = extProb, colour = "Ext"))+
  geom_line(linewidth = 1)+
  geom_line(data = habitatsize[[1]], aes(x = Year, y = diff, colour = "Habitat"), linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long0.8"), linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long0.85"), linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long0.9"), linewidth = 1)+
  geom_line(data = pop_longdisp[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long"), linewidth = 1)+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[1]]$Breadth))+
  xlim(c(90, 170))+
  scale_color_manual(values = c("Pop" = "#FF6A6A", "Habitat" = "gold", "Ext" = "blue"), labels=c( "Extinction probability", "Population size", "Habitat size"))+
  scale_linetype_manual(values = c("long0.8" = "solid", "long0.85" = "dashed", "long0.9" = "dotted", "long" = "twodash"), labels = c("0.95", "0.8", "0.85", "0.9"))+
  labs(color=NULL, linetype = "Dispersal probability")+
  theme(legend.text = element_text(size = 20), legend.key.size = unit(1, "cm"), legend.position = "bottom", legend.title = element_text(size = 18))


shared_legend <- extract_legend(legend)

grid.arrange(arrangeGrob(p1.1,p2.1,p3.1,p4.1,p5.1, ncol = 3, nrow = 2),p7.1,shared_legend, ncol = 1, nrow = 3, heights = c(10,1.5,1.5))

# look at the real abundances and occupancy values

p1.2 <- ggplot(pop_longdisp[[1]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[1]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[1]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[1]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[1]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[1]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

p2.2 <- ggplot(pop_longdisp[[2]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[2]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[2]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[2]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[2]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[2]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

p3.2 <- ggplot(pop_longdisp[[3]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[3]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[3]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[3]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[3]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[3]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

p4.2 <- ggplot(pop_longdisp[[4]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[4]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[4]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[4]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[4]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[4]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

p5.2 <- ggplot(pop_longdisp[[5]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[5]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[5]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[5]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[5]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[5]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

legend <- ggplot(pop_longdisp[[5]], aes(x= Year, y = Abundance, color = "0.95"))+
  geom_line(linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[5]], aes(x=Year, y= Abundance, color = "0.8"), linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[5]], aes(x=Year, y= Abundance, color = "0.85"), linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[5]], aes(x=Year, y= Abundance, color = "0.9"), linewidth = 1)+
  geom_line(data = pop_shortdisp[[5]], aes(x=Year, y= Abundance, color = "short"), linewidth = 1)+
  ggtitle(unique(pop_longdisp[[5]]$Breadth))+
  ylim(c(0,max(pop_longdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))+
  scale_color_manual(values = c("0.95" = "#FF0000", "0.8" = "#00FF66", "0.85" = "#0066FF", "0.9" = "#CC00FF", "short" = "black"), labels=c( "0.8", "0.85", "0.9", "0.95", "short dispersal"))+
  theme(legend.text = element_text(size = 20), legend.key.size = unit(1, "cm"), legend.position = "bottom", legend.title = element_text(size = 18))+
  labs(color="dispersal probability")

shared_legend <- extract_legend(legend)

grid.arrange(arrangeGrob(p1.2,p2.2,p3.2,p4.2,p5.2, ncol = 3, nrow = 2),shared_legend, ncol = 1, nrow = 2, heights = c(11,1.5))

# short and long dispersal for Rmax 3 and the second landscape --------------

# Comparison when including long dispersal
results_shortdisp <- readRDS("1_ParameterTesting/data/results_nichebreadths_shortdisp_Rmax3_land2.rds")
results_longdisp0.8 <- readRDS("1_ParameterTesting/data/results_nichebreadths_longdisp0.8_Rmax3_land2.rds")
results_longdisp0.85 <- readRDS("1_ParameterTesting/data/results_nichebreadths_longdisp0.85_Rmax3_land2.rds")
results_longdisp0.9 <- readRDS("1_ParameterTesting/data/results_nichebreadths_longdisp0.9_Rmax3_land2.rds")
results_longdisp <- readRDS("1_ParameterTesting/data/results_nichebreadths_longdisp_Rmax3_land2.rds")
habitatsize <- readRDS("1_ParameterTesting/data/habitatloss_nichebreadths_land2.rds")

# extract the single lists
pop_shortdisp <- results_shortdisp[[1]]
extProb_shortdisp <- results_shortdisp[[2]]

pop_longdisp <- results_longdisp[[1]]
extProb_longdisp <- results_longdisp[[2]]

pop_longdisp0.8 <- results_longdisp0.8[[1]]
extProb_longdisp0.8 <- results_longdisp0.8[[2]]

pop_longdisp0.85 <- results_longdisp0.85[[1]]
extProb_longdisp0.85 <- results_longdisp0.85[[2]]

pop_longdisp0.9 <- results_longdisp0.9[[1]]
extProb_longdisp0.9 <- results_longdisp0.9[[2]]

habitatsize <- lapply(habitatsize, function(x){x$Year <- x$Year + 100; return(x)})

#calculate relative abundance to plot within the plot
pop_shortdisp <- lapply(pop_shortdisp, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

pop_longdisp <- lapply(pop_longdisp, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

pop_longdisp0.8 <- lapply(pop_longdisp0.8, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

pop_longdisp0.85 <- lapply(pop_longdisp0.85, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

pop_longdisp0.9 <- lapply(pop_longdisp0.9, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

# Plot the results
p1 <- ggplot(extProb_shortdisp[[1]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[1]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[1]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[1]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.2))+
  theme(plot.title = element_text(size = 20))

p2 <- ggplot(extProb_shortdisp[[2]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[2]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[2]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[2]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.2))+
  theme(plot.title = element_text(size = 20))

p3 <- ggplot(extProb_shortdisp[[3]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[3]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[3]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[3]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.2))+
  theme(plot.title = element_text(size = 20))

p4 <- ggplot(extProb_shortdisp[[4]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[4]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[4]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[4]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.2))+
  theme(plot.title = element_text(size = 20))

p5 <- ggplot(extProb_shortdisp[[5]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[5]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[5]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[5]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.2))+
  theme(plot.title = element_text(size = 20))

p7 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = paste0("ntemp:0.5+variable; npre:0.5+variable\nK:0.05; Rmax:3; EmigProb:0.4; Dispersal:15000, 250000, 0.95\nlandscape 2")) +
  theme_void()

legend <- ggplot(extProb_shortdisp[[1]], aes(x = Year, y = extProb, colour = "Ext"))+
  geom_line(linewidth = 1)+
  geom_line(data = habitatsize[[1]], aes(x = Year, y = diff, colour = "Habitat"), linewidth = 1)+
  geom_line(data = pop_shortdisp[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "short"), linewidth = 1)+
  geom_line(data = pop_longdisp[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long"), linewidth = 1)+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[1]]$Breadth))+
  xlim(c(90, 170))+
  scale_color_manual(values = c("Pop" = "#FF6A6A", "Habitat" = "gold", "Ext" = "blue"), labels=c( "Extinction probability", "Population size", "Habitat size"))+
  scale_linetype_manual(values = c("long" = "dashed", "short" = "solid"), labels = c("long dispersal", "short dispersal"))+
  labs(color=NULL, linetype = NULL)+
  theme(legend.text = element_text(size = 20), legend.key.size = unit(1, "cm"), legend.position = "bottom")


shared_legend <- extract_legend(legend)

grid.arrange(arrangeGrob(p1,p2,p3,p4,p5, ncol = 3, nrow = 2),p7,shared_legend, ncol = 1, nrow = 3, heights = c(10,1.5,1.5))

# different dispersal probabilities
p1.1 <- ggplot(extProb_longdisp0.8[[1]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[1]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[1]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[1]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[1]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[1]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.2))+
  theme(plot.title = element_text(size = 20))

p2.1 <- ggplot(extProb_longdisp0.8[[2]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[2]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[2]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[2]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[2]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[2]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.2))+
  theme(plot.title = element_text(size = 20))

p3.1 <- ggplot(extProb_longdisp0.8[[3]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[3]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[3]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[3]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[3]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[3]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.2))+
  theme(plot.title = element_text(size = 20))

p4.1 <- ggplot(extProb_longdisp0.8[[4]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[4]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[4]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[4]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[4]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[4]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.2))+
  theme(plot.title = element_text(size = 20))

p5.1 <- ggplot(extProb_longdisp0.8[[5]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[5]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[5]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[5]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[5]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[5]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.2))+
  theme(plot.title = element_text(size = 20))

p7.1 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = paste0("ntemp:0.25+variable; npre:0.5+variable\nK:0.05; Rmax:5; EmigProb:0.4; Dispersal:15000, 250000, variable\nlandscape 2")) +
  theme_void()

legend <- ggplot(extProb_longdisp0.8[[1]], aes(x = Year, y = extProb, colour = "Ext"))+
  geom_line(linewidth = 1)+
  geom_line(data = habitatsize[[1]], aes(x = Year, y = diff, colour = "Habitat"), linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long0.8"), linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long0.85"), linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long0.9"), linewidth = 1)+
  geom_line(data = pop_longdisp[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long"), linewidth = 1)+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[1]]$Breadth))+
  xlim(c(90, 170))+
  scale_color_manual(values = c("Pop" = "#FF6A6A", "Habitat" = "gold", "Ext" = "blue"), labels=c( "Extinction probability", "Population size", "Habitat size"))+
  scale_linetype_manual(values = c("long0.8" = "solid", "long0.85" = "dashed", "long0.9" = "dotted", "long" = "twodash"), labels = c("0.95", "0.8", "0.85", "0.9"))+
  labs(color=NULL, linetype = "Dispersal probability")+
  theme(legend.text = element_text(size = 20), legend.key.size = unit(1, "cm"), legend.position = "bottom", legend.title = element_text(size = 18))


shared_legend <- extract_legend(legend)

grid.arrange(arrangeGrob(p1.1,p2.1,p3.1,p4.1,p5.1, ncol = 3, nrow = 2),p7.1,shared_legend, ncol = 1, nrow = 3, heights = c(10,1.5,1.5))

# look at the real abundances and occupancy values

p1.2 <- ggplot(pop_longdisp[[1]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[1]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[1]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[1]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[1]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[1]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

p2.2 <- ggplot(pop_longdisp[[2]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[2]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[2]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[2]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[2]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[2]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

p3.2 <- ggplot(pop_longdisp[[3]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[3]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[3]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[3]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[3]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[3]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

p4.2 <- ggplot(pop_longdisp[[4]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[4]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[4]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[4]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[4]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[4]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

p5.2 <- ggplot(pop_longdisp[[5]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[5]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[5]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[5]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[5]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[5]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

legend <- ggplot(pop_longdisp[[5]], aes(x= Year, y = Abundance, color = "0.95"))+
  geom_line(linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[5]], aes(x=Year, y= Abundance, color = "0.8"), linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[5]], aes(x=Year, y= Abundance, color = "0.85"), linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[5]], aes(x=Year, y= Abundance, color = "0.9"), linewidth = 1)+
  geom_line(data = pop_shortdisp[[5]], aes(x=Year, y= Abundance, color = "short"), linewidth = 1)+
  ggtitle(unique(pop_longdisp[[5]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))+
  scale_color_manual(values = c("0.95" = "#FF0000", "0.8" = "#00FF66", "0.85" = "#0066FF", "0.9" = "#CC00FF", "short" = "black"), labels=c( "0.8", "0.85", "0.9", "0.95", "short dispersal"))+
  theme(legend.text = element_text(size = 20), legend.key.size = unit(1, "cm"), legend.position = "bottom", legend.title = element_text(size = 18))+
  labs(color="dispersal probability")

shared_legend <- extract_legend(legend)

grid.arrange(arrangeGrob(p1.2,p2.2,p3.2,p4.2,p5.2, ncol = 3, nrow = 2),shared_legend, ncol = 1, nrow = 2, heights = c(11,1.5))

# short and long dispersal for Rmax 3 and the third landscape --------------

# Comparison when including long dispersal
results_shortdisp <- readRDS("1_ParameterTesting/data/results_nichebreadths_shortdisp_Rmax3_land3.rds")
results_longdisp0.8 <- readRDS("1_ParameterTesting/data/results_nichebreadths_longdisp0.8_Rmax3_land3.rds")
results_longdisp0.85 <- readRDS("1_ParameterTesting/data/results_nichebreadths_longdisp0.85_Rmax3_land3.rds")
results_longdisp0.9 <- readRDS("1_ParameterTesting/data/results_nichebreadths_longdisp0.9_Rmax3_land3.rds")
results_longdisp <- readRDS("1_ParameterTesting/data/results_nichebreadths_longdisp_Rmax3_land3.rds")
habitatsize <- readRDS("1_ParameterTesting/data/habitatloss_nichebreadths_land3.rds")

# extract the single lists
pop_shortdisp <- results_shortdisp[[1]]
extProb_shortdisp <- results_shortdisp[[2]]

pop_longdisp <- results_longdisp[[1]]
extProb_longdisp <- results_longdisp[[2]]

pop_longdisp0.8 <- results_longdisp0.8[[1]]
extProb_longdisp0.8 <- results_longdisp0.8[[2]]

pop_longdisp0.85 <- results_longdisp0.85[[1]]
extProb_longdisp0.85 <- results_longdisp0.85[[2]]

pop_longdisp0.9 <- results_longdisp0.9[[1]]
extProb_longdisp0.9 <- results_longdisp0.9[[2]]

habitatsize <- lapply(habitatsize, function(x){x$Year <- x$Year + 100; return(x)})

#calculate relative abundance to plot within the plot
pop_shortdisp <- lapply(pop_shortdisp, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

pop_longdisp <- lapply(pop_longdisp, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

pop_longdisp0.8 <- lapply(pop_longdisp0.8, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

pop_longdisp0.85 <- lapply(pop_longdisp0.85, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

pop_longdisp0.9 <- lapply(pop_longdisp0.9, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})

# Plot the results
p1 <- ggplot(extProb_shortdisp[[1]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[1]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[1]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[1]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.6))+
  theme(plot.title = element_text(size = 20))

p2 <- ggplot(extProb_shortdisp[[2]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[2]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[2]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[2]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.6))+
  theme(plot.title = element_text(size = 20))

p3 <- ggplot(extProb_shortdisp[[3]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[3]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[3]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[3]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.6))+
  theme(plot.title = element_text(size = 20))

p4 <- ggplot(extProb_shortdisp[[4]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[4]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[4]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[4]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.6))+
  theme(plot.title = element_text(size = 20))

p5 <- ggplot(extProb_shortdisp[[5]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp[[5]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data = habitatsize[[5]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_shortdisp[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[5]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.6))+
  theme(plot.title = element_text(size = 20))

p7 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = paste0("ntemp:0.5+variable; npre:0.5+variable\nK:0.05; Rmax:3; EmigProb:0.4; Dispersal:15000, 250000, 0.95\nlandscape 3")) +
  theme_void()

legend <- ggplot(extProb_shortdisp[[1]], aes(x = Year, y = extProb, colour = "Ext"))+
  geom_line(linewidth = 1)+
  geom_line(data = habitatsize[[1]], aes(x = Year, y = diff, colour = "Habitat"), linewidth = 1)+
  geom_line(data = pop_shortdisp[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "short"), linewidth = 1)+
  geom_line(data = pop_longdisp[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long"), linewidth = 1)+
  ylab("Rate")+
  ggtitle(unique(extProb_shortdisp[[1]]$Breadth))+
  xlim(c(90, 170))+
  scale_color_manual(values = c("Pop" = "#FF6A6A", "Habitat" = "gold", "Ext" = "blue"), labels=c( "Extinction probability", "Population size", "Habitat size"))+
  scale_linetype_manual(values = c("long" = "dashed", "short" = "solid"), labels = c("long dispersal", "short dispersal"))+
  labs(color=NULL, linetype = NULL)+
  theme(legend.text = element_text(size = 20), legend.key.size = unit(1, "cm"), legend.position = "bottom")


shared_legend <- extract_legend(legend)

grid.arrange(arrangeGrob(p1,p2,p3,p4,p5, ncol = 3, nrow = 2),p7,shared_legend, ncol = 1, nrow = 3, heights = c(10,1.5,1.5))

# different dispersal probabilities
p1.1 <- ggplot(extProb_longdisp0.8[[1]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[1]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[1]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[1]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[1]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[1]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.6))+
  theme(plot.title = element_text(size = 20))

p2.1 <- ggplot(extProb_longdisp0.8[[2]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[2]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[2]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[2]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[2]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[2]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.6))+
  theme(plot.title = element_text(size = 20))

p3.1 <- ggplot(extProb_longdisp0.8[[3]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[3]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[3]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[3]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[3]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[3]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.6))+
  theme(plot.title = element_text(size = 20))

p4.1 <- ggplot(extProb_longdisp0.8[[4]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[4]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[4]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[4]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[4]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[4]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.6))+
  theme(plot.title = element_text(size = 20))

p5.1 <- ggplot(extProb_longdisp0.8[[5]], aes(x = Year, y = extProb))+
  geom_line(colour = "blue", linewidth = 1)+
  geom_line(data= extProb_longdisp0.85[[5]], colour = "blue", linewidth = 1, linetype = "dashed")+
  geom_line(data= extProb_longdisp0.9[[5]], colour = "blue", linewidth = 1, linetype = "dotted")+
  geom_line(data= extProb_longdisp[[5]], colour = "blue", linewidth = 1, linetype = "twodash")+
  geom_line(data = habitatsize[[5]], aes(x = Year, y = diff), linewidth = 1, colour = "#FF6A6A")+
  geom_line(data = pop_longdisp0.8[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold")+
  geom_line(data = pop_longdisp0.85[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dashed")+
  geom_line(data = pop_longdisp0.9[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "dotted")+
  geom_line(data = pop_longdisp[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "gold", linetype = "twodash")+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[5]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.6))+
  theme(plot.title = element_text(size = 20))

p7.1 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = paste0("ntemp:0.25+variable; npre:0.5+variable\nK:0.05; Rmax:5; EmigProb:0.4; Dispersal:15000, 250000, variable\nlandscape 3")) +
  theme_void()

legend <- ggplot(extProb_longdisp0.8[[1]], aes(x = Year, y = extProb, colour = "Ext"))+
  geom_line(linewidth = 1)+
  geom_line(data = habitatsize[[1]], aes(x = Year, y = diff, colour = "Habitat"), linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long0.8"), linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long0.85"), linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long0.9"), linewidth = 1)+
  geom_line(data = pop_longdisp[[1]], aes(x = Year, y = rel_pop, colour = "Pop", linetype = "long"), linewidth = 1)+
  ylab("Rate")+
  ggtitle(unique(extProb_longdisp[[1]]$Breadth))+
  xlim(c(90, 170))+
  scale_color_manual(values = c("Pop" = "#FF6A6A", "Habitat" = "gold", "Ext" = "blue"), labels=c( "Extinction probability", "Population size", "Habitat size"))+
  scale_linetype_manual(values = c("long0.8" = "solid", "long0.85" = "dashed", "long0.9" = "dotted", "long" = "twodash"), labels = c("0.95", "0.8", "0.85", "0.9"))+
  labs(color=NULL, linetype = "Dispersal probability")+
  theme(legend.text = element_text(size = 20), legend.key.size = unit(1, "cm"), legend.position = "bottom", legend.title = element_text(size = 18))


shared_legend <- extract_legend(legend)

grid.arrange(arrangeGrob(p1.1,p2.1,p3.1,p4.1,p5.1, ncol = 3, nrow = 2),p7.1,shared_legend, ncol = 1, nrow = 3, heights = c(10,1.5,1.5))

# look at the real abundances and occupancy values

p1.2 <- ggplot(pop_longdisp[[1]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[1]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[1]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[1]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[1]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[1]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

p2.2 <- ggplot(pop_longdisp[[2]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[2]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[2]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[2]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[2]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[2]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

p3.2 <- ggplot(pop_longdisp[[3]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[3]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[3]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[3]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[3]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[3]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

p4.2 <- ggplot(pop_longdisp[[4]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[4]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[4]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[4]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[4]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[4]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

p5.2 <- ggplot(pop_longdisp[[5]], aes(x= Year, y = Abundance))+
  geom_line(color = "#FF0000", linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[5]], aes(x=Year, y= Abundance), color = "#00FF66", linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[5]], aes(x=Year, y= Abundance), color = "#0066FF", linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[5]], aes(x=Year, y= Abundance), color = "#CC00FF", linewidth = 1)+
  geom_line(data = pop_shortdisp[[5]], aes(x=Year, y= Abundance), color = "black", linewidth = 1)+
  ggtitle(unique(pop_longdisp[[5]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))

legend <- ggplot(pop_longdisp[[5]], aes(x= Year, y = Abundance, color = "0.95"))+
  geom_line(linewidth = 1)+
  geom_line(data = pop_longdisp0.8[[5]], aes(x=Year, y= Abundance, color = "0.8"), linewidth = 1)+
  geom_line(data = pop_longdisp0.85[[5]], aes(x=Year, y= Abundance, color = "0.85"), linewidth = 1)+
  geom_line(data = pop_longdisp0.9[[5]], aes(x=Year, y= Abundance, color = "0.9"), linewidth = 1)+
  geom_line(data = pop_shortdisp[[5]], aes(x=Year, y= Abundance, color = "short"), linewidth = 1)+
  ggtitle(unique(pop_longdisp[[5]]$Breadth))+
  ylim(c(0,max(pop_shortdisp[[5]]$Abundance)))+
  theme(plot.title = element_text(size = 20))+
  scale_color_manual(values = c("0.95" = "#FF0000", "0.8" = "#00FF66", "0.85" = "#0066FF", "0.9" = "#CC00FF", "short" = "black"), labels=c( "0.8", "0.85", "0.9", "0.95", "short dispersal"))+
  theme(legend.text = element_text(size = 20), legend.key.size = unit(1, "cm"), legend.position = "bottom", legend.title = element_text(size = 18))+
  labs(color="dispersal probability")

shared_legend <- extract_legend(legend)

grid.arrange(arrangeGrob(p1.2,p2.2,p3.2,p4.2,p5.2, ncol = 3, nrow = 2),shared_legend, ncol = 1, nrow = 2, heights = c(11,1.5))
