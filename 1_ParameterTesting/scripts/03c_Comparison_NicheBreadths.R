# Testing the results of different niche breadths
# Questions: Does the double kernel has an effect? Can I remove the dispersal limitations with increasing the niche breadths and by this the population size?

library(ggplot2)
library(gridExtra)

results_wo_long <- readRDS("1_ParameterTesting/data/results_nichebreadths_wo_longdisp.rds")

pop_mean_wo_long <- results_wo_long[[1]]
extProb_list_wo_long <- results_wo_long[[2]]
real_rangechange_wo_long <- results_wo_long[[3]]

#correct years
real_rangechange_wo_long <- lapply(real_rangechange_wo_long, function(x){x$Year <- x$Year + 100; return(x)})

#calculate relative abundance to plot within the plot
pop_mean_wo_long <- lapply(pop_mean_wo_long, function(x){x$rel_pop <- NA;
for (i in 101:nrow(x)){
  x[i, 7] <- x[i, 2] / x[100,2]
};
x[100,7] <- 1; return(x)})


p1 <- ggplot(extProb_list_wo_long[[1]], aes(x = Year, y = extProb))+
  geom_line(colour = "red", linewidth = 1)+
  geom_line(data = real_rangechange_wo_long[[1]], aes(x = Year, y = diff), linewidth = 1)+
  geom_line(data = pop_mean_wo_long[[1]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "blue")+
  ylab("Rate")+
  ggtitle(unique(extProb_list_wo_long[[1]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))

p2 <- ggplot(extProb_list_wo_long[[2]], aes(x = Year, y = extProb))+
  geom_line(colour = "red", linewidth = 1)+
  geom_line(data = real_rangechange_wo_long[[2]], aes(x = Year, y = diff), linewidth = 1)+
  geom_line(data = pop_mean_wo_long[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "blue")+
  ylab("Rate")+
  ggtitle(unique(extProb_list_wo_long[[2]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))

p3 <- ggplot(extProb_list_wo_long[[3]], aes(x = Year, y = extProb))+
  geom_line(colour = "red", linewidth = 1)+
  geom_line(data = real_rangechange_wo_long[[3]], aes(x = Year, y = diff), linewidth = 1)+
  geom_line(data = pop_mean_wo_long[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "blue")+
  ylab("Rate")+
  ggtitle(unique(extProb_list_wo_long[[3]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))

p4 <- ggplot(extProb_list_wo_long[[4]], aes(x = Year, y = extProb))+
  geom_line(colour = "red", linewidth = 1)+
  geom_line(data = real_rangechange_wo_long[[4]], aes(x = Year, y = diff), linewidth = 1)+
  geom_line(data = pop_mean_wo_long[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "blue")+
  ylab("Rate")+
  ggtitle(unique(extProb_list_wo_long[[4]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))

p5 <- ggplot(extProb_list_wo_long[[5]], aes(x = Year, y = extProb))+
  geom_line(colour = "red", linewidth = 1)+
  geom_line(data = real_rangechange_wo_long[[5]], aes(x = Year, y = diff), linewidth = 1)+
  geom_line(data = pop_mean_wo_long[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "blue")+
  ylab("Rate")+
  ggtitle(unique(extProb_list_wo_long[[5]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))

p6 <- ggplot(extProb_list_wo_long[[6]], aes(x = Year, y = extProb))+
  geom_line(colour = "red", linewidth = 1)+
  geom_line(data = real_rangechange_wo_long[[6]], aes(x = Year, y = diff), linewidth = 1)+
  geom_line(data = pop_mean_wo_long[[6]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "blue")+
  ylab("Rate")+
  ggtitle(unique(extProb_list_wo_long[[6]]$Breadth))+
  xlim(c(99, 170))+
  ylim(c(0,1.1))

p7 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = paste0("ntemp:0.25+variable; npre:0.5+variable\nK:0.05; Rmax:3; EmigProb:0.4; Dispersal:15000\nred: Extinction probability; black: Habitat loss")) +
  theme_void()

grid.arrange(arrangeGrob(p1,p2,p3,p4,p5,p6, ncol = 3, nrow = 2),
             p7, nrow = 2, ncol = 1, heights = c(10,2))

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

p1.1 <- ggplot(extProb_list[[1]], aes(x = Year, y = extProb, colour = "Ext"))+
  geom_line(linewidth = 1)+
  geom_line(data = real_rangechange[[1]], aes(x = Year, y = diff, colour = "Habitat"), linewidth = 1)+
  geom_line(data = pop_mean[[1]], aes(x = Year, y = rel_pop, colour = "Pop"), linewidth = 1)+
  ylab("Rate")+
  ggtitle(unique(extProb_list[[1]]$Breadth))+
  xlim(c(90, 170))+
  scale_color_manual(values = c("Pop" = "gold", "Habitat" = "#FF6A6A", "Ext" = "blue"), labels=c( "Extinction probability", "Population size", "Habitat size"))

p2.1 <- ggplot(extProb_list[[2]], aes(x = Year, y = extProb))+
  geom_line(colour = "red", linewidth = 1)+
  geom_line(data = real_rangechange[[2]], aes(x = Year, y = diff), linewidth = 1)+
  geom_line(data = pop_mean[[2]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "blue")+
  ylab("Rate")+
  ggtitle(unique(extProb_list[[2]]$Breadth))+
  xlim(c(90, 170))

p3.1 <- ggplot(extProb_list[[3]], aes(x = Year, y = extProb))+
  geom_line(colour = "red", linewidth = 1)+
  geom_line(data = real_rangechange[[3]], aes(x = Year, y = diff), linewidth = 1)+
  geom_line(data = pop_mean[[3]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "blue")+
  ylab("Rate")+
  ggtitle(unique(extProb_list[[3]]$Breadth))+
  xlim(c(90, 170))

p4.1 <- ggplot(extProb_list[[4]], aes(x = Year, y = extProb))+
  geom_line(colour = "red", linewidth = 1)+
  geom_line(data = real_rangechange[[4]], aes(x = Year, y = diff), linewidth = 1)+
  geom_line(data = pop_mean[[4]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "blue")+
  ylab("Rate")+
  ggtitle(unique(extProb_list[[4]]$Breadth))+
  xlim(c(90, 170))

p5.1 <- ggplot(extProb_list[[5]], aes(x = Year, y = extProb))+
  geom_line(colour = "red", linewidth = 1)+
  geom_line(data = real_rangechange[[5]], aes(x = Year, y = diff), linewidth = 1)+
  geom_line(data = pop_mean[[5]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "blue")+
  ylab("Rate")+
  ggtitle(unique(extProb_list[[5]]$Breadth))+
  xlim(c(90, 170))

p6.1 <- ggplot(extProb_list[[6]], aes(x = Year, y = extProb))+
  geom_line(colour = "red", linewidth = 1)+
  geom_line(data = real_rangechange[[6]], aes(x = Year, y = diff), linewidth = 1)+
  geom_line(data = pop_mean[[6]], aes(x = Year, y = rel_pop), linewidth = 1, colour = "blue")+
  ylab("Rate")+
  ggtitle(unique(extProb_list[[6]]$Breadth))+
  xlim(c(90, 170))

p7.1 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = paste0("ntemp:0.25+variable; npre:0.5+variable\nK:0.05; Rmax:3; EmigProb:0.4; Dispersal:15000, 250000, 0.95\nred: Extinction probability; black: Habitat loss")) +
  theme_void()

grid.arrange(arrangeGrob(p1.1,p2.1,p3.1,p4.1,p5.1,p6.1, ncol = 3, nrow = 2),
             p7.1, nrow = 2, ncol = 1, heights = c(10,2))
