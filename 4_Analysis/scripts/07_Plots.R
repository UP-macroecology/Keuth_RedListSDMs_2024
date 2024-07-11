# Plot results

# load packages
library(ggplot2)
library(gridExtra)
library(ggtext)
library(dplyr)

# Load functions
source("Functions/extract_legend.R")
source("2_Simulations/scripts/text_labels_plots.R")

# load in data
data <- vector("list", 16)

for (i in 1:16){
  # data set for land replication 1
  tmp1 <- readRDS(paste0("4_Analysis/data/data_analysis_relative_long_Batch", i, "_Sim1.rds"))
  # data set for land replication 2
  tmp2 <- readRDS(paste0("4_Analysis/data/data_analysis_relative_long_Batch", i, "_Sim2.rds"))
  # data set for land replication 3
  tmp3 <- readRDS(paste0("4_Analysis/data/data_analysis_relative_long_Batch", i, "_Sim3.rds"))
  
  #rbind the different data sets
  data[[i]] <- rbind(tmp1, tmp2)
  data[[i]] <- rbind(data[[i]], tmp3)
  
  #transform column
  data[[i]]$land <- as.character(data[[i]]$land)
}

# Plot the results
# extinction probability - change in habitat suitability sums/ change in range size ------------
p1 <- #ggplot(data[[1]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  ggplot(data[[1]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  #xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p2 <-#ggplot(data[[2]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  ggplot(data[[2]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  #xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p3 <- #ggplot(data[[3]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  ggplot(data[[3]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  #xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p4 <- #ggplot(data[[4]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  ggplot(data[[4]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  #xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p5 <- #ggplot(data[[5]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  ggplot(data[[5]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  #xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p6 <- #ggplot(data[[6]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  ggplot(data[[6]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  #xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p7 <- #ggplot(data[[7]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  ggplot(data[[7]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  #xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p8 <- #ggplot(data[[8]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  ggplot(data[[8]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  #xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p9 <- #ggplot(data[[9]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  ggplot(data[[9]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  #xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())

p10 <- #ggplot(data[[10]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  ggplot(data[[10]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  #xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p11 <- #ggplot(data[[11]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  ggplot(data[[11]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  #xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p12 <-#ggplot(data[[12]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  ggplot(data[[12]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  #xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())

p13 <- #ggplot(data[[13]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  ggplot(data[[13]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  #xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "")

p14 <-#ggplot(data[[14]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  ggplot(data[[14]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  #xlab("Habitat loss")+
  xlab("Range loss")++
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())

p15 <- #ggplot(data[[15]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  ggplot(data[[15]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  #xlab("Habitat loss")+
  xlab("Range loss")+
  ylab("Extinction probability")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())

p16 <- #ggplot(data[[16]], aes(x=(1-hs_change), y=extProb, col = land, group = land))+
  ggplot(data[[16]], aes(x=(1-range_change), y=extProb, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  #xlab("Habitat loss")+
  xlab("Range loss")+
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
p1 <- ggplot(data[[1]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  #ggplot(data[[1]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  #xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p2 <-ggplot(data[[2]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  #ggplot(data[[2]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  #xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p3 <- ggplot(data[[3]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  #ggplot(data[[3]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  #xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p4 <- ggplot(data[[4]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  #ggplot(data[[4]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  #xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p5 <- ggplot(data[[5]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  #ggplot(data[[5]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  #xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p6 <- ggplot(data[[6]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  #ggplot(data[[6]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  #xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p7 <- ggplot(data[[7]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  #ggplot(data[[7]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  #xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p8 <- ggplot(data[[8]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  #ggplot(data[[8]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  #xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p9 <- ggplot(data[[9]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  #ggplot(data[[9]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  #xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p10 <- ggplot(data[[10]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  #ggplot(data[[10]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  #xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p11 <- ggplot(data[[11]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  #ggplot(data[[11]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  #xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p12 <-ggplot(data[[12]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  #ggplot(data[[12]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  #xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p13 <- ggplot(data[[13]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  #ggplot(data[[13]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  #xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "")+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p14 <-ggplot(data[[14]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  #ggplot(data[[14]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  #xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p15 <- ggplot(data[[15]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  #ggplot(data[[15]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  #xlab("Range loss")+
  ylab("relative Population size")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())+
  ylim(c(0,1.2))+
  xlim(c(0,1))

p16 <- ggplot(data[[16]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  #ggplot(data[[16]], aes(x=(1-range_change), y=pop_sum, col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 1, slope = -1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  #xlab("Range loss")+
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

# habitat loss compared to range loss ----------
p1 <- ggplot(data[[1]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p2 <-ggplot(data[[2]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p3 <- ggplot(data[[3]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p4 <- ggplot(data[[4]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p5 <- ggplot(data[[5]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p6 <- ggplot(data[[6]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p7 <- ggplot(data[[7]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p8 <- ggplot(data[[8]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p9 <- ggplot(data[[9]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.x = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p10 <- ggplot(data[[10]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p11 <- ggplot(data[[11]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p12 <-ggplot(data[[12]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), legend.position = "", axis.title = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p13 <- ggplot(data[[13]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "")+
  ylim(c(0,1))+
  xlim(c(0,1))

p14 <-ggplot(data[[14]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p15 <- ggplot(data[[15]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

p16 <- ggplot(data[[16]], aes(x=(1-hs_change), y=(1-range_change), col = land, group = land))+
  geom_point(size=1.5)+
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed", linewidth = 1)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "", axis.title.y = element_blank())+
  ylim(c(0,1))+
  xlim(c(0,1))

#create and extract shared legend
legend <- ggplot(data[[16]], aes(x=(1-hs_change), y=pop_sum, col = land, group = land))+
  geom_point(size = 2)+
  theme_bw()+
  xlab("Habitat loss")+
  ylab("Range loss")+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), legend.position = "bottom", legend.key.size = unit(1, "cm"), legend.text = element_text(size = 13), 
        legend.title = element_text(size = 15))

shared_legend <- extract_legend(legend)

#Plot large grid
grid.arrange(arrangeGrob(t0, t_cn, t_cw, t_wna, t_ww, t_ss, p1,p3,p2,p4, t_sl, p9,p11,p10,p12, t_fs, p5,p7,p6,p8, t_fl, p13,p15,p14,p16, nrow = 5, ncol = 5, heights= c(1,3.8,4,4,4.2), widths = c(1.5,5,5,5,5)),
             t0,shared_legend, nrow = 2, ncol = 2, heights = c(11.2, 0.8), widths = c(11.7,0.3))

# IUCN classification time -----

# create data frame with all parameter combinations
land_rep <- 1:3
BatchNum <- 1:16
set.seed(8765)
replicates <- sample(0:99, 10)

IUCN_classification <- expand.grid(land_rep = land_rep, BatchNum = BatchNum, replicates = replicates)

#transform BatchNum column
IUCN_classification$BatchNum <- factor(IUCN_classification$BatchNum, levels = c("1", "9", "5", "13", "3", "11", "7", "15", "2", "10", "6", "14", "4", "12", "8","16"))

# create the VU, EN, CR columns for all metrices (Pop, Range, HS, Ext.Prob)
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

