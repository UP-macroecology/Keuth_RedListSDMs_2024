# only concentrating on the pop loss - habitat loss relationship and trying to plot the multi-trait value with constant other traits

# Load packages
library(ggplot2)
library(gridExtra)
library(data.table)
library(dplyr)
library(brms)
library(ordbetareg)

#Load model data
#load("4_Analysis/Model Results/Model_ordbeta_full.Rdata")
source("Functions/extract_legend.R")

# optima <- c("range-contracting", "range-shifting")
# breadth <- c("narrow", "wide")
# rmax <- c("slow", "fast")
# dispersal <- c("short", "long")
# land_rep <- 1:3
# 
# #create data frame with all trait combinations
# sims_long <- expand.grid(optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal, land = land_rep)
# 
# #add new hs_loss predictions to it
# hs_loss <- seq(0,1,length=100)
# 
# # expand data set by length of vector
# new_data <- sims_long[rep(seq_len(nrow(sims_long)), length(hs_loss)), ]
# new_data$index <- as.numeric(row.names(new_data))
# new_data <- new_data[order(new_data$index), ]
# new_data <- cbind(new_data, hs_loss)
# new_data$land <- factor(new_data$land, levels = c("1", "2", "3"))
# 
# # predict to new data
# predictions_data_optima <- cbind(new_data, predict(model, newdata=new_data,  type = "response", se.fit=T))
# predictions_data_breadth <- cbind(new_data, predict(model, newdata=new_data,  type = "response", se.fit=T))
# predictions_data_rmax <- cbind(new_data, predict(model, newdata=new_data,  type = "response", se.fit=T))
# predictions_data_dispersal <- cbind(new_data, predict(model, newdata=new_data,  type = "response", se.fit=T))
# 
# save(predictions_data_optima, predictions_data_breadth, predictions_data_rmax, predictions_data_dispersal, file = "4_Analysis/data/Model_predictions_OBR_plot.Rdata")

load("4_Analysis/data/Model_predictions_OBR_plot.Rdata")

# create mean predictions per niche position and land
predictions_mean_optima <- predictions_data_optima %>%
  group_by(optima, land, hs_loss) %>%
  summarise(mean = mean(Estimate), sd = sd(Estimate))

predictions_mean_breadth <- predictions_data_breadth %>%
  group_by(breadth, land, hs_loss) %>%
  summarise(mean = mean(Estimate), sd = sd(Estimate))

predictions_mean_rmax <- predictions_data_rmax %>%
  group_by(rmax, land, hs_loss) %>%
  summarise(mean = mean(Estimate), sd = sd(Estimate))

predictions_mean_dispersal <- predictions_data_dispersal %>%
  group_by(dispersal, land, hs_loss) %>%
  summarise(mean = mean(Estimate), sd = sd(Estimate))

# plot predictions
# ggplot(predictions_mean, aes(x = hs_loss, y = Estimate, colour = land, linetype = optima, group = interaction(optima, land)))+
#   geom_line()

ggplot(predictions_mean_optima, aes(x=hs_loss, y = mean, col = land, linetype = optima))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_ribbon(aes(ymin = (mean-1.96*sd), ymax = (mean+1.96*sd), fill = land), alpha=0.05, col = "lightgrey") +
  #geom_point()+
  geom_line(linewidth = 2)+
  # geom_point(data = data_adapted_long, aes(x=0, y= pop_sum), col = "red")+
  # geom_point(data = data_adapted_long, aes(x=hs_loss, y= 0), col = "red")+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 22), axis.title = element_text(size = 24),
        legend.position = c(0.91, 0.85),  legend.title = element_text(size = 23), legend.text = element_text(size = 22),
        legend.key.size = unit(2,"line"))+ #axis.title.x = element_blank(),
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"))+
  labs(colour = "Landscape", linetype = NULL)+
  guides(linetype = guide_legend(order = 1), fill = "none")

predictions_mean_optima$optima <- as.character(predictions_mean_optima$optima)
predictions_mean_optima[which(predictions_mean_optima$optima == "range-contracting"), "optima"] <- "marginal"
predictions_mean_optima[which(predictions_mean_optima$optima == "range-shifting"), "optima"] <- "central"
predictions_mean_optima$optima <- factor(predictions_mean_optima$optima, levels = c("marginal", "central"))
predictions_mean_breadth$breadth <- factor(predictions_mean_breadth$breadth, levels = c("wide", "narrow"))
predictions_mean_rmax$rmax <- factor(predictions_mean_rmax$rmax, levels = c("fast", "slow"))
predictions_mean_dispersal$dispersal <- factor(predictions_mean_dispersal$dispersal, levels = c("long", "short"))

p_pos <- ggplot(predictions_mean_optima, aes(x=hs_loss, y = mean, col = land, linetype = optima))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_ribbon(aes(ymin = (mean-1.96*sd), ymax = (mean+1.96*sd), fill = land), alpha=0.05, col='lightgrey') +
  #geom_point()+
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 18), plot.title = element_text(size = 23, face = "italic"), 
        legend.position = c(0.9, 0.89),  legend.title = element_blank(), legend.text = element_text(size = 18), 
        legend.key.size = unit(2,"line"))+ #axis.title.x = element_blank(),
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(-0.1,1.25), expand = c(0.015, 0.015), breaks = c(0,0.25,0.5,0.75,1.0)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"), guide = "none")+
  ggtitle("Niche position")+
  guides(fill = "none")

p_breadth <- ggplot(predictions_mean_breadth, aes(x=hs_loss, y = mean, col = land, linetype = breadth))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_ribbon(aes(ymin = (mean-1.96*sd), ymax = (mean+1.96*sd), fill = land), alpha=0.05, col='lightgrey') +
  #geom_point()+
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), plot.title = element_text(size = 23, face = "italic"), legend.position = c(0.92, 0.90),
        axis.title = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 18), legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(-0.1,1.25), expand = c(0.015, 0.015), breaks = c(0,0.25,0.5,0.75,1.0)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"), guide = "none")+
  ggtitle("Niche breadth")+
  guides(fill = "none")

p_rmax <- ggplot(predictions_mean_rmax, aes(x=hs_loss, y = mean, col = land, linetype = rmax))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_ribbon(aes(ymin = (mean-1.96*sd), ymax = (mean+1.96*sd), fill = land), alpha=0.05, col='lightgrey') +
  #geom_point()+
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 18),  plot.title = element_text(size = 23, face = "italic"), 
        legend.position = c(0.93, 0.89), legend.title = element_blank(), legend.text = element_text(size = 18), legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(-0.1,1.25), expand = c(0.015, 0.015), breaks = c(0,0.25,0.5,0.75,1.0)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"), guide = "none")+
  ggtitle("Growth rate")+
  guides(fill = "none")

p_dispersal <- ggplot(predictions_mean_dispersal, aes(x=hs_loss, y = mean, col = land, linetype = dispersal))+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "twodash", linewidth = 1)+
  geom_ribbon(aes(ymin = (mean-1.96*sd), ymax = (mean+1.96*sd), fill = land), alpha=0.05, col='lightgrey') +
  #geom_point()+
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 18), plot.title = element_text(size = 23, face = "italic"), 
        legend.position = c(0.93, 0.89), axis.title.y = element_blank(), legend.title = element_blank(), legend.text = element_text(size = 18), 
        legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(-0.1,1.25), expand = c(0.015, 0.015), breaks = c(0,0.25,0.5,0.75,1.0)) +
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"), guide = "none")+
  ggtitle("Dispersal")+
  guides(fill = "none")

legend <- ggplot(predictions_mean_dispersal, aes(x=hs_loss, y = mean, col = land, linetype = dispersal))+
  #geom_point()+
  geom_line(linewidth = 2)+
  xlab("Habitat loss")+
  ylab("Relative population size")+
  theme_bw()+
  geom_abline(intercept = 1, slope = -1, col = "#C7C7C7", linetype = "dashed", linewidth = 1)+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), plot.title = element_text(size = 23, face = "italic"), 
        legend.title = element_text(size = 23), legend.text = element_text(size = 18), 
        legend.key.size = unit(2,"line"))+
  scale_x_continuous(limits = c(0,1), expand = c(0.008, 0.008)) +
  scale_y_continuous(limits = c(0,1), expand = c(0.015, 0.015)) +
  guides(linetype = "none", colour = guide_legend(title = "Landscape"))+
  scale_color_manual(values = c("#38A6E5", "#046D51", "#C37B6C"))+
  ggtitle("Dispersal")

shared_legend <- extract_legend(legend)

grid.arrange(arrangeGrob(p_pos,p_breadth, p_rmax, p_dispersal, nrow=2, ncol = 2, heights = c(8,8), widths = c(1,1)), shared_legend, ncol=2, nrow = 1, widths = c(10,1))

# report marginal effects
load("4_Analysis/Model Results/Model_ordbeta_full.Rdata")

library(marginaleffects)

avg_slopes(model, variables="optima")

avg_slopes(model, variables="optima") %>%
  select(Variable="term",
         Level="contrast",
         `5% Quantile`="conf.low",
         `Posterior Mean`="estimate",
         `95% Quantile`="conf.high") %>% 
  knitr::kable(caption = "Marginal Effect of Education on Professor Thermometer",
               format.args=list(digits=2),
               align=c('llccc'))
# # create new predictions data frame
# predictions_data <- data.frame(hs_loss=seq(0,1,length=100), optima = "range-shifting", breadth = "wide", rmax = "slow", dispersal = "short", land = 3)
# predictions_data$hs_loss_squared <- I(predictions_data$hs_loss^2)
# predictions_data3 <- data.frame(hs_loss=seq(0,1,length=100), optima = "range-contracting", breadth = "narrow", rmax = "slow", dispersal = "short", land = 2)
# 
# predictions_rangecontracting_long <- cbind(predictions_rangecontracting_long, predict(model_mbrms, newdata=predictions_rangecontracting_long,  type = "response", se.fit=T))
# predictions_data <- cbind(predictions_data, predict(model_mbrms, newdata=predictions_data,  type = "response", se.fit=T))
# predictions_data3 <- cbind(predictions_data3, predict(model_mbrms, newdata=predictions_data3,  type = "response", se.fit=T))
# 
# test <- posterior_predict(model_mbrms, newdata=predictions_data)
# 
# predictions_data <- data.frame(hs_loss=seq(0,1,length=100), optima = "range-shifting")
# predictions_data <- cbind(predictions_data, predict(model, newdata=predictions_data,  type = "response", se.fit=T))
# 
# posteriorpredictive_CI <- bind_cols(
#   predictions_data,
#   apply(test, 2, mean)
# )
# 
# 
# ggplot(predictions_data, aes(x=hs_loss, y = Estimate))+
#   geom_line()+
#   geom_line(data = predictions_data3,aes(x=hs_loss, y = Estimate), col = "red")+
#   ylim(0,1)
# 
# 
# ggplot(predictions_rangecontracting_long, aes(x=hs_loss, y = Estimate))+
#   geom_point()+
#   facet_wrap(~ land)
# 
# 
# 
# load("4_Analysis/data/Model_Brms_cluster_hsloss_squared.Rdata")
# summary(model_mbrms)
