# only concentrating on the pop loss - habitat loss relationship and trying to plot the multi-trait value with constant other traits

# Load packages
library(ggplot2)
library(gridExtra)
library(data.table)

#Load model data
load("4_Analysis/Model Results/Model_ordbeta_full.Rdata")

optima <- c("range-contracting", "range-shifting")
breadth <- c("narrow", "wide")
rmax <- c("slow", "fast")
dispersal <- c("short", "long")
land_rep <- 1:3

#create data frame with all trait combinations
sims_long <- expand.grid(optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal, land = land_rep)

#add new hs_loss predictions to it
hs_loss <- seq(0,1,length=100)

# expand data set by length of vector
new_data <- sims_long[rep(seq_len(nrow(sims_long)), length(hs_loss)), ]
new_data$index <- as.numeric(row.names(new_data))
new_data <- new_data[order(new_data$index), ]
new_data <- cbind(new_data, hs_loss)

# predict to new data
predictions_data <- cbind(new_data, predict(model, newdata=new_data,  type = "response", se.fit=T))

predictions_data$land <- factor(predictions_data$land, levels = c("1", "2", "3"))

# create mean predictions per niche position and land
predictions_mean <- aggregate(x = predictions_data$Estimate, by = list(predictions_data$optima, predictions_data$land, predictions_data$hs_loss), FUN = mean)
names(predictions_mean) <- c("optima", "land", "hs_loss", "Estimate")

# plot predictions
ggplot(predictions_mean, aes(x = hs_loss, y = Estimate, colour = land, linetype = optima, group = interaction(optima, land)))+
  geom_line()


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
