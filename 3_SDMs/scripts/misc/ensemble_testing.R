# Plot SDM predictions of one example

library(terra)
library(ggplot2)
library(tidyterra)

load("3_SDMs/data/Predictions_curr_Batch10_Sim1_Replication14.RData")

r_preds <- rast(ens_preds_bin)
r_preds_all <- rast(all_preds_bin)

all <- merge(ens_preds_bin, all_preds_bin, by = c("x", "y"))
r_all <- rast(all)

ggplot() +
  geom_spatraster(data = r_all) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_whitebox_c(
    palette = "muted"
  ) +
  labs(fill = "Occ. prob.")

ggplot() +
  geom_spatraster(data = r_preds_all) +
  facet_wrap(~lyr, ncol = 4) +
  scale_fill_whitebox_c(
    palette = "muted"
  ) +
  labs(fill = "Occ. prob.")

load("3_SDMs/data/pred_testdata_Batch1_Sim1_Replication87.Rdata")

# Calculate FP, FN, TP, TN

occ <- readRDS("3_SDMs/data/Occ_Abs_full_list_Batch_10_Sim1.rds")
occ14 <- occ[["14"]]

all1 <- merge(all, occ14, by.x = c("x", "y"), by.y = c("X", "Y"))
test1 <- full_join(all, occ14, join_by(x == X, y == Y))

test1.NA <- test1[which(is.na(test1$mean_prob)),]

occ_raw <- readRDS("3_SDMs/data/Occ_list_Batch10_Sim1.rds")
occ_raw14 <- occ_raw[["14"]]
occ_raw14$X <- occ_raw14$X * 1000
occ_raw14$Y <- occ_raw14$Y * 1000

occ_wo_clim <- readRDS("3_SDMs/data/Occ_Abs_wo_clim_list_Batch_10_Sim1.rds")
occ_wo_clim14 <- occ_wo_clim[["14"]]

test2 <- merge(occ14, occ_raw14, by = c("X", "Y"))

clim <- rast("3_SDMs/data/land1_optima0.5_breadth0.045_ccYear0.grd")
clim1 <- terra::as.data.frame(clim, xy = T, cells = T)

x <- cbind(occ_wo_clim14, extract(x = clim, y = occ_wo_clim14[, c("X", "Y")], cells = T, ID = F, xy = T))

clim2 <- rast("3_SDMs/data/land1_optima0.5_breadth0.045_ccYear0.asc")

y <- cbind(occ_wo_clim14, extract(x = clim2, y = occ_wo_clim14[, c("X", "Y")], cells = T, ID = F, xy = T))

occ14[which(occ14$Y != occ14$y),]

plot(clim[[1]])
points(test12$X, test12$Y)

clim3 <- terra::as.data.frame(clim2, xy = T, cells = T)

occ_all <- readRDS("3_SDMs/data/Occ_year100.rds")
occ_all14 <- occ_all[["14"]]
occ_all14$x <- occ_all14$x * 1000
occ_all14$y <- occ_all14$y * 1000

xz <- cbind(occ_14, extract(x = clim2, y = occ_14[, c("X", "Y")], cells = T, ID = F, xy = T))

occ_fread <- readRDS("3_SDMs/data/Occ_list_fread_Batch10_Sim1.rds")
occ_fread14 <- occ_fread[["14"]]
occ <- readRDS("3_SDMs/data/Occ_list_Batch16_Sim3.rds")
occ_14 <- occ[["14"]]
occ_pop <- readRDS("3_SDMs/data/Occ_list_popread_Batch10_Sim1.rds")
occ_pop14 <- occ_pop[["14"]]
