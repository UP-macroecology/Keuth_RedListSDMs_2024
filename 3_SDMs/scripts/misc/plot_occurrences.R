occ <- readRDS("3_SDMs/data/Occ_Abs_thinned_list_Batch_1_Sim1.rds")
occ1 <- occ[[1]]

plot(occ1$X, occ1$Y, col="red")
occ_pres <- subset(occ1, occ1$occ == 1)
points(occ_pres$X, occ_pres$Y, col = "blue")
occ_abs <- subset(occ1, occ1$occ == 0)

occ <- readRDS("3_SDMs/data/Occ_list_Batch10_Sim1.rds")

land_rep <- 1:3
optima <- c(0.27, 0.5)
breadth <- c(0.045, 0.055)
rmax <- c(3, 5)
dispersal <- c(5000, 15000)

sims <- expand.grid(land_rep = land_rep, optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims$BatchNum <- rep(1:16, each = 3)

for (sim_nr in 1:nrow(sims)) {
  rep_nr <- sims[sim_nr,]$land_rep
  optima <- sims[sim_nr,]$optima
  breadth <- sims[sim_nr,]$breadth
  BatchNum <- sims[sim_nr,]$BatchNum
  occ <- readRDS(paste0("3_SDMs/data/Occ_Abs_full_list_Batch_", BatchNum, "_Sim", rep_nr, ".rds"))
  print(unique(lapply(occ, function(x){nrow(x)})))
}

occ1 <- occ[[1]]
length(which(occ1$occ == 1))

plot(mask)
points(occ_pres$X, occ_pres$Y, col = "red")
points(occ_abs$X, occ_abs$Y, col = "blue")
