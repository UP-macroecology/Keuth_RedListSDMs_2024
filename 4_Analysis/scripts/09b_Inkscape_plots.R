# Create the abundance and prediction plots on the cluster to avoid large data transfer
# PLots are done for all species for landscape 1 and replicate run 3

#Load packages

library(RColorBrewer)
library(terra)

#define file path
sim_dir <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/02_Simulations/")
sdm_dir <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/03_SDMs/")

for(BatchNum in 1:16){
  # Load data (current prediction of SDMs)
  load(paste0(sdm_dir, "predictions/Predictions_curr_Batch", BatchNum, "_Sim1_Replication4.RData"))
  
  #Rasterize data
  r_current <- terra::rast(ens_preds[,1:3])
  
  #plot the results for the prediction of current habitat suitability
  pdf(paste0(sdm_dir, "plots/Inkscape_maps/SDM_predictions_map_current_Batch", BatchNum, "_Rep4_Land1.pdf"))
  plot(r_current, legend = F, axes = F, col = rev(terrain.colors(100)), range = c(0,1))
  dev.off()
  
  # Load data (future predictions of SDMs)
  load(paste0(sdm_dir, "predictions/Predictions_fut_Batch", BatchNum, "_Sim1_Replication4.RData"))
  
  # extract two different years (I am not sure which one I should select)
  fut_preds_20 <- ens_fut_preds[[20]]
  fut_preds_30 <- ens_fut_preds[[30]]
  
  #Rasterize data
  r_fut_20 <- terra::rast(fut_preds_20[,1:3])
  r_fut_30 <- terra::rast(fut_preds_30[,1:3])
  
  #plot the results of both years
  pdf(paste0(sdm_dir, "plots/Inkscape_maps/SDM_predictions_map_future20_Batch", BatchNum, "_Rep4_Land1.pdf"))
  plot(r_fut_20, legend = F, axes = F, col = rev(terrain.colors(100)), range = c(0,1))
  dev.off()
  
  pdf(paste0(sdm_dir, "plots/Inkscape_maps/SDM_predictions_map_future30_Batch", BatchNum, "_Rep4_Land1.pdf"))
  plot(r_fut_30, legend = F, axes = F, col = rev(terrain.colors(100)), range = c(0,1))
  dev.off()
  
  # Abundance plots of the simulation

  # load data of abundances (the before extracted one replication of the pop data set)
  load(paste0(sim_dir, "Outputs/Batch", BatchNum, "_Sim1_Land1_Pop_Rep4.Rdata"))

  # Extract values for year 0
  pop_current <- subset(pop, pop$Year == 100)
  #correct spatial extent
  pop_current_full <- merge(ens_preds[,c(1:2)], pop_current[,c(4:5,7)], by = c("x","y"), all.x = T)
  #correct NAs to 0 (otherwise the plotting looks odd and I know that all NAs are 0s)
  pop_current_full[which(is.na(pop_current_full$NInd)),"NInd"] <- 0
  #rasterize data set
  r_abu_current <- rast(as.data.frame(pop_current_full[,c(1:3)]))

  # Plot abundances for current climatic conditions
  pdf(paste0(sdm_dir, "plots/Inkscape_maps/Abundances_map_current_Batch", BatchNum, "_Rep4_Land1.pdf"))
  plot(r_abu_current, axes = F, range = c(0,11), legend = F, smooth = T, col = c("#F2F2F2", rev(brewer.pal(n = 11, name = "Spectral"))))
  dev.off()
  
  # Extract values for year 20
  pop_fut_20 <- subset(pop, pop$Year == 120)
  #correct spatial extent
  pop_fut_20_full <- merge(ens_preds[,c(1:2)], pop_fut_20[,c(4:5,7)], by = c("x","y"), all.x = T)
  #correct NAs to 0 (otherwise the plotting looks odd and I know that all NAs are 0s)
  pop_fut_20_full[which(is.na(pop_fut_20_full$NInd)),"NInd"] <- 0
  #rasterize data set
  r_abu_fut_20 <- rast(as.data.frame(pop_fut_20_full[,c(1:3)]))
  
  # if there are no abundances in the data sets I set one color for the whole map otherwise I use a palette
  if(length(pop_fut_20_full$NInd) == 1){
    pdf(paste0(sdm_dir, "plots/Inkscape_maps/Abundances_map_future20_Batch", BatchNum, "_Rep4_Land1.pdf"))
    plot(r_abu_fut_20, axes = F, range = c(0,11), legend = F, smooth = T, col = c("#F2F2F2"))
    dev.off()
  } else {
    pdf(paste0(sdm_dir, "plots/Inkscape_maps/Abundances_map_future20_Batch", BatchNum, "_Rep4_Land1.pdf"))
    plot(r_abu_fut_20, axes = F, range = c(0,11), legend = F, smooth = T, col = c("#F2F2F2", rev(brewer.pal(n = 11, name = "Spectral"))))
    dev.off()
  }
  
  # Extract values for year 30
  pop_fut_30 <- subset(pop, pop$Year == 130)
  #correct spatial extent
  pop_fut_30_full <- merge(ens_preds[,c(1:2)], pop_fut_30[,c(4:5,7)], by = c("x","y"), all.x = T)
  #correct NAs to 0 (otherwise the plotting looks odd and I know that all NAs are 0s)
  pop_fut_30_full[which(is.na(pop_fut_30_full$NInd)),"NInd"] <- 0
  #rasterize data set
  r_abu_fut_30 <- rast(as.data.frame(pop_fut_30_full[,c(1:3)]))
  
  # if there are no abundances in the data sets I set one color for the whole map otherwise I use a palette
  if(length(pop_fut_30_full$NInd) == 1){
    pdf(paste0(sdm_dir, "plots/Inkscape_maps/Abundances_map_future30_Batch", BatchNum, "_Rep4_Land1.pdf"))
    plot(r_abu_fut_30, axes = F, range = c(0,11), legend = F, smooth = T, col = c("#F2F2F2"))
    dev.off()
  } else {
    pdf(paste0(sdm_dir, "plots/Inkscape_maps/Abundances_map_future30_Batch", BatchNum, "_Rep4_Land1.pdf"))
    plot(r_abu_fut_30, axes = F, range = c(0,11), legend = F, smooth = T, col = c("#F2F2F2", rev(brewer.pal(n = 11, name = "Spectral"))))
    dev.off()
  }
}

