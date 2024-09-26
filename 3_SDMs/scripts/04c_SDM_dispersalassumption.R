# test using a realistic dispersal assumption for SDMs

# Loading packages
library(terra)

# Test it with one SDM scenario
load("3_SDMs/data/Predictions_curr_Batch10_Sim1_Replication14.RData")
load("3_SDMs/data/Predictions_fut_Batch10_Sim1_Replication14.RData")
bg <- terra::rast("3_SDMs/data/land1_optima0.27_breadth0.045_ccYear0.grd")
bg <- bg$temp

# Plot occurrences in landscape
plot(ens_preds_bin[ens_preds_bin$mean_prob== 1, "y"] ~ ens_preds_bin[ens_preds_bin$mean_prob== 1, "x"], xlim = c(0, 511000), 
     ylim = c(0, 511000))

# create SpatVector of presences
occ_rast <- terra::vect(as.matrix(ens_preds_bin[ens_preds_bin$mean_prob == 1, 1:2]))

# create a buffer of the radius of the sampled median dispersal distance around the presences
v_buf <- terra::buffer(occ_rast, width = med_dist_long)

# Create a background mask with target resolution and extent from climate layers
# Set all raster cells outside the buffer to NA.
disp_buf <- terra::mask(bg, v_buf)

# Plot results
#plot(bg, col='grey90', legend=F)
#plot(disp_buf, add=T, col='grey60', legend=F)
#plot(occ_rast, add = T, col= "red")

# use buffer for deleting habitat suitability values
fut_hs <- ens_fut_preds[[10]]

#Plot habitat suitabilities and buffer
plot(r_fut)
plot(disp_buf, add=T, col='grey60', legend=F)

# set HS values outside of the buffer to 0
r_fut <- terra::rast(fut_hs[,1:3])
fut_new <- terra::mask(r_fut, disp_buf, updatevalue=0)

# Plot predictions with dispersal assumptions
plot(fut_new)

# Extract HS values
df <- terra::as.data.frame(fut_new, xy = T)

