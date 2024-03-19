#Test pattern of a real landscape

# In this script I aim to test and compare how real landscapes would look like at the resolution and pattern of my artificial landscape to test if they are realisitic

library(geodata)

# Download climate data from worldclim
clim <- geodata::worldclim_global(var = 'bio', res = 2.5, download = T, path = 'data')

plot(clim)

#Extract mean annual temperature (bio1) and mean annual precipitation (bio12)
clim_bio1 <- clim[[1]]

plot(clim_bio1)

clim_bio12 <- clim[[12]]

plot(clim_bio12)

#crop maps to germany extent
clim_ger_bio1 <- terra::crop(clim_bio1, c(-5,35,40,60))

plot(clim_ger_bio1)

clim_ger_bio12 <- terra::crop(clim_bio12, c(-5,35,40,60))

plot(clim_ger_bio12)

# change coordinate systems to UTM
clim_ger_bio1 <- project(clim_ger_bio1, "EPSG:25832")
#crop both layers to 512x512km
plot(clim_ger_bio1)

clim_ger_bio12 <- project(clim_ger_bio12, "EPSG:25832")
#crop both layers to 512x512km
plot(clim_ger_bio12)

#crop layers to 512x512km extent
clim_ger_bio1_crop <- terra::crop(clim_ger_bio1, c(150000, 662000, 5000000, 5512000))
plot(clim_ger_bio1_crop)

clim_ger_bio12_crop <- terra::crop(clim_ger_bio12, c(150000, 662000, 5000000, 5512000))
plot(clim_ger_bio12_crop)

