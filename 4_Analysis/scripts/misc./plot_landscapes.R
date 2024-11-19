library(NLMR)
library(terra)

#Create temperature landscape ------------------------------------------------------------------------------------------------
l_temp <- nlm_planargradient(ncol = 511, nrow = 511, direction = 180, resolution = 1000) 
#needs to be one cell smaller than the precipitation landscape otherwise the spatial extent is different

#Adding the spatial noise to the temp landscape
  set.seed(765)
l_noise <- nlm_mpd(ncol = 512, nrow = 512, roughness =  0.96, rand_dev = 1, resolution = 1000) 
l_tn <- mosaic(l_temp, l_noise, fun = "mean") #merge them by using the mean

set.seed(234)

l_pre <- nlm_mpd(ncol = 512, nrow = 512, roughness =  0.75, rand_dev = 2, resolution = 1000)

save(l_tn, l_pre, file = "/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/02_Simulations/land1_temp_pre.Rdata")

set.seed(352)
  
l_noise <- nlm_mpd(ncol = 512, nrow = 512, roughness =  0.96, rand_dev = 1, resolution = 1000) 
l_tn <- mosaic(l_temp, l_noise, fun = "mean") #merge them by using the mean
  
set.seed(987)

l_pre <- nlm_mpd(ncol = 512, nrow = 512, roughness =  0.75, rand_dev = 2, resolution = 1000)

save(l_tn, l_pre, file = "/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/02_Simulations/land2_temp_pre.Rdata")


set.seed(836)
  
l_noise <- nlm_mpd(ncol = 512, nrow = 512, roughness =  0.96, rand_dev = 1, resolution = 1000) 
l_tn <- mosaic(l_temp, l_noise, fun = "mean") #merge them by using the mean
  
set.seed(748)

l_pre <- nlm_mpd(ncol = 512, nrow = 512, roughness =  0.75, rand_dev = 2, resolution = 1000)
  
save(l_tn, l_pre, file = "/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/02_Simulations/land3_temp_pre.Rdata")


# # Nicht fuer den Cluster
# load("4_Analysis/Landscapes/land1_temp_pre.Rdata")
# plot(l_pre)
# png('4_Analysis/Landscapes/land1_pre.png', height=2*nrow(l_pre), width=2*ncol(l_pre)) 
# plot(l_pre, maxpixels=2*ncell(l_pre), cex.axis = 1.5)
# dev.off()
# 
# png('4_Analysis/Landscapes/land1_temp.png', height=2*nrow(l_tn), width=2*ncol(l_tn)) 
# plot(l_tn, maxpixels=2*ncell(l_tn), cex.axis = 1.5)
# dev.off()
# 
# load("4_Analysis/Landscapes/land2_temp_pre.Rdata")
# plot(l_pre)
# png('4_Analysis/Landscapes/land2_pre.png', height=2*nrow(l_pre), width=2*ncol(l_pre)) 
# plot(l_pre, maxpixels=2*ncell(l_pre), cex.axis = 1.5)
# dev.off()
# 
# png('4_Analysis/Landscapes/land2_temp.png', height=2*nrow(l_tn), width=2*ncol(l_tn)) 
# plot(l_tn, maxpixels=2*ncell(l_tn), cex.axis = 1.5)
# dev.off()
# 
# load("4_Analysis/Landscapes/land3_temp_pre.Rdata")
# plot(l_pre)
# png('4_Analysis/Landscapes/land3_pre.png', height=2*nrow(l_pre), width=2*ncol(l_pre)) 
# plot(l_pre, maxpixels=2*ncell(l_pre), cex.axis = 1.5)
# dev.off()
# 
# png('4_Analysis/Landscapes/land3_temp.png', height=2*nrow(l_tn), width=2*ncol(l_tn)) 
# plot(l_tn, maxpixels=2*ncell(l_tn), cex.axis = 1.5)
# dev.off()
# 
# # plot niches
# land <- raster("4_Analysis/Landscapes/land1_optima0.5_breadth0.055_ccYear0.asc")
# 
# png('4_Analysis/Landscapes/land1_niche.png', height=2*nrow(land), width=2*ncol(land)) 
# plot(land, maxpixels=2*ncell(land), cex.axis = 1.5)
# dev.off()
# 
# land <- raster("4_Analysis/Landscapes/land2_optima0.5_breadth0.055_ccYear0.asc")
# 
# png('4_Analysis/Landscapes/land2_niche.png', height=2*nrow(land), width=2*ncol(land)) 
# plot(land, maxpixels=2*ncell(land), cex.axis = 1.5)
# dev.off()
# 
# land <- raster("4_Analysis/Landscapes/land3_optima0.5_breadth0.055_ccYear0.asc")
# 
# png('4_Analysis/Landscapes/land3_niche.png', height=2*nrow(land), width=2*ncol(land)) 
# plot(land, maxpixels=2*ncell(land), cex.axis = 1.5)
# dev.off()
