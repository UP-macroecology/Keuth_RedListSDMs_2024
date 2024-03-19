# In this script I want to test different niche positions to narrow down the parameter options I have to model the cold-adapted (range-contracting) and 
# warm-adapted species (range-shifting)

# Load packages
library(raster)
library(NLMR)
library(virtualspecies)
library(dplyr)

#
tempdir()

path_input <- file.path(paste0("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/"))

# create landscape parameters
pos <- c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6)

ls_spec <- vector("list", length(pos))
for (a in 1:length(pos)){
    
    #Create temperature landscape ------------------------------------------------------------------------------------------------
    l_temp <- nlm_planargradient(ncol = 511, nrow = 511, direction = 180, resolution = 1000) 
    #needs to be one cell smaller than the precipitation landscape otherwise the spatial extent is different
    
    #Adding the spatial noise to the temp landscape
    set.seed(765)
    l_noise <- nlm_mpd(ncol = 512, nrow = 512, roughness =  0.96, rand_dev = 1, resolution = 1000) 
    l_tn <- mosaic(l_temp, l_noise, fun = "mean") #merge them by using the mean
    
    #Create precipitation landscape -----------------------------------------------------------------------------------------------
    set.seed(234)
    l_pre <- nlm_mpd(ncol = 512, nrow = 512, roughness =  0.75, rand_dev = 2, resolution = 1000)
    
    # create habitat suitability maps -----------------------------------------------------------------------------------------
    r_env <- stack(l_tn, l_pre)
    names(r_env) <- c("temp", "pre")
    
    #response curves for the different virtual species (mean of temperature and sd are adapted to the respective scenarios)
    param <- formatFunctions(temp = c(fun = "dnorm", mean= pos[a], sd=0.035),
                             pre = c(fun = "dnorm", mean = 0.5, sd=0.035))
    
    ls_spec[[a]] <- generateSpFromFun(raster.stack = r_env[[c("temp", "pre")]], parameters = param, plot = T) 
    
}

    pdf(paste0(path_input, "landscapes_different_positions.pdf"))
    par(mfrow=c(2,2))
    plot(ls_spec[[1]], main = "position 0.2")
    plot(ls_spec[[2]], main = "position 0.25")
    plot(ls_spec[[3]], main = "position 0.3")
    plot(ls_spec[[4]], main = "position 0.35")
    plot(ls_spec[[5]], main = "position 0.4")
    plot(ls_spec[[6]], main = "position 0.45")
    plot(ls_spec[[7]], main = "position 0.5")
    plot(ls_spec[[8]], main = "position 0.55")
    plot(ls_spec[[9]], main = "position 0.6")
    par(mfrow=c(2,2))
    dev.off()
    
    #clean all temporary files
    gc()
    rm(list=ls())