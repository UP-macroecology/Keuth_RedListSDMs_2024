# Testing of different niche breadths
# I first only model range-shifting species, as additionally to surviving the spin-up they also need to be none dispersal-limited
# the other parameters are set to the values of the critical species (Rmax = 3, Dispersaldistance: 15000)
# additionally rare long distance dispersal events are included, parameters for this are based on Fandos(2023) as well as
# additional Literature research, which reported similar values

path_input <- file.path("/import/ecoc9z/data-zurell/keuth/01_TestingParameters/")
#path_input <- file.path("Bugs/")

# Function for calculating extinction probability
source(paste0(path_input, "Functions/Extinction_probability.R"))

#Packages
library(raster)
library(landscapetools)
library(NLMR)
library(virtualspecies)
require(RangeShiftR)
library(dplyr)
library(scales)
require(foreach)
require(doParallel)
library(tibble)
library(ggplot2)
library(gridExtra)

# define vector for parameter combinations
width <- c(0.025, 0.035)

# prepare lists
pop_mean <- vector("list", length(Disp))
extProb_list <- vector("list", length(Disp))

#Prepare cluster
ncores <- 3
cl <- makeCluster(ncores)
registerDoParallel(cl)

foreach(b=1:length(width), .packages = c("raster", "virtualspecies", "RangeShiftR", "dplyr", "scales", "tibble", "ggplot2", "gridExtra")) %dopar% {
   pdf(paste0(path_input, paste0("Output_Maps/plots_nichebreadth", width[b], ".pdf"))) # PDF with necessary plots

  #obtain number for temperature increase
  t <- 1:90
  alpha <- 0.5
  beta <- 0.9 
  theta <- 0.3
  set.seed(5678)
  ts <- alpha + beta * t + arima.sim(list(ma = theta), n = length(t))
  x <- as.vector(ts)
  temp_rise <- rescale(x, c(0,0.9))
  
  #Load landscape stack under cc
  load(paste0(path_input, "Inputs/landscape_stack.RData"))

  #response curves to the environmental variables
  param <- formatFunctions(temp = c(fun = "dnorm", mean= 0.5, sd=width[b]),
                          pre = c(fun = "dnorm", mean = 0.5, sd=width[b]))


    ls_spec <- vector("list", length = length(ls_cc)) #create a vector of a specific length
    s_name <- c() #create name vector to rename later

    # create habitat suitability maps -----------------------------------------------------------------------------------------

    for (i in 1:length(ls_cc)) {
      temp <- ls_cc[[i]]
      tmp <- generateSpFromFun(raster.stack = temp[[c("temp", "pre")]], parameters = param, plot = F)
      ls_spec[[i]] <- tmp #add the habitat suitability object as a new element in the list
      d <- temp_values[i]
      s_name <- append(s_name, paste0("cc_", d))
    }
    names(ls_spec) <- s_name

    # Plot HS maps under climate change
    par(mfrow=c(2,3))
    plot(ls_spec[[1]], main = "Climate Change year 1")
    plot(ls_spec[[12]], main = "year 12")
    plot(ls_spec[[25]], main = "year 25")
    plot(ls_spec[[35]], main = "year 35")
    plot(ls_spec[[45]], main = "year 45")
    plot(ls_spec[[55]], main = "year 55")
    par(mfrow=c(2,2))
     
    for (i in 1:length(ls_spec)) {
      d <- temp_values[i]
      temp <- ls_spec[[i]][["suitab.raster"]]
      values(temp) <- values(temp)*100 #make values to percentages
      writeRaster(temp, filename = paste(path_loop,"Inputs/habitat_per_breadth", width[b], "_cc", d, ".asc", sep = ""), 
                  overwrite = T, format = "ascii")
    }

    # Set up the dynamic landscapes ------------------------------------------------------------------------------------
    # Numbers of spinup years in dynamic landscape
    spinup <- 100
    
    # Set dynamic landscape parameters
    #create vector to read in landscapes:
    val <- temp_rise
    val <- c("0", val)
    landnames <- c()
    for (i in 1:length(val)){
      d <- val[i]
      k <- paste0("habitat_per_breadth", width[b], "_cc", d, ".asc")
      landnames <- append(landnames, k)
    }
    
    #create vector for the years
    years <- seq(spinup,((length(landnames)-2)*1)+100,1) #minus 2 because we have the spinup and the year 0
    years <- c("0", years)
    years  <- as.numeric(years)
    
    # Define Landscape module ------------------------------------------------------------------------------------
    
    land <- ImportedLandscape(LandscapeFile = landnames,
                              DynamicLandYears = years,
                              Resolution = 1000,
                              HabPercent = TRUE,
                              K_or_DensDep = 0.05)
    
    # Define transition matrix ------------------------------------------------------------------------------------
    
    
    # Define demography module ------------------------------------------------------------------------------------
    
    demo <- Demography(Rmax = 3, ReproductionType = 0) # sexual model with no stage structure
    
      # Define dispersal module ------------------------------------------------------------------------------------
      disp <-  Dispersal(
        # Emigration phase: stage 0 has constant emigration probability of 0.4
        Emigration = Emigration(EmigProb = 0.4),
        # Transfer phase: negative exponential dispersal kernel with mean dispersal distance of 8km
        Transfer = DispersalKernel(DoubleKernel = T, Distances = matrix(c(15000, 250000, 0.95), ncol = 3)),
        # Settlement: if individual arrives in unsuitable cells, it can randomly chose a suitable neighbouring cell or will die
        Settlement = Settlement(Settle = 2)
      )
      
      # Define initial conditions for simulations ------------------------------------------------------------------------------------
      
    init <- Initialise(InitType = 0, FreeType = 1, InitDens = 1)
    
    RepNb <- 10 #100 replicated runs
    sim_years <- 89 #let simulation run for 89 years
    sim <- Simulation(Simulation = 0,
                      Replicates = RepNb,
                      Years = spinup + sim_years,
                      OutIntPop = 1,
                      OutIntOcc = 1)
      
      s <- RSsim(batchnum = b , land = land, demog = demo, dispersal = disp, simul = sim,
                 init = init)
      
      # Run simulations ------------------------------------------------------------------------------------
      
      RunRS(s, path_loop)
      
      # Calculate population and occupancy mean and extinction probability ------------------------------------
      range <- readRange(s, path_loop)
      #range <- read.table(paste0(path_loop, "Outputs/Batch1_Sim0_Land1_Range.txt"), h = T, sep = "\t")
      pop_mean[[b]] <- range %>% group_by(Year) %>% summarise(Abundance = mean(NInds), sd_Ab = sd(NInds),
                                                              Occupancy = mean(NOccupCells), sd_Oc = sd(NOccupCells)) %>% add_column(Dispersal = paste0("Dispersal: ", Disp[e]))
      pop <- readPop(s, path_loop)
      extProb_list[[b]] <- Calc_ExtProb(pop, s) %>% add_column(Dispersal = paste0("Dispersal: ", Disp[e]))
      
}

stopCluster(cl)
    
    plot_list <- vector("list", length = 4)
    plot_list[[1]] <- ggplot(pop_mean[[2]], aes(x = Year, y = Abundance, color=Dispersal))+
      geom_line()+
      geom_line(data=pop_mean[[1]], aes(x = Year, y=Abundance, color=Dispersal))+
      geom_line(data=pop_mean[[3]], aes(x = Year, y=Abundance, color=Dispersal))+
      geom_line(data=pop_mean[[4]], aes(x = Year, y=Abundance, color=Dispersal))+
      geom_line(data=pop_mean[[5]], aes(x = Year, y=Abundance, color=Dispersal))+
      theme(legend.key.size = unit(0.3, 'cm'), #change legend key size
            legend.title = element_text(size=6), #change legend title font size
            legend.text = element_text(size=5)) #change legend text font size
    
    plot_list[[2]] <-  ggplot(pop_mean[[2]], aes(x = Year, y = Occupancy, color= Dispersal))+
      geom_line()+
      geom_line(data=pop_mean[[1]], aes(x = Year, y=Occupancy, color= Dispersal))+
      geom_line(data=pop_mean[[3]], aes(x = Year, y=Occupancy, color=Dispersal))+
      geom_line(data=pop_mean[[4]], aes(x = Year, y=Occupancy, color=Dispersal))+
      geom_line(data=pop_mean[[5]], aes(x = Year, y=Occupancy, color=Dispersal))+
      theme(legend.key.size = unit(0.3, 'cm'), #change legend key size
            legend.title = element_text(size=6), #change legend title font size
            legend.text = element_text(size=5)) #change legend text font size
    
    plot_list[[3]] <- ggplot(extProb_list[[2]], aes(x = Year, y = extProb, color= Dispersal))+
      geom_line()+
      geom_line(data=extProb_list[[1]], aes(x = Year, y=extProb, color= Dispersal))+
      geom_line(data=extProb_list[[3]], aes(x = Year, y=extProb, color= Dispersal))+
      geom_line(data=extProb_list[[4]], aes(x = Year, y=extProb, color= Dispersal))+
      geom_line(data=extProb_list[[5]], aes(x = Year, y=extProb, color= Dispersal))+
      theme(legend.key.size = unit(0.3, 'cm'), #change legend key size
            legend.title = element_text(size=6), #change legend title font size
            legend.text = element_text(size=5)) #change legend text font size
    
    plot_list[[4]] <- ggplot() +
      annotate("text",
               x = 1,
               y = 1,
               size = 4,
               label = paste0("R:0.75\nCC:0.5;0.9;0.3\nCC:1:90,0:0.9\nntemp:0.25+", width[b], "\nnpre:0.5+", width[b], "\nK:0.05\nRmax:", Rmax[a], "\nEmigProb:0.4\nDispersal:variable")) +
      theme_void()
    
    #Plot all of them in the same window
    grid.arrange(grobs = plot_list)
    
    dev.off() #save pdf


#clean all temporary files
gc()
rm(list=ls())