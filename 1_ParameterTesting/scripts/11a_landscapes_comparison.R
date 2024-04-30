# Plot the three different landscapes next to each other
# Goal: Comparing if these different landscapes, show different patterns and how they behave under cc

# File paths
path_input <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/01_TestingParameters/")

# Load packages
library(foreach)
library(doParallel)

# define vector for parameter combinations
width <- c(0.035, 0.04, 0.045, 0.05, 0.055)

#Prepare cluster
ncores <- 5
cl <- makeCluster(ncores)
registerDoParallel(cl)

# Start loops for the SDM fitting
foreach(b=1:length(width), .packages = c("raster", "dplyr", "scales", "tibble", "terra")) %dopar% {
          
          pdf(paste0(path_input, "Output_Maps/landscape_comparison_Breadth",width[b], ".pdf"))
          
          # create climate change values with temporal autocorrelation
          t <- 1:90
          alpha <- 0.5
          beta <- 0.9 
          theta <- 0.3
          set.seed(5678)
          ts <- alpha + beta * t + arima.sim(list(ma = theta), n = length(t))
          x <- as.vector(ts)
          temp_rise <- scales::rescale(x, c(0,0.9))
          
          par(mfrow=c(2,3))
          
          for (i in 1:6) {
            if (i == 1){
              tmp <- rast(paste0(path_input, "Inputs/habitat_per_breadth", width[b], "_cc", temp_rise[1], ".asc"))
              plot(tmp, main = "Year 1, Land 1")
              tmp <- rast(paste0(path_input, "Inputs/habitat_per_land2_breadth", width[b], "_cc", temp_rise[1], ".asc"))
              plot(tmp, main = "Land 2")
              tmp <- rast(paste0(path_input, "Inputs/habitat_per_land3_breadth", width[b], "_cc", temp_rise[1], ".asc"))
              plot(tmp, main = "Land 3")
            } else if (i == 2){
              tmp <- rast(paste0(path_input, "Inputs/habitat_per_breadth", width[b], "_cc", temp_rise[10], ".asc"))
              plot(tmp, main = "Year 10, Land 1")
              tmp <- rast(paste0(path_input, "Inputs/habitat_per_land2_breadth", width[b], "_cc", temp_rise[10], ".asc"))
              plot(tmp, main = "Land 2")
              tmp <- rast(paste0(path_input, "Inputs/habitat_per_land3_breadth", width[b], "_cc", temp_rise[10], ".asc"))
              plot(tmp, main = "Land 3")
            } else if (i == 3){
              tmp <- rast(paste0(path_input, "Inputs/habitat_per_breadth", width[b], "_cc", temp_rise[20], ".asc"))
              plot(tmp, main = "Year 20, Land 1")
              tmp <- rast(paste0(path_input, "Inputs/habitat_per_land2_breadth", width[b], "_cc", temp_rise[20], ".asc"))
              plot(tmp, main = "Land 2")
              tmp <- rast(paste0(path_input, "Inputs/habitat_per_land3_breadth", width[b], "_cc", temp_rise[20], ".asc"))
              plot(tmp, main = "Land 3")
            } else if (i == 4){
              tmp <- rast(paste0(path_input, "Inputs/habitat_per_breadth", width[b], "_cc", temp_rise[30], ".asc"))
              plot(tmp, main = "Year 30, Land 1")
              tmp <- rast(paste0(path_input, "Inputs/habitat_per_land2_breadth", width[b], "_cc", temp_rise[30], ".asc"))
              plot(tmp, main = "Land 2")
              tmp <- rast(paste0(path_input, "Inputs/habitat_per_land3_breadth", width[b], "_cc", temp_rise[30], ".asc"))
              plot(tmp, main = "Land 3")
            } else if (i == 5){
              tmp <- rast(paste0(path_input, "Inputs/habitat_per_breadth", width[b], "_cc", temp_rise[40], ".asc"))
              plot(tmp, main = "Year 40, Land 1")
              tmp <- rast(paste0(path_input, "Inputs/habitat_per_land2_breadth", width[b], "_cc", temp_rise[40], ".asc"))
              plot(tmp, main = "Land 2")
              tmp <- rast(paste0(path_input, "Inputs/habitat_per_land3_breadth", width[b], "_cc", temp_rise[40], ".asc"))
              plot(tmp, main = "Land 3")
            } else {
              tmp <- rast(paste0(path_input, "Inputs/habitat_per_breadth", width[b], "_cc", temp_rise[50], ".asc"))
              plot(tmp, main = "Year 50, Land 1")
              tmp <- rast(paste0(path_input, "Inputs/habitat_per_land2_breadth", width[b], "_cc", temp_rise[50], ".asc"))
              plot(tmp, main = "Land 2")
              tmp <- rast(paste0(path_input, "Inputs/habitat_per_land3_breadth", width[b], "_cc", temp_rise[50], ".asc"))
              plot(tmp, main = "Land 3")
            }
          }
            
            dev.off()
          }
          stopCluster(cl)
          
          #clean all temporary files
          gc()
          rm(list=ls())