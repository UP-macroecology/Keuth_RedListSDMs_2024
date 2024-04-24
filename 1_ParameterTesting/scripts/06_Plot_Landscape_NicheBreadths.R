# Plot the landscapes with the different niche breadths under climate change

#Load packages
library(gridExtra)
library(scales)
library(terra)

# Specify input file path
path_input <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/01_TestingParameters/")

# set different niche breadths
width <- c(0.025, 0.035, 0.045, 0.055, 0.065, 0.075)

# obtain temperature values
t <- 1:90
alpha <- 0.5
beta <- 0.9 
theta <- 0.3
set.seed(5678)
ts <- alpha + beta * t + arima.sim(list(ma = theta), n = length(t))
x <- as.vector(ts)
temp_rise <- scales::rescale(x, c(0,0.9))

# Plot 6 landscapes (6 different years) for every niche breadth
pdf(paste0(path_input, "Output_Maps/Landscape_nichebreadths_comparison.pdf"))
par(mfrow=c(2,3))
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[1], "_cc", temp_rise[1], ".asc")), main = paste0("Breadth:", width[1], " Year 0"))
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[1], "_cc", temp_rise[11], ".asc")), main = "Year 10")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[1], "_cc", temp_rise[21], ".asc")), main = "Year 20")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[1], "_cc", temp_rise[31], ".asc")), main = "Year 30")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[1], "_cc", temp_rise[41], ".asc")), main = "Year 40")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[1], "_cc", temp_rise[51], ".asc")), main = "Year 50")

plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[2], "_cc", temp_rise[1], ".asc")), main = paste0("Breadth:", width[2], " Year 0"))
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[2], "_cc", temp_rise[11], ".asc")), main = "Year 10")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[2], "_cc", temp_rise[21], ".asc")), main = "Year 20")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[2], "_cc", temp_rise[31], ".asc")), main = "Year 30")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[2], "_cc", temp_rise[41], ".asc")), main = "Year 40")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[2], "_cc", temp_rise[51], ".asc")), main = "Year 50")

plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[3], "_cc", temp_rise[1], ".asc")), main = paste0("Breadth:", width[3], " Year 0"))
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[3], "_cc", temp_rise[11], ".asc")), main = "Year 10")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[3], "_cc", temp_rise[21], ".asc")), main = "Year 20")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[3], "_cc", temp_rise[31], ".asc")), main = "Year 30")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[3], "_cc", temp_rise[41], ".asc")), main = "Year 40")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[3], "_cc", temp_rise[51], ".asc")), main = "Year 50")

plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[4], "_cc", temp_rise[1], ".asc")), main = paste0("Breadth:", width[4], " Year 0"))
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[4], "_cc", temp_rise[11], ".asc")), main = "Year 10")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[4], "_cc", temp_rise[21], ".asc")), main = "Year 20")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[4], "_cc", temp_rise[31], ".asc")), main = "Year 30")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[4], "_cc", temp_rise[41], ".asc")), main = "Year 40")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[4], "_cc", temp_rise[51], ".asc")), main = "Year 50")

plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[5], "_cc", temp_rise[1], ".asc")), main = paste0("Breadth:", width[5], " Year 0"))
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[5], "_cc", temp_rise[11], ".asc")), main = "Year 10")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[5], "_cc", temp_rise[21], ".asc")), main = "Year 20")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[5], "_cc", temp_rise[31], ".asc")), main = "Year 30")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[5], "_cc", temp_rise[41], ".asc")), main = "Year 40")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[5], "_cc", temp_rise[51], ".asc")), main = "Year 50")

plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[6], "_cc", temp_rise[1], ".asc")), main = paste0("Breadth:", width[6], " Year 0"))
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[6], "_cc", temp_rise[11], ".asc")), main = "Year 10")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[6], "_cc", temp_rise[21], ".asc")), main = "Year 20")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[6], "_cc", temp_rise[31], ".asc")), main = "Year 30")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[6], "_cc", temp_rise[41], ".asc")), main = "Year 40")
plot(rast(paste0(path_input, "Inputs/habitat_per_breadth", width[6], "_cc", temp_rise[51], ".asc")), main = "Year 50")
dev.off()
