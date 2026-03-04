# Red List criteria underestimate climate-related extinction risk of range-shifting species


#-------------------------------------------------------------------------------

# ---------------------------------------------------------- #
#              07c. Abundance data for Figure 2              #
# ---------------------------------------------------------- #


#-------------------------------------------------------------------------------

# Extract abundance data for one replicate run for Figure 2

# Load packages
library(doParallel)
library(foreach)

# define file paths
home_folder <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/") #needs to be adjusted based on own folder structure
sim_dir <- file.path(paste0(home_folder, "Simulations/"))
sdm_dir <- file.path(paste0(home_folder, "output_data/SDMs/"))

# set up HPC
ncores <- 16
cl <- makeCluster(ncores)
registerDoParallel(cl)

# loop for every single species
foreach(BatchNum=1:16, .packages = c("data.table")) %dopar% {
  
  #read in pop data
  pop <- fread(paste0(sim_dir, "Outputs/Batch", BatchNum, "_Sim1_Land1_Pop.txt"))
  
  # extract replicate run
  pop <- subset(pop, pop$Rep == 3)
  
  #correct coordinates
  pop$x <- (pop$x + 0.5) * 1000
  pop$y <- (pop$y + 0.5) * 1000
  
  #save data set
  save(pop, file = paste0(sim_dir, "Outputs/Batch", BatchNum, "_Sim1_Land1_Pop_Rep4.Rdata"))
}

stopCluster(cl)

gc()
rm(list=ls())