# Extract abundances from pop file for only one replicate run (reduces the size of the file strongly)

library(doParallel)
library(foreach)


#define file path
sim_dir <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/02_Simulations/")

# start cluster
ncores <- 16
cl <- makeCluster(ncores)
registerDoParallel(cl)

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