# Spatial thinning of the data points for every of the 16 scenarios for each landscape replica

# Load packages
library(terra)
library(dismo)
library(dplyr)
require(foreach)
require(doParallel)
library(tibble)
library(data.table)

#define file path
sim_dir <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/02_Simulations/")
sdm_dir <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/03_SDMs/")

# create data frame with all parameter combinations
land_rep <- 1:3
optima <- c(0.27, 0.5)
breadth <- c(0.045, 0.055)
rmax <- c(3, 5)
dispersal <- c(5000, 15000)

sims <- expand.grid(land_rep = land_rep, optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims$BatchNum <- rep(1:16, each = 3)

ncores <- 48
cl <- makeCluster(ncores)
registerDoParallel(cl)

foreach(sim_nr=1:nrow(sims), .packages = c("dismo", "dplyr", "tibble", "terra", "data.table")) %dopar% {
  
  # Prepare variables --------------
  rep_nr <- sims[sim_nr,]$land_rep
  optima <- sims[sim_nr,]$optima
  breadth <- sims[sim_nr,]$breadth
  BatchNum <- sims[sim_nr,]$BatchNum
  
  # #read in current landscape
  # clim <- rast(paste0(sdm_dir, "data/landscapes/land", rep_nr, "_optima",  optima, "_breadth", breadth, "_ccYear0.grd"))
  # #clim <- rast("3_SDMs/data/land1_optima0.27_breadth0.045_ccYear0.grd")
  # mask <- clim[[1]]
  # 
  # #Load in presence of respective scenario
  # presences <- readRDS(paste0(sdm_dir, "data/occurrences/Occ_list_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
  # #presences <- readRDS("3_SDMs/data/Occ_list_Batch1_Sim1.rds")
  # 
  # presences <- lapply(presences, function(x){x <- as.data.frame(x); return(x)})
  # 
  # #Create Absence Points -----------------------------------------------------------------
  # #create absences for every replicate run
  # absences <- vector("list", length = length(presences))
  # for (i in 1:length(presences)) {
  #   tmp.presences <- presences[[i]][,2:3]
  #   cell.numbers <- c(1:length(values(mask))) #obtain all cell numbers
  #   presences.cellnumbers <- cellFromXY(mask, tmp.presences) #obtain cell numbers of presences
  #   absences.cellnumbers <- setdiff(cell.numbers, presences.cellnumbers) #obtain non occupied cell numbers
  #   tmp.absences <- as.data.frame(xyFromCell(mask, absences.cellnumbers)) #obtain coordinates from the cell numbers
  #   tmp.absences$occ <- 0
  #   colnames(tmp.absences)[colnames(tmp.absences) == 'x'] <- "X"
  #   colnames(tmp.absences)[colnames(tmp.absences) == 'y'] <- "Y"
  #   absences[[i]] <- tmp.absences
  #   rm(tmp.presences, cell.numbers, presences.cellnumbers, absences.cellnumbers, tmp.absences)
  # }
  # names(absences) <- names(presences)
  # 
  # # combine presences and absences -----------------------------------------------------------
  # points_full <- lapply(presences, function(x){x <- data.frame(x[,2:3], occ = 1); return(x)})
  # for (i in 1:length(points_full)) {
  #   points_full[[i]] <- rbind(points_full[[i]], absences[[i]])
  # }
  # 
  # #saveRDS(points_full, paste0(sdm_dir, "data/occurrences/Occ_Abs_wo_clim_list_Batch_", BatchNum, "_Sim", rep_nr, ".rds"))
  # 
  # # extract climatic values for every cell
  # points_full <- lapply(points_full, function(x){x <- cbind(x, extract(x = clim, y = x[, c("X", "Y")], cells = T, ID = F, xy = T)); return(x)})
  # saveRDS(points_full, paste0(sdm_dir, "data/occurrences/Occ_Abs_full_list_Batch_", BatchNum, "_Sim", rep_nr, ".rds"))
  # 
  # # spatial thinning -------------------------------------------------------------------------
  # xy <- lapply(points_full, function(x){x <- gridSample(x[,c("X", "Y")], mask, chess = "white"); return(x)})
  # points_thinned <- vector("list", length = length(points_full))
  # for (i in 1:length(points_full)) {
  #   points_thinned[[i]] <- merge(xy[[i]], points_full[[i]], by=c("X", "Y"))
  # }
  # names(points_thinned) <- names(points_full)
  # 
  # saveRDS(points_thinned, paste0(sdm_dir, "data/occurrences/Occ_Abs_thinned_list_Batch_", BatchNum, "_Sim", rep_nr, ".rds"))
  points_thinned <- readRDS(paste0(sdm_dir, "data/occurrences/Occ_Abs_thinned_list_Batch_", BatchNum, "_Sim", rep_nr, ".rds"))

  #number of presence and absence points for the replicated runs used in the SDMs ----------------

  # select 10 random replications from the 100
  set.seed(8765)
  replicates <- sample(0:99, 10) # these represent the position in the list and not the actual replicate runs

  # presences and absences for those replicated runs
  points.short <- points_thinned[replicates]
  points.short <- lapply(points.short, function(x){x <- as.data.frame(x); return(x)})

  #obtain presences and absences
  points.pre <- lapply(points.short, function(x){x <- x %>% filter(occ == 1); return(x)})
  points.abs <- lapply(points.short, function(x){x <- x %>% filter(occ == 0); return(x)})

  # prepare table
  tab.num <- data.frame(presences = 0, absences = 0)

  # obtain number of presences and absences
  for (i in 1:10) {
    tmp <- data.frame(presences = nrow(points.pre[[i]]), absences = nrow(points.abs[[i]]))
    tab.num <- rbind(tab.num, tmp)
  }

  sink(paste0(sdm_dir, "data/occurrences/number_presences_absences_Batch", BatchNum, "_Sim", rep_nr, ".txt"))
  print("mean Presences")
  print(mean(tab.num$presences[-1]))
  print("sd Presences")
  print(sd(tab.num$presences[-1]))
  print("mean Absences")
  print(mean(tab.num$absences[-1]))
  print("sd Absences")
  print(sd(tab.num$absences[-1]))
  sink()
}

stopCluster(cl)

gc()
rm(list=ls())
