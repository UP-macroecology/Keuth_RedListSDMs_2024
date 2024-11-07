# Estimate SDMs
# Estimate SDMs to all 16 scenarios and every landscape replicate and predict the habitat suitability to all future years

#Load packages
library(maxnet)
library(gbm)
library(dplyr)
require(foreach)
require(doParallel)
library(scales)
library(tibble)
library(terra)
library(randomForest)
library(mecofun)
library(data.table)

# Load functions
#source("/import/ecoc9z/data-zurell/keuth/final_simulations/Functions/evalSDM.R") 
#source("/import/ecoc9z/data-zurell/keuth/final_simulations/Functions/response_curves.R")

#define file path
sdm_dir <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/03_SDMs/")

# create data frame with all parameter combinations
land_rep <- 1:3
optima <- c(0.27, 0.5)
breadth <- c(0.045, 0.055)
rmax <- c(3, 5)
dispersal <- c(5000, 15000)

sims <- expand.grid(land_rep = land_rep, optima = optima, breadth = breadth, rmax = rmax, dispersal = dispersal)
sims$BatchNum <- rep(1:16, each = 3)

# select 10 random replications from the 100
set.seed(8765)
replicates <- sample(0:99, 10)

#set up cluster
ncores <- 24
cl <- makeCluster(ncores)
registerDoParallel(cl)
                     
# Loops for the SDM fitting
foreach(sim_nr=25:nrow(sims), .packages = c("raster", "maxnet", "gbm", "dplyr", "tibble", "terra", "randomForest", "mecofun", "data.table")) %dopar% {
     
  # Prepare variables --------------
  rep_nr <- sims[sim_nr,]$land_rep
  optima <- sims[sim_nr,]$optima
  breadth <- sims[sim_nr,]$breadth
  BatchNum <- sims[sim_nr,]$BatchNum

  #read in climate landscape
  clim <- rast(paste0(sdm_dir, "data/landscapes/land", rep_nr, "_optima",  optima, "_breadth", breadth, "_ccYear0.grd"))
  clim_pack <- wrap(clim)

  #Read in thinned points
  points_thinned <- readRDS(paste0(sdm_dir, "data/occurrences/Occ_Abs_thinned_list_Batch_", BatchNum, "_Sim", rep_nr, ".rds"))
  #points_thinned <- readRDS("3_SDMs/data/Occ_Abs_thinned_list_Batch_1_Sim1.rds")
  
  points_thinned <- lapply(points_thinned, function(x){x <- x[,-which(names(x) %in% c("cell","x", "y"))]; return(x)})
    
  # create lists for the different obtained data
  performance_mean <- vector("list", length = length(replicates))
  performance_raw <- vector("list", length = length(replicates))
  hs_change <- vector("list", length = length(replicates))
  range_size <- vector("list", length = length(replicates))
    
  # Start loop for the 10 different replicates (fitting a SDM to each of them)
  for(replicate_nr in 1:length(replicates)){
    #create the data frames
    performance_raw_loop <- c()
    performance_mean_loop <- c()
      
    #get presences and absences for respective replicated run
    data_train <- points_thinned[[replicates[replicate_nr]]]
    #get testing data (all replicated runs except the one used for fitting)
    data_test <- points_thinned[-(replicates[replicate_nr])]

    # Fit GLM ------------------------------------------------------------------------

    loop_glm <- step(glm(occ ~ temp + I(temp^2) + pre + I(pre^2), family = "binomial", data = data_train))

    #save output in txt.file
    sink(paste0(sdm_dir, "evaluation/algorithm_output/GLM_Outputs_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".txt"))
    print(summary(loop_glm))
    sink()

    #Fit RF -------------------------------------------------------------------------
    loop_rf <- randomForest(x=data_train[,c("temp", "pre")], y=data_train$occ, ntree=1000, nodesize = 20, importance =T)

    #save output in txt.file
    sink(paste0(sdm_dir, "evaluation/algorithm_output/RF_Outputs_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".txt"))
    print(randomForest::importance(loop_rf,type=1))
    # Look at single trees:
    print(getTree(loop_rf,1,T))
    sink()

    # Fit Maxent -------------------------------------------------------------------
    loop_maxent <- maxnet(p=data_train$occ, data=data_train[,c("temp", "pre")], maxnet.formula(p=data_train$occ, data=data_train[,c("temp", "pre")]), classes = "lqh")

    #save algorithms
    save(loop_glm, loop_rf, loop_maxent, file = paste0(sdm_dir, "algorithms/Algorithms_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".RData"))
    #load(paste0(sdm_dir, "algorithms/Algorithms_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".RData"))
    #load("3_SDMs/data/Algorithms_Batch1_Sim1_Replication87.RData")

    #PDF for response curves--------------------------------------------------------
    pdf(paste0(sdm_dir, "evaluation/algorithm_output/Response_curves_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".pdf"))

    #Response curves for GLM
    par(mfrow=c(1,2))
    partial_response(loop_glm, predictors = data_train[,c("temp", "pre")], main='GLM (partial)', ylab='Occurrence probability')
    inflated_response(loop_glm, predictors = data_train[,c("temp", "pre")], main='GLM (inflated)', ylab='Occurrence probability')

    # Response curves for RF
    partial_response(loop_rf, predictors = data_train[,c("temp", "pre")], main='Random Forest (partial)', ylab='Occurrence probability')
    inflated_response(loop_rf, predictors = data_train[,c("temp", "pre")], main='Random Forest (inflated)', ylab='Occurrence probability')
    par(mfrow=c(1,1))

    varImpPlot(loop_rf)

    #partial response curves for Maxent
    par(mfrow=c(1,2))
    partial_response(loop_maxent, predictors = data_train[,c("temp", "pre")], main='Maxent (partial)', ylab='Occurrence probability')
    inflated_response(loop_maxent, predictors = data_train[,c("temp", "pre")], main='Maxent (inflated)', ylab='Occurrence probability')
    par(mfrow=c(1,1))

    dev.off()

    # Evaluate performance -----------------------------------------------------------------------------------
    #validated separately against all 99 replicate runs that are not used for fitting

    #performance measures of GLM
    for (i in 1:length(data_test)) {
      test <- data_test[[i]]
      perf_glm <- evalSDM(test$occ, predict(loop_glm, test[,c("temp", "pre")], type='response') ) #evaluate performance
      performance_raw_loop <- rbind(performance_raw_loop, perf_glm %>% add_column(testRep = names(data_test[i]), Algorithm = "GLM") %>% relocate(c(testRep, Algorithm)))
      rm(perf_glm)
    }

    #performance measures of RF
    for (i in 1:length(data_test)) {
      test <- data_test[[i]]
      perf_rf <- evalSDM(test$occ, predict(loop_rf, test[,c("temp", "pre")], type='response') )
      performance_raw_loop <- rbind(performance_raw_loop, perf_rf %>% add_column(testRep = names(data_test[i]), Algorithm = "RF") %>% relocate(c(testRep, Algorithm)))
      rm(perf_rf)
    }

    #performance measures of Maxent
    for (i in 1:length(data_test)) {
      test <- data_test[[i]]
      perf_maxent <- evalSDM(test$occ, predict(loop_maxent, test[,c("temp", "pre")], type='logistic'))
      performance_raw_loop <- rbind(performance_raw_loop, perf_maxent %>% add_column(testRep = names(data_test[i]), Algorithm = "Maxent") %>% relocate(c(testRep, Algorithm)))
      rm(perf_maxent)
    }

    # back transform to data frame
    performance_raw_loop <- as.data.frame(performance_raw_loop)

    #Calculate predictions for each test replicated run for later calculating ensembles -----------------------------------------------
    pred_glm <- c()
    pred_rf <- c()
    pred_maxent <- c()
    pred_names <- c()
    for (i in 1:length(data_test)) {
      test <- data_test[[i]]
      loop.pred.glm <- predict(loop_glm, test[,c("temp", "pre")], type="response")
      pred_glm <- cbind(pred_glm, loop.pred.glm)
      loop.pred.rf <- predict(loop_rf, test[,c("temp", "pre")], type='response')
      pred_rf <- cbind(pred_rf, loop.pred.rf)
      loop.pred.maxent <- predict(loop_maxent, test[,c("temp", "pre")], type = "logistic")
      pred_maxent <- cbind(pred_maxent, loop.pred.maxent)
      pred_names <- append(pred_names, paste0("Rep", names(data_test[i])))
    }

    #transform into dataframe and change column names
    pred_glm <- as.data.frame(pred_glm)
    colnames(pred_glm) <- pred_names
    pred_rf <- as.data.frame(pred_rf)
    colnames(pred_rf) <- pred_names
    pred_maxent <- as.data.frame(pred_maxent)
    colnames(pred_maxent) <- pred_names

    #Take mean of predictions of all test replications
    pred_testdata <- data.frame(
      RF = rowMeans(pred_rf),
      GLM = rowMeans(pred_glm),
      Maxent = rowMeans(pred_maxent)
    )

    #save(pred_testdata, file = paste0(sdm_dir, "evaluation/algorithm_output/pred_testdata_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".Rdata"))

    #Calculate ensembles --------------------------------------------------------------------------------
    # Mean of probabilities
    ensemble_mean <- rowMeans(pred_testdata)
    # Calculate uncertainty measures
    sd_prob <- apply(pred_testdata, 1, sd)

    #Save ensembles
    save(ensemble_mean, sd_prob, file = paste0(sdm_dir, "algorithms/Ensemble_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".RData"))
    #load(paste0(sdm_dir, "algorithms/Ensemble_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".RData"))

    #Evaluate performance of ensemble model -----------------------------------------------------------
    for (i in 1:length(data_test)) {
      test <- data_test[[i]]
      perf_mean <- evalSDM(test$occ, ensemble_mean)
      performance_raw_loop <- rbind(performance_raw_loop, perf_mean %>% add_column(testRep = names(data_test[i]), Algorithm = "mean_prob") %>%
                                                          relocate(c(testRep, Algorithm)))
      rm(perf_mean)
      }

    # back transform into data frame
    performance_raw_loop <- as.data.frame(performance_raw_loop)

    # save performance measures in list
    performance_raw[[replicate_nr]] <- performance_raw_loop

    # Average evaluation metrics ----------------------------------------------------------------------
    #calculate mean over performance measures of all test replicate runs per algorithm and the ensemble
    performance_mean_loop <- performance_raw_loop %>% group_by(Algorithm) %>% summarise(mean_AUC = mean(AUC), sd_AUC = sd(AUC), mean_TSS = mean(TSS),
                                                                                        sd_TSS = sd(TSS), mean_Kappa = mean(Kappa), sd_Kappa = sd(Kappa),
                                                                                        mean_Sens = mean(Sens), sd_Sens = sd(Sens), mean_Spec = mean(Spec),
                                                                                        sd_Spec = sd(Spec), mean_PCC = mean(PCC), sd_PCC = sd(PCC),
                                                                                        mean_D2 = mean(D2), sd_D2 = sd(D2), mean_thresh = mean(thresh),
                                                                                        sd_thresh = sd(thresh), .groups = "keep") %>%
                                                      add_column(Rep = replicates[replicate_nr]) %>% relocate(Rep, .before = Algorithm)

    # back transform because it is saved as a tibble
    performance_mean_loop <- as.data.frame(performance_mean_loop)
    # save as single element in list of the performance measures of all replicated runs
    performance_mean[[replicate_nr]] <- performance_mean_loop

    #save(performance_raw_loop, performance_mean_loop, file = paste0(sdm_dir, "evaluation/algorithm_output/performance_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".Rdata"))
    #load(paste0(sdm_dir, "evaluation/algorithm_output/performance_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".Rdata"))

    # Make predictions to current climate ----------------------------------------------------------------------------------
    #Load in landscapes for predictions
    bio_curr <- unwrap(clim_pack)

    # Transform raster objects into data frames
    bio_curr_df <- data.frame(crds(bio_curr),as.points(bio_curr))

    #Predictions using all algorithms
    all_preds <- data.frame(bio_curr_df[,1:2],
                            GLM = predict(loop_glm, bio_curr_df, type='response'),
                            RF = predict(loop_rf, bio_curr_df, type='response'),
                            Maxent = predict(loop_maxent, bio_curr_df, type="logistic"))

    #Binarize them
    all_preds_bin <- data.frame(bio_curr_df[,1:2],
                                sapply(names(all_preds[,-c(1:2)]), FUN=function(alg){
                                    ifelse(all_preds[,alg]>=performance_mean_loop[performance_mean_loop$Algorithm==alg,'mean_thresh'],1,0)
                                }))

    # Make Predictions using ensembles
    ens_preds <- data.frame(bio_curr_df[,1:2],
                                 mean_prob = rowMeans(all_preds[,-c(1:2)]),
                                 sd_prob = apply(all_preds[,-c(1:2)], 1, sd))

    # Binarise ensemble predictions
    ens_preds_bin <- data.frame(bio_curr_df[,1:2],
                                     sapply('mean_prob',
                                            FUN=function(x){ifelse(ens_preds[,x]>= performance_mean_loop[performance_mean_loop$Algorithm == x,
                                                                                                           'mean_thresh'],1,0)}))

    # save predictions
    save(all_preds, all_preds_bin, ens_preds, ens_preds_bin, file = paste0(sdm_dir, "predictions/Predictions_curr_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".RData"))
    #load(paste0(sdm_dir, "predictions/Predictions_curr_Batch", BatchNum, "_Sim", rep_nr, "_Replication", replicates[replicate_nr], ".RData"))

    # calculate sum of habitat suitability of the ensemble for current climate --------------------------------------------
    ens_pred_cur <- ens_preds[,1:3]

    #remove cells below a threshold of habitat suitability
    ens_pred_cur <- ens_pred_cur[which(ens_pred_cur$mean_prob >= performance_mean_loop[performance_mean_loop$Algorithm == "mean_prob",'mean_thresh']),]

    #sum up habitat suitabilities
    hs_change_loop <- sum(ens_pred_cur[,"mean_prob"], na.rm = T)
    
    #Calculate current range size
    range_loop <- length(which(ens_preds_bin$mean_prob == 1))

    #prepare data frames for future predictions ---------------------------------------------------------------------------
    all_fut_preds <- vector("list", length = 89)
    all_fut_preds_bin <- vector("list", length = 89)
    ens_fut_preds <- vector("list", length = 89)
    ens_fut_preds_bin <- vector("list", length = 89)

    # Loop for predictions to future climate
    for (year_nr in 1:89) {
      # Load data set
      bio_fut <- terra::rast(paste0(sdm_dir, "data/landscapes/land", rep_nr, "_optima",  optima, "_breadth", breadth, "_ccYear", year_nr, ".grd"))
      #bio_fut <- terra::rast("3_SDMs/data/land1_optima0.27_breadth0.045_ccYear1.grd")

      #Transform data frame
      bio_fut_df <- data.frame(crds(bio_fut),as.points(bio_fut))

      # Make predictions to future climate -----------------------------------------------------------------------------------
      # predictions using all algorithms
      all_fut_preds[[year_nr]] <- data.frame(bio_fut_df[,1:2],
                                  GLM = predict(loop_glm, bio_fut_df, type='response'),
                                  RF = predict(loop_rf, bio_fut_df, type='response'),
                                  Maxent = predict(loop_maxent, bio_fut_df, type="logistic"))

      # Binarise predictions of all algorithms
      all_fut_preds_bin[[year_nr]] <- data.frame(bio_fut_df[,1:2],
                                        sapply(names(all_fut_preds[[year_nr]][,-c(1:2)]), FUN=function(alg){
                                          ifelse(all_fut_preds[[year_nr]][,alg]>=performance_mean_loop[performance_mean_loop$Algorithm==alg,'mean_thresh'],1,0)
                                        }))

      #Predictions using ensemble
      ens_fut_preds[[year_nr]] <- data.frame(bio_fut_df[,1:2],
                                    mean_prob = rowMeans(all_fut_preds[[year_nr]][,-c(1:2)]),
                                    sd_prob = apply(all_fut_preds[[year_nr]][,-c(1:2)], 1, sd))

      # Binarise ensemble predictions
      ens_fut_preds_bin[[year_nr]] <- data.frame(bio_fut_df[,1:2],
                                           sapply(c('mean_prob'),
                                                  FUN=function(x){ifelse(ens_fut_preds[[year_nr]][,x]>=
                                                                  performance_mean_loop[performance_mean_loop$Algorithm == x,'mean_thresh'],1,0)}))

      # calculate sum of habitat suitability of the ensemble for future climate --------------------------------------------
      # obtain arithmetic mean ensemble values
      ens_pred_fut <- ens_fut_preds[[year_nr]][,1:3]

      #remove cells below a threshold of habitat suitability
      ens_pred_fut <- ens_pred_fut[which(ens_pred_fut$mean_prob >= performance_mean_loop[performance_mean_loop$Algorithm == "mean_prob",'mean_thresh']),]

      #append to habitat values
      hs_change_loop <- append(hs_change_loop, sum(ens_pred_fut[,"mean_prob"], na.rm = T))
      
      #append to range values
      range_loop <- append(range_loop, length(which(ens_fut_preds_bin[[year_nr]]$mean_prob == 1)))

      }#close future predictions loop

      # add hs_change calculation to list
      hs_change[[replicate_nr]] <- hs_change_loop
      
      # add range size calculation to list
      range_size[[replicate_nr]] <- range_loop

      # save predictions
      save(all_fut_preds, all_fut_preds_bin, ens_fut_preds, ens_fut_preds_bin, file = paste0(sdm_dir, "predictions/Predictions_fut_Batch", BatchNum, "_Sim", rep_nr,
                                                                                                                       "_Replication", replicates[replicate_nr], ".RData"))

    } #close replication loop
    saveRDS(performance_raw, file = paste0(sdm_dir, "evaluation/performance_measures/performance_raw_SDM_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
    saveRDS(performance_mean, file = paste0(sdm_dir, "evaluation/performance_measures/performance_mean_SDM_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
    saveRDS(hs_change, file = paste0(sdm_dir, "results/habitat_suitability_SDM_Batch", BatchNum, "_Sim", rep_nr, ".rds"))
    saveRDS(range_size, file = paste0(sdm_dir, "results/range_size_SDM_Batch", BatchNum, "_Sim", rep_nr, ".rds"))

} #close foreach loop
stopCluster(cl)

gc()
rm(list=ls())