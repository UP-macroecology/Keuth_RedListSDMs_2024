# Red List criteria underestimate climate-related extinction risk of range-shifting species


#-------------------------------------------------------------------------------

# --------------------------------------------------------------------- #
#                         08a. Ordbeta Regression Model                 #
# --------------------------------------------------------------------- #


#-------------------------------------------------------------------------------

# Ordered beta regression model to analyse the relationship between population size and habitat loss

# define file paths
home_folder <- file.path("/import/ecoc9z/data-zurell/keuth/SDM_Extinctions/") #needs to be adjusted based on own folder structure
sim_dir <- file.path(paste0(home_folder, "Simulations/"))
sdm_dir <- file.path(paste0(home_folder, "output_data/SDMs/"))

#Load packages
library(brms)
library(ordbetareg)

load(paste0(home_folder, "analysis_data/data_mean_longformat.Rdata"))

#run null model without any random effects
null_model <- ordbetareg(pop_sum ~ hs_loss + position + breadth + rmax + dispersal + hs_loss:position + hs_loss:breadth + hs_loss:rmax + hs_loss:dispersal, 
                                                       data = data_adapted_long, control=list(adapt_delta=0.95), chains = 4, cores = 4, iter = 4000, warmup = 2000, refresh = 0)
                         
save(null_model, file= paste0(home_folder, "model_results/Model_ordbeta_wo_randomeffect.Rdata"))

# run model with random intercept
model_intercept <- ordbetareg(pop_sum ~ hs_loss + position + breadth + rmax + dispersal + hs_loss:position + hs_loss:breadth + hs_loss:rmax + hs_loss:dispersal + (1|land), 
                          data = data_adapted_long, control=list(adapt_delta=0.95), chains = 4, cores = 4, iter = 4000, warmup = 2000, refresh = 0)

save(model_intercept, file= paste0(home_folder, "model_results/Model_ordbeta_randomintercept.Rdata"))

# run model with random slope and random intercept
model_slope <- ordbetareg(pop_sum ~ hs_loss + position + breadth + rmax + dispersal + hs_loss:position + hs_loss:breadth + hs_loss:rmax + hs_loss:dispersal + (1+land|land), 
                    data = data_adapted_long, control=list(adapt_delta=0.95), chains = 4, cores = 4, iter = 4000, warmup = 2000, refresh = 0)

save(model_slope, file= paste0(home_folder, "model_results/Model_ordbeta_full.Rdata"))

# Compare the different models based on loo (find the most parsimonious one)

sink(paste0(home_folder, "model_results/Model_ordbeta_comparison_null_ri.txt"))
loo(model_intercept, null_model)
sink()
#The model with the random intercept explains more variance

sink(paste0(home_folder, "model_results/Model_ordbeta_comparison_rs_ri.txt"))
loo(model_slope, model_intercept)
sink()
# The model with the random intercept explains similar variance as the one with the ranom slope -> continue with model with random intercept