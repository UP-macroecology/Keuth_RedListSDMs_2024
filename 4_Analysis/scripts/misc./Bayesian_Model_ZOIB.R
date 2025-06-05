# Bayesian models for the analysis of pop loss - hs loss relationship and the influences of the different variables

#Load packages
library(brms)

# load("/import/ecoc9z/data-zurell/keuth/data_bayes_model.Rdata")

# model <- brm(
#   bf(
#     pop_sum ~ hs_loss + optima + breadth + rmax + dispersal + hs_loss:optima + hs_loss:breadth + hs_loss:rmax + hs_loss:dispersal + (1|land),
#     phi ~ hs_loss + optima + breadth + rmax + dispersal + hs_loss:optima + hs_loss:breadth + hs_loss:rmax + hs_loss:dispersal + (1|land),
#     zoi ~ hs_loss + optima + breadth + rmax + dispersal + hs_loss:optima + hs_loss:breadth + hs_loss:rmax + hs_loss:dispersal + (1|land),
#     coi ~ hs_loss + optima + breadth + rmax + dispersal + hs_loss:optima + hs_loss:breadth + hs_loss:rmax + hs_loss:dispersal + (1|land)
#   ),
#   data = data_adapted_long,
#   family = zero_one_inflated_beta(), chains = 4, cores = 4, control=list(adapt_delta=0.9),
#   iter = 4000, warmup = 2000
# )

#save(model, file= "/import/ecoc9z/data-zurell/keuth/Model_ordbeta_full.Rdata")