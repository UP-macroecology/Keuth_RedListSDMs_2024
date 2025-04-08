# Bayesian models for the analysis of pop loss - hs loss relationship and the influences of the different variables


library(brms)
library(ordbetareg)

load("/import/ecoc9z/data-zurell/keuth/data_bayes_model.Rdata")
#load("4_Analysis/data/data_bayes_model.Rdata")
#df_sub <- subset(data_adapted_long, c(data_adapted_long$breadth == "narrow", data_adapted_long$rmax == "slow", data_adapted_long$dispersal == "short"))

null_model <- ordbetareg(pop_sum ~ hs_loss + optima + breadth + rmax + dispersal + hs_loss:optima + hs_loss:breadth + hs_loss:rmax + hs_loss:dispersal, 
                             data = data_adapted_long, control=list(adapt_delta=0.95), chains = 4, cores = 4, iter = 4000, warmup = 2000, refresh = 0)

save(null_model, file= "/import/ecoc9z/data-zurell/keuth/Model_ordbeta_wo_randomeffect.Rdata")

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

# to avoid convergence issues I could set delta to 0.99, iter to 10000

# prior <- get_prior(pop_sum ~ hs_loss + optima + breadth + rmax + dispersal + hs_loss:optima + hs_loss:breadth + hs_loss:rmax + hs_loss:dispersal + (1|land), 
#                     data = data_adapted_long, family = zero_one_inflated_beta())

# model_mbrms <- brms::brm(pop_sum ~ hs_loss + hs_loss_squared + optima + breadth + rmax + dispersal + hs_loss_squared:optima + hs_loss_squared:breadth + hs_loss_squared:rmax + 
#                            hs_loss_squared:dispersal + (1|land),
#                          data = data_adapted_long, family = zero_one_inflated_beta(), chains = 4, cores = 4, control=list(adapt_delta=0.9),
#                          iter = 3000, warmup = 2000)

#save(model, file= "/import/ecoc9z/data-zurell/keuth/Model_ordbeta_full.Rdata")