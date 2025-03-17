# Bayesian models for the analysis of pop loss - hs loss relationship and the influences of the different variables


library(brms)

# load("/import/ecoc9z/data-zurell/keuth/raw_data_longformat.RData")
# 
# model_mbrms <- brms::brm(pop_sum ~ hs_loss + optima + breadth + rmax + dispersal + hs_loss:optima + hs_loss:breadth + hs_loss:rmax + 
#                            hs_loss:dispersal,
#                          data = data_adapted_long, family = zero_one_inflated_beta(), chains = 4, cores = 4, control=list(adapt_delta=0.9),
#                          iter = 3000, warmup = 2000)
# 
# save(model_mbrms, file= "/import/ecoc9z/data-zurell/keuth/Model_Brms_cluster_wo_randomeffect.Rdata")

load("/import/ecoc9z/data-zurell/keuth/Model_Brms_cluster.Rdata")
model_mbrms_ri <- model_mbrms
load("/import/ecoc9z/data-zurell/keuth/Model_Brms_cluster_randomslope.Rdata")

sink("/import/ecoc9z/data-zurell/keuth/Model_Brms_comparison_ri_rs.txt")
loo(model_mbrms, model_mbrms_ri)
sink()