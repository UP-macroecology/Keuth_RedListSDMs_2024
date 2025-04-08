# Bayesian models for the analysis of pop loss - hs loss relationship and the influences of the different variables


library(brms)
library(ordbetareg)

load("/import/ecoc9z/data-zurell/keuth/data_bayes_model.Rdata")
# 
model_slope <- ordbetareg(pop_sum ~ hs_loss + optima + breadth + rmax + dispersal + hs_loss:optima + hs_loss:breadth + hs_loss:rmax + hs_loss:dispersal + (1+land|land), 
                    data = data_adapted_long, control=list(adapt_delta=0.95), chains = 4, cores = 4, iter = 4000, warmup = 2000, refresh = 0)

save(model_slope, file= "/import/ecoc9z/data-zurell/keuth/Model_ordbeta_randomslope.Rdata")

# load("/import/ecoc9z/data-zurell/keuth/Model_Brms_cluster.Rdata")
# model_mbrms_ri <- model_mbrms
# load("/import/ecoc9z/data-zurell/keuth/Model_Brms_cluster_randomslope.Rdata")
# 
# sink("/import/ecoc9z/data-zurell/keuth/Model_Brms_comparison_ri_rs.txt")
# loo(model_mbrms, model_mbrms_ri)
# sink()