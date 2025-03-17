# Trying different statistical tests for the different figures

#Load packages
library(dplyr)
library(tidyr)
library(stringr)
library(modelsummary)
library(brms)

# Testing the difference/ effect of the traits on the hs-loss/pop-loss relationship
load("4_Analysis/data/raw_data_longformat.RData")

# model_mbrms <- brms::brm(pop_sum ~ hs_loss + optima + breadth + rmax + dispersal + hs_loss:optima + hs_loss:breadth + hs_loss:rmax + 
#                            hs_loss:dispersal + (1|land),
#                            data = data_adapted_long, family = zero_one_inflated_beta(), chains = 3,
#                            iter = 3000, warmup = 1000)
# save(model_mbrms, file= "4_Analysis/Model Results/Model_Brms.Rdata")

load("4_Analysis/data/Model_Brms_cluster.Rdata")
model_mbrms_ri <- model_mbrms
load("4_Analysis/data/Model_Brms_cluster_wo_randomeffect.Rdata")

loo(model_mbrms, model_mbrms_ri)

#check model fit
plot(model_mbrms)
pp_check(model_mbrms)

#check divergent transition
hmc_diagnostics <- nuts_params(model_mbrms)
div_trans <- sum(subset(hmc_diagnostics, Parameter == "divergent__")$Value)
print(paste("divergent transitions:", div_trans))
#divergent transition from 0 should (if not 0 than there are random spikes in the data where my optimizer doesn't reach it)
n_eff_ratios <- neff_ratio(model_mbrms) #minimum should not be smaller than 0.1

# effective number of samples in bulk and tail:
out_sum <- summary(model_mbrms)
n_eff_ratio_bulk <- out_sum$fixed$Bulk_ESS/out_sum$total_ndraws
n_eff_ratio_tail <- out_sum$fixed$Tail_ESS/out_sum$total_ndraws

# look at model summary
s <- summary(model_mbrms)
summary(model_mbrms, waic = T)
effective_sample(model_mbrms)
coef(model_mbrms)
fixef(model_mbrms)
# I obtain divergent warnings, but Rhat and the EFF look all good so presumably the model as a whole converged

#Extract the levels of the random effect
ref <- ranef(model_mbrms)

# Look at the response curves
hs <- seq(0,1,length.out = 20)

# curve for range-contracting
rc1 <- s$fixed$Estimate[1] + -0.1869040 + s$fixed$Estimate[2]*hs
rc2 <- s$fixed$Estimate[1] + 0.2833304 + s$fixed$Estimate[2]*hs
rc3 <- s$fixed$Estimate[1] + -0.2690098 + s$fixed$Estimate[2]*hs

# curve for range-shifting
rs1 <- s$fixed$Estimate[1] + -0.1869040 + s$fixed$Estimate[3] + (s$fixed$Estimate[2] + s$fixed$Estimate[7]) *hs
rs2 <- s$fixed$Estimate[1] + 0.2833304 + s$fixed$Estimate[3] + (s$fixed$Estimate[2] + s$fixed$Estimate[7]) *hs
rs3 <- s$fixed$Estimate[1] + -0.2690098 + s$fixed$Estimate[3] + (s$fixed$Estimate[2] + s$fixed$Estimate[7]) *hs

#plotting the results
plot(hs, exp(rc1)/(1+exp(rc1)), type = "n")
lines(hs, exp(rc1)/(1+exp(rc1)))
lines(hs, exp(rc2)/(1+exp(rc2)))
lines(hs, exp(rc3)/(1+exp(rc3)))

lines(hs, exp(rs1)/(1+exp(rs1)), lty = "dashed")
lines(hs, exp(rs2)/(1+exp(rs2)), lty = "dashed")
lines(hs, exp(rs3)/(1+exp(rs3)), lty = "dashed")

# curve for narrow niche
nn1 <- s$fixed$Estimate[1] + -0.1869040 + s$fixed$Estimate[2]*hs
nn2 <- s$fixed$Estimate[1] + 0.2833304 + s$fixed$Estimate[2]*hs
nn3 <- s$fixed$Estimate[1] + -0.2690098 + s$fixed$Estimate[2]*hs

# curve for wide niche
nw1 <- s$fixed$Estimate[1] + -0.1869040 + s$fixed$Estimate[4] + (s$fixed$Estimate[2] + s$fixed$Estimate[8]) *hs
nw2 <- s$fixed$Estimate[1] + 0.2833304 + s$fixed$Estimate[4] + (s$fixed$Estimate[2] + s$fixed$Estimate[8]) *hs
nw3 <- s$fixed$Estimate[1] + -0.2690098 + s$fixed$Estimate[4] + (s$fixed$Estimate[2] + s$fixed$Estimate[8]) *hs

#plotting the results
plot(hs, exp(rc1)/(1+exp(rc1)), type = "n")
lines(hs, exp(nn1)/(1+exp(nn1)))
lines(hs, exp(nn2)/(1+exp(nn2)))
lines(hs, exp(nn3)/(1+exp(nn3)))

lines(hs, exp(nw1)/(1+exp(nw1)), lty = "dashed")
lines(hs, exp(nw2)/(1+exp(nw2)), lty = "dashed")
lines(hs, exp(nw3)/(1+exp(nw3)), lty = "dashed")

# curve for slow rmax
sr1 <- s$fixed$Estimate[1] + -0.1869040 + s$fixed$Estimate[2]*hs
sr2 <- s$fixed$Estimate[1] + 0.2833304 + s$fixed$Estimate[2]*hs
sr3 <- s$fixed$Estimate[1] + -0.2690098 + s$fixed$Estimate[2]*hs

# curve for fast rmax
fr1 <- s$fixed$Estimate[1] + -0.1869040 + s$fixed$Estimate[5] + (s$fixed$Estimate[2] + s$fixed$Estimate[9]) *hs
fr2 <- s$fixed$Estimate[1] + 0.2833304 + s$fixed$Estimate[5] + (s$fixed$Estimate[2] + s$fixed$Estimate[9]) *hs
fr3 <- s$fixed$Estimate[1] + -0.2690098 + s$fixed$Estimate[5] + (s$fixed$Estimate[2] + s$fixed$Estimate[9]) *hs

#plotting the results
plot(hs, exp(rc1)/(1+exp(rc1)), type = "n")
lines(hs, exp(sr1)/(1+exp(sr1)))
lines(hs, exp(sr2)/(1+exp(sr2)))
lines(hs, exp(sr3)/(1+exp(sr3)))

lines(hs, exp(fr1)/(1+exp(fr1)), lty = "dashed")
lines(hs, exp(fr2)/(1+exp(fr2)), lty = "dashed")
lines(hs, exp(fr3)/(1+exp(fr3)), lty = "dashed")

# curve for short dispersal
sd1 <- s$fixed$Estimate[1] + -0.1869040 + s$fixed$Estimate[2]*hs
sd2 <- s$fixed$Estimate[1] + 0.2833304 + s$fixed$Estimate[2]*hs
sd3 <- s$fixed$Estimate[1] + -0.2690098 + s$fixed$Estimate[2]*hs

# curve for long dispersal
ld1 <- s$fixed$Estimate[1] + -0.1869040 + s$fixed$Estimate[6] + (s$fixed$Estimate[2] + s$fixed$Estimate[10]) *hs
ld2 <- s$fixed$Estimate[1] + 0.2833304 + s$fixed$Estimate[6] + (s$fixed$Estimate[2] + s$fixed$Estimate[10]) *hs
ld3 <- s$fixed$Estimate[1] + -0.2690098 + s$fixed$Estimate[6] + (s$fixed$Estimate[2] + s$fixed$Estimate[10]) *hs

#plotting the results
plot(hs, exp(rc1)/(1+exp(rc1)), type = "n")
lines(hs, exp(sd1)/(1+exp(sd1)))
lines(hs, exp(sd2)/(1+exp(sd2)))
lines(hs, exp(sd3)/(1+exp(sd3)))

lines(hs, exp(ld1)/(1+exp(ld1)), lty = "dashed")
lines(hs, exp(ld2)/(1+exp(ld2)), lty = "dashed")
lines(hs, exp(ld3)/(1+exp(ld3)), lty = "dashed")

# Plot the results in a table
get_estimates(model_mbrms)
modelsummary(model_mbrms, statistic = "mad")

report_table(model_mbrms, verbose = F)

#####
# # Testing the differences between classification times between the single trait levels
# 
# load("4_Analysis/data/IUCN_classification_times_allreplicates.RData")
# 
# # extract data for VU and elongate data set and clean columns
# IUCN_classification_long <- IUCN_classification %>% select(BatchNum:VU_Pop,VU_HS, VU_Ext, optima:dispersal) %>% pivot_longer(., c(4:6), names_to = "metric", values_to = "VU") %>%
#   mutate(metric = str_remove(metric, "VU_"))
# IUCN_classification_long <- IUCN_classification %>% select(BatchNum:replicates, EN_Pop,EN_HS, EN_Ext, optima:dispersal) %>% 
#   pivot_longer(., c(4:6), names_to = "metric", values_to = "EN") %>% mutate(metric = str_remove(metric, "EN_")) %>% full_join(IUCN_classification_long, .)
# IUCN_classification_long <- IUCN_classification %>% select(BatchNum:replicates, CR_Pop,CR_HS, CR_Ext, optima:dispersal) %>% 
#   pivot_longer(., c(4:6), names_to = "metric", values_to = "CR") %>% mutate(metric = str_remove(metric, "CR_")) %>% full_join(IUCN_classification_long, .)
# 
# IUCN_range <- as.data.frame(subset(IUCN_classification_long, IUCN_classification_long$optima == "marginal"))
# model_VU <- lm(formula = IUCN_range$CR ~ IUCN_range$metric)
# 
# model_VU <- glm(VU ~ metric + optima + breadth + rmax + dispersal, data = IUCN_classification_long, family = "poisson")
# model_EN <- glm(EN ~ metric + optima + breadth + rmax + dispersal, data = IUCN_classification_long, family = "poisson")
# model_CR <- glm(CR ~ metric + optima + breadth + rmax + dispersal, data = IUCN_classification_long, family = "poisson")
# par(mfrow=c(2,2))
# plot(model_VU)
# plot(model_EN)
# plot(model_CR)
# # only for VU it looks weird, the rest looks okayish
# 
# summary(model_VU)
# # Weirdly enough we have significant differences between the different niche breadths, but none between the growth rate and dispersal
# anova(model_VU)
# 
# # Post-hoc comparison
# TukeyHSD(aov(model_VU))
# pairwise.t.test(IUCN_classification_long$VU, IUCN_classification_long$metric, p.adjust.method = "bonferroni")
# 
# summary(model_EN)
# # For this all traits are significant
# anova(model_VU)
# # Again only metric, optima and breadth are relevant for the model (still the question why breadth it doesn't look like it in the plot)
# TukeyHSD(aov(model_EN))
# pairwise.t.test(IUCN_classification_long$EN, IUCN_classification_long$metric, p.adjust.method = "bonferroni")
# 
# summary(model_CR)
# anova(model_CR)
# # for CR all traits are significant
# TukeyHSD(aov(model_CR))
# pairwise.t.test(IUCN_classification_long$CR, IUCN_classification_long$metric, p.adjust.method = "bonferroni")


# working on including landscape as a random effect
null_model <- glm(pop_sum ~ hs_loss + optima + breadth + rmax + dispersal + hs_loss:optima + hs_loss:breadth + hs_loss:rmax + 
                    hs_loss:dispersal, data = data_adapted_long, family = "binomial")



model_rs <- glmer(pop_sum ~ hs_loss + optima + breadth + rmax + dispersal + hs_loss:optima + hs_loss:breadth + hs_loss:rmax + 
                    hs_loss:dispersal + (1+land|land), data = data_adapted_long, family = "binomial",control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

model_ri_op2 <- glmer(pop_sum ~ hs_loss + optima + breadth + rmax + dispersal + hs_loss:optima + hs_loss:breadth + hs_loss:rmax + 
                        hs_loss:dispersal + (1|land), data = data_adapted_long,control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)), family = "binomial")

model_wo_in <- glmer(pop_sum ~ hs_loss + optima + breadth + rmax + dispersal + (1|land), data = data_adapted_long, family = "binomial")


aa <- allFit(model_ri_op2)
ss <- summary(aa)
ss$ fixef               ## fixed effects
ss$ llik                ## log-likelihoods
ss$ sdcor               ## SDs and correlations
ss$ theta               ## Cholesky factors
ss$ which.OK            ## which fits worked
# model_randomslope <- glmer(pop_sum ~ hs_loss + optima + breadth + rmax + dispersal + hs_loss:optima + hs_loss:breadth + hs_loss:rmax + 
#                              hs_loss:dispersal + (1+land|land), data = data_adapted_long, family = "binomial")
# 
# null_model <- glm(pop_sum ~ hs_loss + optima + breadth + rmax + dispersal + hs_loss:optima + hs_loss:breadth + hs_loss:rmax + 
#                              hs_loss:dispersal, data = data_adapted_long, family = "binomial")

anova(model_ri_op2, null_model) #model with random intercept as a lower AIC
anova(model_rs, model_ri_op2) #model with random slope doesn't have the lower AIC
# -> the model with the randomintercept seems to be the most parsimonious one
# still I have to figure out the whole model diagnostic situation

# check for model diagnostic using DHARMa package
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = model_ri_op2, plot = F)

plot(simulationOutput) # KS test and Dispersion test are both significant indicating a slight under dispersion

testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
# my model shows an underdispersion

#Conclusion: the best model is the model which just includes a random intercept and nothing more
# when looking at the model diagnostics my model shows a slight underdispersion, which is not too worse as it biases the p-value in the direction of a more
# conservative estimation; it indicates that I use too many predictors in my model (well that makes sense)
# I am not 100% sure how to continue as all of this doesn't confidently allow me to use the mode

#separate data set for all landscapes

list_landscapes <- split(data_adapted_long, data_adapted_long$land)

#start with just the optima model
model_optima_l1 <- glm(pop_sum ~ hs_loss + optima + hs_loss*optima, data=list_landscapes[[1]], family = "binomial")
plot(model_optima_l1)
summary(model_optima_l1)
anova(model_optima_l1)

# then put together a model with all traits
model_l1 <- glm(pop_sum ~ hs_loss + optima + breadth + rmax + dispersal + hs_loss:optima + hs_loss:breadth + hs_loss:rmax + hs_loss:dispersal, data=list_landscapes[[1]], family = "binomial")
plot(model_l1)
summary(model_l1)
anova(model_l1)

model_l2 <- glm(pop_sum ~ hs_loss + optima + breadth + rmax + dispersal + hs_loss:optima + hs_loss:breadth + hs_loss:rmax + hs_loss:dispersal, data=list_landscapes[[2]], family = "binomial")
plot(model_l2)
summary(model_l2)
anova(model_l2)

model_l3 <- glm(pop_sum ~ hs_loss + optima + breadth + rmax + dispersal + hs_loss:optima + hs_loss:breadth + hs_loss:rmax + hs_loss:dispersal, data=list_landscapes[[3]], family = "binomial")
plot(model_l3)
summary(model_l3)
anova(model_l3)

library(modelsummary)
models <- list(Land1 = model_l1, Land2 = model_l2, Land3 = model_l3)

modelsummary(models, statistic = c("s.e. = {std.error}", "p = {p.value}{stars}"), coef_rename = c('hs_loss' = 'HS loss', 'optimarange-shifting' = 'Niche optima (range-shifting)'
                                                                                                  , "breadthwide" = "Niche breadth (wide)", "rmaxfast" = "Growth rate (fast)",
                                                                                                  "dispersallong" = "Dispersal distance (long)", "hs_loss × optimarange-shifting" = "HS loss * Niche optima (range shifting)",
                                                                                                  "hs_loss × breadthwide" = "HS loss * Niche breadth (wide)", "hs_loss × rmaxfast" = "HS loss * Growth rate (fast)",
                                                                                                  "hs_loss × dispersallong" = "HS loss * Dispersal distance (long)"))
modelsummary(models, output = "4_Analysis/plots/Paper/table_models_poploss_hsloss.html", statistic = c("s.e. = {std.error}", "p = {p.value}{stars}"), coef_rename = c('hs_loss' = 'HS loss', 'optimarange-shifting' = 'Niche optima (range-shifting)'
                                                                                                                                                                      
  
))                                                                                                                                                                
model <- suppressWarnings(brm(mpg ~ qsec + wt, data = mtcars, refresh = 0, iter = 300))
