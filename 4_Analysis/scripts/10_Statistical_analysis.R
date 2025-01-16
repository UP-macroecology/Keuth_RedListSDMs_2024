# Trying different statistical tests for the different figures

#Load packages
library(dplyr)
library(tidyr)
library(stringr)

# Testing the differences between classification times between the single trait levels

load("4_Analysis/data/IUCN_classification_times_allreplicates.RData")

# extract data for VU and elongate data set and clean columns
IUCN_classification_long <- IUCN_classification %>% select(BatchNum:VU_Pop,VU_HS, VU_Ext, optima:dispersal) %>% pivot_longer(., c(4:6), names_to = "metric", values_to = "VU") %>%
  mutate(metric = str_remove(metric, "VU_"))
IUCN_classification_long <- IUCN_classification %>% select(BatchNum:replicates, EN_Pop,EN_HS, EN_Ext, optima:dispersal) %>% 
  pivot_longer(., c(4:6), names_to = "metric", values_to = "EN") %>% mutate(metric = str_remove(metric, "EN_")) %>% full_join(IUCN_classification_long, .)
IUCN_classification_long <- IUCN_classification %>% select(BatchNum:replicates, CR_Pop,CR_HS, CR_Ext, optima:dispersal) %>% 
  pivot_longer(., c(4:6), names_to = "metric", values_to = "CR") %>% mutate(metric = str_remove(metric, "CR_")) %>% full_join(IUCN_classification_long, .)


# test for normality
shapiro.test(subset(IUCN_classification_long, IUCN_classification_long$optima == "marginal")$VU)
shapiro.test(subset(IUCN_classification_long, IUCN_classification_long$optima == "central")$VU)
shapiro.test(subset(IUCN_classification_long, IUCN_classification_long$metric == "Pop")$VU)
shapiro.test(subset(IUCN_classification_long, IUCN_classification_long$metric == "HS")$VU)
shapiro.test(subset(IUCN_classification_long, IUCN_classification_long$metric == "Ext")$VU)
# all of them are not normally distributed and I even have some values that have no variance -> which test can I use

model_VU <- lm(VU ~ metric + optima + breadth + rmax + dispersal, data = IUCN_classification_long)
model_EN <- lm(EN ~ metric + optima + breadth + rmax + dispersal, data = IUCN_classification_long)
model_CR <- lm(CR ~ metric + optima + breadth + rmax + dispersal, data = IUCN_classification_long)
plot(model_VU)
plot(model_EN)
plot(model_CR)
# only for VU it looks weird, the rest looks okayish

summary(model_VU)
# Weirdly enough we have significant differences between the different niche breadths, but none between the growth rate and dispersal
anova(model_VU)

# Post-hoc comparison
TukeyHSD(aov(model_VU))
pairwise.t.test(IUCN_classification_long$VU, IUCN_classification_long$metric, p.adjust.method = "bonferroni")

summary(model_EN)
# For this all traits are significant
anova(model_VU)
# Again only metric, optima and breadth are relevant for the model (still the question why breadth it doesn't look like it in the plot)
TukeyHSD(aov(model_EN))
pairwise.t.test(IUCN_classification_long$EN, IUCN_classification_long$metric, p.adjust.method = "bonferroni")

summary(model_CR)
anova(model_CR)
# for CR all traits are significant
TukeyHSD(aov(model_CR))
pairwise.t.test(IUCN_classification_long$CR, IUCN_classification_long$metric, p.adjust.method = "bonferroni")

# Testing the difference/ effect of the traits on the hs-loss/pop-loss relationship
# ANCOVA

load("4_Analysis/data/data_model_poploss_hsloss.Rdata")

# seperate data sets per landscape
list_optima <- split(data_optima, data_optima$land)

# apply model
model_optima_l1 <- lm(predictions ~ hs_loss + optima + hs_loss*optima, data=list_optima[[1]])
summary(model_optima_l1)
anova(model_optima_l1)

model_optima <- lm(predictions ~ hs_loss + optima + land + hs_loss*optima + hs_loss*land, data=data_optima)
summary(model_optima)
anova(model_optima)

model_breadth <- lm(predictions ~ hs_loss + breadth + land + hs_loss*breadth + hs_loss*land, data=data_breadth)
summary(model_breadth)
anova(model_breadth)

model_rmax <- lm(predictions ~ hs_loss + rmax + land + hs_loss*rmax + hs_loss*land, data=data_rmax)
summary(model_rmax)
anova(model_rmax)

model_dispersal <- lm(predictions ~ hs_loss + dispersal + land + hs_loss*dispersal + hs_loss*land, data=data_dispersal)
summary(model_dispersal)
anova(model_dispersal)
