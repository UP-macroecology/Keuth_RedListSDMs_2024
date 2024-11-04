# Create the values for the dispersal assumption

# Dispersal distances
# short distance: 5000
# long distance: 15000, 250000, 0.95


# Obtaining a sample of a negative exponential distribution
# short distance
distances_short <- rexp(1000, rate = 1/5000) 
distances_long1 <- rexp(950, rate = 1/15000)
distances_long2 <- rexp(50, rate = 1/250000)
distances_long <- c(distances_long1, distances_long2)

# Take the median from both distances
med_dist_short <- median(distances_short)
med_dist_long <- median(distances_long)

# extract the 95% quantile
quant_dist_short <- as.numeric(quantile(distances_short, probs = 0.95))
quant_dist_long <- as.numeric(quantile(distances_long, probs = 0.95))

# save dispersal distances
save(med_dist_short, med_dist_long, quant_dist_short, quant_dist_long, file = "3_SDMs/data/values_dispersal_assumption.Rdata")
