# Plot results

# load packages
library(ggplot2)
library(gridExtra)

# load in data

data <- vector("list", 16)

for (i in 1:16){
  # data set for land replication 1
  tmp1 <- readRDS(paste0("4_Analysis/data/data_analysis_relative_long_Batch", i, "_Sim1.rds"))
  # data set for land replication 2
  tmp2 <- readRDS(paste0("4_Analysis/data/data_analysis_relative_long_Batch", i, "_Sim2.rds"))
  # data set for land replication 3
  tmp3 <- readRDS(paste0("4_Analysis/data/data_analysis_relative_long_Batch", i, "_Sim3.rds"))
  
  #rbind the different data sets
  data[[i]] <- rbind(tmp1, tmp2)
  data[[i]] <- rbind(data[[i]], tmp3)
  
  data[[i]]$land <- as.character(data[[i]]$land)
}

# Plot the results
# extinction probability - change in habitat suitability sums
ggplot(data[[1]], aes(x=(1-hs_change), y=extProb, col = land))+
  geom_point()

ggplot(data[[2]], aes(x=(1-hs_change), y=extProb, col = land))+
  geom_point()

ggplot(data[[3]], aes(x=(1-hs_change), y=extProb, col = land))+
  geom_point()

ggplot(data[[10]], aes(x=(1-hs_change), y=extProb, col = land))+
  geom_point()
