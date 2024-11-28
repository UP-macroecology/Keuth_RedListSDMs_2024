# calculate mean dispersal distance from output files

#load file 

dist <- read.table("2_Simulations/data/Batch22_Sim1_Land1_Rep0_Inds.txt", header = T, sep = "\t")

unique(dist$Year)
dist <- subset(dist, dist$Year == 99)

hist(dist$DistMoved)
mean(dist$DistMoved)

load("3_SDMs/data/vec_distances.Rdata")

hist(vec_distances)
mean(vec_distances)
median(vec_distances)
# when keeping the 0s my median is 0 but the mean is rather low

vec_distances <- vec_distances[ !vec_distances == 0]

hist(vec_distances)
mean(vec_distances)
median(vec_distances)
# when removing the 0s the median is even larger then the mean when using the 0s

load("3_SDMs/data/values_dispersal_assumption.Rdata")
