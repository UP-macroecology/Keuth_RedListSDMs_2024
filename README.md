# Quantifying extinction risk using SDMs

Quantifying the extinction risk of species using species distribution models by using a spatially-explicit simulation approach with different virtual species in a neutral landscape

## Workflow
For this study the distribution and the extinction of virtual species under climate change in an artificial neutral landscape is modelled using the spatially-explicit individual-based modelling plattform RangeShifter. The data from the simulation is later used 
for estimating SDMs and comparing the model predictions of the SDMs to the results of the simulation data to investigate the habitat loss - extinction risk relationship.
The virtual species has 4 different traits, which are varied in a two factorial way to represent different vulnerability of species under climate change. These traits are niche optimum to model range-contracting and range-shifting under climate change, 
niche breadth to model habitat fragmentation and specilization, growth rate for representing different reproductive strategies and dispersal distance for including dispersal limitations for some species.

## 1. Parameter testing for the simulation model
To establish the parameters for the simulation model I first test different parameter values to establish values that represent the patterns that I want to investigate as well as to obtain a stable population size after the spin-up period in 
my simulation model. In the different scripts I tested different niche positions, different niche breadths and different dispersal distances. For the dispersal distance I modelled a short dispersal distance and a long dispersal distance with a double exponential kernel. For the long dispersal distance I also tested different probabilities for the long dispersal distance. The results of the parameter testing can be found in the ReadMe.txt

## 2. Simulations
To simulate the population dynamics under climate change, I first created different neutral landscapes under climate change. I replicated each landscape three times by using the same statistical settings for all three replications. I simulated four different landscape settings, which differed in the niche optima and the niche breadth
Here, I simulate the population dynamics of the different virtual species in the neutral landscape under climate change. In total I simulated 16 different species, due to the four traits with the two-factorial design. These species were modelled in three different landscapes by replicating a landscape with the same statistical settings three times. I first created three replications of four landscapes with differing niche optima, niche breadth combinations under climate change. I then simulated the population dynamics using RangeShifter and finally calculated the extinction probability and obtained the occurrences from the species before climate change 
