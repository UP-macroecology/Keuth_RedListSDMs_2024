# Quantifying extinction risk using SDMs

The aim of this study is to quantify the extinction risk of species using species distribution models. For this a spatially-explicit simulation approach with different virtual species in a neutral landscape is used. The data from the simulation results is then used to fit and evaluate the SDMs.

## Workflow
For this study the distribution and the extinction of virtual species under climate change in an artificial neutral landscape is modelled using the spatially-explicit individual-based modelling plattform RangeShifter. The data from the simulation is later used 
for estimating SDMs and comparing the model predictions of the SDMs to the results of the simulation data to investigate the habitat loss - extinction risk relationship.
The virtual species has 4 different traits, which are varied in a two factorial way to represent different vulnerability of species under climate change. These traits are niche optimum to model range-contracting and range-shifting under climate change, 
niche breadth to model habitat fragmentation and specilization, growth rate for representing different reproductive strategies and dispersal distance for including dispersal limitations for some species.

## 1. Parameter testing for the simulation model
To establish the parameters for the simulation model I first test different parameter values to establish values that represent the patterns that I want to investigate as well as to obtain a stable population size after the spin-up period in 
my simulation model. In the different scripts I tested different niche positions, different niche breadths and different dispersal distances. For the dispersal distance I modelled a short dispersal distance and a long dispersal distance with a double exponential kernel. For the long dispersal distance I also tested different probabilities for the long dispersal distance. The results of the parameter testing can be found in the ReadMe.txt.

## 2. Simulations
As a first step, I simulated the population dynamics of the different species under climate change in the neutral landscapes.

*Script:* 01_CreateLandscape.R

My landscape consists of two environmental variables (temperature and precipitation). The final landscapes contain the suitable habitat for the respective virtual species, which is then used in the simulation. The suitable habitat for the different species was obtained using the different niche optima and niche breadth values. This resulted in four different landscapes. Of each landscape three replicates were obtained by using the same statistical settings for all three replicates when randomly creating the landscape.

*Script:* 02_Simulations.R

Here, I simulate the population dynamics of the different virtual species in the neutral landscape under climate change using RangeShifter. In total I simulated 16 different species, due to the four traits with the two-factorial design. In the simulation the traits growth rate an dispersal distance were changed.

*Script:* 02a_Simulations_data_graphs.R

In this script, I calculate the extinction probability of each year and extract the occurrences of the species in the landscape in the different replicated runs. I further plot the results of the simulation to visualize the abundances, extinction probabilities and habitat loss.

*Script:* 02b_Simulations_Evaluation.R

In this script, I plot the abundances and extinction probabilities of the simulations.

*Script:* text_labels_plots.R

This script contains the text labels to label 2x2 plots.

## 3. SDMs
As the second step, I fitted SDMs using the simulation data.

*Script:* 03_Spatial_Thinning.R

In this script, I first obtain the absences and then thin the data points by removing every second cell. To obtain the absences I marked every cell that was not occupied by an individual in the respective replicated run and year as absence. After that I thinned the data points.

*Script:* 04_SDM.R, 04_SDM_2.R, 04a_SDM_rangesize.R

I estimated SDMs to ten randomly selected replicated runs. I fitted three algorithms and further calculated an ensemble model and predicted the habitat suitability to every year under climate change. To obtain the habitat suitability sums for every year I removed cells below a certain threshold (obtained by maxTSS) and sumed up the habitat suitability for the other cells. I further calculated the range size by marking a cell as a presence if the habitat suitability was above a certain threshold (masTSS) and summing up the number of cells. I evaluated the performance of the SDMs against all 99 replicated runs that were not used for the fitting of the SDM.

*Script:* 04b_plot_predictions.R

In this script I plotted the predictions of all algorithms for year 0 and further also plotted the occurrence points of the respective scenario on top of it.

## 4. Analysis
Here, I plot and statistically analysed the results.

*Script:* 5_SDM_performance.R

Plots of the different performance measures of the different SDM algorithms.

*Script:* 5a_occupancy_plots.R

Plots of the occupancy probability of the different scenarios and landscapes.

*Script:* 06_create_dataset.R

In this script, I joined all the different values I obtained during my workflow (extinction probabilities, population size, habitat suitability, range size). I further calculated the relative population size, habitat suitability and range size (relative to year 0).

*Script:* 07_Plots.R

This script includes all plots I made during the exploratory analysis as well as the final plots.

*Script:* 08_plot_SDM_predictions.R

In this script I plotted the predictions of the ensemble model for every single year and further also plotted the occurrence points of the respective scenario on top of it.

