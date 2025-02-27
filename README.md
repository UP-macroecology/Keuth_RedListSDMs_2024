# Quantifying extinction risk using SDMs

The aim of this study is to test the validity of the guidelines of the IUCN Red List for assessing species threatened by climate change. As well as testing how different traits might influence the assessment of species threatened by climate change. For this, a spatially-explicit simulation of virtual species under a simulated climate change in three different artificial landscapes is used. Simulation data was used to fit different SDM algorithms according to the IUCN Red List Guidelines. The relationship between simulated population size and SDM-predicted habitat loss under scenarios of climate change was investigated and the classification time in the IUCN Red List is determined against criterion A3 and criterion E using simulated population size, SDM-predicted habitat loss and quantitative estimates of extinction probability.

## Workflow
First, the population dynamics of virtual species in an artifical landscape under simulated climate change is modelled using the spatially-explicit individual-based modelling plattform RangeShifter. Species differed in their traits niche position (central vs. marginal), niche width (narrow vs. wide), growth rate (slow vs. fast) and dispersal (short vs. long) leading to 16 (24) different species to cover all trait combinations. Species occurrence data and climate data were extracted from the simulation at equilibrium before climate change set in. Based on these data we fitted species distribution models (SDMs) and predicted habitat suitability under climate change. From the simulations, we observed population sizes across the entire time frame and estimated extinction probability per year. From the SDMs, we derived the predicted habitat suitabilities for each year and compared these to true population size. Last, we applied the IUCN Red List criteria to each of these three metrics to classify species into different threatened categories.

## Extras:
Additionally, this repository includes scripts and code, which investigate several different aspects such as the habitat loss - extinction risk relationship or the habitat loss - range loss relationship. This code is sometimes commented in the orginal scripts, sometimes included as miscellaneous scripts.

## 1. Parameter testing for the simulation model
To establish the parameters for the simulation model I first test different parameter values to establish values that represent the patterns that I want to investigate as well as to obtain a stable population size after the spin-up period in 
my simulation model. In the different scripts I tested different niche positions, different niche breadths and different dispersal distances. For the dispersal distance I modelled a short dispersal distance and a long dispersal distance with a double exponential kernel. For the long dispersal distance I also tested different probabilities for the long dispersal distance. The results of the parameter testing can be found in the ReadMe.txt.

## 2. Simulations
As a first step for the study, I simulated the population dynamics of the different species under climate change in the three neutral landscapes.

*Script:* 01_CreateLandscape.R

In this script, I create the three artifical landscapes under climate change. First, the landscapes are created, which consist of two environmental variables that can be interpreted as temperature and precipitation. Next, climate change is modelled by linearly increasing temperature over 90 years with spatially-autocorrelated noise using an ARIMA model of the order 0,0,1. Based on these landscapes the virtual species niche was modelled for the four different niche parameter combinations. These maps of suitable habitat are further used in the simulation model. The whole process was replicated three times to obtain three replicates of the same statistical settings. In this part of the study, the traits niche breadth and niche position were adapted.

*Script:* 02_Simulations.R

Here, I simulate the population dynamics of the different virtual species in the neutral landscape under climate change using RangeShifter. In the simulation the traits growth rate an dispersal distance were changed. In total I modelled 16 different species. The simulation was replicated for all three artifical landscapes.

*Script:* 02a_Simulations_data_graphs.R

In this script, I calculate the extinction probability of each year and extract the occurrences of the species from year 100 (at equilibrium conditions) for the different replicated runs. I further plot the results of the simulation to visualize the abundances, extinction probabilities and habitat loss and plot the occurrences in the landscape under climate change.

*Script:* 02b_Simulations_Results_Plots.R

In this script, I plot the abundances, extinction probabilities and real habitat loss values for all different species and landscapes for exploratory purposes.

*Script:* 02c_Simulation_extract_dispersal.R

In this script, I rerun the simulations to extract the dispersal distances of every individual for every single year. This is done for every species and every replicated landscape.

*misc. Scripts:*

*Simulation_nr_replicateruns.R*: Here, I extract the number of replicated runs with a viable population size for every year from the simulation outputs.

*Script:* text_labels_plots.R: This script contains text labels for various plots.

*Script:* Cluster_clean.R: This script is used to reduce the amount of data files on the HPC. Dispersal distances of individuals are stored in seperate files for every single replicated run. From these files I extract the data I need and store it all together in an Rdata file.

## 3. SDMs
As the second step, I fitted SDMs using the simulation data.

*Script:* 03_Spatial_Thinning.R

In this script, I first obtain the absences and then thin the data points by removing every second cell. To obtain the absences I marked every cell that was not occupied by an individual in the respective replicated run as absence.

*Script:* 04_SDM.R

I estimated SDMs to ten randomly selected replicated runs using the presences and absences and the climate data from the simulation. I fitted three algorithms and further calculated an ensemble model and predicted the habitat suitability to every year under climate change. To obtain the habitat suitability sums for every year I removed cells below a certain threshold (obtained by maxTSS) and sumed up the habitat suitability for the other cells. I evaluated the performance of the SDMs against all 99 replicated runs that were not used for the fitting of the SDM.

*Script:* 04d_SDM_dispersalassumption.R

In this script, I used dispersal assumptions based on empirical dispersal distances in the SDMs. For this, I calculated a buffer of the size of 10 times the estimated dispersal distance for one year (reflecting the 10 year timeframe of the IUCN Red List under criterion A3). All values outside of this buffer were set to 0.

*misc. Scripts:*

*Script:* 04_SDM_2.R: This script is a replicate script of 04_SDM.R to increase the speed of the SDM calculation.

*Script:* 04b_plot_predictions.R: In this script I plotted the predictions of all algorithms for year 0 and further also plotted the occurrence points of the respective scenario on top of it.

*Script:* 04a_SDM_rangesize.R: In this script, I did an additional calculation. I calculated the range size of each species by marking a cell as a presence if the habitat suitability was above a certain threshold (masTSS) and summing up the number of cells.

*Script:* ensemble_testing.R: This script was used for trouble-shooting. When I first fitted my models I obtained weird performance values for my ensemble model. Here, I obtained the differences in the predictions between the three algorithms and the ensemble model. Now, I just keep the code in case I need it again some day.

*Script:* plot_occurrences.R: Here, I plot the presences and absences of the different species.

*Script:* plot_SDM_predictions_current.R: In this script, I plotted the predicitons of habitat suitability and binarized habitat suitability for all species of current climate conditions.

*Script:* plot_SDM_predictions_future.R: In this script, I plotted the predicitons of habitat suitability and the occurrences under climate change.

## 4. Analysis
Here, I plot and statistically analyse the results.

*Script:* 5_SDM_performance.R

Boxplots of the different performance values for all SDM algorithms and the ensemble.

*Script:* 06a_dispersal_assumptions_dataset.R

Here, I calculate the habitat loss for every "start Year" (meaning the year in which I check if the species fulfills the criteria) to 10 years into the future. This is needed to later determine the classification time points when using dispersal assumptions in SDMs under criterion A3.

*Script:* 06_create_dataset.R

In this script, I joined all the different values I obtained during my workflow (extinction probabilities, population size, habitat suitability, range size). I further calculated the relative population size, habitat suitability and range size (relative to year 0). I saved these data sets in different formats (long and wide format).

*Script:* 07_MW_IUCN_classifications.R

Here, I obtain the classification time point, when a species would be assessed in the IUCN Red List under two criteria using three metrics. For this I write a function, which calculates population loss and habitat loss respective to the start Year of the assessment. I then look for every year, if the species would fulfill the criteria of the IUCN Red List in one of the three threatened categories and if not proceed with the next year. This means that if a species was predicted to reach a habitat loss of 30% by year 10, the year 0 would be marked as classification time point for listing the species as “Vulnerable” under criterion A3 and using the metric sum of SDM-derived habitat suitabilities. If the species was predicted to reach a habitat loss of 30% only by year 35, then the year 25 would be marked as classification time for the “Vulnerable” category.

*Script:* 08_Plots.R

This script includes the plots that are used in the paper as well as some exploratory plots.

*Script:* 09a_popdata_Inkscape.R

In this script, I create a shorter data set of the abundance values for later creating plots with it. For this, I extract one replicate run from the pop data set and adjust calculation of coordinates.

*Script:* 09b_Inkscape_plots.R

This script is used to produce plots which are needed for figures, which are created in Inkscape. I saved plots of the habitat suitability predictions for the first landscape for current and future (year 20+30) SDM predictions and also the abundances of species for the same year obtained from the simulation model.

*Script:* 09_Inkscape_plots.R

Here, I created various plots, which I used in several figures in the paper. This includes time trajectories of habitat size, population size and extinction probability, SDM predictions for current and future climatic conditions and the spatial distribution of the abundances of the species. All of this is done for two species in the first landscape.

*Script:* 10_Statistical_anaylsis.R

Here, I tried several different statistical analysis for investigating the influence of the traits on the classification time points and on the population loss - habitat loss relationship. For the first analysis I tested a multi-way ANOVA for detemining the effects of the traits on the classification time point, but decided against it. For the second analysis, I used GLMs and GLMER but had problems with overdispersion and convergence, which is why I transitioned to using a Bayesian model.

*Script:* 11_Bayesian_models_cluster.R

In this script, I perform a random-intercept Bayesian model to evaluate the effects of the landscape and the traits on the population loss.

*misc. Scripts:*

*Script:* misc_Plots.R: This script includes some misc. Plots, which I did throughout the whole plotting process but didn't end up in the final paper.

*Script:* occupancy_plots.R: Plots of the occupancy probability of the different scenarios and landscapes.

*Script:* plot_landscapes.R: In this script, I plotted the different landscapes separated into the different environmental variables and also for the one example virtual species niche.
