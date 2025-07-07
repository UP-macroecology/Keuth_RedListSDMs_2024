# Red List criteria underestimate climate-related extinction risk of range-shifting species

Raya Keuth<sup>1</sup>, Susanne Fritz<sup>2,3</sup>, Damaris Zurell<sup>1</sup>

1. University of Potsdam, Institute of Biochemistry and Biology, Potsdam, Germany
2. German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig, Germany
3. Institute of Biodiversity, Ecology and Evolution, Friedrich Schiller University Jena, Germany


### ABSTRACT:

Keywords: IUCN Red List, extinction risk, species distribution models, climate change, virtual species, spatially explicit population models

This repository contains the R scripts needed to reproduce all results and plots.


Funding: This study was supported by the German Research Foundation DFG (grant no. ZU 361/6-1)

---------------------------------------------------------------
**Workflow**
---------------------------------------------------------------

### 0 - Data setup
scripts [folder structure](https://github.com/UP-macroecology/Keuth_SDMExtinctions_2024/blob/main/scripts/00_create_folder_structure.R), [functions](https://github.com/UP-macroecology/Keuth_SDMExtinctions_2024/blob/main/scripts/00_functions.R)

The required folder structure is set up and the needed functions are listed.

### 1 - Artificial landscapes
scripts [01](https://github.com/UP-macroecology/Keuth_SDMExtinctions_2024/blob/main/scripts/01_Create_Landscapes.R)

The artificial landscapes are created, which consist of two environmental variables that can be interpreted as temperature and precipitation. To simulate climate change, temperature is linearly increased with a temporal autocorrelated noise over 90 years. Precipitation remains static under climate change. The landscape is replicated three times using the same settings.

### 2 - Virtual species
scripts [02a](https://github.com/UP-macroecology/Keuth_SDMExtinctions_2024/blob/main/scripts/02a_Virtual_species_niche.R), [02b](https://github.com/UP-macroecology/Keuth_SDMExtinctions_2024/blob/main/scripts/02b_Simulations.R)

Based on the artifical landscapes the virtual species niche is modelled under climate change. Species niche varied in the parameters niche position (central vs. marginal) and niche breadth (wide vs. narrow), resulting in four different niche settings. The population dynamics under climate change are simulated for the different species using a spatially explicit individual-based modelling plattform. In the simulation dispersal distance (long vs. short) and growth rate (fast vs. slow) are adapted and the different virtual species niches are used as input data. To cover all parameter combinations 16 different virtual species are modelled. The simulation is replicated 100 times.

### 3 - Data preparation from simulations
scripts [03](https://github.com/UP-macroecology/Keuth_SDMExtinctions_2024/blob/main/scripts/03_Data_preparation.R)

The population size per year and species are extracted from the simulation results and extinction probability for every year is calculated. Occurrence points for the SDMs are obtained based on the pre-climate change distribution of the invidiuals. The dispersal distances for each species are extracted from the simulation results.

### 4 - Evaluation of simulation results
scripts [04](https://github.com/UP-macroecology/Keuth_SDMExtinctions_2024/blob/main/scripts/04_Simulations_plots.R)

Results of the simulations are plotted for visual inspection. This includes abundance, occupancy and extinction probability over time and the distribution of the individuals in the landscape under climate change.

### 5 - Spatial thinning
scripts [05](https://github.com/UP-macroecology/Keuth_SDMExtinctions_2024/blob/main/scripts/05_Spatial_Thinning.R)

For every species, absences are obtained based on the pre-climate change distribution of the individuals in the landscape (i.e., every cell not occupied by an individual is marked as an absence). Presences and absences are related with the climate data extracted from the artificial landscapes and spatially thinned by extracting every second cell.

### 6 - SDM fitting and validation
scripts [06a](https://github.com/UP-macroecology/Keuth_SDMExtinctions_2024/blob/main/scripts/06a_SDM.R), [06b](https://github.com/UP-macroecology/Keuth_SDMExtinctions_2024/blob/main/scripts/06b_SDM_dispersalassumption.R)

SDMs are fitted for each individual species and landscape replicate. To account for variation between the replicate runs, ten replicate runs are selected randomly and SDMs are fitted to each separately. This resulted in 480 SDMs (16 species x 3 landscape replicates x 10 replicate runs). SDMs are fitted using three different model algorithms (GLM, RF, Maxent) as well as an ensemble model. For each model, the performance was assessed using the 99 replicate runs not used for model fitting across four performance measures: The area under the receiver operating characteristic curve (AUC), true skill statistic (TSS), sensitivity, specificity. Following the guidelines of the Red List the sum of habitat suitabilities is calculated excluding values below the maxTSS threshold.
To test the effect of dispersal assumptions on the predictions of SDMs the predictions are binarised using the maxTSS and around the known presences a buffer with the size of the estimated dispersal distance of an individual in 10 years (timeframe of the Red List) is created. Dispersal distances are obtained from the simulations. Habitat suitabilities outside the buffer are set to 0.

### 7 - Data set preparation for statistical analysis
scripts [07a](https://github.com/UP-macroecology/Keuth_SDMExtinctions_2024/blob/main/scripts/07a_Join_datasets.R), [07b](https://github.com/UP-macroecology/Keuth_SDMExtinctions_2024/blob/main/scripts/07b_Prepare_data_analysis.R), [07c](https://github.com/UP-macroecology/Keuth_SDMExtinctions_2024/blob/main/scripts/07c_Prepare_data_hsloss_dispersal_assumptions.R), [07d](https://github.com/UP-macroecology/Keuth_SDMExtinctions_2024/blob/main/scripts/07d_Popdata_Fig2.R)

Data sets for the statistical analysis are prepared. This includes joining extinction probability, population size and habitat suitability based on ensemble predictions in one data frame, calculating the relative population size and habitat suitability and calculating the mean over all replicate runs. The results of the performance measures of the SDMs are also joined in one data frame. For the habitat suitability using the basic dispersal assumptions, the relative change in habitat suitability between year x and year x+10 are calculated. For plots, the data on the distribution of abundance in the landscape are extracted for one replicate run from the simulation output.

### 8 - Statistical analysis
scripts [08a](https://github.com/UP-macroecology/Keuth_SDMExtinctions_2024/blob/main/scripts/08a_Ordbeta_model.R), [08b](https://github.com/UP-macroecology/Keuth_SDMExtinctions_2024/blob/main/scripts/08b_MW_IUCN_classifications.R)

For the statistical analysis, an ordered beta regression model is used to estimate the influence of the traits on the relationship between population size and habitat loss. Landscape is included as a random effect and the best setting for this is determined using the difference in expected log predictive density between the models. The classification time of the different species in each of the threatened categories are determined using a moving window approach, testing for every year if the species would fulfil the Red List criteria in the following years. This is applied to population loss, habitat loss, extinction probability and the habitat loss estimated using dispersal assumptions.

### 9 - Visualisation of results
scripts [09a](https://github.com/UP-macroecology/Keuth_SDMExtinctions_2024/blob/main/scripts/09a_Plots_Maintext.R), [09b](https://github.com/UP-macroecology/Keuth_SDMExtinctions_2024/blob/main/scripts/09b_Plots_Supplementary.R)

These scripts contain the codes for the figures in the main text and the supplement.

---------------------------------------------------------------
**Required folder structure**
---------------------------------------------------------------
```

Simulations/
├── Inputs/
├── Output_Maps/
├── Outputs/

output_data/
├── occurrences/
├── landscapes/
├── SDMs
│   ├── algorithms/
│   ├── evaluation/
│   │   ├── algorithm_output/
│   │   └── performance_measures/
│   ├── predictions/
│   │   └──  prediction_maps/
│   └── plots/
├── analysis_data/
├── plots/
└── model_results/

scripts/

```

---------------------------------------------------------------
**Operating system info**
---------------------------------------------------------------
* R version 4.2.2 (2022-11-10 r83330)
* Platform: x86_64-pc-linux-gnu (64-bit)
* Running under: Debian GNU/Linux 12 (bookworm)

* Attached packages:
[1] data.table_1.17.0  [2] gridExtra_2.3  [3] terra_1.7-78  [4] ggplot2_3.5.1  [5] dplyr_1.1.2  [6] scales_1.3.0  [7] ordbetareg_0.8  [8] lhs_1.1.6  [9] PresenceAbsence_1.1.11  [10] gbm_2.1.9  [11] maxnet_0.1.4  [12] randomForest_4.7-1.1  [13] dismo_1.3-14  [14] virtualspecies_1.5.1  [15] raster_3.6-26  [16] NLMR_1.1.1  [17] RangeShiftR_1.0.4  [18] doParallel_1.0.16  [19] foreach_1.5.2 [20] tibble_3.2.1 [21] RColorBrewer_1.1-2
