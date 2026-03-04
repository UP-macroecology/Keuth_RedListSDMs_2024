# Red List criteria underestimate climate-related extinction risk of range-shifting species


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                         00. Create folder structure                    #
# ---------------------------------------------------------------------- #


#-------------------------------------------------------------------------------


# A folder structure is created to read in data and store outputs
# for the analyses (Define full folder structure)
folders <- c(
  # Simulations
  "Simulations/Inputs",
  "Simulations/Output_Maps",
  "Simulations/Outputs",
  
  # Output
  "output_data/occurrences",
  "output_data/landscapes",
  "output_data/SDMs/algorithms",
  "output_data/SDMs/evaluation/algorithm_output",
  "output_data/SDMs/evaluation/performance_measures",
  "output_data/SDMs/predictions/prediction_maps",
  "output_data/SDMs/plots",
  
  "output_data/analysis_data",
  
  "output_data/plots/final_plots",
  "output_data/plots/detailed_plots",
  
  "output_data/model_results",
  
  # Scripts
  "scripts/",
  
  # Functions
  "functions/"
)

# Create directories if not present
for (folder in folders) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
    message("Created: ", folder)
  }
}