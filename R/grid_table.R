library(sf)
library(data.table)

# Replace with your actual file paths
setwd("/Users/kristina/Desktop/MA/")

grid_file <- "data study area/Alatoo/Grid_5x5.shp"

# Replace with the directory path where the herder folders are located
herder_folders <- c("shapefiles/herder1", "shapefiles/herder2", "shapefiles/herder3", "shapefiles/herder4", "shapefiles/herder5")

# Define the shapefile names without numerical suffix
herder_shapefile_names <- c("survey_effort", "livestock", "no_conflict", "conflict")

# Create an empty data.table to store the results
result_table <- data.table(Grid_ID = character(0))

# Loop through the herder folders
for (herder_folder in herder_folders) {
  # Create a data.table for this herder
  herder_result_table <- data.table(Grid_ID = character(0))
  
  # Loop through the shapefile names
  for (i in 1:length(herder_shapefile_names)) {
    shapefile_name <- paste0(herder_shapefile_names[i], i)
    
    # Read the shapefile
    obs_data <- st_read(file.path(herder_folder, paste0(shapefile_name, ".shp")))
    
    # Read the grid shapefile for this herder
    grid_file <- st_read(file.path(herder_folder, "survey_effort1.shp")) # Replace with the actual grid shapefile name
    grid_data <- st_read(grid_file)
    
    # Overlay the shapefile with the grid
    overlay_data <- st_join(grid_data, obs_data, join = st_intersects)
    
    # Calculate the sum of observations for each grid cell
    herder_result_table[, (herder_shapefile_names[i]) := rowSums(overlay_data[, -c(1, 2), with = FALSE], na.rm = TRUE), by = .(Grid_ID)]
  }
  
  # Combine the herder-specific results
  result_table <- rbind(result_table, herder_result_table)
}

# Print the resulting table
print(result_table)






