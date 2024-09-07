library(sf)
library(dplyr)
install.packages("readxl")
library(readxl)


# Set the path to your 'shapefiles' directory
shapefiles_dir <- "shapefiles/"
grid <- st_read("data study area/Alatoo/Grid_5x5.shp")
questionnaire <- read_excel("questionnaire data /interview data.xlsx")

# Get a list of all subdirectories within 'shapefiles' directory
subdirs <- list.dirs(path = shapefiles_dir, full.names = TRUE, recursive = FALSE)

# Initialize an empty list to store the combined data frames
combined_data_list <- list()

# Loop through each subdirectory
for (subdir in subdirs) {
  # Get the list of shapefiles in the current subdirectory
  shapefiles <- list.files(subdir, pattern = "\\.shp", full.names = TRUE)
  
  # Initialize an empty data frame to store the combined attributes for this subdirectory
  combined_data <- data.frame(Grid_ID = grid$PageNumber)  # Assuming 'Grid_ID' is the grid identifier
  
  # Loop through each shapefile in the current subdirectory
  for (shapefile in shapefiles) {
    # Read the current shapefile
    current_data <- st_read(shapefile)
    
    # Check if the shapefile is empty
    if (nrow(current_data) > 0) {
      # Perform a spatial join with the grid
      grid_with_data <- st_join(grid, current_data, join = st_intersects)
      
      # Add the attributes from the current shapefile to the combined data frame
      col_name <- sub(".shp", "", basename(shapefile))  # Use the shapefile name as the column name
      combined_data[[col_name]] <- grid_with_data$id
    }
  }
  
  # Remove the geometry column
  combined_data <- subset(combined_data, select = -geometry)
  
  # Add the combined data frame to the list
  combined_data_list[[subdir]] <- combined_data
}

# Merge the combined data frames from all subdirectories
final_combined_data <- Reduce(function(x, y) merge(x, y, by = "Grid_ID", all.x = TRUE), combined_data_list)

# Save the final combined data as a CSV file
output_path <- "output_directory"
output_name <- "combined_grid.csv"  # You can change the name and use ".csv" extension
write.csv(final_combined_data, file.path(output_path, output_name), row.names = FALSE)
