library(tidyr)

grid_data <- data_frame("data study area/Alatoo/Grid_5x5.shp")


grid_data <- data.frame(
  Grid_ID = 1:4,
  survey_effort_Observer1 = c(10, 15, 8, 12),
  survey_effort_Observer2 = c(5, 9, 6, 11),
  survey_effort_Observer3 = c(7, 11, 5, 9),
  livestock_Observer1 = c(20, 25, 18, 22),
  livestock_Observer2 = c(15, 19, 16, 21),
  livestock_Observer3 = c(17, 21, 15, 19),
  conflict_Observer1 = c(3, 1, 2, 4),
  conflict_Observer2 = c(2, 4, 3, 1),
  conflict_Observer3 = c(1, 3, 2, 4),
  no_conflict_Observer1 = c(7, 10, 6, 9),
  no_conflict_Observer2 = c(8, 11, 7, 10),
  no_conflict_Observer3 = c(6, 9, 5, 8)
)

# Print the resulting data frame
print(grid_data)

# Assuming you already have the grid_data data frame
# First, gather the data into a longer format
grid_data_long <- pivot_longer(
  data = grid_data,
  cols = -Grid_ID,
  names_to = c("Observation_Type", "Observer"),
  names_pattern = "^(.+)_(Observer\\s\\d)$"
)

# Separate the 'Observation_Type' column into 'Category' and 'Observer'
grid_data_long <- separate(grid_data_long, col = Observation_Type, into = c("Category", "Observer"), sep = "_")

# Pivot the data back to have 'Grid_ID' as rows and 'Category' as columns
grid_data_wide <- pivot_wider(grid_data_long, names_from = c("Category", "Observer"), values_from = value)

# Print the resulting data frame
print(grid_data_wide)

####################################################
library(dplyr)

# Assuming you already have the grid_data data frame
# Group the data by Grid_ID and summarize the values
grid_data_aggregated <- grid_data %>%
  group_by(Grid_ID) %>%
  summarize(
    survey_effort_Observer1 = sum(survey_effort_Observer1),
    survey_effort_Observer2 = sum(survey_effort_Observer2),
    survey_effort_Observer3 = sum(survey_effort_Observer3),
    livestock_Observer1 = sum(livestock_Observer1),
    livestock_Observer2 = sum(livestock_Observer2),
    livestock_Observer3 = sum(livestock_Observer3),
    conflict_Observer1 = sum(conflict_Observer1),
    conflict_Observer2 = sum(conflict_Observer2),
    conflict_Observer3 = sum(conflict_Observer3),
    no_conflict_Observer1 = sum(no_conflict_Observer1),
    no_conflict_Observer2 = sum(no_conflict_Observer2),
    no_conflict_Observer3 = sum(no_conflict_Observer3)
  ) %>%
  ungroup()

# Print the resulting data frame
print(grid_data_aggregated)

