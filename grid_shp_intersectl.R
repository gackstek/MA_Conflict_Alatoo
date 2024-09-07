library(sf)
library(dplyr)

#setwd("Desktop/MA/")
grid <- st_read("data study area/Alatoo/Grid_5x5.shp")
survey_effort1 <- st_read("shapefiles/herder1/survey_effort1.shp")
survey_effort2 <- st_read("shapefiles/herder2/survey_effort2.shp")
survey_effort3 <- st_read("shapefiles/herder3/survey_effort3.shp")
survey_effort4 <- st_read("shapefiles/herder4/survey_effort4.shp")
survey_effort5 <- st_read("shapefiles/herder5/survey_effort5.shp")
survey_effort6 <- st_read("shapefiles/herder6/survey_effort6.shp")
survey_effort7 <- st_read("shapefiles/herder7/survey_effort7.shp")
survey_effort8 <- st_read("shapefiles/herder8/survey_effort8.shp")
survey_effort9 <- st_read("shapefiles/herder9/survey_effort9.shp")
survey_effort10 <- st_read("shapefiles/herder10/survey_effort10.shp")

############################################## livestock
livestock1 <- st_read("shapefiles/herder1/livestock1.shp")
livestock2 <- st_read("shapefiles/herder2/livestock2.shp")
livestock3 <- st_read("shapefiles/herder3/livestock3.shp")
livestock4 <- st_read("shapefiles/herder4/livestock4.shp")
livestock5 <- st_read("shapefiles/herder5/livestock5.shp")
livestock6 <- st_read("shapefiles/herder6/livestock6.shp")
livestock7 <- st_read("shapefiles/herder7/livestock7.shp")
livestock8 <- st_read("shapefiles/herder8/livestock8.shp")
livestock9 <- st_read("shapefiles/herder9/livestock9.shp")
livestock10 <- st_read("shapefiles/herder10/livestock10.shp")

############################################## conflict
conflict1 <- st_read("shapefiles/herder1/conflict1.shp")
conflict2 <- st_read("shapefiles/herder2/conflict2.shp")
conflict3 <- st_read("shapefiles/herder3/conflict3.shp")
conflict4 <- st_read("shapefiles/herder4/conflict4.shp")
conflict5 <- st_read("shapefiles/herder5/conflict5.shp")
conflict6 <- st_read("shapefiles/herder6/conflict6.shp")
conflict7 <- st_read("shapefiles/herder7/conflict7.shp")
conflict8 <- st_read("shapefiles/herder8/conflict8.shp")
conflict9 <- st_read("shapefiles/herder9/conflict9.shp")
conflict10 <- st_read("shapefiles/herder10/conflict10.shp")

############################################## no conflict
no_conflict1 <- st_read("shapefiles/herder1/no_conflict1.shp")
no_conflict2 <- st_read("shapefiles/herder2/no_conflict2.shp")
no_conflict3 <- st_read("shapefiles/herder3/no_conflict3.shp")
no_conflict4 <- st_read("shapefiles/herder4/no_conflict4.shp")
no_conflict5 <- st_read("shapefiles/herder5/no_conflict5.shp")
no_conflict6 <- st_read("shapefiles/herder6/no_conflict6.shp")
no_conflict7 <- st_read("shapefiles/herder7/no_conflict7.shp")
no_conflict8 <- st_read("shapefiles/herder8/no_conflict8.shp")
no_conflict9 <- st_read("shapefiles/herder9/no_conflict9.shp")
no_conflict10 <- st_read("shapefiles/herder10/no_conflict10.shp")




#############################################

# Perform spatial join with shapefile1
grid_with_survey_effort1 <- st_join(grid, survey_effort1, join = st_intersects)
grid_with_survey_effort2 <- st_join(grid, survey_effort2, join = st_intersects)
grid_with_survey_effort3 <- st_join(grid, survey_effort3, join = st_intersects)
grid_with_survey_effort4 <- st_join(grid, survey_effort4, join = st_intersects)
grid_with_survey_effort5 <- st_join(grid, survey_effort5, join = st_intersects)
grid_with_survey_effort6 <- st_join(grid, survey_effort6, join = st_intersects)
grid_with_survey_effort7 <- st_join(grid, survey_effort7, join = st_intersects)
grid_with_survey_effort8 <- st_join(grid, survey_effort8, join = st_intersects)
grid_with_survey_effort9 <- st_join(grid, survey_effort9, join = st_intersects)
grid_with_survey_effort10 <- st_join(grid, survey_effort10, join = st_intersects)

############################################# livestock grid
grid_with_livestock1 <- st_join(grid, livestock1, join = st_intersects)
grid_with_livestock2 <- st_join(grid, livestock2, join = st_intersects)
grid_with_livestock3 <- st_join(grid, livestock3, join = st_intersects)
grid_with_livestock4 <- st_join(grid, livestock4, join = st_intersects)
grid_with_livestock5 <- st_join(grid, livestock5, join = st_intersects)
grid_with_livestock6 <- st_join(grid, livestock6, join = st_intersects)
grid_with_livestock7 <- st_join(grid, livestock7, join = st_intersects)
grid_with_livestock8 <- st_join(grid, livestock8, join = st_intersects)
grid_with_livestock9 <- st_join(grid, livestock9, join = st_intersects)
grid_with_livestock10 <- st_join(grid, livestock10, join = st_intersects)

############################################## conflict
grid_with_conflict1 <- st_join(grid, conflict1, join = st_intersects)
grid_with_conflict2 <- st_join(grid, conflict2, join = st_intersects)
#grid_with_conflict3 <- st_join(grid, conflict3, join = st_intersects)
grid_with_conflict4 <- st_join(grid, conflict4, join = st_intersects)
grid_with_conflict5 <- st_join(grid, conflict5, join = st_intersects)
grid_with_conflict6 <- st_join(grid, conflict6, join = st_intersects)
grid_with_conflict7 <- st_join(grid, conflict7, join = st_intersects)
grid_with_conflict8 <- st_join(grid, conflict8, join = st_intersects)
grid_with_conflict9 <- st_join(grid, conflict9, join = st_intersects)
grid_with_conflict10 <- st_join(grid, conflict10, join = st_intersects)

############################################## no conflict
grid_with_no_conflict1 <- st_join(grid, no_conflict1, join = st_intersects)
#grid_with_no_conflict2 <- st_join(grid, no_conflict2, join = st_intersects)
grid_with_no_conflict3 <- st_join(grid, no_conflict3, join = st_intersects)
grid_with_no_conflict4 <- st_join(grid, no_conflict4, join = st_intersects)
grid_with_no_conflict5 <- st_join(grid, no_conflict5, join = st_intersects)
grid_with_no_conflict6 <- st_join(grid, no_conflict6, join = st_intersects)
grid_with_no_conflict7 <- st_join(grid, no_conflict7, join = st_intersects)
grid_with_no_conflict8 <- st_join(grid, no_conflict8, join = st_intersects)
grid_with_no_conflict9 <- st_join(grid, no_conflict9, join = st_intersects)
grid_with_no_conflict10 <- st_join(grid, no_conflict10, join = st_intersects)


# Add attributes from shapefile1 as new columns in the original grid
grid$survey_effort1 <- grid_with_survey_effort1$id
grid$survey_effort2 <- grid_with_survey_effort2$id
grid$survey_effort3 <- grid_with_survey_effort3$id
#grid$survey_effort4 <- grid_with_survey_effort4$id
grid$survey_effort5 <- grid_with_survey_effort5$id
#grid$survey_effort6 <- grid_with_survey_effort6$id
grid$survey_effort7 <- grid_with_survey_effort7$id
grid$survey_effort8 <- grid_with_survey_effort8$id
grid$survey_effort9 <- grid_with_survey_effort9$id
grid$survey_effort10 <- grid_with_survey_effort10$id

############################################# livestock grid
grid$livestock1 <- grid_with_livestock1$id
grid$livestock2 <- grid_with_livestock2$id
grid$livestock3 <- grid_with_livestock3$id
grid$livestock4 <- grid_with_livestock4$id
grid$livestock5 <- grid_with_livestock5$id
#grid$livestock6 <- grid_with_livestock6$id
grid$livestock7 <- grid_with_livestock7$id
grid$livestock8 <- grid_with_livestock8$id
grid$livestock9 <- grid_with_livestock9$id
#grid$livestock10 <- grid_with_livestock10$id


############################################## conflict
grid$conflict1 <- grid_with_conflict1$id
grid$conflict2 <- grid_with_conflict2$id
#grid$conflict3 <- grid_with_conflict3$id
grid$conflict4 <- grid_with_conflict4$id
grid$conflict5 <- grid_with_conflict5$id
grid$conflict6 <- grid_with_conflict6$id
grid$conflict7 <- grid_with_conflict7$id
grid$conflict8 <- grid_with_conflict8$id
grid$conflict9 <- grid_with_conflict9$id
grid$conflict10 <- grid_with_conflict10$id

############################################## no conflict
grid$no_conflict1 <- grid_with_no_conflict1$id
#grid$no_conflict2 <- grid_with_no_conflict2$id
grid$no_conflict3 <- grid_with_no_conflict3$id
grid$no_conflict4 <- grid_with_no_conflict4$id
grid$no_conflict5 <- grid_with_no_conflict5$id
grid$no_conflict6 <- grid_with_no_conflict6$id
grid$no_conflict7 <- grid_with_no_conflict7$id
grid$no_conflict8 <- grid_with_no_conflict8$id
grid$no_conflict9 <- grid_with_no_conflict9$id
grid$no_conflict10 <- grid_with_no_conflict10$id


# Remove geometry column from the joined data if necessary
grid <- subset(grid, select = -PageName)
grid <- subset(grid, select = -geometry)

output_path <- "questionnaire data /"
output_name <- "grid_example3.csv"  # You can change the name and use ".csv" extension

write.csv(grid, file = file.path(output_path, output_name), row.names = FALSE)



