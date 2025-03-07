library(terra)
library(tidyverse)


folder_path1 <- "C:/Users/jessiexm.stu/OneDrive - UBC/Desktop/FCOR599/LiDAR Data/DEM"
dtm_files <- c(list.files(folder_path1, pattern = "\\.tif", full.names = TRUE))
folder_path2 <- "C:/Users/jessiexm.stu/OneDrive - UBC/Desktop/FCOR599/LiDAR Data/CHM"
chm_files <- c(list.files(folder_path2, pattern = "\\.tif", full.names = TRUE))
folder_path3 <- "C:/Users/jessiexm.stu/OneDrive - UBC/Desktop/FCOR599/LiDAR Data/Understory"
understory_files <- c(list.files(folder_path3, pattern = "\\.tif", full.names = TRUE))

camera <- read.csv("C:/Users/jessiexm.stu/OneDrive - UBC/Desktop/FCOR599/LiDAR Data/Plot_metrics.csv")

# Initialize a data frame to store results
results <- data.frame()

# Iterate over each LAS file
for (file in dtm_files) {
  # Load the dtm file
  dtm <- rast(file)
  average_elevation <- global(dtm, fun = "mean", na.rm = TRUE)
  slope <- terrain(dtm, unit = "degrees")
  average_slope <- global(slope, fun = "mean", na.rm = TRUE)
  results <- rbind(results, data.frame(elevation = average_elevation, slope = average_slope))
  
}
colnames(results) <- c("elevation", "slope")

result_2 <- data.frame()

for (file in chm_files) {
  # Canopy height model
  chm <- rast(file)

  # Canopy cover
  height_threshold <- 4
  canopy_binary <- chm > height_threshold
  total_cells <- ncell(canopy_binary)
  canopy_cells <- global(canopy_binary, fun = "sum", na.rm = TRUE)
  canopy_cells_count <- canopy_cells[1, "sum"]
  canopy_cover_percentage <- (canopy_cells_count / total_cells) * 100
  result_2 <- rbind(result_2, data.frame(Canopy_Cover_Percentage = canopy_cover_percentage))
  
}

results <- results %>%
 mutate(Canopy_Cover_Percentage = result_2$Canopy_Cover_Percentage)

results <- results %>%
  mutate(stat = camera$stat)


# Print the results data frame
print(results)

output_file <- "C:/Users/jessiexm.stu/OneDrive - UBC/Desktop/FCOR599/LiDAR Data/plot_metrics_2.csv"

# Save the data frame to a CSV file
write.csv(results, file = output_file, row.names = FALSE)
