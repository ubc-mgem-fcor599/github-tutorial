#Install and load package
library(lidR)
library(terra)
library(tidyverse)
library(dplyr)

#set working directory
setwd("C:/Users/jessiexm.stu/OneDrive - UBC/Desktop/FCOR599/LiDAR Data")

#create variable string of wd
wd = "C:/Users/jessiexm.stu/OneDrive - UBC/Desktop/FCOR599/LiDAR Data"

#2) Read multiple .las files into LAScatalog object ----
#Create LAScatalog object from MKRF las tiles
cat <- readLAScatalog("cutresult")
las_check(cat) #perform deep inspection
summary(cat) #get summary report
plot(cat) #plotting the catalog

#Set the output directory for the filtered .las data
opt_output_files(cat) <- paste(wd, "/Filtered/filtered_{ID}", sep = "")
cat <- filter_duplicates(cat) #remove duplicte points and speed up processing
filtered_cat <- readLAScatalog("Filtered")
summary(filtered_cat) #get summary report
plot(filtered_cat) #plotting the catalog

#3) Normalize catalog ----
#Create DEM
opt_output_files(filtered_cat) <- paste(wd, "/DEM/dem_{ID}", sep = "")
dem_allLAS_mkrf <- rasterize_terrain(filtered_cat, 2, tin())
dem_1 <- rast("C:/Users/jessiexm.stu/OneDrive - UBC/Desktop/FCOR599/LiDAR Data/DEM/dem_1.tif")
plot(dem_1)
plot_dtm3d(dem_1)

#Create color palette
col_1 <- height.colors(50) 
#Plot DEM using color palette
plot(dem_allLAS_mkrf, col = col_1) #plot in 2D

#define LAScatalog engine options
opt_output_files(filtered_cat) <- paste(wd, "/Normalized/norm_{ID}", sep = "")

#normalize all tiles in cat with the DEM 
norm_tiles_mkrf <- normalize_height(filtered_cat, dem_allLAS_mkrf) #check your folder when complete 

#check to see if the normalization worked
norm_mkrf_1 <- readLAS("Normalized/norm_2.las")
plot(norm_mkrf_1,axis = TRUE) #plot the nomalized tile

#read normalized las into catalog to continue processing
norm_cat <- readLAScatalog("Normalized")


#4) Produce CHM from normalized catalog ----
#Create CHM for all normalized MKRF Tiles
chm_mkrf <- rasterize_canopy(norm_cat, 2, p2r())
plot(chm_mkrf, col = col_1) #plot in 2D
opt_output_files(norm_cat) <- paste(wd, "/chm/chm_{ID}", sep = "")
norm_cat <- rasterize_canopy(norm_cat, 2, p2r())
chm_1 <- rast("C:/Users/jessiexm.stu/OneDrive - UBC/Desktop/FCOR599/LiDAR Data/chm/chm_1.tif")
plot(chm_1, col = col_1)
plot_dtm3d(chm_1)

#5) Calculating Cloud Metrics ----
#Calculate cloud metrics for all plots
#create empty dataframe
plot_metrics <- data.frame() 

#For loop to calculate cloud metrics for all plots and add them to 'mkrf_cloud_metrics'
for(i in 1:19){ #for loop == number of rows in plot_table (20)
  plot <- readLAS(paste("Normalized/norm_", i, ".las", sep= ""), filter = "-keep_first -drop_z_below 2") #read las
  metrics <- cloud_metrics(plot, .stdmetrics) #compute standard metrics
  plot_metrics <- rbind(plot_metrics, metrics) #add the new 'metrics' to 'mkrf_cloud_metrics'
}

# Save understory vegetation by filtering out the point cloud over 4
for(i in 1:19){ #for loop == number of rows in plot_table (20)
  plot <- readLAS(paste("Normalized/norm_", i, ".las", sep= ""), filter = "-drop_z_above 4") #read las
  las_path <- paste("Total_under4/under4_", i, ".las", sep= "")
  writeLAS(plot, las_path)
}

#check understory result
understory <- readLAScatalog("Understory")
understory_total <- readLAScatalog("Total_under4")
understory_point <- understory@data$Number.of.point.records
understory_point <- as.data.frame(understory_point)
total_point <- understory_total@data$Number.of.point.records
total_point <- as.data.frame(total_point)
point_df <- cbind(understory_point, total_point)
understory_density <- point_df$understory_point / point_df$total_point
  

plot(understory, axis = TRUE)

#Get stat name
file_list <- list.files(path = "cutresult", full.names = TRUE)
stat <- substr(file_list,11,14)


#Export mkrf_plot_metrics as .csv in Plots folder
plot_metrics <- plot_metrics %>%
  mutate(stat = stat)
plot_metrics_height <- plot_metrics %>%
  select(stat, zmean, zsd, zkurt, zq20, zq50, zq70, zq95, zmax)

plot_metrics_height <- cbind(plot_metrics_height, understory_density)

write_csv(plot_metrics_height, "Plot_metrics.csv")


