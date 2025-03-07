library(terra)
library(landscapemetrics)

reclass_table <- matrix(c(
  -Inf, 0.5, 1,   # Grass: height < 0.5 m (set to 1 for grass)
  0.5, 4, 2,   # Shrubs: height between 0.5 and 4 m (set to 2 for shrubs)
  4, Inf, 3),    # Trees: height > 4 m (set to 3 for trees)
  byrow = TRUE,
  ncol = 3
)

folder_path <- "C:/Users/jessiexm.stu/OneDrive - UBC/Desktop/FCOR599/LiDAR Data/CHM"
chm_files <- c(list.files(folder_path, pattern = "\\.tif", full.names = TRUE))

#Classify CHM
for (i in seq_along(chm_files)){
  rec <- rast(chm_files[i])
  rec <- classify(rec, reclass_table)
  plot(rec)
  output_path <- paste0("C:/Users/jessiexm.stu/OneDrive - UBC/Desktop/FCOR599/LiDAR Data/Reclassified/Reclassified_", i, ".tif", seq = "")
  writeRaster(rec,output_path,overwrite = TRUE)
}

rcl_path <- "C:/Users/jessiexm.stu/OneDrive - UBC/Desktop/FCOR599/LiDAR Data/Reclassified"
rcl_files <- c(list.files(rcl_path, pattern = "\\.tif", full.names = TRUE))
target <- camera$stat
layout(matrix(1:20, nrow = 4, ncol = 5))
for (i in 1:length(rcl_files)) {
  rcl <- rast(rcl_files[i])
  plot(rcl, main= target[i],legend = FALSE)
}


list <- list_lsm()

landscape_metrics <- data.frame()

for (i in seq_along(rcl_files)){
df <- data.frame(id = target[i])
rcl <- rast(rcl_files[i])

#patch density
pd_l <- lsm_l_pd(rcl, directions = 8)
pd_c <- lsm_c_pd(rcl, directions = 8)
pd_shrubs <- pd_c[2,]
pd_trees <- pd_c[3,]
df_1 <- data.frame(pd_l = pd_l$value, pd_shrubs = pd_shrubs$value, pd_trees = pd_trees$value)
df_1 <- cbind(df, df_1)

#large patch index
lpi_l <- lsm_l_lpi(rcl, directions = 8)
lpi_c <- lsm_c_lpi(rcl, directions = 8)
lpi_shrubs <- lpi_c[2,]
lpi_trees <- lpi_c[3,]
df_2 <- data.frame(lpi_l = lpi_l$value, lpi_shrubs = lpi_shrubs$value, lpi_trees = lpi_trees$value)
df_2 <- cbind(df_1,df_2)

#connectivity index
pa <- lsm_p_area(rcl, directions = 8)
pa_shrubs <- pa %>% filter(class == 2)
pa_trees <- pa %>% filter(class == 3)
area <- ncell(rcl) * (2*2)
CI_t <- 1 - sum((pa$value*10000/area)^2)
CI_shrubs <- 1 - sum((pa_shrubs$value*10000/area)^2)
CI_trees <- 1 - sum((pa_trees$value*10000/area)^2)
df_3 <- data.frame(CI_t = CI_t, CI_shrubs = CI_shrubs, CI_trees = CI_trees)
df_3 <- cbind(df_2,df_3)

landscape_metrics <- rbind(landscape_metrics, df_3)
}

output_file <- "C:/Users/jessiexm.stu/OneDrive - UBC/Desktop/FCOR599/LiDAR Data/plot_metrics_3.csv"
write.csv(landscape_metrics, file = output_file, row.names = FALSE)

camera <- read.csv("C:/Users/jessiexm.stu/OneDrive - UBC/Desktop/FCOR599/camera_data.csv")
lidar_result_1 <- read.csv("C:/Users/jessiexm.stu/OneDrive - UBC/Desktop/FCOR599/LiDAR Data/Plot_metrics.csv")
target = lidar_result_1$stat
landscape_metrics <- landscape_metrics %>%
  rename(stat = id)
landscape_metrics <- inner_join(landscape_metrics,camera,by = "stat")

model <- lm(blacktailed_deer ~ CI_trees, data=landscape_metrics)
summary(model)
plot(landscape_metrics$CI_t, landscape_metrics$blacktailed_deer,         
     xlab="patch density", ylab="number of Mammals",  
     main="Patch Density vs Number of Mammals",                
     pch=20)                           
abline(model, col="red")    
