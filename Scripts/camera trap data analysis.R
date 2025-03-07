library(tidyverse)
library(dplyr)

camera_trap <- read.csv("C:/Users/jessiexm.stu/OneDrive - UBC/Desktop/FCOR599/camera trap data/final/weekly_modeling_final.csv")
#camera_offtrail <- camera_trap %>%
 # filter(trail_DIST > 250,
  #       road_DIST > 250)

# Define week ranges
GE_start_week <- "2020-15"  # April 8, 2020 is in the 15th week
GE_end_week <- "2020-20"    # May 13, 2020 is in the 20th week

MK_start_week <- "2020-13"  # March 23, 2020 is in the 13th week
MK_end_week <- "2020-23"    # June 5, 2020 is in the 23rd week


# Initialize an empty data frame to store valid rows
target <- data.frame()

# Iterate over each row
for (i in seq_len(nrow(camera_trap))) {
  row <- camera_trap[i, ]
  
  # Check the conditions (for example purposes, change this logic to your actual conditions)
  if (row$trail_DIST >= 250 & row$road_DIST >= 250) {
    # Only keep rows that satisfy the condition
    target <- rbind(target, row)
  }
  if (row$trail_DIST <= 250 | row$road_DIST<= 250){
    year <- substr(camera_trap$year_week[i], 1, 4) 
    month <- as.numeric(substr(camera_trap$year_week[i], 6,7))
    Human_activity <- camera_trap$HOMO.n[i] + camera_trap$HIKER.n[i] + camera_trap$BIKE.n[i] + 
      camera_trap$HORSE.n[i] + camera_trap$MOTOR.n[i] + camera_trap$DOG.n[i]
    
    if(substr(camera_trap$stat[i], 1, 2) == "GE"){
      if(year == '2020' & (month > 15 & month < 20)){
        if(Human_activity < 10){
          target <- rbind(target, row)
        }
      }
    }
    if(substr(camera_trap$stat[i], 1, 2) == "MK"){
      if(year == '2020' & (month > 13 & month < 23)){
        if(Human_activity < 10){
          target <- rbind(target, row)
        }
      }
    }
  }
}

#Get rid of MKRF
target <- target %>%
  filter(substr(stat, 1,4) != "MKRF")

camera_count <- target %>%
  group_by(stat) %>%
  summarize(times_of_observation = n(),
            coyotes = sum(CALA.n)/n(),
            bobcats = sum(LYRU.n)/n(),
            cougars = sum(PUCO.n)/n(),
            black_bears = sum(URAM.n)/n(),
            blacktailed_deer = sum(ODHE.n)/n(),
            snowshoe_hares = sum(LEAM.n)/n(),
            dist_water = mean(water_DIST),
            dist_trail = mean(trail_DIST),
            dist_road = mean(road_DIST),
            slope = mean(SLOPE),
            elevation = mean(ELEVATION),
            NDVI = mean(NDVI))

camera_count <- camera_count %>%
  mutate(mammals = (coyotes + bobcats + cougars + black_bears + blacktailed_deer + snowshoe_hares))

#Get rid of traps with no observation
camera_count <- camera_count %>%
  filter(mammals != 0)

#Get rid of traps with no sufficient amount of data
camera_count <- camera_count %>%
  filter(times_of_observation > 4)

#Get total valid observation for following analysis
total_observation <- sum(camera_count$times_of_observation)
total_stat <- count(camera_count)

camera_location <- read.csv("C:/Users/jessiexm.stu/OneDrive - UBC/Desktop/FCOR599/camera location.csv")

camera_sum <- inner_join(camera_count, camera_location, by = "stat")

camera_longer <- pivot_longer(data = camera_sum, 
                              cols = coyotes : snowshoe_hares,
                              names_to = "species", 
                              values_to = "Average_counts_week")

ggplot(data = camera_longer, aes(x = stat, y = Average_counts_week, fill = species)) +
  geom_bar(stat = "identity")+
  labs(x = "camera trap id", y = "number of mammals captured")+
  ggtitle("Average Number of mammals captured by camera traps per week")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

write.csv(camera_sum, file = "C:/Users/jessiexm.stu/OneDrive - UBC/Desktop/FCOR599/camera_data.csv", row.names = FALSE)


