# Tidy script

library(tidyverse)



# Level 1: WMA

colnames <- c('FullName', 'NumberID', 'WMA')

tidy_Level1_WMA <- tblLandGroupings

colnames(tidy_Level1_WMA) <- colnames



# Level 2 (bird): Point Visits

colnames2 <- c('PointID', 'Date', 'Temperature', 'Wind', 'Sky', 'StartTime', 'EndTime', 'WMA', 'Longitude', 'Latitude', 'Observer')

tidy_BirdLevel2_PointVisits <- Bird_PointHeaderData[,-c(1:2)]

colnames(tidy_SamplePoint_Coordinates)[5] <- 'PointID'

tidy_BirdLevel2_PointVisits <- merge(tidy_BirdLevel2_PointVisits, tidy_SamplePoint_Coordinates[,c(4:7)], by="PointID")

obsEncounter <- Bird_EncounterData[,c(1:3)][!duplicated(Bird_EncounterData[,c(1:3)]), ]

tidy_BirdLevel2_PointVisits <- merge(tidy_BirdLevel2_PointVisits, obsEncounter[,c(1,3)], by="PointID")

colnames(tidy_BirdLevel2_PointVisits) <- colnames2

# Check for duplicates based on PointID and Date
duplicate_points_lvl2 <- tidy_BirdLevel2_PointVisits %>% 
  group_by(PointID, Date) %>% 
  filter(n() > 1) %>% 
  ungroup()

# View duplicates to decide how to handle them
View(duplicate_points_lvl2)



#Level 3 (bird): Encounters
colnames3 <- c('PointID', 'Interval', 'Species', 'Direction', 'Distance', 'ObsCode')

tidy_BirdLevel3_Encounters <- Bird_EncounterData[,-c(2,3)]

colnames(tidy_BirdLevel3_Encounters) <-  colnames3



write.csv(tidy_Level1_WMA, 'tidy_Level1_WMA.csv', row.names=FALSE)
write.csv(tidy_BirdLevel2_PointVisits, 'tidy_BirdLevel2_PointVisits.csv', row.names=FALSE)
write.csv(tidy_BirdLevel3_Encounters, 'tidy_BirdLevel3_Encounters.csv', row.names=FALSE)