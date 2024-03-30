
library(tidyverse)

birds = read.csv("C:/Users/jjv0016/OneDrive - Auburn University/ICP2.0/Github/icp1/Tidy Tables/tidy_BirdLevel3_Encounters.csv")
veg = read.csv("C:/Users/jjv0016/OneDrive - Auburn University/ICP2.0/Github/icp1/Tidy Tables/tidy_VegLevel2.csv")

tmp = birds %>% 
  group_by(Species) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

tmp1 = birds %>% 
  select(PointID, Species) %>% 
  unique() %>% 
  mutate(count=1) %>% 
  full_join(expand.grid('PointID' = unique(veg$PntNum), 'Species'=unique(birds$Species)), by=c('PointID', 'Species')) %>% 
  mutate(count = ifelse(is.na(count), 0, count)) %>% 
  group_by(Species) %>% 
  summarise(prop = mean(count)) %>% 
  arrange(desc(prop))
  