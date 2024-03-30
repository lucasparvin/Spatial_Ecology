library(dplyr)



# Assuming your date column is named "Date" in tidy_BirdLevel2_PointVisits
# Check for duplicates based on PointID and Date
duplicate_points_lvl2 <- tidy_BirdLevel2_PointVisits %>% 
  group_by(PointID, Date) %>% 
  filter(n() > 1) %>% 
  ungroup()

# View duplicates to decide how to handle them
View(duplicate_points_lvl2)

# If duplicates exist, decide how to handle them. For example, you might keep the first occurrence:
tidy_BirdLevel2_PointVisits_unique <- tidy_BirdLevel2_PointVisits %>%
  distinct(PointID, Date, .keep_all = T)

wma_sp_comp <- tidy_BirdLevel3_Encounters %>%
  left_join(select(tidy_BirdLevel2_PointVisits_unique, PointID, WMA), by = "PointID")

# View the result
View(wma_sp_comp)
str(wma_sp_comp)

# Print distinct values in the WMA column
distinct_wma_values <- distinct(wma_sp_comp, WMA)
print(distinct_wma_values)

# Filter rows where WMA is NA
na_wma_rows <- filter(wma_sp_comp, is.na(WMA))

# View these rows
View(na_wma_rows)


wma_sp_comp <- wma_sp_comp %>%
  # Remove rows where WMA is NA and PointID is 8081
  filter(!(is.na(WMA) & PointID == 8081)) %>%
  # Reassign WMA as "BA" where WMA is NA and PointID is 4056
  mutate(WMA = ifelse(is.na(WMA) & PointID == 4056, "BA", WMA)) %>%
  # Remove rows where both WMA and PointID are NA
  filter(!(is.na(WMA) & is.na(PointID)))

# View the modified data frame
View(wma_sp_comp)
distinct_wma_values <- distinct(wma_sp_comp, WMA)
print(distinct_wma_values)


wma_sp_comp <- wma_sp_comp %>%
  mutate(Category = case_when(
    WMA %in% c("WC", "GS", "OM", "GU") ~ "State Park",
    TRUE ~ "WMA"
  ))

# View the modified data frame
View(wma_sp_comp)



# Merging the tables
wma_sp_comp <- tidy_BirdLevel3_Encounters %>%
  left_join(select(tidy_BirdLevel2_PointVisits, PointID, WMA), by = "PointID")

wma_sp_comp
View(wma_sp_comp)


# Calculate distinct counts for PointID and Species in each category
distinct_counts <- wma_sp_comp %>%
  group_by(Category) %>%
  summarise(
    Distinct_PointIDs = n_distinct(PointID),
    Distinct_Species = n_distinct(Species)
  )

# Plot for Distinct PointID counts
p_pointid <- ggplot(distinct_counts, aes(x = Category, y = Distinct_PointIDs, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Distinct PointID Counts by Category", x = "Category", y = "Count of Distinct PointIDs") +
  theme_minimal()

# Plot for Distinct Species counts
p_species <- ggplot(distinct_counts, aes(x = Category, y = Distinct_Species, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Distinct Species Counts by Category", x = "Category", y = "Count of Distinct Species") +
  theme_minimal()

# Display the plots
print(p_pointid)
print(p_species)



library(dplyr)
library(tidyr)
library(ggplot2)

# Assuming 'Category' represents 'State Parks' and 'WMAs'
# Reshape the data to long format
long_data <- distinct_counts %>%
  pivot_longer(cols = c(Distinct_PointIDs, Distinct_Species),
               names_to = "Metric",
               values_to = "Count") %>%
  mutate(Metric = recode(Metric, 
                         "Distinct_PointIDs" = "Distinct Surveys", 
                         "Distinct_Species" = "Species Richness"))

# Create a bar chart with Metric on the x-axis and Category as fill
p_combined <- ggplot(long_data, aes(x = Metric, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = Count), vjust = -0.5, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("blue", "red"), 
                    labels = c("State Parks", "WMAs")) +
  labs(title = "ICP 1.0: Survey Effort & Species Richness in WMAs & State Parks",
       x = "Metric",
       y = "Count") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Display the combined plot
print(p_combined)




library(dplyr)
library(ggplot2)

# Calculate the average number of distinct species for each WMA
avg_species_per_wma <- wma_sp_comp %>%
  group_by(WMA, PointID) %>%
  summarise(Distinct_Species = n_distinct(Species), .groups = 'drop') %>%
  group_by(WMA) %>%
  summarise(Avg_Distinct_Species = mean(Distinct_Species), .groups = 'drop') %>%
  mutate(Category = ifelse(WMA %in% c("WC", "GS", "OM", "GU"), "State Parks", "WMAs"))

# Plot the average number of distinct species per WMA
# Plot the average number of distinct species per WMA with a legend
p_avg_species_per_wma <- ggplot(avg_species_per_wma, aes(x = WMA, y = Avg_Distinct_Species, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Avg_Distinct_Species, 2)), vjust = -0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("State Parks" = "blue", "WMAs" = "red"),
                    labels = c("State Parks", "WMAs")) +
  labs(title = "Species Richness per Survey in WMAs and State Parks",
       x = "Study Area",
       y = "Average Number of Distinct Species") +
  theme_minimal()

print(p_avg_species_per_wma)



library(dplyr)
library(ggplot2)

# Calculate the overall mean and standard error for each category
overall_means_with_error <- avg_species_per_wma %>%
  group_by(Category) %>%
  summarise(
    Overall_Avg_Distinct_Species = mean(Avg_Distinct_Species),
    Std_Error = sd(Avg_Distinct_Species) / sqrt(n())
  )

# Create a bar plot to compare the overall means of WMAs and State Parks with error bars
p_overall_means <- ggplot(overall_means_with_error, aes(x = Category, y = Overall_Avg_Distinct_Species, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Overall_Avg_Distinct_Species - Std_Error, ymax = Overall_Avg_Distinct_Species + Std_Error), width = 0.2) +
  geom_text(aes(label = round(Overall_Avg_Distinct_Species, 2)), vjust = -0.3) +
  scale_fill_manual(values = c("State Parks" = "blue", "WMAs" = "red")) +
  labs(title = "Distinct Species Per Survey in State Parks & WMAs",
       x = "Category",
       y = "Average Across Areas") +
  theme_minimal() +
  theme(legend.position = "none")

print(p_overall_means)













