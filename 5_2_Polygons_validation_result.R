# From Ging-Yan Ho
# Note: most accuracy assessment done manually in QGIS

library(tidyverse)
library(sf)


points_validation <- read_csv("D:\\Polygon_validations\\points.csv")
polys_validation <- read_csv("D:\\Polygon_validations\\polygons.csv")

# Count for each categories (polygons)
polys_validation %>% count(Visual_Class)

# Reclassify valuable polygons, and have BA ratio as a column
poly_eval <- polys_validation %>%
  mutate(
    evaluable = !Visual_Class %in% c("No point - has canopy", "No point - point nearby", "No point - tree removed"),
    BA_ratio = BA_rest / BA_biggest
  )

# Summarize BA ratio of undersegment polygon
undersegment_ba_summary <- poly_eval %>%
  filter(evaluable, Visual_Class == "Undersegment") %>%
  summarise(
    n_undersegment = n(),
    mean_BA_ratio = mean(BA_ratio, na.rm = TRUE),
    median_BA_ratio = median(BA_ratio, na.rm = TRUE),
    min_BA_ratio = min(BA_ratio, na.rm = TRUE),
    max_BA_ratio = max(BA_ratio, na.rm = TRUE)
  )

undersegment_ba_summary


calc_accuracy <- function(threshold) {
  
  data_eval <- poly_eval %>% filter(evaluable == TRUE)
  
  data_eval <- data_eval %>% 
    mutate(
      acceptable_undersegment = Visual_Class == "Undersegment" & BA_ratio < threshold,
      correct_adjusted = Visual_Class == "Match" | acceptable_undersegment
    )
  
  data_eval %>% 
    summarise(
      threshold = threshold,
      n_evaluable = n(),
      n_match = sum(Visual_Class == "Match", na.rm = TRUE),
      n_undersegment_reclassified = sum(acceptable_undersegment, na.rm = TRUE),
      n_correct_adjusted = sum(correct_adjusted, na.rm = TRUE),
      adjusted_accuracy = n_correct_adjusted / n_evaluable
    )
}


accuracy_sensitivity <- bind_rows(
  calc_accuracy(0.50),
  calc_accuracy(0.25),
  calc_accuracy(0.10)
)

accuracy_sensitivity

# Count for each categories (points)
points_validation_no_M_street <- points_validation %>% filter(Borough != 'Manhattan' | Area_Type != 'Street')

table(points_validation$Assessment)

table(points_validation_no_M_street$Assessment)
