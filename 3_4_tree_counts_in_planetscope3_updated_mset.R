# Number of trees by date

# Repeat, but filter for the trees in what we can call the Maximum Set of Elegant Trees (MSET) (high quality trees)
library(tidyverse)
library(data.table)
library(purrr)
'%notin%' <- Negate('%in%')

# Need to make a new original version of this tree point + polygon intersected file
tree_poly_unq <- readRDS("/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal_extract/tree_pointextract_polyid_all_output_merged.rds")

gen_list <- c("Platanus", "Gleditsia", "Pyrus", "Quercus", "Acer", "Tilia", 
              "Prunus", "Zelkova", "Ginkgo", "Styphnolobium", "Ulmus", 
              "Liquidambar", "Malus", "Robinia", "Liriodendron", "Betula", "Ailanthus", "Fraxinus")

# Filter based on minimum summer NDVI threshold for all years
setwd('/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal_extract')
mean_ndvi_all <- fread("mean_summer_ndvi.csv")
ndvi_min <- 0.3 # Thapa

# drop 2017, only need them to be green 2018-2024 for classification, 2017 is incomplete
mean_ndvi_all_live <- mean_ndvi_all %>% select(!ndvi_2017) %>% filter_at(vars(starts_with("ndvi")), all_vars(. > ndvi_min))

# combine everything to setup filtering
tree_poly_unq_sub <- tree_poly_unq %>% filter(dbh > 4 & tpstructur == "Full" &  tpconditio %in% c("Excellent", "Good", "Fair") & genus %in% gen_list & Poly_ID %in% unique(mean_ndvi_all_live$Object_ID)) 

# add in variables from our lidar crown metrics
setwd("/Volumes/NYC_geo/nyc_lidar_metrics/crown_metrics")
crown_metrics_file_list <- list.files()
tree_crown_metrics <- purrr::map_df(crown_metrics_file_list, fread)
tree_poly_unq_sub_wmetrics <- merge(tree_poly_unq_sub, tree_crown_metrics, by = "Poly_ID")

# updated mset
# changing filtering to only relevant columns
mset <- tree_poly_unq_sub_wmetrics %>% 
  select("Poly_ID", "Point_ID", "genus", "species", "dbh", "tpstructur", "tpconditio", 
         "npts", "height_max_ft", "height_med_ft",
         "int_mean", "int_mean_above_medh", "int_mean_below_medh", "wid_medh_max_ft", "wid_medh_circlerad_ft", "hw_rat_2_max", "hw_rat_2_circlerad", "cp_3_max", "cp_3_circlerad") %>% 
  na.omit()

setwd("/Volumes/NYC_geo/tree_classification")
saveRDS(mset, "mset_v2_wlidarvars.rds")

# this is what goes into the later part of the script
mset_genus <- mset[c("Poly_ID", "genus")]
#mset <- merge(mset_genus, mset_vars, by = "Poly_ID")
unique(mset$genus) %>% sort()
mset %>% na.omit() %>% nrow() # 235839, same length
#243675, same length
table(mset$genus) # counts for each genus

# this is now the input file for the set of the trees going into the classifier

#####
# counting tree availability, not directly needed for classification script


#####

# Monthly composites (4 band)
# Now do summarizing by date ranges

# Monthly summaries
setwd("/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal_extract/tree_outputs_point")
file_list <- list.files()
names(file_list) <- strsplit(file_list, "[.]") %>% lapply('[[', 1) %>% stringr::str_sub(start = 19, end = 26)  # name the list

# setup unique names as month-year combinations
unq_names <- seq(ymd('2018-01-01'), ymd('2024-12-31'), by = "1 month")

mset_month <- cbind.data.frame(mset_genus, as.data.frame(matrix(NA, nrow = nrow(mset_genus), ncol = length(unq_names))))
colnames(mset_month) <- c(colnames(mset_genus), as.character(unq_names))

for (i in 1:length(unq_names)){
  #i <- 1
  print(unq_names[i])
  time_span <- interval(ymd(unq_names[i]), ymd(unq_names[i]) + months(1) - days(1)) # a one month time span
  file_inds <- which(ymd(names(file_list)) %within% time_span)
  if (length(file_inds) > 0){
    tree_spectra <- purrr::map_df(file_list[file_inds], fread, .id = 'date') %>% filter(Object_ID %in% mset_genus$Poly_ID)

    tree_obs_counts <- as.data.frame(table(tree_spectra$Object_ID))
    colnames(tree_obs_counts)[1] <- "Poly_ID"
    tree_obs_counts$Poly_ID <- as.numeric(as.character(tree_obs_counts$Poly_ID))
    mset_joined_span <- full_join(mset_genus, tree_obs_counts) # check if factor indexing doesn't get changed

    mset_month[, i+2] <- mset_joined_span$Freq
  } else {
    print("No extracted data in time range")
  }
}

setwd("/Volumes/NYC_geo/tree_classification")
write.csv(mset_month, "mset_counts_month_v2_4b.csv", row.names = FALSE)

# Note: no extracted trees in Feb 2021, poor images throughout that month...

# Can do all month related outputs from this resulting data frame

# Monthly
# Composite across years
mset_month_colinds <- c(NA, NA, month(ymd(colnames(mset_month)[3:ncol(mset_month)])))
mset_month_composite <- cbind.data.frame(mset_month[,1:2], as.data.frame(matrix(NA, nrow = nrow(mset_month), ncol = 12)))
colnames(mset_month_composite)[3:14] <- paste0("month_", 1:12)
for (i in 1:12){
  mset_month_composite[,i+2] <- rowSums(mset_month[,which(mset_month_colinds == i)], na.rm = TRUE)
}

mset_month_composite %>% na.omit %>% nrow()
table(na.omit(mset_month_composite)$genus)
# no drop off in poly ids when we do monthly composites across years

# Other tests for monthly drop off
# Drop February for all years
mset_month_nofeb <- cbind.data.frame(mset_month[,1:2], mset_month[,which(mset_month_colinds != 2)]) # NAs are omitted from which()!
mset_month_nofeb %>% na.omit %>% nrow() # 169657
table(na.omit(mset_month_nofeb)$genus)

# Drop January and February for all years
mset_month_nojf <- cbind.data.frame(mset_month[,1:2], mset_month[,which(mset_month_colinds %notin% 1:2)]) # NAs are omitted from which()!
mset_month_nojf %>% na.omit %>% nrow()
table(na.omit(mset_month_nojf)$genus)

# Drop December, January, and February for all years
mset_month_nodjf <- cbind.data.frame(mset_month[,1:2], mset_month[,which(mset_month_colinds %notin% c(1, 2, 12))]) # NAs are omitted from which()!
mset_month_nodjf %>% na.omit %>% nrow()
table(na.omit(mset_month_nodjf)$genus)

#####

#####

# 8 band!
# Need to do the same thing for 8 band, likely 2021 through 2024
# Monthly summaries
setwd("/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_8b_highsunonly_cal_extract/tree_outputs_point")
file_list <- list.files()
names(file_list) <- strsplit(file_list, "[.]") %>% lapply('[[', 1) %>% stringr::str_sub(start = 19, end = 26)  # name the list

# setup unique names as month-year combinations
unq_names <- seq(ymd('2021-01-01'), ymd('2024-12-31'), by = "1 month")

mset_month <- cbind.data.frame(mset_genus, as.data.frame(matrix(NA, nrow = nrow(mset_genus), ncol = length(unq_names))))
colnames(mset_month) <- c(colnames(mset_genus), as.character(unq_names))

for (i in 1:length(unq_names)){
  #i <- 1
  print(unq_names[i])
  time_span <- interval(ymd(unq_names[i]), ymd(unq_names[i]) + months(1) - days(1)) # a one month time span
  print(time_span)
  file_inds <- which(ymd(names(file_list)) %within% time_span)
  if (length(file_inds) > 0){
    tree_spectra <- purrr::map_df(file_list[file_inds], fread, .id = 'date') %>% filter(Object_ID %in% mset_genus$Poly_ID)
    
    tree_obs_counts <- as.data.frame(table(tree_spectra$Object_ID))
    colnames(tree_obs_counts)[1] <- "Poly_ID"
    tree_obs_counts$Poly_ID <- as.numeric(as.character(tree_obs_counts$Poly_ID))
    mset_joined_span <- full_join(mset_genus, tree_obs_counts) # check if factor indexing doesn't get changed 
    
    mset_month[, i+2] <- mset_joined_span$Freq
  } else {
    print("No extracted data in time range")
  }
}

# setwd("/Users/dlm356/dlm356_files/nyc_trees/tree_color_sniping/tree_count_figures")
# write.csv(mset_month, "mset_counts_8b_month.csv", row.names = FALSE)

setwd("/Volumes/NYC_geo/tree_classification")
write.csv(mset_month, "mset_counts_month_v2_8b.csv", row.names = FALSE)

# test composites for 8 band
mset_month_colinds <- c(NA, NA, month(ymd(colnames(mset_month)[3:ncol(mset_month)])))
mset_month_composite <- cbind.data.frame(mset_month[,1:2], as.data.frame(matrix(NA, nrow = nrow(mset_month), ncol = 12)))
colnames(mset_month_composite)[3:14] <- paste0("month_", 1:12)
for (i in 1:12){
  mset_month_composite[,i+2] <- rowSums(mset_month[,which(mset_month_colinds == i)], na.rm = TRUE)
}

mset_month_composite %>% na.omit %>% nrow()
table(na.omit(mset_month_composite)$genus)
# no drop off in poly ids when we do 8b monthly composites across years