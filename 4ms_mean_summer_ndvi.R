library(tidyverse)
library(purrr)
library(data.table)

calcNormDif <- function(b1, b2){
  return((b1-b2)/(b1+b2))
}

setwd('/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal_extract/tree_outputs_point')

file_list <- list.files()
names(file_list) <- strsplit(file_list, "[.]") %>% lapply('[[', 1) %>% stringr::str_sub(start = 19, end = 27) %>% ymd()

# Need to loop over years
for (yr in 2017:2024){
  print(yr)
  file_list_sub <- file_list[which(month(names(file_list)) %in% 6:8 & year(names(file_list)) == yr)]
  tree_df_sub <- purrr::map_df(file_list_sub, fread, .id = 'date') 
  
  mean_red <- aggregate(tree_df_sub$red, by = list(tree_df_sub$Object_ID), FUN = 'mean', na.rm = T)
  colnames(mean_red) <- c('Object_ID', 'refl')
  mean_nir <- aggregate(tree_df_sub$nir, by = list(tree_df_sub$Object_ID), FUN = 'mean', na.rm = T)
  colnames(mean_nir) <- c('Object_ID', 'refl')
  
  mean_ndvi <- mean_nir
  mean_ndvi$refl <- calcNormDif(mean_nir$refl, mean_red$refl)
  
  colnames(mean_ndvi)[2] <- paste0('ndvi_', yr)
  
  if (yr == 2017) {
    mean_ndvi_all <- mean_ndvi
  } else if (yr > 2017){
    mean_ndvi_all <- merge(mean_ndvi_all, mean_ndvi)
  }
}

mean_ndvi_all[,2:9] <- round(mean_ndvi_all[,2:9], 4)

# Updated this path
setwd('/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal_extract/')
write.csv(mean_ndvi_all, "mean_summer_ndvi.csv", row.names = FALSE)