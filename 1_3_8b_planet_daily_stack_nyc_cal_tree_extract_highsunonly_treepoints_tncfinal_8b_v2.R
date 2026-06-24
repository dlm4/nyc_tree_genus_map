library(terra)
library(lubridate)
library(sf)
library(exactextractr)
library(reshape2)
library(future)
library(future.apply)

test_rast <- rast("/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_8b_highsunonly_cal/nyc_planet_composite_8band_20220710_nyccal20220710ref.tif")
# use this for reprojecting
plotRGB(test_rast, r = 4, g = 3, b = 2, stretch = "lin")

tree_poly_path_full <- "/Volumes/NYC_geo/tree_polygons/tnc_2021/Trees_Centroids_Crown_Objects_2021.gdb" # Note this is the FINAL TNC dataset
tnc_gdb_polys <- st_read(tree_poly_path_full, layer = "treeobjects_2021_nyc")
tnc_gdb_polys$Object_ID <- seq(1,nrow(tnc_gdb_polys)) # so there is a unique identifier for each tree # this is OK but in future use # fid_column_name = "OBJECTID"
tnc_gdb_polys_reproj <- st_transform(tnc_gdb_polys, crs(test_rast)) # this takes about a minute fyi

# Tree extraction for entire city is almost too big to do with single files in multicore, so break it up into more manageable pieces (7 sections)

# change to just centroids - for points
tnc_gdb_points_reproj <- st_centroid(tnc_gdb_polys_reproj)

top_output_dir <- "/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_8b_highsunonly_cal_extract" # no trailing "/" 

# HERE WE GO
setwd("/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_8b_highsunonly_cal") # source directory
tif_file_list <- list.files(pattern = glob2rx("nyc_planet_composite_8band*nyccal20220710ref*"))
# this has a hard coded substring range, make sure to check this
tif_file_list_datestrings <- strsplit(tif_file_list, "[.]") %>% lapply('[[', 1) %>% stringr::str_sub(start = -26, end = -19)
tif_file_list_dates <- ymd(tif_file_list_datestrings)

sel_tif_date_range <- interval(ymd("20160101"), ymd("20251231")) # expanded date range to capture everything
sel_tif_inds <- which(tif_file_list_dates %within% sel_tif_date_range)
sel_tif_images <- tif_file_list[sel_tif_inds]
sel_tif_images_datestrings <- tif_file_list_datestrings[sel_tif_inds]
sel_tif_images_dates <- tif_file_list_dates[sel_tif_inds]

calcNDVI <- function(NIR, red){
  return((NIR - red)/(NIR + red))
}

extractTreesPointByObjectIDRange <- function(i, sel_tif_images, tree_point_sf, set_num){
  sel_tif_rast <- rast(sel_tif_images[i]) # sel_tif_images[i] # could also check if image is within borough
  extracted_trees <- terra::extract(sel_tif_rast, tree_point_sf) # need to use terra::extract for points
  colnames(extracted_trees)[1] <- "Object_ID" # the original IDs for terra::extract() are just the row numbers
  extracted_trees$Object_ID <- tree_point_sf$Object_ID # so these need to get appended. This works fine when it's doing everything at once, but not with unique IDs
  extracted_trees2 <- extracted_trees[which(complete.cases(extracted_trees)),]
  write.csv(extracted_trees2, paste0(top_output_dir, "/tree_outputs_point/nyc_trees_objset", as.character(set_num), "_", as.character(sel_tif_images_datestrings[i]), "_nyccal20220710ref_point.csv"), row.names = FALSE)
  # clear files
  rm(sel_tif_rast)
  rm(extracted_trees)
  rm(extracted_trees2)
  gc()
  tmpFiles(remove=TRUE)
}
# Note for Conor: alt methods of extraction in older versions scripts (mean, ndvi75)

# Need to start a multisession here
plan(multisession, workers = 8)

# To do by tree object_id index range, loop over this and subset the tnc_gdb_polys_reproj each time

start_time <- Sys.time()
# i is the set number, tnc_gdb_polys_reproj_sub is the subset of the larger polygon file set
# Set up ranges for rows (of trees!) to loop over
rowstart <- c(1, 300001, 600001, 900001, 1200001, 1500001, 1800001)
rowend <- c(300000, 600000, 900000, 1200000, 1500000, 1800000, nrow(tnc_gdb_points_reproj))
#i <- 1 # for loop
for (i in 1:length(rowstart)){
#for (i in 1:3){
  tnc_gdb_points_reproj_sub <- tnc_gdb_points_reproj[rowstart[i]:rowend[i],]
  future_lapply(1:length(sel_tif_images), FUN = extractTreesPointByObjectIDRange, sel_tif_images, tnc_gdb_points_reproj_sub, i, future.seed = TRUE)
}
stop_time <- Sys.time()
stop_time - start_time