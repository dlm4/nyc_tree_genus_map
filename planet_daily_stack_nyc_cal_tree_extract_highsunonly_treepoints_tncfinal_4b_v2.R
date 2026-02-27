library(terra)
library(lubridate)
library(sf)
library(exactextractr)
library(reshape2)
#library(dplyr)
library(future)
library(future.apply)

#borough_name <- "Bronx"

test_rast <- rast("/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal/nyc_planet_composite_4band_20220710_nyccal20220710ref.tif")
# use this for reprojecting
plotRGB(test_rast, r = 4, g = 3, b = 2, stretch = "lin")

#tree_poly_path_full <- "/Volumes/NYC_geo/tree_polygons/TNC/DRAFT_CONFIDENTIAL_TreeCanopy2021NYC.gdb" # Note this is the OLD beta dataset
tree_poly_path_full <- "/Volumes/NYC_geo/tree_polygons/tnc_2021/Trees_Centroids_Crown_Objects_2021.gdb" # Note this is the FINAL TNC dataset
tnc_gdb_polys <- st_read(tree_poly_path_full, layer = "treeobjects_2021_nyc")
tnc_gdb_polys$Object_ID <- seq(1,nrow(tnc_gdb_polys)) # so there is a unique identifier for each tree # this is OK but in future use # fid_column_name = "OBJECTID"
tnc_gdb_polys_reproj <- st_transform(tnc_gdb_polys, crs(test_rast)) # this takes about a minute fyi

# subset area to Bronx in Planet projection, doesn't need to happen for the entire city
# should subset this down to the area of the Bronx first - NO NEED TO SUBSET TO BRONX
# boroughs_sf <- st_read("/Volumes/NYC_geo/vectors/Borough Boundaries/geo_export_da133389-a6c6-45c3-a980-14295f0e4c2f.shp")
# boro_sf <- boroughs_sf[which(boroughs_sf$boro_name == borough_name),]
# boro_sf_reproj <- st_transform(boro_sf, crs(test_rast))
# tnc_gdb_polys_reproj_boro <- st_intersection(tnc_gdb_polys_reproj, boro_sf_reproj) # subset to just the boro

# Tree extraction for entire city is almost too big to do with single files in multicore, so break it up into more manageable pieces (7 sections)

# change to just centroids - for points
tnc_gdb_points_reproj <- st_centroid(tnc_gdb_polys_reproj)

top_output_dir <- "/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal_extract" # no trailing "/" 

# HERE WE GO
setwd("/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal") # source directory
tif_file_list <- list.files(pattern = glob2rx("nyc_planet_composite_4band*nyccal20220710ref*"))
# this has a hard coded substring range, make sure to check this
tif_file_list_datestrings <- strsplit(tif_file_list, "[.]") %>% lapply('[[', 1) %>% stringr::str_sub(start = -26, end = -19)
tif_file_list_dates <- ymd(tif_file_list_datestrings)

# Will need to do 20180101 to 20231231
#sel_tif_date_range <- interval(ymd("20180101"), ymd("20231231")) # choose date range using Interval class, could change this setup to be within a month(). This should be everything here.
sel_tif_date_range <- interval(ymd("20160101"), ymd("20251231")) # expanded date range to capture everything
sel_tif_inds <- which(tif_file_list_dates %within% sel_tif_date_range)
sel_tif_images <- tif_file_list[sel_tif_inds]
sel_tif_images_datestrings <- tif_file_list_datestrings[sel_tif_inds]
sel_tif_images_dates <- tif_file_list_dates[sel_tif_inds]

calcNDVI <- function(NIR, red){
  return((NIR - red)/(NIR + red))
}

# sel_tif_rast <- rast(sel_tif_images[1])
# extracted_trees <- terra::extract(sel_tif_rast, tnc_gdb_points_reproj_boro)
# colnames(extracted_trees)[1] <- "Object_ID"
# extracted_trees$Object_ID <- as.integer(extracted_trees$Object_ID)
# extracted_trees_poly <- exactextractr::exact_extract(sel_tif_rast, tnc_gdb_polys_reproj_boro, fun = "mean", append_cols = c("Object_ID"), progress = FALSE)

# # NDVI 75% for each tree
# sel_tif_rast <- rast(sel_tif_images[1])
# sel_tif_rast_ndvi <- calcNDVI(sel_tif_rast$nir, sel_tif_rast$red)
# extracted_trees <- exactextractr::exact_extract(sel_tif_rast_ndvi, tnc_gdb_polys_reproj_boro, fun = "quantile", quantiles = c(0.75), append_cols = c("Object_ID"), progress = FALSE) # tnc_gdb_polys_reproj_boro

extractTrees <- function(i, sel_tif_images, tree_poly_sf){
  sel_tif_rast <- rast(sel_tif_images[i]) # sel_tif_images[i] # could also check if image is within borough
  extracted_trees <- exactextractr::exact_extract(sel_tif_rast, tree_poly_sf, fun = "mean", append_cols = c("Object_ID"), progress = FALSE) # tnc_gdb_polys_reproj
  extracted_trees2 <- extracted_trees[which(complete.cases(extracted_trees)),] # screen out polygons that have NAs for reducing the size of the output files
  # there's a dplyr way to do this with mutate but doing it this way just to be safe
  extracted_trees2$mean.blue <- round(extracted_trees2$mean.blue, digits = 0)
  extracted_trees2$mean.green <- round(extracted_trees2$mean.green, digits = 0)
  extracted_trees2$mean.red <- round(extracted_trees2$mean.blue, digits = 0)
  extracted_trees2$mean.nir <- round(extracted_trees2$mean.blue, digits = 0)
  write.csv(extracted_trees2, paste0(top_output_dir, "/tree_outputs_mean/nyc_trees_", as.character(sel_tif_images_datestrings[i]), "_nyccal20220710ref_mean.csv"), row.names = FALSE)
  # clear files
  rm(sel_tif_rast)
  rm(extracted_trees)
  rm(extracted_trees2)
  gc()
  tmpFiles(remove=TRUE)
}

# send a subset of tree_poly_sf over a range of tree object IDs that can looped in sets, with names appended as "set num", each roughly Bronx sized 300000 objects
extractTreesByObjectIDRange <- function(i, sel_tif_images, tree_poly_sf, set_num){
  sel_tif_rast <- rast(sel_tif_images[i]) # sel_tif_images[i] # could also check if image is within borough
  extracted_trees <- exactextractr::exact_extract(sel_tif_rast, tree_poly_sf, fun = "mean", append_cols = c("Object_ID"), progress = FALSE) # tnc_gdb_polys_reproj
  extracted_trees2 <- extracted_trees[which(complete.cases(extracted_trees)),] # screen out polygons that have NAs for reducing the size of the output files
  # there's a dplyr way to do this with mutate but doing it this way just to be safe
  extracted_trees2$mean.blue <- round(extracted_trees2$mean.blue, digits = 0)
  extracted_trees2$mean.green <- round(extracted_trees2$mean.green, digits = 0)
  extracted_trees2$mean.red <- round(extracted_trees2$mean.red, digits = 0)
  extracted_trees2$mean.nir <- round(extracted_trees2$mean.nir, digits = 0)
  write.csv(extracted_trees2, paste0(top_output_dir, "/tree_outputs_mean/nyc_trees_objset", as.character(set_num), "_", as.character(sel_tif_images_datestrings[i]), "_nyccal20220710ref_mean.csv"), row.names = FALSE)
  # clear files
  rm(sel_tif_rast)
  rm(extracted_trees)
  rm(extracted_trees2)
  gc()
  tmpFiles(remove=TRUE)
}

extractTreesNDVI75 <- function(i, sel_tif_images, tree_poly_sf){
  sel_tif_rast <- rast(sel_tif_images[i]) # sel_tif_images[i] # could also check if image is within borough
  sel_tif_rast_ndvi <- calcNDVI(sel_tif_rast$nir, sel_tif_rast$red)
  extracted_trees <- exactextractr::exact_extract(sel_tif_rast_ndvi, tree_poly_sf, fun = "quantile", quantiles = c(0.75), append_cols = c("Object_ID"), progress = FALSE) # tnc_gdb_polys_reproj
  extracted_trees2 <- extracted_trees[which(complete.cases(extracted_trees)),] # screen out polygons that have NAs for reducing the size of the output files
  extracted_trees2$q75 <- round(extracted_trees2$q75*10000, digits = 0) # multiply by 10000 and round the values
  write.csv(extracted_trees2, paste0(top_output_dir, "/tree_outputs_ndvi75/nyc_trees_", as.character(sel_tif_images_datestrings[i]), "_nyccal20220710ref_ndvi75.csv"), row.names = FALSE)
  # clear files
  rm(sel_tif_rast)
  rm(sel_tif_rast_ndvi)
  rm(extracted_trees)
  rm(extracted_trees2)
  gc()
  tmpFiles(remove=TRUE)
}

extractTreesNDVI75ByObjectIDRange <- function(i, sel_tif_images, tree_poly_sf, set_num){
  sel_tif_rast <- rast(sel_tif_images[i]) # sel_tif_images[i] # could also check if image is within borough
  sel_tif_rast_ndvi <- calcNDVI(sel_tif_rast$nir, sel_tif_rast$red)
  extracted_trees <- exactextractr::exact_extract(sel_tif_rast_ndvi, tree_poly_sf, fun = "quantile", quantiles = c(0.75), append_cols = c("Object_ID"), progress = FALSE) # tnc_gdb_polys_reproj
  extracted_trees2 <- extracted_trees[which(complete.cases(extracted_trees)),] # screen out polygons that have NAs for reducing the size of the output files
  extracted_trees2$q75 <- round(extracted_trees2$q75*10000, digits = 0) # multiply by 10000 and round the values
  write.csv(extracted_trees2, paste0(top_output_dir, "/tree_outputs_ndvi75/nyc_trees_objset", as.character(set_num), "_", as.character(sel_tif_images_datestrings[i]), "_nyccal20220710ref_ndvi75.csv"), row.names = FALSE)
  # clear files
  rm(sel_tif_rast)
  rm(sel_tif_rast_ndvi)
  rm(extracted_trees)
  rm(extracted_trees2)
  gc()
  tmpFiles(remove=TRUE)
}

extractTreesPoint <- function(i, sel_tif_images, tree_point_sf){
  sel_tif_rast <- rast(sel_tif_images[i]) # sel_tif_images[i] # could also check if image is within borough
  extracted_trees <- terra::extract(sel_tif_rast, tree_point_sf) # need to use terra::extract for points
  colnames(extracted_trees)[1] <- "Object_ID"
  extracted_trees$Object_ID <- as.integer(extracted_trees$Object_ID)
  extracted_trees2 <- extracted_trees[which(complete.cases(extracted_trees)),] # screen out polygons that have NAs for reducing the size of the output files
  # how big was this without complete.cases? 31 GB. With complete.cases? 19 GB
  write.csv(extracted_trees2, paste0(top_output_dir, "/tree_outputs_point/nyc_trees_", as.character(sel_tif_images_datestrings[i]), "_nyccal20220710ref_point.csv"), row.names = FALSE)
  # clear files
  rm(sel_tif_rast)
  rm(extracted_trees)
  rm(extracted_trees2)
  gc()
  tmpFiles(remove=TRUE)
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

# Need to start a multisession here
plan(multisession, workers = 8) # can increase this again, seems ok for mean calculation when full set is cut up
# doing 8 workers instead of 10 to not push the memory so hard

# DONE
# start_time <- Sys.time()
# future_lapply(1:length(sel_tif_images), FUN = extractTreesPoint, sel_tif_images, tnc_gdb_points_reproj, future.seed = TRUE)
# stop_time <- Sys.time()
# stop_time - start_time

# OLD method, can't do this for whole city, need to break into pieces
# start_time <- Sys.time()
# #future_lapply(1:length(sel_tif_images), FUN = extractTrees, sel_tif_images, tnc_gdb_polys_reproj_boro, future.seed = TRUE)
# future_lapply(1:length(sel_tif_images), FUN = extractTrees, sel_tif_images, tnc_gdb_polys_reproj, future.seed = TRUE)
# stop_time <- Sys.time()
# stop_time - start_time

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


#i <- 1 # for loop
#for (i in 1:length(rowstart)){
for (i in 1:length(rowstart)){
  tnc_gdb_polys_reproj_sub <- tnc_gdb_polys_reproj[rowstart[i]:rowend[i],]
  future_lapply(1:length(sel_tif_images), FUN = extractTreesByObjectIDRange, sel_tif_images, tnc_gdb_polys_reproj_sub, i, future.seed = TRUE)
}
stop_time <- Sys.time()
stop_time - start_time
# can combine by objset files into single file per date after the fact if desired

# NDVI 75
# start_time <- Sys.time()
# future_lapply(1:length(sel_tif_images), FUN = extractTreesNDVI75, sel_tif_images, tnc_gdb_polys_reproj, future.seed = TRUE)
# stop_time <- Sys.time()

start_time <- Sys.time()
for (i in 1:length(rowstart)){
  tnc_gdb_polys_reproj_sub <- tnc_gdb_polys_reproj[rowstart[i]:rowend[i],]
  future_lapply(1:length(sel_tif_images), FUN = extractTreesNDVI75ByObjectIDRange, sel_tif_images, tnc_gdb_polys_reproj_sub, i, future.seed = TRUE)
}
stop_time <- Sys.time()
stop_time - start_time