# Create DTM for all of NYC
# 1.64 ft spatial resolution (0.5 m, approx)

library(tidyverse)
library(sf)
library(lidR)
library(future)
library(terra) # for rasters
# Note: terra masks from lidR: area, crs, crs <-, is.empty, watershed

#test_ids <-  "5240" # "2242"
# test_ids <- c("2242", "5242", "7242",
#               "2240", "5240", "7240",
#               "2237", "5237", "7237")
#test_files <- paste("/Volumes/DLM_backup/lidar_2021/NYC_2021/", test_ids, ".las", sep="") # will want to move this back over to SSD later

pts_classes <- c(LASGROUND, LASLOWVEGETATION, LASMEDIUMVEGETATION, LASHIGHVEGETATION, LASBUILDING,
                 LASWATER, LASRAIL, LASROADSURFACE, LASWIREGUARD, LASWIRECONDUCTOR, LASTRANSMISSIONTOWER,
                 LASBRIGDE) # add to readLAS filter instead of filterPOI, bridge typo is in package

#pts_classes <- c(LASGROUND, LASLOWVEGETATION, LASMEDIUMVEGETATION, LASHIGHVEGETATION, LASWATER, LASRAIL, LASROADSURFACE) # add to readLAS filter instead of filterPOI, bridge typo is in package

filter_string <- paste("-drop_z_below 0 -keep_class", paste(as.character(sort(pts_classes)), collapse = " "))

#las <- readLAS(test_files, filter = filter_string)

grid_size <- 1.64 # ft approx 0.5 # half meter chm

# make DTM
#dtm <- rasterize_terrain(las, grid_size, tin())

# las catalog
setwd("/Volumes/DLM_backup/lidar_2021/NYC_2021/")
file_list <- list.files(pattern = glob2rx("*.las"))

# test_ids <- c("2242", "5242", "7242",
#               "2240", "5240", "7240",
#               "2237", "5237", "7237")
#test_ids <- file_list
#test_files <- paste("/Volumes/DLM_backup/lidar_2021/NYC_2021/", test_ids[1:30], ".las", sep="") # will want to move this back over to SSD later

test_files <- file_list

ctg <- readLAScatalog(test_files, filter = filter_string)

plan(multisession, workers = 8) # don't use all the cores, easier on memory
temp_output_path <- "/Volumes/NYC_geo/processing_temporary/" # do this on the external disk

# set up new run for terrain, rpf is old naming convention
#ctg_rpf <- readLAScatalog(unlist(ctg))
opt_output_files(ctg) <- paste(temp_output_path, "terrain_raster_1p64ft_{ORIGINALFILENAME}", sep = "")
opt_chunk_buffer(ctg) <- 328 # units in feet, approx 100 m

grid_size <- 1.64 # 0.5 # in feet instead of meters

rasterizeTerrain <- function(chunk, grid_size) {
  # Load the chunk + buffer
  las <- readLAS(chunk)
  if (lidR::is.empty(las)) return(NULL)
  if (any(las$Classification == 2)){ # test for requirement that there be some bare ground (LASGROUND = 2)
    # do something
    output <- rasterize_terrain(chunk, grid_size, tin())

    # remove the buffer of the output
    output <- crop(output, st_bbox(chunk)) #crop for spatraster
    return(output) # do we want to temporarily write out the output instead and retain as a las catalog
  }
}

terrain_raster_files <- catalog_apply(ctg, rasterizeTerrain, grid_size)
# Note: nearest neighbor interpolation warnings
# 
# #####
# # Will write this out
# terrain_raster_sprc <- sprc(unlist(terrain_raster_files))
# terrain_raster <- merge(terrain_raster_sprc) # merge crashed, file too big
# # write out all here
# 
# setwd("/Volumes/NYC_geo/tree_classification")
# writeRaster(terrain_raster, "nyc_dtm_2021_1p64ft.tif")
# 
# #####
# # Try doing to raster merge in pieces instead
# start_vals <- seq(1, 1601, 100)
# end_vals <- c(seq(100, 1600, 100), 1702)
# for (i in 1:length(start_vals)){
#   start_val <- start_vals[i]
#   end_val <- end_vals[i]
#   print(start_val)
#   terrain_raster_sprc_sub <- terrain_raster_sprc[start_val:end_val]
#   terrain_raster_sub <- merge(terrain_raster_sprc_sub)
#   setwd("/Volumes/NYC_geo/tree_classification")
#   writeRaster(terrain_raster_sub, paste0("nyc_dtm_2021_1p64ft_", start_val, "_", end_val, ".tif"))
# }
# 
# #####
# # Try doing it by borough
# boros <- vect('/Volumes/NYC_geo/vectors/Borough Boundaries/geo_export_da133389-a6c6-45c3-a980-14295f0e4c2f.shp')
# lidar_tiles <- vect('/Volumes/DLM_backup/lidar_2021/NYC_2021/NYC2021_LAS_Index.shp')
# boros <- project(boros, crs(lidar_tiles))
# 
# setwd("/Volumes/NYC_geo/processing_temporary")
# terrain_raster_tile_list <- list.files(pattern = glob2rx("terrain_raster_1p64ft_*.tif"))
# tile_ids <- sapply(terrain_raster_tile_list, function(x) unlist(strsplit(substr(x, 23, 500), "[.]"))[1])
# 
# i <- 4
# lidar_tiles_sub <- intersect(boros[i], lidar_tiles)
# lidar_tiles_sub_df <- as.data.frame(lidar_tiles_sub)
# 
# tile_id_sub_inds <- which(tile_ids %in% lidar_tiles_sub_df$LAS_ID)
# terrain_raster_tile_list_sub <- terrain_raster_tile_list[tile_id_sub_inds]
# 
# setwd("/Volumes/NYC_geo/processing_temporary")
# terrain_raster_sprc_sub <- sprc(terrain_raster_tile_list_sub)
# 
# setwd("/Volumes/NYC_geo/tree_classification")
# output_name <- paste0("nyc_dtm_2021_1p64ft_", boros$boro_name[i], ".tif")
# merge(terrain_raster_sprc_sub, filename = output_name)
# 
# rast_test <- rast(output_name)
# plot(rast_test)
# #writeRaster(terrain_raster_sub, paste0("nyc_dtm_2021_1p64ft_", boros$boro_name[i], ".tif"))
# This worked with Manhattan (4.7 gb), worked with Staten Island (18 gb), and broke with Bronx
# output file is too big with floating point (and we want to keep that precision with LAS subtraction)
# So the answer must be doing subtraction on the fly as we walk across the city with the tiles
# 