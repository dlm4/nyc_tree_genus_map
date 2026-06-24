# Create DTM for all of NYC
# 1.64 ft spatial resolution (0.5 m, approx)

library(tidyverse)
library(sf)
library(lidR)
library(future)
library(terra) # for rasters
# Note: terra masks from lidR: area, crs, crs <-, is.empty, watershed

pts_classes <- c(LASGROUND, LASLOWVEGETATION, LASMEDIUMVEGETATION, LASHIGHVEGETATION, LASBUILDING,
                 LASWATER, LASRAIL, LASROADSURFACE, LASWIREGUARD, LASWIRECONDUCTOR, LASTRANSMISSIONTOWER,
                 LASBRIGDE) # add to readLAS filter instead of filterPOI, bridge typo is in package

filter_string <- paste("-drop_z_below 0 -keep_class", paste(as.character(sort(pts_classes)), collapse = " "))

#las <- readLAS(test_files, filter = filter_string)

grid_size <- 1.64 # ft approx 0.5 # half meter chm

# make DTM
# las catalog
setwd("/Volumes/DLM_backup/lidar_2021/NYC_2021/")
file_list <- list.files(pattern = glob2rx("*.las"))

test_files <- file_list # reassign

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
    return(output) 
  }
}

terrain_raster_files <- catalog_apply(ctg, rasterizeTerrain, grid_size)
# Note: nearest neighbor interpolation warnings