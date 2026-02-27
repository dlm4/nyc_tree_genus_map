library(tidyverse)
library(jsonlite)
library(sf)
library(raster)

# Planet metadata filtering

# Load all metadata JSON files
# Doing all 8b files that had max 10% cloud

# Keep only files with:
#   > 20 solar elevation angle
#   Only best image per day for a given location (based on highest solar angle because clear is sometimes missing in the json)

setwd("/Volumes/NYC_geo/Planet/raw_images/time_series")

all_json_files <- list.files(pattern = glob2rx("*metadata.json"), recursive = TRUE)
subset_inds <- str_detect(all_json_files, "4b/files/")
json_files <- all_json_files[subset_inds]
rm(all_json_files, subset_inds)

i <- 1

#json <- fromJSON(json_files[1])

for (i in 1:length(json_files)){
  print(i)
  geojson <- read_sf(json_files[i]) # try this with geojson files
  if (i == 1){
    all_geojson <- geojson
  } else {
    all_geojson <- bind_rows(all_geojson, geojson)
  }
}

# filtering step
clear_imgs <- which(all_geojson$sun_elevation > 20) # swapping this out to just high sun
#clear_imgs <- which(all_geojson$clear_percent > 90 & all_geojson$sun_elevation > 20)
# Many images are missing clear flag metadata so may unnecessarily remove >400 of images

clear_geojson <- all_geojson[clear_imgs,] # substitute this in for the other json file metadata cleanup

# to keep best image, use terra::merge() (keeps first image) instead of terra::mosaic() (averages)
# before using terra::merge, for each day, sort based on sum of clear percent and solar elevation angle, highest values go on top
# Do the cloud masking first and then this should fill in the holes with anything that's underneath if using na.rm()

# modified from planet_daily_stack_bronx_maxcloud10
#####

# Get list of all Planet images
# Load in all Planet json files into single file
# Loop through dates (starting 1 Jan 2018)
# If no images on a date, do nothing
# If there are images on a date, 
# Get list of all images on that date (4B or 8B)
# for each image:
# Load image and udm2
# Mask to keep only clear cloud-free pixels
# Mosaic (merge) and average overlapping areas to generate single big image
# output single big image

# Later:
# Load all images for a time range (eg 2018)
# Stack everything together into one superstack image
# Output this single superstack image

# Extract tree polygons within Bronx for time range from the superstack image

library(terra)
library(lubridate)
library(reshape2)

img_dir <- "/Volumes/NYC_geo/Planet/raw_images/time_series"
img_vect_sub_stack <- vect(clear_geojson) 

# date range
date_range <- seq(ymd("20160101"), ymd("20171231"), "days") # seq(ymd("20180101"), ymd("20231231"), "days")
img_vect_sub_stack$date <- ymd(substr(img_vect_sub_stack$id, 1, 8))

#NOTE: terra files up temporary /tmp files on the main hard disk as it's processing. These get wiped out computer restart.
# But for a long processing scheme, can potentially fill up hundreds of GB. Can set this /tmp directory to be elsewhere if needed.

for (ind in 1:length(date_range)){
  obs_date <- date_range[ind]
  print(as.character(obs_date))
  
  # Get date inds and reorder based on sun elevation ONLY - best lit images on top
  date_inds <- which(img_vect_sub_stack$date == obs_date)
  qual_vals <- img_vect_sub_stack$sun_elevation[date_inds]
  date_inds <- date_inds[order(qual_vals, decreasing = TRUE)] # reordered so the images with the highest sun elevation go on top, ties go to earlier image
  
  if (length(date_inds) > 0){
    #View(as.data.frame(img_vect_sub_stack[date_inds,]))
    print(paste0("Working on: ", as.character(obs_date)))
    
    img_ids <- unique(img_vect_sub_stack$id[date_inds])
    img_ids_sr <- paste0(img_ids, "_3B_AnalyticMS_SR_harmonized_clip.tif")
    #img_ids_sr <- paste0(img_ids, "_3B_AnalyticMS_SR_8b_harmonized_clip.tif") # 8 band
    img_ids_udm2 <- paste0(img_ids, "_3B_udm2_clip.tif")
    
    first_append_ind <- TRUE
    for (i in 1:length(img_ids)){
      print(paste0("Masking image number: ", i, " of ", length(img_ids)))
      #i <- 2
      
      img_id_sr <- list.files(img_dir, pattern = glob2rx(img_ids_sr[i]), recursive = TRUE, full.names = TRUE)
      img_id_udm2 <- list.files(img_dir, pattern = glob2rx(img_ids_udm2[i]), recursive = TRUE, full.names = TRUE)
      
      # if this is true that means we have two of the same file, just use the first one.
      if (length(img_id_sr) > 1 | length(img_id_udm2 > 1)){
        img_id_sr <- img_id_sr[1]
        img_id_udm2 <- img_id_udm2[1]
      }
      
      # check if file is missing and skip if needed
      if (length(img_id_sr) == 0 & length(img_id_udm2) == 0){
        print(paste0("Missing sr and udm2: ", img_ids[i]))
      } else if (length(img_id_sr) == 0){
        print(paste0("Missing sr: ", img_ids[i]))
      } else if (length(img_id_udm2) == 0){
        print(paste0("Missing udm2: ", img_ids[i]))
      } else {
        img_id_sr_rast <- rast(img_id_sr)
        img_id_udm2_rast <- rast(img_id_udm2)
        
        img_id_sr_rast[img_id_udm2_rast$clear == 0] <- NA # mask to keep clear pixels only, clear is a binary 1 or 0 mask
        
        if (first_append_ind == TRUE){
          img_date_sr_sprc <- sprc(img_id_sr_rast)
          first_append_ind <- FALSE
        } else {
          add(img_date_sr_sprc) <- img_id_sr_rast
        }
      }
    }
    
    print(paste0("Mosaicking and outputting: ", as.character(obs_date)))
    
    # SWAP mosaic out to use terra::merge(), but with order from sprc in priority of greatest sun_elevation (earlier in date inds)
    img_date_sr_mosaic <- merge(img_date_sr_sprc, first = TRUE, na.rm = TRUE) # mosaic(img_date_sr_sprc)
    
    output_dir <- "/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly/" # export to this new directory
    output_filename <- paste0(output_dir, "nyc_planet_composite_4band_", gsub("-", "", as.character(obs_date)), ".tif")
    #writeRaster(img_date_sr_mosaic, filename = output_filename)
    
    # reassign to fix data format
    img_date_sr_mosaic[img_date_sr_mosaic > 32767] <- 32767
    img_date_sr_mosaic[img_date_sr_mosaic < -32768] <- -32768
    
    writeRaster(img_date_sr_mosaic, filename = output_filename, datatype = "INT2S", overwrite = TRUE)
    
    print(paste0("Written out raster: ", output_filename))
    
    # Cleanup
    
    # Clear memory
    rm(img_id_sr)
    rm(img_id_udm2)
    rm(img_date_sr_mosaic)
    gc()
    
    # Remove any leftover temp files
    tmpFiles(remove = TRUE)
  }
}
