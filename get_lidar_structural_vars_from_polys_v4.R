# Calculate structural variables from lidar for tree crowns
# Set of structural variables, mostly all selected variables from Alonzo et al 2014

# Y: Max crown height, see what we get as a sanity check to the TNC data, good to have anyway
# Y: Median height of returns in crown
# Y: Crown width at median height of returns in crown
# Y: Ratio of crown height to width: median height
# Y: Average intensity below median height
# N: Crown surface intensity: 0.5* m spatial resolution (doing it a little coarser because we have lower points/m2)
# N: Surface heights (0.5* m)/surface heights (1 m)
# Y: Count of returns in 0.5 m vertical slice at 90th percentile divided by width at that height

# npts: Number of points
# height_max_ft: maximum height (in feet)
# height_med_ft: median height (in feet)
# int_mean: mean intensity (unitless return intensity value)
# int_mean_above_medh: mean intensity above the median height (unitless return intensity value)
# int_mean_below_medh: mean intensity below the median height (unitless return intensity value)
# wid_medh_max_ft: crown width at median height, with width calculated as maximum distance between x,y points within +/- 1.64 ft (0.5 m) of median height (in feet)
# wid_medh_circlerad_ft: crown width at median height, with width calculated as radius of circle with equal area to convex hull of x,y points within +/- 1.64 ft (0.5 m) of median height (in feet)
# hw_rat_2_max: ratio of median height to width at the median height, with width calculated as maximum distance between x,y points within +/- 1.64 ft (0.5 m) of median height (unitless ratio)
# hw_rat_2_circlerad: ratio of median height to width at the median height, with width calculated as radius of circle with equal area to convex hull of x,y points within +/- 0.82 ft (0.25 m) of 90th percentile height (unitless ratio)
# cp_3_max: Count of returns in 0.5 m vertical slice (+/- 0.82 ft) at 90th percentile height divided by width at that height, with width calculated as maximum distance between x,y points within +/- 0.82 ft (0.25 m) of median height (units 1 / feet)
# cp_3_circle_rad: Count of returns in 0.5 m vertical slice (+/- 0.82 ft) at 90th percentile height divided by width at that height, with width calculated as radius of circle with equal area to convex hull of x,y points within +/- 0.82 ft (0.25 m) of median height (units 1 /feet)


#####

library(tidyverse)
library(sf)
library(lidR)
library(future)
library(terra) # for rasters
# Note! : terra masks from lidR, need to call with lidR:: area, crs, crs <-, is.empty, watershed

#####
# LAS Metrics functions
# could improve efficiency to reduce duplicated calculations

# Average intensity above median height
fIntHigh <- function(z,i){
  med_height <- median(z)
  hi_inds <- which(z > med_height)
  return(mean(i[hi_inds]))
}

# Average intensity below median height
fIntLow <- function(z,i){
  med_height <- median(z)
  low_inds <- which(z < med_height)
  return(mean(i[low_inds]))
}

# Crown width at median height of returns in crown
# Implemented as get median, get all points +/- 1.64 ft (1 m bounds)
# then take max of x,y distances between these points, call this the width
fW1widthmax <- function(x,y,z){
  med_height <- median(z)
  ind_at_med <- which(z < med_height+1.64 & z > med_height-1.64)
  xy <- cbind(x[ind_at_med], y[ind_at_med])
  dist_xy <- dist(xy)
  return(max(dist_xy))
}

# Alternative version of the crown width
# create a convex hull of the points at the median height range
# calculate area, then calculate diameter of this area if it was a circle
fW1widthcircle <- function(x,y,z){
  med_height <- median(z)
  ind_at_med <- which(z < med_height+1.64 & z > med_height-1.64)
  xy <- cbind(x[ind_at_med], y[ind_at_med])
  xy_area <- st_area(st_convex_hull(st_multipoint(xy)))
  return(2*sqrt(xy_area/pi))
}

# Ratio of crown height to width at median height, max implementation
fHWrat2max <- function(x,y,z){
  med_height <- median(z)
  ind_at_med <- which(z < med_height+1.64 & z > med_height-1.64)
  xy <- cbind(x[ind_at_med], y[ind_at_med])
  dist_xy <- dist(xy)
  return(max(dist_xy))
}

# Ratio of crown height to width at median height, circle implementation
fHWrat2circle <- function(x,y,z){
  med_height <- median(z)
  ind_at_med <- which(z < med_height+1.64 & z > med_height-1.64)
  xy <- cbind(x[ind_at_med], y[ind_at_med])
  xy_area <- st_area(st_convex_hull(st_multipoint(xy)))
  med_width <- 2*sqrt(xy_area/pi)
  return(med_height/med_width)
}

# Count of returns in 0.5 m vertical slice at 90th percentile height divide by width at that height
fCP3circle <- function(x, y, z){
  q90_height <- unname(quantile(z, 0.9, na.rm = TRUE))
  if (is.na(q90_height)){
    return(NA)
  } else{
    ind_at_q90 <- which(z < q90_height+0.82 & z > q90_height-0.82) # half meter window
    slice_count <- length(ind_at_q90)
    
    xy <- cbind(x[ind_at_q90], y[ind_at_q90])
    xy_area <- st_area(st_convex_hull(st_multipoint(xy)))
    wid <- 2*sqrt(xy_area/pi)
    
    return(slice_count/wid)
  }
}

fCP3max <- function(x, y, z){
  q90_height <- unname(quantile(z, 0.9, na.rm = TRUE))
  if (is.na(q90_height)){
    return(NA)
  } else{
    ind_at_q90 <- which(z < q90_height+0.82 & z > q90_height-0.82) # half meter window
    slice_count <- length(ind_at_q90)
    
    xy <- cbind(x[ind_at_q90], y[ind_at_q90])
    dist_xy <- dist(xy)
    wid <- max(dist_xy)
    
    return(slice_count/wid)
  }
}

# Wrapper for all structural metric functions
calcMetrics <- function(x, y, z, i){
  list(npts = length(z),
       height_max_ft = max(z),
       height_med_ft = median(z),
       int_mean = mean(i),
       int_mean_above_medh = fIntHigh(z,i),
       int_mean_below_medh = fIntLow(z,i),
       wid_medh_max_ft = fW1widthmax(x,y,z),
       wid_medh_circlerad_ft = fW1widthcircle(x,y,z),
       hw_rat_2_max = fHWrat2max(x,y,z),
       hw_rat_2_circlerad = fHWrat2circle(x,y,z),
       cp_3_max = fCP3max(x,y,z),
       cp_3_circlerad = fCP3circle(x,y,z))
}
# add number of points (likely a length or nrow call)

#####

# Load list of output lidar DTM tiles
setwd("/Volumes/NYC_geo/nyc_lidar_metrics/terrain_rasters")
terrain_raster_tile_list <- list.files(pattern = glob2rx("terrain_raster_1p64ft_*.tif"))
tile_ids <- sapply(terrain_raster_tile_list, function(x) unlist(strsplit(substr(x, 23, 500), "[.]"))[1])

# Load TNC polygons
tree_poly_path_full <- "/Volumes/NYC_geo/tree_polygons/tnc_2021/Trees_Centroids_Crown_Objects_2021.gdb" # Note this is the FINAL TNC dataset
tnc_gdb_polys <- st_read(tree_poly_path_full, layer = "treeobjects_2021_nyc")
tnc_gdb_polys$Poly_ID <- 1:nrow(tnc_gdb_polys)

# Load las tile vector
tile_polys <- read_sf("/Volumes/DLM_backup/lidar_2021/NYC_2021/NYC2021_LAS_Index.shp")
tile_polys <- tile_polys[which(tile_polys$LAS_ID %in% tile_ids),] # Do ID intersect to only work with tiles where a DTM raster could be created

# Single test terrain raster for reprojection, doesn't matter which one
i <- 3
plot(tile_polys[i,][1])
rast_test <- rast(terrain_raster_tile_list[which(tile_ids == tile_polys$LAS_ID[i])])
#plot(rast_test)

# reproject
tnc_gdb_polys_reproj <- st_transform(tnc_gdb_polys, st_crs(rast_test))
tnc_gdb_polys_reproj_centroid <- st_centroid(tnc_gdb_polys_reproj)

#####
# Set new i and loop here
for (i in 101:1702){
  print(i)
  start_time <- Sys.time()
  
  setwd("/Volumes/NYC_geo/nyc_lidar_metrics/terrain_rasters")
  rast_test <- rast(terrain_raster_tile_list[which(tile_ids == tile_polys$LAS_ID[i])])
  rast_tile_extent <- st_as_sf(vect(ext(rast_test)))
  st_crs(rast_tile_extent) <- st_crs(rast_test)
  tnc_gdb_polys_reproj_centroid_sub <- st_intersection(tnc_gdb_polys_reproj_centroid, rast_tile_extent)
  
  if (nrow(tnc_gdb_polys_reproj_centroid_sub) > 0){
    
    # Get all relevant neighboring LAS tiles to deal with trees on edges
    selected_tile <- tile_polys[i,]
    selected_tile_buff <- st_buffer(selected_tile, 328) # 100 m buffer, 328 ft
    tile_set_sub <- st_intersection(tile_polys, selected_tile_buff)
    
    # Get DTM raster
    sel_tile_inds <- which(tile_ids %in% tile_set_sub$LAS_ID)
    terrain_raster_sprc <- sprc(terrain_raster_tile_list[sel_tile_inds])
    terrain_raster_sub <- merge(terrain_raster_sprc) # this is fast at this spatial scale
    
    # Get LAS files
    setwd("/Volumes/DLM_backup/lidar_2021/NYC_2021/")
    las_file_list <- list.files(pattern = glob2rx("*.las"))
    las_ids <- sapply(las_file_list, function(x) unlist(strsplit(x, "[.]"))[1])
    
    sel_las_inds <- which(las_ids %in% tile_ids[sel_tile_inds]) # select on this to only load LAS where a DTM was able to be created
    
    pts_classes <- c(LASLOWVEGETATION, LASMEDIUMVEGETATION, LASHIGHVEGETATION) # load only veg classes!
    filter_string <- paste("-drop_z_below 0 -keep_class", paste(as.character(sort(pts_classes)), collapse = " "))
    
    las <- readLAS(las_file_list[sel_las_inds], filter = filter_string) # this is comparatively slow with it being on spinning disk
    
    # remove ground for veg height
    las_veg_height <- las - terrain_raster_sub
    rm(las)
    
    # Get polygon subset from relevant centroids
    tnc_gdb_polys_reproj_sub <- tnc_gdb_polys_reproj[which(tnc_gdb_polys_reproj$Poly_ID %in% tnc_gdb_polys_reproj_centroid_sub$Poly_ID),]
    
    # Prep LAS file
    las_veg_height_mask <- classify_poi(las_veg_height, as.integer(1), roi = tnc_gdb_polys_reproj_sub, inverse_roi = TRUE)
    las_veg_height_inpoly <- filter_poi(las_veg_height_mask, Classification %in% pts_classes) # this removes all the points that are not within polygons
    # if this is empty, then no tree points within the remaining las, and this won't work
    if (!lidR::is.empty(las_veg_height_inpoly)){
      las_veg_height_inpoly_labeled <- merge_spatial(las_veg_height_inpoly, tnc_gdb_polys_reproj_sub, attribute = "Poly_ID")
      
      # Calculate crown metrics and strip geometry
      tnc_metrics_sub <- crown_metrics(las_veg_height_inpoly_labeled, ~calcMetrics(X, Y, Z, Intensity), attribute = "Poly_ID") %>% st_drop_geometry()
      # Can redo this without droppping geometry if we want to view it as a shapefile (for example)
      
      # Write out metrics
      setwd('/Volumes/NYC_geo/nyc_lidar_metrics/crown_metrics')
      metric_output_filename <- paste0("nyc_crown_metrics_tnc_polys_tile_", tile_polys$LAS_ID[i], ".csv")
      write.csv(tnc_metrics_sub, metric_output_filename, row.names = FALSE)
      
      # remove unneeded files
      rm(las_veg_height)
      rm(las_veg_height_inpoly)
      rm(las_veg_height_inpoly_labeled)
      rm(las_veg_height_mask)
    } else {
      print(paste0("No vegetation points in LAS for tree polygons: tile ", tile_polys$LAS_ID[i]))
    }
    end_time <- Sys.time()
    print(end_time - start_time)
   
  } else {
    print(paste0("No tree polygons found in LAS: tile ", tile_polys$LAS_ID[i]))
  }
  # could include an else statement to write out a little file if no polygons were found in the intersection
}

# Take screenshots of these steps to include in presentation (like for AGU)
# Something pretty

# 3D View, colored by tree factor by angle
# Flythrough would be cool

#####

# Update shapefile with files that that been completed

full_tile_polys <- read_sf("/Volumes/DLM_backup/lidar_2021/NYC_2021/NYC2021_LAS_Index.shp")

setwd('/Volumes/NYC_geo/nyc_lidar_metrics/crown_metrics')
completed_tile_list <- list.files(pattern = glob2rx("nyc_crown_metrics_tnc_polys_tile_*.csv"))
completed_tile_ids <- sapply(completed_tile_list, function(x) unlist(strsplit(substr(x, 34, 500), "[.]"))[1])

full_tile_polys$completed <- "N"
full_tile_polys$completed[which(full_tile_polys$LAS_ID %in% completed_tile_ids)] <- "Y"

st_write(full_tile_polys, "../las_crown_metric_processing_completion4.shp")
