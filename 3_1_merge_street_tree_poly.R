library(tidyverse)
library(data.table)
library(sf)
library(purrr)

# Merging extracted data with the street tree database
# Load geodatabase of all polygon ids
# Load street tree database with unique info
# Merge together street tree ids with the polygons

# TNC polys
tree_poly_path_full <- "/Volumes/NYC_geo/tree_polygons/tnc_2021/Trees_Centroids_Crown_Objects_2021.gdb" # Note this is the FINAL TNC dataset
tnc_gdb_polys <- st_read(tree_poly_path_full, layer = "treeobjects_2021_nyc")
tnc_gdb_polys$Object_ID <- seq(1,nrow(tnc_gdb_polys)) 

# Street tree database
tree_pts <- st_read("/Users/dlm356/dlm356_files/nyc_trees/Forestry Tree Points_20240228/geo_export_3a66edb6-f0e4-44ec-9a82-e8b36349c231.shp")

# this is the needed spatial intersection step
tree_pts_reproj <- st_transform(tree_pts, st_crs(tnc_gdb_polys))
colnames(tnc_gdb_polys)[which(colnames(tnc_gdb_polys) == "Object_ID")] <- "Poly_ID"
colnames(tree_pts_reproj)[which(colnames(tree_pts_reproj) == "objectid")] <- "Point_ID"
tree_intersect <- st_intersection(tnc_gdb_polys, tree_pts_reproj) # intersection step

# which polygons only appear once after intersection, so only 1 stem
poly_ids_ints <- table(tree_intersect$Poly_ID) %>% as.data.frame()
poly_ids_1unique <- poly_ids_ints$Var1[which(poly_ids_ints$Freq == 1)] %>% as.character()

# subset polygon set based on these identified polygons
tnc_gdb_polys_sub2 <- tnc_gdb_polys[which(tnc_gdb_polys$Poly_ID %in% poly_ids_1unique),] # if want polygons

tree_intersect_sub2 <- tree_intersect[which(tree_intersect$Poly_ID %in% poly_ids_1unique),] # if want points

tree_intersect_sub2_reproj <- st_transform(tree_intersect_sub2, crs = "EPSG:4326") # unprojected lat long

lon_lat <- st_coordinates(tree_intersect_sub2_reproj)
colnames(lon_lat) <- c("Lon", "Lat")

tree_intersect_sub2_reproj_lon_lat <- cbind.data.frame(tree_intersect_sub2_reproj, lon_lat)

# Tree species names
front <- sapply(strsplit(tree_intersect_sub2_reproj_lon_lat$genusspeci, " - "), '[', 1)
species <- sapply(strsplit(front, " '"), '[', 1)
species2 <- sapply(strsplit(species, " var. inermis"), '[', 1)
genus <- sapply(strsplit(species, " "), '[', 1)

tree_intersect_sub2_reproj_lon_lat$genus <- genus
species2[which(species2 == "Platanus x acerfolia")] <- "Platanus x acerifolia" # fix typo in original dataset
tree_intersect_sub2_reproj_lon_lat$species <- species2

sub_cols <- c("Poly_ID", "Point_ID", "genusspeci", "genus", "species", "Lon", "Lat",
              "Height", "Radius", "SHAPE_Length", "SHAPE_Area",
              "dbh", "tpstructur", "tpconditio", "riskrating")

output_df <- st_drop_geometry(tree_intersect_sub2_reproj_lon_lat[,sub_cols])
setwd("/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal_extract")
saveRDS(output_df, "tree_pointextract_polyid_all_output_merged.rds")
# update future files downstream of this to use RDS file for data retention
