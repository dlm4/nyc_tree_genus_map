library(tidyverse)
library(data.table)
library(sf)
library(purrr)

# Merging extracted phenologies with the street tree database
# Load geodatabase of all polygon ids
# Load street tree database with unique info
# Merge together street tree ids with the polygons

# TNC polys
tree_poly_path_full <- "/Volumes/NYC_geo/tree_polygons/tnc_2021/Trees_Centroids_Crown_Objects_2021.gdb" # Note this is the FINAL TNC dataset
tnc_gdb_polys <- st_read(tree_poly_path_full, layer = "treeobjects_2021_nyc")
tnc_gdb_polys$Object_ID <- seq(1,nrow(tnc_gdb_polys)) 

#tnc_gdb_polys_sub <- tnc_gdb_polys[which(tnc_gdb_polys$Object_ID %in% seq(1, 300000)),]
#tnc_gdb_polys_sub <- tnc_gdb_polys # swapping this out because we're not subsetting

# Street tree database
tree_pts <- st_read("/Users/dlm356/dlm356_files/nyc_trees/Forestry Tree Points_20240228/geo_export_3a66edb6-f0e4-44ec-9a82-e8b36349c231.shp")

# Read in phenology data
# REMOVE THIS, just do poly and point intersection
#setwd("/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal_extract")
#setwd("/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal_pheno/raw_pheno_extract_springfit")
#pheno_file_list <- list.files(pattern = glob2rx("trees_pheno_output_springfit_objset*point.csv")) # stored as different sets by object id
#pheno_file_all <- purrr::map_df(pheno_file_list, fread, .id = 'object_id_group') # big load

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

#pheno_file_all_sub2 <- pheno_file_all[which(pheno_file_all$Object_ID %in% poly_ids_1unique),]
#tree_pheno_full_info <- merge(tree_intersect_sub2, pheno_file_all_sub2, by.x = "Poly_ID", by.y = "Object_ID")

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

# Update the sub_cols with the newly available columns from the springfit
# REMOVE Columns here, do not want anything related to pheno
# sub_cols <- c("Poly_ID", "Point_ID", "genusspeci", "genus", "species", "Lon", "Lat",
#               "Height", "Radius", "SHAPE_Length", "SHAPE_Area",
#               "dbh", "tpstructur", "tpconditio", "riskrating", "object_id_group",
#               "agg_method", "meth",
#               "Year", "SOS_50", "EOS_50", "SOS_20", "EOS_20", "SOS_80", "EOS_80",
#               "n_SOS_50_3day", "n_EOS_50_3day", "n_SOS_50_7day", "n_EOS_50_7day", "n_SOS_50_14day", "n_EOS_50_14day", "n_SOS_20_80", "n_EOS_20_80",
#               "SOS_50_date", "EOS_50_date", "SOS_20_date", "EOS_20_date", "SOS_80_date", "EOS_80_date",
#               "R2", "NSE", "R", "RMSE", "pvalue", "n_sim",
#               "NDVI_fit_SOS_min", "NDVI_fit_SOS_max", "NDVI_fit_SOS_20", "NDVI_fit_SOS_80", "SOS_R2_20_80", "SOS_R2_wtd_20_80")

sub_cols <- c("Poly_ID", "Point_ID", "genusspeci", "genus", "species", "Lon", "Lat",
              "Height", "Radius", "SHAPE_Length", "SHAPE_Area",
              "dbh", "tpstructur", "tpconditio", "riskrating")

output_df <- st_drop_geometry(tree_intersect_sub2_reproj_lon_lat[,sub_cols])
setwd("/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal_extract")
saveRDS(output_df, "tree_pointextract_polyid_all_output_merged.rds")
# update future files downstream of this to use RDS file for data retention
