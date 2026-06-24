library(tidyverse)
library(data.table)
library(purrr)
'%notin%' <- Negate('%in%')
library(caret)
library(xgboost)
library(sf)
library(ggrepel)

# Prepped tree IDs with lidar
good_trees_prep_ids <- readRDS("/Volumes/NYC_geo/tree_classification/mset_v2_wlidarvars.rds")

# For classifier, will need to remove following columns:
# Poly_ID, Point_ID, species, dbh, tpstructur, tpconditio
# everything else can be appended and used by classifier
# Keep Poly_ID for now, this is later revised in the model prep section
good_trees_prep_ids <- good_trees_prep_ids[c("Poly_ID", "genus",
                                           "npts", "height_max_ft", "height_med_ft",
                                           "int_mean", "int_mean_above_medh", "int_mean_below_medh", "wid_medh_max_ft", "wid_medh_circlerad_ft", "hw_rat_2_max", "hw_rat_2_circlerad", "cp_3_max", "cp_3_circlerad")]

# Aggregated spectra
# 4 band
setwd("/Volumes/NYC_geo/tree_classification/extracted_4band_monthcomp/zscaled/")
file_list <- list.files()
tree_spectra_4band <- purrr::map_df(file_list, fread)
colnames(tree_spectra_4band)[2:ncol(tree_spectra_4band)] <- paste0("4b_", colnames(tree_spectra_4band)[2:ncol(tree_spectra_4band)])
tree_spectra_4band_sub <- tree_spectra_4band %>% filter(Object_ID %in% good_trees_prep_ids$Poly_ID)

tree_spectra_4band_sub %>% na.omit() %>% nrow()

rm(tree_spectra_4band)
gc()

# 8 band
setwd("/Volumes/NYC_geo/tree_classification/extracted_8band_monthcomp/zscaled/")
file_list <- list.files()
tree_spectra_8band <- purrr::map_df(file_list, fread)
colnames(tree_spectra_8band)[2:ncol(tree_spectra_8band)] <- paste0("8b_", colnames(tree_spectra_8band)[2:ncol(tree_spectra_8band)])
tree_spectra_8band_sub <- tree_spectra_8band %>% filter(Object_ID %in% good_trees_prep_ids$Poly_ID)

tree_spectra_8band_sub %>% na.omit() %>% nrow()

rm(tree_spectra_8band)
gc()

# Combine and remove individual versions
tree_spectra <- merge(tree_spectra_4band_sub, tree_spectra_8band_sub, by = c("Object_ID"))
rm(tree_spectra_4band_sub)
rm(tree_spectra_8band_sub)

# add prepped genus + lidar to tree spectra
tree_spectra_wlabels <- merge(good_trees_prep_ids, tree_spectra, by.x = "Poly_ID", by.y = "Object_ID")
rm(tree_spectra)
tree_spectra_wlabels %>% na.omit() %>% nrow()

# Remove any remaining trees with infinite or bad values
tree_df_for_cls <- na.omit(tree_spectra_wlabels)
tree_cls_mat <- as.matrix(tree_df_for_cls[,3:ncol(tree_df_for_cls)])
summed_rows <- rowSums(tree_cls_mat) # much faster on matrix
tree_df_for_cls_clean <- tree_df_for_cls[is.finite(summed_rows), ] # removing any more trees with missing data.

# remove unneeded variables to conserve memory
rm(tree_cls_mat)
rm(tree_df_for_cls)
gc()

saveRDS(tree_df_for_cls_clean, "/Volumes/NYC_geo/tree_classification/cls_prepped_file_v2_monthcomp.rds")
tree_df_for_cls_clean <- readRDS("/Volumes/NYC_geo/tree_classification/cls_prepped_file_v2_monthcomp.rds")

# Could split out lower part into another script
#####

# XGBoost
# THIS SECTION WAS RUN ON BIG COMPUTER, ~3 hours

out_path <- "E:/nyc/classification/outputs_practical_v2_monthcomp"
name_prefix <- "practical_v2_monthcomp_"

# reloading df created in first section
tree_df_for_cls_clean <- readRDS("E:/nyc/classification/cls_prepped_file_v2_monthcomp.rds")

#####
# Do an xgboost run
set.seed(14)

colnames(tree_df_for_cls_clean)[1] <- "Object_ID"

df_cls_agg_mean_cc <- tree_df_for_cls_clean # full version

genus_list <- sort(unique(df_cls_agg_mean_cc$genus))

rm(tree_df_for_cls_clean)

df_cls_agg_mean_cc$genus <- as.numeric(as.factor(as.character(df_cls_agg_mean_cc$genus))) - 1 # note: this scrambles the order of the genera unless df_cls_agg_mean_cc is redefined
train_index <- sample(1:nrow(df_cls_agg_mean_cc), round(0.8*nrow(df_cls_agg_mean_cc), 0)) # 80% training sample
train_data <- df_cls_agg_mean_cc[train_index, ]
test_data <- df_cls_agg_mean_cc[-train_index, ]

train_data_noid <- train_data %>% select(!Object_ID)
test_data_noid <- test_data %>% select(!Object_ID)

train_data_input <- train_data_noid %>% select(!genus) %>% as.matrix() 
train_matrix <- xgb.DMatrix(data = train_data_input, label = train_data_noid$genus)

test_data_input <- test_data_noid %>% select(!genus) %>% as.matrix() 
test_matrix <- xgb.DMatrix(data = test_data_input, label = test_data_noid$genus)

# XGBoost parameters prep
numberOfClasses <- length(unique(train_data_noid$genus))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses,
                   "nthread" = 8)
nround    <- 999999 # number of XGBoost rounds, unreasonably high, setup to stop before this is ever reached
esr <- 10 # early stopping rounds, will stop after accuracy does not improve after this many rounds

wl <- list(train = train_matrix, eval = test_matrix) # note: sometimes crashes and R aborts with a watchlist, just need to babysit it
# XGBoost run itself
start_time <- Sys.time()
bst_model <- xgb.train(params = xgb_params, data = train_matrix, nrounds = nround, watchlist = wl, verbose = TRUE, early_stopping_rounds = esr) # added verbose setting and early_stopping_rounds = 10
t_dif <- Sys.time() - start_time
print(t_dif)

# Save with xgb.save()
# Reload with xgb.load()

# output xgb model
setwd(out_path)
xgb_outname <- paste0(name_prefix, "xgb_full.model")
xgb.save(bst_model, xgb_outname)
#test_load <- xgb.load(xgb_outname)


# Predict hold-out test set
test_pred <- predict(bst_model, newdata = test_matrix)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_data_noid$genus + 1,
         max_prob = max.col(., "last"))

xgb_imp <- xgb.importance(model = bst_model)
xgb_imp_df <- paste0(name_prefix, "xgb_full_importance.csv")
write.csv(xgb_imp, xgb_imp_df, row.names = FALSE)

# Importance plotting
# Additional code for if there are less than 25 variables for plotting importance
nvar <- nrow(xgb_imp)
if(nvar > 25){
  var_plotted <- 25
} else {
  var_plotted <- nvar
}

ggplot(xgb_imp[1:var_plotted,]) +
  geom_col(aes(x = Gain, y = fct_reorder(Feature, Gain))) +
  labs(x = "Gain", y = "Variable", title = paste0("Top ", var_plotted, " most important variables"))
xgb_imp_fig <- paste0(name_prefix, "xgb_full.jpg")
ggsave(xgb_imp_fig, height = 5, width = 8, units = "in")

# Confusion matrix
xgb_cm <- confusionMatrix(factor(test_prediction$max_prob),
                          factor(test_prediction$label),
                          mode = "everything")
xgb_cm_out <- paste0(name_prefix, "xgb_full_confmat.rds")
saveRDS(xgb_cm, xgb_cm_out)

# save test prediction for referencing
test_prediction_path <- paste0(name_prefix, "xgb_full_predictions.csv")
write.csv(cbind.data.frame(test_data[,1:2], test_prediction), test_prediction_path, row.names = FALSE)

prod_acc <- unname(xgb_cm$byClass[,1])
user_acc <- unname(xgb_cm$byClass[,3])
class_num_xgb <- sort(unique(test_data$genus))
classes <- genus_list[class_num_xgb + 1] # add 1 because xgb numbering for classes starts at 0
cm_results <- cbind.data.frame(classes, class_num_xgb, prod_acc, user_acc)

test_prediction_accuracies <- paste0(name_prefix, "xgb_full_accuracies.csv")
write.csv(cm_results, test_prediction_accuracies, row.names = FALSE)

cm_results$classes <- as.factor(cm_results$classes)

ggplot(cm_results) +
  geom_point(aes(x = prod_acc, y = user_acc)) +
  geom_text_repel(aes(x = prod_acc, y = user_acc, label = classes)) +
  coord_equal() +
  ylim(0,1) + xlim(0,1) +
  labs(x = "Producer's Accuracy", y = "User's Accuracy") +
  theme_bw()
xgb_acc_fig <- paste0(name_prefix, "xgb_full_acc.jpg")
ggsave(xgb_acc_fig, height = 6, width = 6, units = "in")


#####
# Reload classification confusion matrix, if needed
confmat <- readRDS("/Volumes/NYC_geo/tree_classification/outputs_practical_v2_monthcomp/practical_v2_monthcomp_xgb_full_confmat.rds")

#####
# Make classification map
# Predict back onto extracted tree data to make citywide classification

tree_df_for_cls_clean <- readRDS("/Volumes/NYC_geo/tree_classification/cls_prepped_file_v2_monthcomp.rds")
col_order <- colnames(tree_df_for_cls_clean)
rm(tree_df_for_cls_clean)
gc()

# Load xgb model
bst_model <- xgb.load("/Volumes/NYC_geo/tree_classification/outputs_practical_v2_monthcomp/practical_v2_monthcomp_xgb_full.model")

# Load crown metrics
setwd("/Volumes/NYC_geo/nyc_lidar_metrics/crown_metrics")
crown_metrics_file_list <- list.files()
tree_crown_metrics <- purrr::map_df(crown_metrics_file_list, fread)

# Aggregated spectra
# 4 band
setwd("/Volumes/NYC_geo/tree_classification/extracted_4band_monthcomp/zscaled/")
file_list <- list.files()
tree_spectra_4band <- purrr::map_df(file_list, fread)
colnames(tree_spectra_4band)[2:ncol(tree_spectra_4band)] <- paste0("4b_", colnames(tree_spectra_4band)[2:ncol(tree_spectra_4band)])
tree_spectra_4band_sub <- tree_spectra_4band %>% filter(Object_ID %in% tree_crown_metrics$Poly_ID)
rm(tree_spectra_4band)
gc()

# 8 band
setwd("/Volumes/NYC_geo/tree_classification/extracted_8band_monthcomp/zscaled/")
file_list <- list.files()
tree_spectra_8band <- purrr::map_df(file_list, fread)
colnames(tree_spectra_8band)[2:ncol(tree_spectra_8band)] <- paste0("8b_", colnames(tree_spectra_8band)[2:ncol(tree_spectra_8band)])
tree_spectra_8band_sub <- tree_spectra_8band %>% filter(Object_ID %in% tree_crown_metrics$Poly_ID)
rm(tree_spectra_8band)
gc()

# merge crown metrics, 4 b, and 8 b into single file
tree_spectra <- merge(tree_spectra_4band_sub, tree_spectra_8band_sub, by = c("Object_ID"))
rm(tree_spectra_4band_sub)
rm(tree_spectra_8band_sub)

tree_spectra_lcm <- merge(tree_crown_metrics, tree_spectra, by.x = "Poly_ID", by.y = "Object_ID")
rm(tree_spectra)
rm(tree_crown_metrics)
gc()

# na omit
tree_spectra_lcm <- na.omit(tree_spectra_lcm) 
tree_spectra_lcm_mat <- as.matrix(tree_spectra_lcm)
summed_rows <- rowSums(tree_spectra_lcm_mat) # much faster on matrix
tree_spectra_lcm_clean <- tree_spectra_lcm[is.finite(summed_rows), ] # removing any more trees with missing or infinite data.
# losing 4180 trees because some data is missing, 1810991 to 1806811

# check column ordering
predict_cols <- colnames(tree_spectra_lcm_clean)[2:ncol(tree_spectra_lcm_clean)]
model_cols <- col_order[3:length(col_order)]
all(predict_cols == model_cols) # TRUE, they match

# Setup loop
colnames(tree_spectra_lcm_clean)[1] <- "Object_ID" # matching with existing code structure

tree_ids <- tree_spectra_lcm_clean$Object_ID

stepsize <- 50000
top <- floor(length(tree_ids)/stepsize)*stepsize+1
step_ranges <- seq(1, top, stepsize)

# for genus names
tree_df_for_cls_clean <- readRDS("/Volumes/NYC_geo/tree_classification/cls_prepped_file_v2_monthcomp.rds")
genus_list <- tree_df_for_cls_clean$genus %>% unique() %>% sort()
rm(tree_df_for_cls_clean)
gc()

# Loop XGBoost prediction outputs, can't do whole thing at once
for (idset in 1:length(step_ranges)){
  
  print(idset)
  
  range_min <- step_ranges[idset]
  idset_range <- seq(range_min, range_min + stepsize - 1)
  tree_ids_sub <- tree_ids[idset_range]
  
  tree_spectra_lcm_clean_sub <- tree_spectra_lcm_clean %>% filter(Object_ID %in% tree_ids_sub)
  
  # setup as xgb.DMatrix
  tree_spectra_lcm_noid <- tree_spectra_lcm_clean_sub %>% select(!Object_ID)
  tree_spectra_lcm_input <- tree_spectra_lcm_noid %>% as.matrix() # memory limit hit here if doing the whole thing, loop subset
  test_matrix <- xgb.DMatrix(data = tree_spectra_lcm_input) 
  
  numberOfClasses <- 18 # hard coded, 18 classes
  
  # predict on the model
  test_pred <- predict(bst_model, newdata = test_matrix)
  test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                            ncol=length(test_pred)/numberOfClasses) %>%
    t() %>%
    data.frame() 
  
  colnames(test_prediction) <- genus_list
  
  test_prediction <- test_prediction %>% mutate(max_prob = max.col(., "last"))
  
  test_prediction <- cbind.data.frame(tree_spectra_lcm_clean_sub$Object_ID, genus_list[test_prediction$max_prob], test_prediction[,1:numberOfClasses])
  colnames(test_prediction)[1:2] <- c("Poly_ID", "Genus_Predicted")
  
  output_name <- paste0("/Volumes/NYC_geo/tree_classification/outputs_practical_v2_monthcomp/predictions/", "practical_xgb_v2_predictions_set", idset, ".csv")
  write.csv(test_prediction, output_name, row.names = FALSE)
}

rm(tree_spectra_lcm_mat)
gc()

#####
# Merge XGBoost predictions into TNC polygons
# include known polygons
# Genus_Merged
# Genus_Predicted
# Genus_Known
# Species
# Cultivar

# Modified from: merge_street_tree_pheno2.R

# Load TNC polygons
tree_poly_path_full <- "/Volumes/NYC_geo/tree_polygons/tnc_2021/Trees_Centroids_Crown_Objects_2021.gdb" # Note this is the FINAL TNC dataset
tnc_gdb_polys <- st_read(tree_poly_path_full, layer = "treeobjects_2021_nyc")
tnc_gdb_polys$Poly_ID <- 1:nrow(tnc_gdb_polys)

# Load tree points
tree_pts <- st_read('/Users/dlm356/dlm356_files/nyc_trees/Forestry Tree Points_20240228/geo_export_3a66edb6-f0e4-44ec-9a82-e8b36349c231.shp')

# These are for reference labels
# Remove points with bad tpconditio and bad tpstructur, folding this into the final dataset with labels
# this won't be the same strict set as training input, but captures all decent stuff
tree_pts <- tree_pts %>% filter(tpconditio %in% c("Excellent", "Good", "Fair", "Poor", "Critical", "Unknown"), # only remove dead
                                      tpstructur %in% c("Full")) # only want Full trees, not stumps or only shafts or retired points

tree_pts_reproj <- st_transform(tree_pts, st_crs(tnc_gdb_polys))

colnames(tree_pts_reproj)[which(colnames(tree_pts_reproj) == "objectid")] <- "Point_ID"

tree_intersect <- st_intersection(tnc_gdb_polys, tree_pts_reproj) # intersection step

# which polygons only appear once after intersection, so only 1 stem
poly_ids_ints <- table(tree_intersect$Poly_ID) %>% as.data.frame()
poly_ids_1unique <- poly_ids_ints$Var1[which(poly_ids_ints$Freq == 1)] %>% as.character() %>% as.numeric()
# these are the trees we actually have unique labels for!

# String splitting genus + species + cultivar information from source tree points data set
tree_intersect_1unique_df <- tree_intersect %>% filter(Poly_ID %in% poly_ids_1unique) %>% st_drop_geometry()
front <- sapply(strsplit(tree_intersect_1unique_df$genusspeci, " - "), '[', 1)
species <- sapply(strsplit(front, " '"), '[', 1)
species2 <- sapply(strsplit(species, " var. inermis"), '[', 1) # gleditsia split
genus <- sapply(strsplit(species, " "), '[', 1)
species2[which(species2 == "Platanus x acerfolia")] <- "Platanus x acerifolia" # fix typo in original dataset

# merge altogether, these are filtered!
tree_intersect_1unique_df <- cbind.data.frame(genus, species2, tree_intersect_1unique_df)

# Load predicted genus info
setwd("/Volumes/NYC_geo/tree_classification/outputs_practical_v2_monthcomp/predictions")
file_list <- list.files()
predicted_trees <- purrr::map_df(file_list, fread)

tree_intersect_1unique_df <- tree_intersect_1unique_df %>% select(-Height, -Radius, -SHAPE_Length, -SHAPE_Area) # drop these columns from TNC, will add this in for every row

# do two full joins to merge everything
predictions_w_tree_points <- full_join(predicted_trees, tree_intersect_1unique_df, by = "Poly_ID")
predictions_w_tree_points_and_polys <- full_join(predictions_w_tree_points, tnc_gdb_polys, by = "Poly_ID")

known_genus_inds <- which(!is.na(predictions_w_tree_points_and_polys$genus))
predictions_w_tree_points_and_polys$Genus_Merged <- predictions_w_tree_points_and_polys$Genus_Predicted
predictions_w_tree_points_and_polys$Genus_Merged[known_genus_inds] <- predictions_w_tree_points_and_polys$genus[known_genus_inds]

# Move columns
out_geodf <- predictions_w_tree_points_and_polys %>% relocate(Genus_Merged, .after = Poly_ID)
out_geodf <- out_geodf %>% relocate(genus, .after = Genus_Predicted)
out_geodf <- out_geodf %>% relocate(species2, .after = genus)
out_geodf <- out_geodf %>% relocate(genusspeci, .after = species2)
colnames(out_geodf)[4:6] <- c("Genus_Ref", "Species_Ref", "FullCultivar_Ref")

out_geodf <- out_geodf %>% mutate(across(all_of(genus_list), round, 4)) %>% 
  mutate(across(all_of(genus_list), format, scientific = FALSE)) # reformat accuracy estimates, 4 digit, no scientific notation

#####
# Merge lidar data into geopackage with existing classification

setwd("/Volumes/NYC_geo/nyc_lidar_metrics/crown_metrics")
crown_metrics_file_list <- list.files()
tree_crown_metrics <- purrr::map_df(crown_metrics_file_list, fread)
tree_crown_metrics <- tree_crown_metrics[order(tree_crown_metrics$Poly_ID),]

out_geodf <- out_geodf %>% rename("TNC_Height" = "Height")
out_geodf <- out_geodf %>% rename("TNC_Radius" = "Radius")
out_geodf <- out_geodf %>% rename("TNC_SHAPE_Length" = "SHAPE_Length")
out_geodf <- out_geodf %>% rename("TNC_SHAPE_Area" = "SHAPE_Area")

tree_crown_metrics[,2:ncol(tree_crown_metrics)] <- round(tree_crown_metrics[,2:ncol(tree_crown_metrics)], 4)

out_geodf_2 <- left_join(out_geodf, tree_crown_metrics, by = "Poly_ID") # lcm = lidar crown metrics
rm(list = setdiff(ls(), "out_geodf_2"))
gc()

setwd("/Volumes/NYC_geo/tree_classification/outputs_practical_v2_monthcomp/")
st_write(out_geodf_2, "nyc_class_tree_genus_polygons_v2.gpkg")

table(out_geodf_2$Genus_Predicted)

#####
# Rename genus columns with "_prob" as requested by reviewer 1
setwd("/Volumes/NYC_geo/tree_classification/outputs_practical_v2_monthcomp/")
tree_cls <- st_read("/Volumes/NYC_geo/tree_classification/outputs_practical_v2_monthcomp/nyc_class_tree_genus_polygons_v2.gpkg")
gen_list <- c("Acer", "Ailanthus", "Betula", "Fraxinus", "Ginkgo", "Gleditsia", "Liquidambar", "Liriodendron", "Malus", "Platanus", "Prunus", "Pyrus", "Quercus", "Robinia", "Styphnolobium", "Tilia", "Ulmus", "Zelkova")
gen_list_prob <- paste0(gen_list, "_prob")
tree_cls_2 <- tree_cls %>% rename_at(vars(all_of(gen_list)), ~ gen_list_prob)
st_write(tree_cls_2, "for_zenodo_v2_1/nyc_class_tree_genus_polygons_v2_1.gpkg")
