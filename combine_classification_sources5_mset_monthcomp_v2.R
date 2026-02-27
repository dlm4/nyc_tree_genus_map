library(tidyverse)
library(data.table)
library(purrr)
'%notin%' <- Negate('%in%')
library(caret)
library(xgboost)
library(sf)
library(ggrepel)

# this is from an earlier version of the script, need to replace to do appending correctly. This is not the actual file we want
# earlier versions use the tree_pheno file, which is the actual source we want
#good_trees_prep <- readRDS("/Volumes/NYC_geo/tree_classification/cls_prepped_file.rds") # isn't this file already prepped? where is this from
#good_trees_prep <- readRDS("/Volumes/NYC_geo/tree_classification/cls_prepped_file.rds")
#good_trees_prep_ids <- good_trees_prep %>% select(Poly_ID, genus)
#rm(good_trees_prep)
#gc()


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

# add genus labels to tree spectra
#tree_labels <- tree_pheno %>% filter(Poly_ID %in% tree_spectra$Object_ID) %>% select("Poly_ID", "genus")
#tree_labels <- tree_labels[match(tree_spectra$Object_ID, good_trees_prep_ids$Poly_ID),]
tree_spectra_wlabels <- merge(good_trees_prep_ids, tree_spectra, by.x = "Poly_ID", by.y = "Object_ID")
rm(tree_spectra)
tree_spectra_wlabels %>% na.omit() %>% nrow()

# Remove any remaining trees with infinite or bad values
tree_df_for_cls <- na.omit(tree_spectra_wlabels)
tree_cls_mat <- as.matrix(tree_df_for_cls[,3:ncol(tree_df_for_cls)])
summed_rows <- rowSums(tree_cls_mat) # much faster on matrix
tree_df_for_cls_clean <- tree_df_for_cls[is.finite(summed_rows), ] # removing any more trees with missing data.

# remove unneeded variables to converve memory
#rm(tree_crown_metrics)
rm(tree_cls_mat)
#rm(tree_labels)
#rm(tree_spectra_wlabels_sos)
#rm(tree_spectra_wlabels_sos_lcm)
#rm(tree_pheno)
#rm(pheno_vars_wide)
rm(tree_df_for_cls)
gc()

saveRDS(tree_df_for_cls_clean, "/Volumes/NYC_geo/tree_classification/cls_prepped_file_v2_monthcomp.rds")
tree_df_for_cls_clean <- readRDS("/Volumes/NYC_geo/tree_classification/cls_prepped_file_v2_monthcomp.rds")

# # Filter for not dead and reasonable crowns only
# Now doing this upstream in the earlier prep filtering script
# tree_df_for_cls_clean <- readRDS("/Volumes/NYC_geo/tree_classification/cls_prepped_file_practical_v1_monthcomp.rds")
# ref_trees <- fread('/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal_pheno/tree_pheno_pointextract_polyid_all_output_2017_2024.csv')
# 
# ref_trees_sub <- ref_trees %>% filter(tpconditio %in% c("Excellent", "Good", "Fair"),
#                      tpstructur %in% c("Full"))
# 
# tree_df_for_cls_clean_filt <- tree_df_for_cls_clean %>% filter(Poly_ID %in% unique(ref_trees_sub$Poly_ID))
# # from 225014 trees to 217609, not much but it's not nothing
# saveRDS(tree_df_for_cls_clean_filt, "/Volumes/NYC_geo/tree_classification/cls_prepped_file_practical_v1_monthcomp_filt.rds")


# MAKE THIS a new script below?
#####

### THIS SECTION WAS RUN ON BIG COMPUTER, needs editing

out_path <- "E:/nyc/classification/outputs_practical_v2_monthcomp"
#out_path <- "/Volumes/NYC_geo/tree_classification/outputs_practical_v2_monthcomp"
#name_prefix <- "no_platanus_"
name_prefix <- "practical_v2_monthcomp_"
#name_prefix <- "everything_no_sos_" # if any name modifications are desired appended to beginning of each output, make to sure include trailing "_"
#name_prefix <- "lidar_only_"
#name_prefix <- "sos_lidar_only_"

tree_df_for_cls_clean <- readRDS("E:/nyc/classification/cls_prepped_file_v2_monthcomp.rds")
#tree_df_for_cls_clean <- readRDS("/Volumes/NYC_geo/tree_classification/cls_prepped_file_v2_monthcomp.rds")

#####
# Do an xgboost run
set.seed(14)
#set.seed(26) # CHANGED SEED FOR THIS RUN, otherwise was 14

colnames(tree_df_for_cls_clean)[1] <- "Object_ID"

#df_cls_agg_mean_cc <- tree_df_for_cls_clean[sample(1:nrow(tree_df_for_cls_clean), 2000, 0),] # try test run length
df_cls_agg_mean_cc <- tree_df_for_cls_clean # full version
#df_cls_agg_mean_cc <- tree_df_for_cls_clean %>% filter(genus != "Platanus") # try a no platanus run

genus_list <- sort(unique(df_cls_agg_mean_cc$genus))

rm(tree_df_for_cls_clean)

df_cls_agg_mean_cc$genus <- as.numeric(as.factor(as.character(df_cls_agg_mean_cc$genus))) - 1 # this scrambles the order of the genera unless df_cls_agg_mean_cc is redefined
train_index <- sample(1:nrow(df_cls_agg_mean_cc), round(0.8*nrow(df_cls_agg_mean_cc), 0))
train_data <- df_cls_agg_mean_cc[train_index, ]
test_data <- df_cls_agg_mean_cc[-train_index, ]

train_data_noid <- train_data %>% select(!Object_ID)
test_data_noid <- test_data %>% select(!Object_ID)

# https://rpubs.com/mharris/multiclass_xgboost
# might need to normalize classes

train_data_input <- train_data_noid %>% select(!genus) %>% as.matrix() 
train_matrix <- xgb.DMatrix(data = train_data_input, label = train_data_noid$genus)

test_data_input <- test_data_noid %>% select(!genus) %>% as.matrix() 
test_matrix <- xgb.DMatrix(data = test_data_input, label = test_data_noid$genus)

#
numberOfClasses <- length(unique(train_data_noid$genus))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses,
                   "nthread" = 8)
nround    <- 999999 # number of XGBoost rounds, was originally 50 but setting to giant number to see if it does in fact stop
esr <- 10 # early stopping rounds, need to figure out how to use this
cv.nfold  <- 5

# this is for cross validation folding (not needed right now)
# Fit cv.nfold * cv.nround XGB models and save OOF predictions
# try with verbose?
# start_time <- Sys.time()
# cv_model <- xgb.cv(params = xgb_params,
#                    data = train_matrix,
#                    nrounds = nround,
#                    nfold = cv.nfold,
#                    verbose = TRUE,
#                    prediction = TRUE)
# t_dif <- Sys.time() - start_time
# print(t_dif) # 8.4 hours to run...
# 
# OOF_prediction <- data.frame(cv_model$pred) %>%
#   mutate(max_prob = max.col(., ties.method = "last"),
#          label = train_data_noid$genus + 1)
# head(OOF_prediction)
# # confusion matrix
# confusionMatrix(factor(OOF_prediction$max_prob),
#                 factor(OOF_prediction$label),
#                 mode = "everything")
# 
# xgb_impcv <- xgb.importance(model = cv_model)
# 
# ggplot(xgb_impcv[1:25,]) +
#   geom_col(aes(x = Gain, y = fct_reorder(Feature, Gain))) +
#   labs(x = "Gain", y = "Variable", title = "Top 25 most important variables")


wl <- list(train = train_matrix, eval = test_matrix) # sometimes crashes and R aborts with a watchlist, didn't happen before
start_time <- Sys.time()
#bst_model <- xgb.train(params = xgb_params, data = train_matrix, nrounds = nround)
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
# confusion matrix of test set
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")
# so xgboost is not a panacea, but does work better, especially for some small classes. 
# 50 rounds takes 8x longer to run than ranger with 500 rf trees though

xgb_imp <- xgb.importance(model = bst_model)
xgb_imp_df <- paste0(name_prefix, "xgb_full_importance.csv")
write.csv(xgb_imp, xgb_imp_df, row.names = FALSE)

# Additional code for if there are less than 25 variables
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

#
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
# Classification confusion matrix
confmat <-readRDS("/Volumes/NYC_geo/tree_classification/outputs_practical_v2_monthcomp/practical_v2_monthcomp_xgb_full_confmat.rds")



#####
# Updating from here

# Predict back onto extracted tree data to make citywide classification

tree_df_for_cls_clean <- readRDS("/Volumes/NYC_geo/tree_classification/cls_prepped_file_v2_monthcomp.rds")
col_order <- colnames(tree_df_for_cls_clean)
rm(tree_df_for_cls_clean)
gc()

# Load xgb model
#bst_model <- xgb.load("/Volumes/NYC_geo/tree_classification/outputs_practical_v1_1_monthcomp_filt/practical_v1_1_monthcomp_filt_xgb_full.model")
bst_model <- xgb.load("/Volumes/NYC_geo/tree_classification/outputs_practical_v2_monthcomp/practical_v2_monthcomp_xgb_full.model")

# Load crown metrics
setwd("/Volumes/NYC_geo/nyc_lidar_metrics/crown_metrics")
crown_metrics_file_list <- list.files()
tree_crown_metrics <- purrr::map_df(crown_metrics_file_list, fread)

# Loop (if needed)

# Load each ID set for monthcomp:
# Load 4 b
# Load 8 b

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

#tree_spectra_lcm <- merge(tree_spectra, tree_crown_metrics, by.x = "Object_ID", by.y = "Poly_ID")
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
# would be able to fill in later versions

# check column ordering
predict_cols <- colnames(tree_spectra_lcm_clean)[2:ncol(tree_spectra_lcm_clean)]
model_cols <- col_order[3:length(col_order)]
all(predict_cols == model_cols) # TRUE, they match

# Setup loop

colnames(tree_spectra_lcm_clean)[1] <- "Object_ID" # matching with existing code structure

tree_ids <- tree_spectra_lcm_clean$Object_ID

#i <- 1
stepsize <- 50000
top <- floor(length(tree_ids)/stepsize)*stepsize+1
step_ranges <- seq(1, top, stepsize)

# for genus names
#tree_df_for_cls_clean <- readRDS("/Volumes/NYC_geo/tree_classification/cls_prepped_file_practical_v1_1_monthcomp_filt.rds")
tree_df_for_cls_clean <- readRDS("/Volumes/NYC_geo/tree_classification/cls_prepped_file_v2_monthcomp.rds")
genus_list <- tree_df_for_cls_clean$genus %>% unique() %>% sort()
rm(tree_df_for_cls_clean)
gc()

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
  
  #output_name <- paste0("/Volumes/NYC_geo/tree_classification/outputs_practical_v1_1_monthcomp_filt/predictions/", "practical_xgb_v1_1_predictions_set", idset, ".csv")
  output_name <- paste0("/Volumes/NYC_geo/tree_classification/outputs_practical_v2_monthcomp/predictions/", "practical_xgb_v2_predictions_set", idset, ".csv")
  write.csv(test_prediction, output_name, row.names = FALSE)
}

rm(tree_spectra_lcm_mat)
gc()

#####
# merge into tnc polygons
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
#setwd("/Volumes/NYC_geo/tree_classification/outputs_practical_v1_1_monthcomp_filt/predictions")
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

## Write this whole thing out as a geopackage
#setwd("/Volumes/NYC_geo/tree_classification/outputs_practical_v1_1_monthcomp_filt/")
#st_write(out_geodf, "cls_poly_practical_v1_1_monthcomp_filt.gpkg")

# load confusion matrix
#confmat <- readRDS("practical_v1_1_monthcomp_filt_xgb_full_confmat.rds")

#####
# Merge lidar data into new geopackage with existing classification
#setwd("/Volumes/NYC_geo/tree_classification/outputs_practical_v1_1_monthcomp_filt/")
#out_geodf <- st_read("cls_poly_practical_v1_1_monthcomp_filt.gpkg")

setwd("/Volumes/NYC_geo/nyc_lidar_metrics/crown_metrics")
crown_metrics_file_list <- list.files()
tree_crown_metrics <- purrr::map_df(crown_metrics_file_list, fread)
tree_crown_metrics <- tree_crown_metrics[order(tree_crown_metrics$Poly_ID),]

out_geodf <- out_geodf %>% rename("TNC_Height" = "Height")
out_geodf <- out_geodf %>% rename("TNC_Radius" = "Radius")
out_geodf <- out_geodf %>% rename("TNC_SHAPE_Length" = "SHAPE_Length")
out_geodf <- out_geodf %>% rename("TNC_SHAPE_Area" = "SHAPE_Area")

tree_crown_metrics[,2:ncol(tree_crown_metrics)] <- round(tree_crown_metrics[,2:ncol(tree_crown_metrics)], 4)

#tree_spectra_wlabels_sos_lcm <- merge(tree_spectra_wlabels_sos, tree_crown_metrics) # lcm = lidar crown metrics
out_geodf_2 <- left_join(out_geodf, tree_crown_metrics, by = "Poly_ID") # lcm = lidar crown metrics
rm(list = setdiff(ls(), "out_geodf_2"))
gc()

setwd("/Volumes/NYC_geo/tree_classification/outputs_practical_v2_monthcomp/")
st_write(out_geodf_2, "nyc_class_tree_genus_polygons_v2.gpkg")


table(out_geodf_2$Genus_Predicted)


#####

# Additional accuracy assessment
# further set of known polygons that are not included in the strict "good quality" training + validation filters
# this will give another check on accuracy - for trees that may not be in perfect condition, how accurate is the model?

# Need classification, these are already predicted
out_geodf_2 <- st_read("/Volumes/NYC_geo/tree_classification/outputs_practical_v2_monthcomp/nyc_class_tree_genus_polygons_v2.gpkg")

# Area calculations for Data Records section
# which(is.na(out_geodf_2$Genus_Predicted)) %>% length()
# which(!is.na(out_geodf_2$Genus_Predicted)) %>% length()
# which(!is.na(out_geodf_2$Genus_Ref)) %>% length()
# which(!is.na(out_geodf_2$Genus_Merged)) %>% length()
# 
# area_predicted <- sum(out_geodf_2$TNC_SHAPE_Area[which(!is.na(out_geodf_2$Genus_Predicted))])
# area_merged <- sum(out_geodf_2$TNC_SHAPE_Area[which(!is.na(out_geodf_2$Genus_Merged))])
# area_na <- sum(out_geodf_2$TNC_SHAPE_Area[which(is.na(out_geodf_2$Genus_Predicted))])
# area_total <- sum(out_geodf_2$TNC_SHAPE_Area)
# area_na/area_total*100
# area_predicted/area_total*100
# area_merged/area_total*100

# Need original source training + validation set (to exclude these IDs)
tree_df_for_cls_clean <- readRDS("/Volumes/NYC_geo/tree_classification/cls_prepped_file_v2_monthcomp.rds")

out_geodf_2_sub <- out_geodf_2 %>% filter(Poly_ID %notin% tree_df_for_cls_clean$Poly_ID, !is.na(Genus_Ref))

# This is knowing it won't get everything right because Genus_Ref includes genera that the predictor doesn't know about
bonus_test_cm <- confusionMatrix(factor(out_geodf_2_sub$Genus_Predicted),
                          factor(out_geodf_2_sub$Genus_Ref),
                          mode = "everything")
# Overall accuracy = 44.86%, not bad considering classifier doesn't know about most of these classes
# 201218 crowns

out_geodf_2_sub2 <- out_geodf_2 %>% filter(Poly_ID %notin% tree_df_for_cls_clean$Poly_ID, 
                                           Genus_Ref %in% unique(Genus_Predicted) & !is.na(Genus_Ref))

bonus_test_cm2 <- confusionMatrix(factor(out_geodf_2_sub2$Genus_Predicted),
                                 factor(out_geodf_2_sub2$Genus_Ref),
                                 mode = "everything")
# Overall accuracy = 57.49% for many lower quality crowns
# 155444 crowns

out_geodf_2 %>% filter(!is.na(Genus_Ref)) %>% nrow() # 437044 crowns

#xgb_cm_out <- paste0(name_prefix, "xgb_full_confmat.rds")
#saveRDS(xgb_cm, xgb_cm_out)


out_geodf_2 %>% filter(Genus_Ref %in% unique(Genus_Predicted) & !is.na(Genus_Ref)) %>% nrow() # 391270
# In Genus_Ref column, 391270/437044 = 89.5% are one of the Genus_Predicted types (tails off after this)

options(scipen = 999)
out_geodf_2_full_genus_ref <- out_geodf_2 %>% filter(!is.na(Genus_Ref))
round(sort(table(out_geodf_2_full_genus_ref$Genus_Ref), decreasing = TRUE) / nrow(out_geodf_2_full_genus_ref) * 100, 2)
# most of the major genera included are > 1%

options(scipen = 0)

#
235826/437044
155444/437044
201218/437044

44.86*0.46 + 81.98*0.54
