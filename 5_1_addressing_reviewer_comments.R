# Responding to a couple of reviewer comments

library(tidyverse)
library(xgboost)

# R2, last comment regarding Line 225 addressing variable importance
# Recreate variable importance plot

`%notin%` <- Negate(`%in%`)

xgb_imp <- read.csv("/Volumes/NYC_geo/tree_classification/outputs_practical_v2_monthcomp/practical_v2_monthcomp_xgb_full_importance.csv")
mset <- readRDS("/Volumes/NYC_geo/tree_classification/cls_prepped_file_v2_monthcomp.rds")

12*(28+8+4+6)

colnames(mset)[which(colnames(mset) %notin% xgb_imp$Feature)]

xgb_model <- xgb.load("/Volumes/NYC_geo/tree_classification/outputs_practical_v2_monthcomp/practical_v2_monthcomp_xgb_full.model")

# Note: hw_rat_2_max is not included in the outputted variable importance but was included in the original XGB model input. Must have a variable importance no different from 0.

# Make a new importance figure:

# Additional code for if there are less than 25 variables
nvar <- nrow(xgb_imp)
if(nvar > 25){
  var_plotted <- 25
} else {
  var_plotted <- nvar
}

ggplot(xgb_imp[1:var_plotted,]) +
  geom_col(aes(x = Gain, y = fct_reorder(Feature, Gain))) +
  labs(x = "Gain", y = "Variable") +
  theme_bw() +
  theme(#panel.grid.minor = element_line(color = "gray90", linetype = "dotted", linewidth = 0.1),
        #panel.grid = element_line(color = "gray80", linetype = "dotted", linewidth = 0.2),
        axis.title = element_text(size = 7, color = "black"),
        axis.text = element_text(size = 5, color = "black"))

xgb_imp_fig_r <- paste0("/Volumes/NYC_geo/tree_classification/outputs_practical_v2_monthcomp/practical_v2_monthcomp_xgb_full_importance_formatted.jpg")
ggsave(xgb_imp_fig_r, height = 3, width = 4, units = "in")
xgb_imp_fig_rs <- paste0("/Volumes/NYC_geo/tree_classification/outputs_practical_v2_monthcomp/practical_v2_monthcomp_xgb_full_importance_formatted.svg")
ggsave(xgb_imp_fig_rs, height = 3, width = 4, units = "in")


#####

# Technical validation new part 3, reviewer 3 comment on line 311: How many trees are overwritten in genus_merged and how accurate are they?

library(sf)

tree_poly <- st_read("/Volumes/NYC_geo/tree_classification/outputs_practical_v2_monthcomp/for_zenodo_v2/nyc_class_tree_genus_polygons_v2.gpkg")
rvp <- table(tree_poly$Genus_Ref, tree_poly$Genus_Predicted)
rvp_df <- rvp %>% as.data.frame()
colnames(rvp_df)[1:2] <- c("Ref", "Predicted")

freq_sum <- sum(rvp_df$Freq)
freq_sum_matched <- sum(rvp_df$Freq[which(as.character(rvp_df$Ref) == as.character(rvp_df$Predicted))])

freq_sum_inc_genus <- sum(rvp_df$Freq[which(as.character(rvp_df$Ref) != as.character(rvp_df$Predicted) & as.character(rvp_df$Ref) %in% as.character(rvp_df$Predicted))])
freq_sum_other_genus <- sum(rvp_df$Freq[which(as.character(rvp_df$Ref) != as.character(rvp_df$Predicted) & as.character(rvp_df$Ref) %notin% as.character(rvp_df$Predicted))])

freq_sum_inc_genus/(freq_sum_inc_genus + freq_sum_other_genus)
freq_sum_other_genus/(freq_sum_inc_genus + freq_sum_other_genus)

freq_sum_matched/(freq_sum - freq_sum_other_genus)

# Of the 422671 known trees in the Genus_Ref column, the Genus_Predicted column matched 308239 of these (72.93%). 
# Of the remaining trees that did not match between these columns (114432), 
# 73394 (64.14%) were one of the 18 tree genera that the classifier could have gotten correct, 
# while the remainder (n = 41038, 35.86%) were other genera that were not included in the classifier.
# Removing this remainder of tree genera not included in the classifier, the classifier got 80.77% of the trees correct (n = 308239) that it was possible for it to get correct (n = 381633).