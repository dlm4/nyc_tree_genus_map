---
editor_options: 
  markdown: 
    wrap: 72
---

R scripts related to processing NYC classification, stored on Zenodo
repository:

**A map of trees in New York City classified at the genus level from
remote sensing**

<https://doi.org/10.5281/zenodo.17654184>

**1. PlanetScope imagery prep and extraction:**

1.1 Create daily composite mosaics for all PlanetScope imagery, 4 band
and 8 band versions

1_1_4b: planet_daily_stack_highsunonly_nyc_4b_revised.R

1_1_8b: planet_daily_stack_highsunonly_nyc_8b.R

1.2 Calibrate (empirical line correction) and filter daily composite
images using ground reference targets

1_2: ref_targets_planet_highsunonly_allnyc_4or8band.R

1.3 Extract calibrated reflectance data from PlanetScope imagery using
tree crown polygon objects, 4 band and 8 band versions

1_3_4b_planet_daily_stack_nyc_cal_tree_extract_highsunonly_treepoints_tncfinal_4b_v2.R

1_3_8b_planet_daily_stack_nyc_cal_tree_extract_highsunonly_treepoints_tncfinal_8b_v2.R

**2. Lidar prep:**

2.1 Make digital terrain model tiles from the point cloud data

2_1_lascatalog_apply_dtm.R

2.2 Calculate lidar crown metrics for tree crown polygons

2_2_get_lidar_structural_vars_from_polys_v4.R

**3. Tree curation and aggregation:**

3.1 Intersect tree polygons with known labeled street tree points

3_1_merge_street_tree_poly.R

3.2 Calculate mean summer NDVI based on 4-band PlanetScope

3_2_mean_summer_ndvi.R

3.3 Aggregate extracted spectra to monthly composites

3_3_4b: 4ms_tree_spectra_agg_monthcomp_4b.R

3_3_8b: 4ms_tree_spectra_agg_monthcomp_8b.R

3.4 Curate extracted trees to refine dataset for classification and
counts available tree observations

3_4: tree_counts_in_planetscope3_updated_mset.R

**4. Classification data organization, running, and validation**

4.1 Combine all datasets, run XGBoost, and apply predictions to polygon
map

4_1: Combine_classification_sources5_mset_monthcomp_v2.R

4.2 Validate XGBoost model predictions, high quality and expanded

4_2: xgb_model_validations_updated.R

**5. Replies to reviewer comments**

5.1 Additional tests for reviewer comments, including accuracy
assessment and variable importance

5_1: addressing_reviewer_comments.R

5.2 Validate tree polygon accuracy

5_2 Polygons_validation_result.R
