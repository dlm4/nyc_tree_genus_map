R scripts related to processing NYC classification, stored on Zenodo repository:

**A map of trees in New York City classified at the genus level from remote sensing**

<https://doi.org/10.5281/zenodo.17654184>

&nbsp;

&nbsp;

**1. PlanetScope imagery prep and extraction:**

1.1 Create daily composite mosaics for all PlanetScope imagery, 4 band and 8 band versions

1_1_4b_planet_daily_stack_highsunonly_nyc_4b_revised.R

1_1_8b_planet_daily_stack_highsunonly_nyc_8b.R

&nbsp;

1.2 Calibrate (empirical line correction) and filter daily composite images using ground reference targets

1_2_ref_targets_planet_highsunonly_allnyc_4or8band.R

&nbsp;

1.3 Extract calibrated reflectance data from PlanetScope imagery using tree crown polygon objects, 4 band and 8 band versions

1_3_4b_planet_daily_stack_nyc_cal_tree_extract_highsunonly_treepoints_tncfinal_4b_v2.R

1_3_8b_planet_daily_stack_nyc_cal_tree_extract_highsunonly_treepoints_tncfinal_8b_v2.R

&nbsp;

&nbsp;

**2. Lidar prep:**

2.1 Make digital terrain model tiles from the point cloud data

2_1_lascatalog_apply_dtm.R

&nbsp;

2.2 Calculate lidar crown metrics for tree crown polygons

2_2_get_lidar_structural_vars_from_polys_v4.R

&nbsp;

&nbsp;

**3. Tree curation and aggregation:**

3.1 Intersect tree polygons with known labeled street tree points

3_1_merge_street_tree_poly.R

&nbsp;

3.2 Calculate mean summer NDVI based on 4-band PlanetScope

3_2_mean_summer_ndvi.R

&nbsp;

3.3 Aggregate extracted spectra to monthly composites

3_3_4b_tree_spectra_agg_monthcomp_4b.R

3_3_8b_tree_spectra_agg_monthcomp_8b.R

&nbsp;

3.4 Curate extracted trees to refine dataset for classification and counts available tree observations

3_4_tree_counts_in_planetscope3_updated_mset.R

&nbsp;

&nbsp;

**4. Classification data organization, running, and validation**

4.1 Combine all datasets, run XGBoost, and apply predictions to polygon map

4_1_combine_classification_sources5_mset_monthcomp_v2.R

&nbsp;

4.2 Validate XGBoost model predictions, high quality and expanded

4_2_xgb_model_validations_updated.R

&nbsp;

&nbsp;

**5. Replies to reviewer comments**

5.1 Additional tests for reviewer comments, including accuracy assessment and variable importance

5_1_addressing_reviewer_comments.R

&nbsp;

5.2 Validate tree polygon accuracy

5_2_Polygons_validation_result.R
