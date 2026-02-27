

# Load in shapefile of temporally invariant targets (Bronx)
# Get list of Planet images (daily composites for now)
#   Will need to reduce to just the 10% cloud max set for new composites
# Extract 4-band values for all targets for all bands


# Would want to regress all targets for each band, but many images likely will miss targets in certain images because they don't cover the full extent
# Get median value as x-axis reference, or choose a "good" reference image - how to know which image is a "good" image?
# If no targets, cannot calibrate image and image must be removed
# If regression isn't possible, do time series for available targets
# check median (or reference) then evaluate


# even post-cleanup, will still need to remove bad outliers from the NDVI time series if there are anomalous clouds for bad jumps in the data...

library(terra)
library(sf)
library(tidyverse)
library(exactextractr)
library(lubridate)
`%notin%` <- Negate(`%in%`)
# Ref targets

#

# Prep input stuff
num_bands <- 8 # 4 or 8
path_planet_mosaics <- "/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_8b_highsunonly"

# to write out
output_prefix <- "all_nyc_8b_"
output_target_refl_dir <- "/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_8b_highsunonly_cal" # no trailing "/"
#...

# # Prep input stuff
# num_bands <- 8 # 4 or 8
# path_planet_mosaics <- "/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_8b_highsunonly"
# 
# # to write out
# output_prefix <- "all_nyc_8b_"
# output_target_refl_dir <- "/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_8b_highsunonly_cal"
# #...

#####
# First section, extracting target band info
setwd("/Users/dlm356/dlm356_files/nyc_trees/ref_targets/")
ref_targets_bronx <- read_sf("bronx_ground_ref_targets.shp")
ref_targets_brooklyn <- read_sf("brooklyn_ground_ref_targets.shp")
ref_targets_queens <- read_sf("queens_ground_ref_targets.shp") # this was fixed in check_updated_ref_targets.R
ref_targets_manhattan <- read_sf("manhattan_ground_ref_targets.shp")
ref_targets_statenisland <- read_sf("statenisland_ground_ref_targets.shp") 

# Combine ref polygons to entire borough
ref_targets_bronx$Borough <- "Bronx"
ref_targets_brooklyn$Borough <- "Brooklyn"
ref_targets_queens$Borough <- "Queens"
ref_targets_manhattan$Borough <- "Manhattan"
ref_targets_statenisland$Borough <- "Staten_Island"

# Reprojection
ref_targets_brooklyn <- st_transform(ref_targets_brooklyn, crs(ref_targets_bronx))
ref_targets_queens <- st_transform(ref_targets_queens, crs(ref_targets_bronx))
ref_targets_manhattan <- st_transform(ref_targets_manhattan, crs(ref_targets_bronx))
ref_targets_statenisland <- st_transform(ref_targets_statenisland, crs(ref_targets_bronx))

ref_targets <- rbind(ref_targets_bronx, ref_targets_brooklyn, ref_targets_queens, ref_targets_manhattan, ref_targets_statenisland)

setwd(path_planet_mosaics)

img_list <- list.files(pattern = glob2rx("*.tif"))

if (num_bands == 4){
  output_cols <- c("id", "Name", "Borough", "mean.blue", "mean.green", "mean.red", "mean.nir", "img_file", "date")
} else if (num_bands == 8){
  output_cols <- c("id", "Name", "Borough", "mean.coastal_blue", "mean.blue", "mean.green_i", "mean.green", "mean.yellow", "mean.red", "mean.rededge", "mean.nir", "img_file", "date")
} else {
  print("Needs to be either 4 or 8 bands")
}
all_target_df <- data.frame(matrix(NA, nrow = nrow(ref_targets)*length(img_list), ncol = length(output_cols)))
colnames(all_target_df) <- output_cols

ref_targets_reproj <- st_transform(ref_targets, crs(rast(img_list[1])))

row_start <- 1
for (ind in 1:length(img_list)){
  #for (ind in 1:100){
  row_end <- row_start + nrow(ref_targets) - 1
  #ind <- 1
  print(ind)
  img <- img_list[ind]
  img_rast <- rast(img)
  
  ref_targets_reproj <- st_transform(ref_targets, crs(img_rast))
  
  ref_target_extracted <- exact_extract(img_rast, ref_targets_reproj, fun = "mean", append_cols = c("id", "Name", "Borough"))
  ref_target_extracted$img_file <- img_list[ind]
  ref_target_extracted$date <- ymd(unlist(strsplit(img_list[ind], "_|\\."))[5]) # CAUTION: hard coded date index
  
  all_target_df[row_start:row_end,] <- ref_target_extracted
  
  row_start <- row_end + 1
  
  # Cleanup
  rm(img)
  rm(img_rast)
  gc()
  tmpFiles(remove=TRUE) # remove temporary files
}

# remove rows with NaNs
all_target_df <- all_target_df[complete.cases(all_target_df),]
all_target_df$date <- as.Date(all_target_df$date) # date got lost and converted to integer somehow

#####
all_target_df$ndvi <- (all_target_df$mean.nir - all_target_df$mean.red)/(all_target_df$mean.nir + all_target_df$mean.red)
setwd(output_target_refl_dir)
write.csv(all_target_df, file = paste0(output_prefix, "_ref_targets_refl.csv"), row.names = F)

#####
all_target_df <- read.csv(paste0(output_prefix, "_ref_targets_refl.csv")) # just read this in here...
all_target_df$Borough_Name <- paste(all_target_df$Borough, "_", all_target_df$Name, sep = "") 

# Preview time series

p1 <- ggplot(data = all_target_df) +
  geom_point(aes(x = date, y = mean.red), size = 0.5) +
  facet_wrap(~Borough_Name, ncol = 19) +
  theme_bw()
ggsave(paste0(output_prefix, "_all_ref_targets_nyc_ts_red.png"), p1, width = 34, height = 22, units = "in")

p2 <- ggplot(data = all_target_df) +
  geom_point(aes(x = date, y = mean.nir), size = 0.5) +
  facet_wrap(~Borough_Name, ncol = 19) +
  theme_bw()
ggsave(paste0(output_prefix, "_all_ref_targets_nyc_ts_nir.png"), p2, width = 34, height = 22, units = "in")

p3 <- ggplot(data = all_target_df) +
  geom_point(aes(x = date, y = ndvi), size = 0.5) +
  facet_wrap(~Borough_Name, ncol = 19) +
  theme_bw()
ggsave(paste0(output_prefix, "_all_ref_targets_nyc_ts_ndvi.png"), p3, width = 34, height = 22, units = "in")

# Editing notes

# ggplot(data = all_target_df[which(all_target_df$Name == "OrchBeach2"),]) +
#   geom_point(aes(x = date, y = mean.nir)) +
#   facet_wrap(~Name)

# Corrections to targets
# Bronx: 
#   Botanical (DONE, now FordhamPrk), 
#   EastBronx (DONE, now EastBrUSPS), 
#   FtSchuyler (DONE, now FtSchuyl2, had to be moved because of power plant shadow), 
#   Wakefield (DONE, now Wakefield2, had to be moved because of building shadows)
# Brooklyn: 
#   BestBuy, (DONE, now PCRichard, nearby tall shadow issues, replaced)
#   DrJohnsPlg, (DONE, now Dunkin, playground surface may have been degrading over time)
#   DykerPark, (DONE, now FortHamStp, infield baseball surface got greener over time)
#   FortHam, (DONE, now BeltPkwy, tennis court got greener and affected by tree shadows)
#   Lowes, (DONE, now HomeDepotR, Lowes roof was repainted),
#   NYsketches, (DONE, now BushTerm, sketches roof was redone in 2023)
# Manhattan: 
#   Heckscher5, (DONE, replaced with northside of ParkAveArm, hecksher baseball field was greening over time)
#   Presbyteri, (DONE, replaced with a new target on FDRdrive, some cleaning consistency of the Presbysterian roof)
#   PS79, (DONE, replaced with NYCPodMed, PS79 roof brightness seems to be declining)
#   Randalls90, (DONE, replaced with Randalls63, strange split reflectance signal for the field)
#   Riverbank (DONE, replaced with Riverbank2, reflectance of field increasing through time)
# Queens: 
#   Corona, (DONE, replaced with Corona2, signal got strangely clean in 2022 and 2023)
#   Hollis, (DONE, replaced with OConnell, NIR and red declining over time)
#   HomeDepot, (DONE, replaced with StopShopQ, home depot roof may have been repainted)
#   JamaicaBay, (DONE, replaced with JamaicaB2, nir was increasing over time)
#   LittleNeck, (DONE, replaced with LittleNec2, roof got repainted at end of 2018)
#   Macys, (DONE, replaced with JackieRobT, might be a little too clean the last two years) - !! was not deleted
#   PresChurch, (DONE, replaced with PresChur2, there was some veg signal mixing, shifted down the street)
#   SeasonsQns, (DONE, replaced with Cunningham, had strong seasonal cycle, suspected mixing with tree) !! was not deleted
#   StarSubaru, (DONE, replaced with StarToyota, may have been repainted in 2022-2023)
#   StPius (DONE, replaceddue to inconsistent later years)

# number 34 deleted? which others?

# Staten Island: 
#   CollegeSI, (DONE, replaced with Willowbrok, tennis courts were repainted in 2020) 
#   ForestGrov, (DONE, replaced with Penske, lots of variability in the old roof)
#   FtWadswort, (DONE, replaced with FortWads2 on the beach, ndvi increasing over time likely due to tree encroachment)
#   HylanBlvd, (DONE, replaced with ConfHouseB on the beach, ndvi increasing over time)
#   IS007Berns, (DONE, replaced with StJosephTn tennis court, red declining through time so ndvi is increasing)
#   NewSpringv, (DONE, replaced with Carteret, reflectance noisier than it should be)
#   RichmondCC, (DONE, replaced with Midland, reflectance slowly increasing over time)
#   StAndrewC, (DONE, replaced with FarrellHS, ndvi slowly increasing over time)
#   StJoseph, (DONE, replaced with AmboyRd, variability changes, suspected repaving)
#   SuperFresh, (DONE, replaced with Lutheran, introduced distinct seasonality in later years)
#   Surfside, (DONE, replaced with SurfsideB, seasonality appears in NDVI in later years)
#   TonysBest, (DONE, replaced with JCC, possible repaving)
#   Walgreens, (DONE, replaced with FawnRidge tennis courts, reflectance increasing over time)
#   WWIIskate (DONE, replaced with CloveBase baseball field, reflectance was seasonally varying and wildly inconsistent)

#####

# Target removal and application of correction and plotting

# Need to edit below here for 8b and if extended the time series

setwd(output_target_refl_dir)
all_target_df <- read.csv(paste0(output_prefix, "_ref_targets_refl.csv"))
all_target_df$Borough_Name <- paste(all_target_df$Borough, "_", all_target_df$Name, sep = "")
all_target_df$date <- ymd(all_target_df$date) # date imported as character

# REMOVE THESE, big lighting cycles
# bad_targets <- c("Bronx_MorrisPark", "Bronx_PelhamMan2", 
#   "Brooklyn_Narrows", "Brooklyn_OasisBar", "Brooklyn_ParkwayPlz",
#   "Manhattan_ParkAveArm", "Manhattan_Pier99", 
#   "Queens_Cambria", "Queens_Diehls", "Queens_Distribute", "Queens_GreenOlive", "Queens_LeNoble", "Queens_OldNavy", "Queens_RachelCars", "Queens_Supermkt", "Queens_Ticketech", "Queens_Wholesale",
#   "Staten_Island_SAnnadale")

bad_targets <- c("Bronx_BronxHSS", "Bronx_MorrisHght", "Bronx_MorrisPark", "Bronx_PelhamMan2", # 4
                 "Brooklyn_Narrows", "Brooklyn_NBrookPrep", "Brooklyn_OasisBar", "Brooklyn_ParkwayPlz", # 4
                 "Manhattan_ParkAveArm", "Manhattan_Riverbank2", "Manhattan_Pier99", # 3
                 "Queens_BroadChan", "Queens_Cambria", "Queens_Diehls", "Queens_Distribute", "Queens_FritoLay", "Queens_FtTotten", "Queens_GreenOlive", "Queens_LeNoble", "Queens_OldNavy", "Queens_Queensboro", "Queens_RachelCars", "Queens_RockawayB2", "Queens_Supermkt", "Queens_Ticketech", "Queens_Wholesale", # 15
                 "Staten_Island_IS075FDP", "Staten_Island_SAnnadale") # 2

all_target_df_clean <- all_target_df[which(all_target_df$Borough_Name %notin% bad_targets),]

unique(all_target_df_clean$Borough_Name) %>% length() # 238
unique(all_target_df$Borough_Name) %>% length() # 266

all_target_df_old <- all_target_df
all_target_df <- all_target_df_clean

p4 <- ggplot(data = all_target_df) +
  geom_point(aes(x = date, y = mean.red), size = 0.5) +
  facet_wrap(~Borough_Name, ncol = 19) +
  theme_bw()
ggsave(paste0(output_prefix, "_ref_targets_nyc_ts_red_clean.png"), p4, width = 34, height = 22, units = "in")

p5 <- ggplot(data = all_target_df) +
  geom_point(aes(x = date, y = mean.nir), size = 0.5) +
  facet_wrap(~Borough_Name, ncol = 19) +
  theme_bw()
ggsave(paste0(output_prefix, "_ref_targets_nyc_ts_nir_clean.png"), p5, width = 34, height = 22, units = "in")

p6 <- ggplot(data = all_target_df) +
  geom_point(aes(x = date, y = ndvi), size = 0.5) +
  facet_wrap(~Borough_Name, ncol = 19) +
  theme_bw()
ggsave(paste0(output_prefix, "_ref_targets_nyc_ts_ndvi_clean.png"), p6, width = 34, height = 22, units = "in")

# General setup
# Referencing single image, 20220710
ref_date <- all_target_df[which(all_target_df$date == ymd("20220710")),]

# Test with another image
test_date <- all_target_df[which(all_target_df$date == ymd("20200821")),]

ref_test_merged <- merge(test_date, ref_date, by = "Name")

# can add more regressions here
ggplot(ref_test_merged, aes(x = mean.nir.x, y = mean.nir.y)) + 
  geom_point() +
  geom_text(aes(label = Name)) +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = "Test: 2020-08-21 NIR", y = "Ref: 2022-07-10 NIR") +
  theme_bw()
ggsave(paste0(output_prefix, "_regression_example.png"), width = 12, height = 8, units = "in")

fit <- lm(ref_test_merged$mean.nir.y ~ ref_test_merged$mean.nir.x)
summary(fit)

# Empirical line correction
# Picking complete image from the SuperDove cycle so we can also use it for the 8 band images if we choose to do so
# Ideally in the mid spring since that's the green up we want to model best
# 20220710 has complete coverage over all NYC and is very clear and close to the summer solstice, it's SuperDove, let's do it

# for final version of empirical line correction, will need to use targets across the entire city at once so that it's consistent calibration everywhere

# To setup, do regressions for each image, band - this will need to be revised for 8band variant
ref_date <- all_target_df[which(all_target_df$date == ymd("20220710")),]
#colname_list <- c("Name", "mean.blue", "mean.green", "mean.red", "mean.nir", "img_file", "date")
if (num_bands == 4){
  band_list <- c("blue", "green", "red", "nir")
  band_list_x <- c("mean.blue.x", "mean.green.x", "mean.red.x", "mean.nir.x")
  band_list_y <- c("mean.blue.y", "mean.green.y", "mean.red.y", "mean.nir.y")
} else if (num_bands == 8){
  band_list <- c("coastal_blue", "blue", "green_i", "green", "yellow", "red", "rededge", "nir")
  band_list_x <- c("mean.coastal_blue.x", "mean.blue.x", "mean.green_i.x", "mean.green.x", "mean.yellow.x", "mean.red.x", "mean.rededge.x", "mean.nir.x")
  band_list_y <- c("mean.coastal_blue.y", "mean.blue.y", "mean.green_i.y", "mean.green.y", "mean.yellow.y", "mean.red.y", "mean.rededge.y", "mean.nir.y")
} else {
  print("Needs to be either 4 or 8 bands")
}

# output setup
output_colnames <- c("img_file", "date", "band", "n", "slope", "intercept", "r2", "pvalue")
output_empline <- data.frame(matrix(data = NA, nrow = length(img_list)*num_bands, ncol = length(output_colnames)))
colnames(output_empline) <- output_colnames

output_rowind <- 1
for (img_ind in 1:length(img_list)){
  #for (img_ind in 1:10){
  #img_ind <- 1
  print(img_ind)
  test_date <- all_target_df[which(all_target_df$img_file == img_list[img_ind]),]
  if (nrow(test_date) < 2){
    output_empline$img_file[output_rowind:(output_rowind + (length(num_bands)-1))] <- img_list[img_ind]
    output_empline$band[output_rowind:(output_rowind + (length(num_bands)-1))] <- band_list
    output_empline[output_rowind:(output_rowind + (length(num_bands)-1)), c("date", "band", "n", "slope", "intercept", "r2", "pvalue")] <- NA
    
    #output_rowind <- output_rowind + 4 # this + 4 is related to number of bands
    output_rowind <- output_rowind + num_bands
  } else {
    ref_test_merged <- merge(test_date, ref_date, by = "Name") # test is x, ref is y
    
    for (band_ind in 1:length(band_list_x)){
      #band_ind <- 1
      x_vals <- ref_test_merged[,band_list_x[band_ind]]
      y_vals <- ref_test_merged[,band_list_y[band_ind]]
      # plotting if wanted
      # ggplot(ref_test_merged, aes(x = mean.blue.x, y = mean.blue.y)) + 
      #   geom_point() +
      #   geom_text(aes(label = Name)) +
      #   geom_smooth(method = "lm") +
      #   geom_abline(slope = 1, intercept = 0)
      fit <- lm(y_vals ~ x_vals)
      
      output_empline$img_file[output_rowind] <- img_list[img_ind]
      output_empline$date[output_rowind] <- test_date$date[1] # date format doesn't stick, will need to do it again afterwards
      output_empline$band[output_rowind] <- band_list[band_ind]
      output_empline$n[output_rowind] <- nrow(ref_test_merged)
      output_empline$intercept[output_rowind] <- fit$coefficients[1]
      output_empline$slope[output_rowind] <- fit$coefficients[2]
      output_empline$r2[output_rowind] <- summary(fit)$r.squared
      output_empline$pvalue[output_rowind] <- summary(fit)$coefficients[2,4] # slope p value
      
      output_rowind <- output_rowind + 1
    }
  }
}

output_empline$date <- as.Date(output_empline$date)

write.csv(output_empline, file = paste0(output_prefix, "_all_ref_targets_nyc_revised_empline.csv"), row.names = F)
# these are the empirical line corrections for each image
#####

n_targets <- unique(all_target_df_clean$Borough_Name) %>% length()

output_empline_cc <- output_empline[complete.cases(output_empline),]
# ggplot(output_empline_cc, aes(x = date, y = r2)) + 
#   geom_point(aes(alpha = n/n_targets)) + 
#   facet_wrap(~band)
# ggsave("all_ref_targets_nyc_v2_empline_r2_ts.png", width = 10, height = 8, units = "in")
# appears to have a seasonal cycle with r2 across all bands, highest values are usually in the middle of the summer

# Pick an r2 threshold, do a p-value threshold too?
r2_min <- 0.85
output_empline_gtr2min <- output_empline_cc[which(output_empline_cc$r2 > r2_min),]

# remove any images missing bands
output_empline_gtr2min$n_kept_bands <- NA
img_list_sub <- unique(output_empline_gtr2min$img_file)
for (i in 1:length(img_list_sub)){
  img_sub_inds <- which(output_empline_gtr2min$img_file == img_list_sub[i])
  output_empline_gtr2min$n_kept_bands[img_sub_inds] <- length(img_sub_inds)
}

output_empline_f <- output_empline_gtr2min[which(output_empline_gtr2min$n_kept_bands == num_bands),]
output_empline_f2 <- output_empline_f[which(output_empline_f$n >= 10),]
# minimum 10 points to do a regression

# ggplot(output_empline_f2, aes(x = date, y = r2)) + 
#   geom_point(aes(alpha = n/n_targets)) + 
#   facet_wrap(~band)
# ggsave("all_ref_targets_nyc_v2_empline_r2_ts_f2.png", width = 10, height = 8, units = "in")
# 
# ggplot(output_empline, aes(x = date, y = pvalue)) + 
#   geom_point(aes(alpha = n/n_targets)) + 
#   facet_wrap(~band)

setwd(output_target_refl_dir)
write.csv(output_empline_f2, paste0(output_prefix, "_nyc_daily_stack_highsunonly_calibrated_to_nyc_ground_ref_targets_revised_empline.csv"), row.names = F)

#####

# Apply empirical line corrections
# setup

setwd(output_target_refl_dir)
output_empline_f2 <- read.csv(paste0(output_prefix, "_nyc_daily_stack_highsunonly_calibrated_to_nyc_ground_ref_targets_revised_empline.csv")) # this will change between runs
output_empline_f2$date <- ymd(output_empline_f2$date)

# Loop through output_empline_f2 for each image
# apply slope and intercept for each band
# output each calibrated image (bronxcal20220710ref)

img_list_f2 <- unique(output_empline_f2$img_file)
cal_name <- "nyccal20220710ref"
setwd(path_planet_mosaics)
#band_list <- c("blue", "green", "red", "nir")

for (i in 1:3) { # test range
  #i <- 2
  img_prefix <- unlist(strsplit(img_list_f2[i], "\\."))[1]
  
  print(img_prefix)
  img <- rast(img_list_f2[i])
  output_empline_f2_sub <- output_empline_f2[which(output_empline_f2$img_file == img_list_f2[i]),]
  
  # calcCor <- function(img, output_empline_f2_sub, b){
  #   b_ind <- which(output_empline_f2_sub$band == b)
  #   slope <- output_empline_f2_sub$slope[b_ind]
  #   intercept <- output_empline_f2_sub$intercept[b_ind]
  #   new_img_layer <- round(img[[b]]*slope + intercept, 0)
  #   return(new_img_layer)
  # }
  
  calcCor <- function(img, output_empline_f2_sub, b){
    b_ind <- which(output_empline_f2_sub$band == b)
    slope <- output_empline_f2_sub$slope[b_ind]
    intercept <- output_empline_f2_sub$intercept[b_ind]
    new_img_layer <- round(img[[b]]*slope + intercept, 0)
    return(new_img_layer)
  }
  
  # 4 band
  img2 <- img
  img2[["blue"]] <- calcCor(img, output_empline_f2_sub, "blue")
  img2[["green"]] <- calcCor(img, output_empline_f2_sub, "green")
  img2[["red"]] <- calcCor(img, output_empline_f2_sub, "red")
  img2[["nir"]] <- calcCor(img, output_empline_f2_sub, "nir")
  
  # 8 band includes 4 band plus 4 more
  if (num_bands == 8){
    img2[["coastal_blue"]] <- calcCor(img, output_empline_f2_sub, "coastal_blue")
    img2[["green_i"]] <- calcCor(img, output_empline_f2_sub, "green_i")
    img2[["yellow"]] <- calcCor(img, output_empline_f2_sub, "yellow")
    img2[["rededge"]] <- calcCor(img, output_empline_f2_sub, "rededge")
  }
  
  #output_dir <- "/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_highsunonly_cal/"
  #output_dir <- "/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_4b_highsunonly_cal_v2_test/" # new output dir for tests
  output_dir <- output_target_refl_dir
  output_filename <- paste0(output_dir, "/", img_prefix, "_", cal_name, ".tif")
  
  img2[img2 > 10000] <- 10000 # make sure reflectance is capped at 10000 (1 x 10000) after calibrating
  img2[img2 < 0] <- 0 # make sure no reflectance is below zero after calibrating
  
  writeRaster(img2, filename = output_filename, datatype = "INT2S")
  
  print(paste0("Written out raster: ", output_filename))
  
  # Cleanup
  rm(img)
  rm(img2)
  gc()
  tmpFiles(remove=TRUE) # remove temporary files
}
