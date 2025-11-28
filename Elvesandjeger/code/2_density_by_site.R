#Effektovervaaking - elvesandjeger
# Step 2: calculating the density of elvesandjeger observations in the clearing sites
#date: "2025"

# --- SETUP ---
# Define working directory
setwd("path")

#Load packages
library(dplyr)
library(sf)
library(terra)

#CALLING IN THE DATA
#A weighted density map was created for each elvesandjeger variable (i.e- stadium 1, stadium 2, stadium 3, all larvae, adults) 
#using the observations from August each year. This was calculated using the "Point Density" tool in the ArcGIS spatial analyst toolbox 
#with the variable of interest as the population field. The density maps were calculated with an output cell size of 2m and a 
#circular neighbourhood radius of 20m. This means that the density of each cell is derived from the number of observations within 20m, 
#divided by the area of the circle with that radius to give the number of observations per m2. This technique was chosen over the 
#absolute number of observations to account for GPS geometric error in the observations. The "Point Density" tool also allows for the 
#density to be weighted by a population field. This is important because the GPS coordinates do not represent a single observation 
#(as is the normal input for heatmaps), but rather a count of all larvae or adults at that point. Therefore, a single point may represent ex. 1 larva hole or 1000.

# Read site polygons and create siteID
sites <- st_read("./Lupinluking_2025/Lupinluking_2025_MidNat/Lupinluking_2025_MidNat.shp") %>%
  mutate(siteID = dense_rank(Lokalitet))

# EPSG:25832 (ETRS89 / UTM Zone 32N)
target_crs <- "EPSG:25832"

# Convert site polygons to SpatVector and reproject
sites_vect <- vect(sites) %>%
  project(target_crs)

# Function to load all rasters for one year as a named list
# When loading rasters, force CRS to EPSG:25832
load_rasters <- function(year) {
  list(
    s1 = project(rast(paste0("./Elvesandjeger/Heatmaps/dens_stad1_", year, ".tif")), target_crs),
    s2 = project(rast(paste0("./Elvesandjeger/Heatmaps/dens_stad2_", year, ".tif")), target_crs),
    s3 = project(rast(paste0("./Elvesandjeger/Heatmaps/dens_stad3_", year, ".tif")), target_crs)
  )
}

# Function to extract mean values and rename columns
extract_yearly_densities <- function(rasters, site_data, year_suffix) {
  names_map <- c("s1" = paste0("S1_", year_suffix),
                 "s2" = paste0("S2_", year_suffix),
                 "s3" = paste0("S3_", year_suffix)
  )
  
  for (layer in names(rasters)) {
    extracted <- terra::extract(rasters[[layer]], site_data, fun = mean, na.rm = TRUE)
    site_data[[names_map[[layer]]]] <- extracted[[2]]
  }
  site_data
}

# --- PROCESSING ---

# Start with base SpatVector
site_dens <- sites_vect

# Years to process
years <- 2020:2025

# Loop through each year and append results
for (yr in years) {
  rasters <- load_rasters(yr)
  site_dens <- extract_yearly_densities(rasters, site_dens, substr(yr, 3, 4))
}

site_dens$area_m2 <- as.numeric(expanse(site_dens, unit = "m"))
site_dens_df <- as.data.frame(site_dens)

# Identify density columns
density_cols <- grep("^S[123]_", names(site_dens_df), value = TRUE)

# Multiply by area and round
site_dens_df[density_cols] <- lapply(density_cols, function(col) {
  round(as.numeric(site_dens_df[[col]]) * site_dens_df$area_m2)
})


# --- EXPORT ---

# Convert back to sf for export as shapefile
site_dens_sf <- st_as_sf(site_dens)

# Export results
#st_write(site_dens_sf, "./Elvesandjeger/elvesandjeger_density_2025.shp", delete_layer = TRUE)
write.csv(site_dens_df, "./Elvesandjeger/elvesandjeger_results_2025_obs.csv", row.names = FALSE)
