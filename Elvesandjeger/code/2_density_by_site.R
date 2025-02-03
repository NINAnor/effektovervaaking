#Effektovervaaking - elvesandjeger
# Step 2: calculating the density of elvesandjeger observations in the clearing sites
#author: Megan Nowell
#date: 2024


#Load packages

library(dplyr)
library(sf)
library(raster)



#CALLING IN THE DATA
#A weighted density map was created for each elvesandjeger variable (i.e- stadium 1, stadium 2, stadium 3, all larvae, adults) using the observations from August each year. This was calculated using the "Point Density" tool in the ArcGIS spatial analyst toolbox with the variable of interest as the population field. The density maps were calculated with an output cell size of 2m and a circular neighbourhood radius of 20m. This means that the density of each cell is derived from the number of observations within 20m, divided by the area of the circle with that radius to give the number of observations per m2. This technique was chosen over the absolute number of observations to account for GPS geometric error in the observations. The "Point Density" tool also allows for the density to be weighted by a population field. This is important because the GPS coordinates do not represent a single observation (as is the normal input for heatmaps), but rather a count of all larvae or adults at that point. Therefore, a single point may represent ex. 1 larva hole or 1000.

#Set the working drive and call in the data. Check that the data are overlapping.
#From R studio server

#2024
sites <- st_read("./Elvesandjeger/data/Lupinluking_2024.shp")

s1.2020 <- raster("./Elvesandjeger/data/dens_stad1_2020.tif")
s2.2020 <- raster("./Elvesandjeger/data/dens_stad2_2020.tif")
s3.2020 <- raster("./Elvesandjeger/data/dens_stad3_2020.tif")
a.2020 <- raster("./Elvesandjeger/data/dens_adults_2020.tif")

s1.2021 <- raster("./Elvesandjeger/data/dens_stad1_2021.tif")
s2.2021 <- raster("./Elvesandjeger/data/dens_stad2_2021.tif")
s3.2021 <- raster("./Elvesandjeger/data/dens_stad3_2021.tif")
a.2021 <- raster("./Elvesandjeger/data/dens_adults_2021.tif")

s1.2022 <- raster("./Elvesandjeger/data/dens_stad1_2022.tif")
s2.2022 <- raster("./Elvesandjeger/data/dens_stad2_2022.tif")
s3.2022 <- raster("./Elvesandjeger/data/dens_stad3_2022.tif")
a.2022 <- raster("./Elvesandjeger/data/dens_adults_2022.tif")

s1.2023 <- raster("./Elvesandjeger/data/dens_stad1_2023.tif")
s2.2023 <- raster("./Elvesandjeger/data/dens_stad2_2023.tif")
s3.2023 <- raster("./Elvesandjeger/data/dens_stad3_2023.tif")
a.2023 <- raster("./Elvesandjeger/data/dens_adults_2023.tif")

s1.2024 <- raster("./Elvesandjeger/data/dens_stad1_2024.tif")
s2.2024 <- raster("./Elvesandjeger/data/dens_stad2_2024.tif")
s3.2024 <- raster("./Elvesandjeger/data/dens_stad3_2024.tif")
a.2024 <- raster("./Elvesandjeger/data/dens_adults_2024.tif") #no adult obs in Aug 2024

head(sites)

#PREPARATION OF THE DATAFRAME
#The number of observations per site needs to be calculated. This is done by calculating the average density per site and multiplying it by the area of the site. Note that only observations from August are included to avoid double counting the the larvae in different stages. 
#Create column called siteID because "Lokalitet" has Norwegian letters
sites <- sites %>%
  mutate(siteID = dense_rank(Lokalitet))

#Check that all IDs are unique
nrow(sites)
n_distinct(sites$siteID)
n_distinct(sites$Lokalitet)

#Add a site ID column
site.id <- sites %>%
  dplyr::select(siteID)

#Extract the mean density for each polygon in the sites
#The reason why I can't use a for loop here is because it overwrites the data from the previous stage/year
site.dens.s1.2020 <- raster::extract(s1.2020, site.id, fun = mean, sp = TRUE)
colnames(site.dens.s1.2020@data)[2]="S1_20"

site.dens.s2.2020 <- raster::extract(s2.2020, site.dens.s1.2020, fun = mean, sp = TRUE)
colnames(site.dens.s2.2020@data)[3]="S2_20"

site.dens.s3.2020 <- raster::extract(s3.2020, site.dens.s2.2020, fun = mean, sp = TRUE)
colnames(site.dens.s3.2020@data)[4]="S3_20"

site.dens.a.2020 <- raster::extract(a.2020, site.dens.s3.2020, fun = mean, sp = TRUE)
colnames(site.dens.a.2020@data)[ncol(site.dens.a.2020@data)]="A_20"

site.dens.s1.2021 <- raster::extract(s1.2021, site.dens.a.2020, fun = mean, sp = TRUE)
colnames(site.dens.s1.2021@data)[ncol(site.dens.s1.2021@data)]="S1_21"

site.dens.s2.2021 <- raster::extract(s2.2021, site.dens.s1.2021, fun = mean, sp = TRUE)
colnames(site.dens.s2.2021@data)[ncol(site.dens.s2.2021@data)]="S2_21"

site.dens.s3.2021 <- raster::extract(s3.2021, site.dens.s2.2021, fun = mean, sp = TRUE)
colnames(site.dens.s3.2021@data)[ncol(site.dens.s3.2021@data)]="S3_21"

site.dens.a.2021 <- raster::extract(a.2021, site.dens.s3.2021, fun = mean, sp = TRUE)
colnames(site.dens.a.2021@data)[ncol(site.dens.a.2021@data)]="A_21"

site.dens.s1.2022 <- raster::extract(s1.2022, site.dens.a.2021, fun = mean, sp = TRUE)
colnames(site.dens.s1.2022@data)[ncol(site.dens.s1.2022@data)]="S1_22"


site.dens.s2.2022 <- raster::extract(s2.2022, site.dens.s1.2022, fun = mean, sp = TRUE)
colnames(site.dens.s2.2022@data)[ncol(site.dens.s2.2022@data)]="S2_22"

site.dens.s3.2022 <- raster::extract(s3.2022, site.dens.s2.2022, fun = mean, sp = TRUE)
colnames(site.dens.s3.2022@data)[ncol(site.dens.s3.2022@data)]="S3_22"

site.dens.a.2022 <- raster::extract(a.2022, site.dens.s3.2022, fun = mean, sp = TRUE)
colnames(site.dens.a.2022@data)[ncol(site.dens.a.2022@data)]="A_22"

site.dens.s1.2023 <- raster::extract(s1.2023, site.dens.a.2022, fun = mean, sp = TRUE)
colnames(site.dens.s1.2023@data)[ncol(site.dens.s1.2023@data)]="S1_23"

site.dens.s2.2023 <- raster::extract(s2.2023, site.dens.s1.2023, fun = mean, sp = TRUE)
colnames(site.dens.s2.2023@data)[ncol(site.dens.s2.2023@data)]="S2_23"

site.dens.s3.2023 <- raster::extract(s3.2023, site.dens.s2.2023, fun = mean, sp = TRUE)
colnames(site.dens.s3.2023@data)[ncol(site.dens.s3.2023@data)]="S3_23"

site.dens.a.2023 <- raster::extract(a.2023, site.dens.s3.2023, fun = mean, sp = TRUE)
colnames(site.dens.a.2023@data)[ncol(site.dens.a.2023@data)]="A_23"

site.dens.s1.2024 <- raster::extract(s1.2024, site.dens.a.2023, fun = mean, sp = TRUE)
colnames(site.dens.s1.2024@data)[ncol(site.dens.s1.2024@data)]="S1_24"

site.dens.s2.2024 <- raster::extract(s2.2024, site.dens.s1.2024, fun = mean, sp = TRUE)
colnames(site.dens.s2.2024@data)[ncol(site.dens.s2.2024@data)]="S2_24"

site.dens.s3.2024 <- raster::extract(s3.2024, site.dens.s2.2024, fun = mean, sp = TRUE)
colnames(site.dens.s3.2024@data)[ncol(site.dens.s3.2024@data)]="S3_24"

site.dens.a.2024 <- raster::extract(a.2024, site.dens.s3.2024, fun = mean, sp = TRUE)
colnames(site.dens.a.2024@data)[ncol(site.dens.a.2024@data)]="A_24"

site.dens <- sites %>%
  left_join(site.dens.a.2024@data, by = "siteID", copy = TRUE)

#Check columns
colnames(site.dens)
str(site.dens)

#Add area
site.dens$area_m2 <- as.numeric(st_area(site.dens))

str(site.dens)


#Convert all average densities to number of observations per lokalitet
for (i in 23:42) {
  print(i)
  
  # Convert the character column to numeric
  site.dens[[i]] <- as.numeric(site.dens[[i]])
  
  # Multiply by area_m2 and round to the nearest integer
  site.dens[[i]] <- round(site.dens[[i]] * site.dens$area_m2)
}


head(site.dens)

st_write(site.dens, "./Elvesandjeger/data/elvesandjeger_results_2024_obs.csv", delete_layer = TRUE)

