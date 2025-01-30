
#title: Effektovervaaking - elvebreddedderkopp
#author: Megan Nowell
#date: 2024
#output: html_document


#Load packages
library(tidyverse)
library(dplyr)
library(sf)
library(raster)
library(tidyr)
library(here)

#A weighted density map was created for each 
#variable (i.e- voksne, juvenile) using the observations 
#from September each year. This was calculated using the 
#"Point Density" tool in the ArcGIS spatial analyst toolbox with the variable of interest as the population field. The density maps were calculated with an output cell size of 2m and a circular neighbourhood radius of 20m. This means that the density of each cell is derived from the number of observations within 20m, divided by the area of the circle with that radius to give the number of observations per m2. This technique was chosen over the absolute number of observations to account for GPS geometric error in the observations. The "Point Density" tool also allows for the density to be weighted by a population field. This is important because the GPS coordinates do not represent a single observation (as is the normal input for heatmaps), but rather a count of all larvae or adults at that point. Therefore, a single point may represent ex. 1 larva hole or 1000.


sites <- st_read("./Stor-elvebreddedderkopp/data//Lupinluking_2024.shp")

#Call in heatmaps
a.2020 <- raster("./Stor-elvebreddedderkopp/data/density_adults_sep2020.tif")
a.2021 <- raster("./Stor-elvebreddedderkopp/data//density_adults_sep2021.tif")
a.2022 <- raster("./Stor-elvebreddedderkopp/data//density_adults_sep2022.tif")
a.2023 <- raster("./Stor-elvebreddedderkopp/data//density_adults_sep2023.tif")
a.2024 <- raster("./Stor-elvebreddedderkopp/data//density_adults_sep2024.tif")

j.2020 <- raster("./Stor-elvebreddedderkopp/data//density_juveniles_sep2020.tif")
j.2021 <- raster("./Stor-elvebreddedderkopp/data//density_juveniles_sep2021.tif")
j.2022 <- raster("./Stor-elvebreddedderkopp/data//density_juveniles_sep2022.tif")
j.2023 <- raster("./Stor-elvebreddedderkopp/data//density_juveniles_sep2023.tif")
j.2024 <- raster("./Stor-elvebreddedderkopp/data//density_juveniles_sep2024.tif")


#PREPARATION OF THE DATAFRAME
#The number of observations per site needs to be calculated. This is done by calculating the average density per site and multiplying it by the area of the site. Note that only observations from August are included to avoid double counting the the larvae in different stages. 
str(sites)
sites <- sites %>%
  dplyr::mutate(siteID = dense_rank(Lokalitet))
names(sites)
head(sites)

site.id <- sites %>%
  dplyr::select(siteID)

site.dens.a.2020 <- raster::extract(a.2020, site.id, fun = mean, sp = TRUE)
colnames(site.dens.a.2020@data)[2]="a_20"

site.dens.a.2021 <- raster::extract(a.2021, site.dens.a.2020, fun = mean, sp = TRUE)
colnames(site.dens.a.2021@data)[3]="a_21"

site.dens.a.2022 <- raster::extract(a.2022, site.dens.a.2021, fun = mean, sp = TRUE)
colnames(site.dens.a.2022@data)[4]="a_22"

site.dens.a.2023 <- raster::extract(a.2023, site.dens.a.2022, fun = mean, sp = TRUE)
colnames(site.dens.a.2023@data)[ncol(site.dens.a.2023@data)]="a_23"

site.dens.a.2024 <- raster::extract(a.2024, site.dens.a.2023, fun = mean, sp = TRUE)
colnames(site.dens.a.2024@data)[ncol(site.dens.a.2024@data)]="a_24"

#Juveniles

site.dens.j.2020 <- raster::extract(j.2020, site.dens.a.2024, fun = mean, sp = TRUE)
colnames(site.dens.j.2020@data)[ncol(site.dens.j.2020@data)]="j_20"

site.dens.j.2021 <- raster::extract(j.2021, site.dens.j.2020, fun = mean, sp = TRUE)
colnames(site.dens.j.2021@data)[ncol(site.dens.j.2021@data)]="j_21"

site.dens.j.2022 <- raster::extract(j.2022, site.dens.j.2021, fun = mean, sp = TRUE)
colnames(site.dens.j.2022@data)[ncol(site.dens.j.2022@data)]="j_22"

site.dens.j.2023 <- raster::extract(j.2023, site.dens.j.2022, fun = mean, sp = TRUE)
colnames(site.dens.j.2023@data)[ncol(site.dens.j.2023@data)]="j_23"

site.dens.j.2024 <- raster::extract(j.2024, site.dens.j.2023, fun = mean, sp = TRUE)
colnames(site.dens.j.2024@data)[ncol(site.dens.j.2024@data)]="j_24"


#Merge
summary(site.dens.j.2024)

site.dens <- sites %>%
  left_join(site.dens.j.2024@data, by = "siteID", copy = TRUE)

#Check columns
str(site.dens)

#Add area
site.dens$area_m2 <- as.numeric(st_area(site.dens))

names(site.dens)

#Convert all average densities to number of observations per lokalitet
for (i in 23:32) {
  print(i)
  
  # Convert the character column to numeric
  site.dens[[i]] <- as.numeric(site.dens[[i]])
  
  # Multiply by area_m2 and round to the nearest integer
  site.dens[[i]] <- round(site.dens[[i]] * site.dens$area_m2)
}


st_write(site.dens, "./Stor-elvebreddedderkopp/data/elvebreddedderkopp_results_2024.csv")

