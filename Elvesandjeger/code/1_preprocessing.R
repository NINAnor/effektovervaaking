#Elvesandjeger
#Script for preparing the data into a workable dataframe #Script is updated because Arne has taken over from Oddvar and uses different headers
#Author: Megan Nowell
#Date:2025

setwd("path")

# Load the libraries
library(readxl)
library(dplyr)
library(sf)

# Define the file path to your Excel file
file_path <- "./Elvesandjeger/2025/elvesandjeger_2025.xlsx"

# Read each sheet and add a month column
data_june <- read_excel(file_path, sheet = "jun25")

data_july <- read_excel(file_path, sheet = "jul25")

data_august <- read_excel(file_path, sheet = "aug25")


# Combine all sheets into a single dataframe
merged_data <- bind_rows(data_june, data_july, data_august)

# View the merged dataframe
print(merged_data)
nrow(merged_data)
str(merged_data)

#Replace zeros in observation columns
merged_data <- merged_data %>%
  mutate(
    `stadium 1` = coalesce(`stadium 1`, 0),
    `stadium 2` = coalesce(`stadium 2`, 0),
    `stadium 3` = coalesce(`stadium 3`, 0)
  )


str(merged_data)

#Change column names so that they don't start with a number
merged_data <- merged_data %>%
  rename(
    stad1 = `stadium 1`,
    stad2 = `stadium 2`,
    stad3 = `stadium 3`
  )

str(merged_data)
merged_data <- merged_data %>%
  select(stad1, stad2, stad3, lokalitet, month, lat, lon)
names(merged_data)

#Now the data is ready to be converted intro a spatial feature
#Remove any observations with no spatial information
merged_data <- merged_data %>%
  filter(!is.na(lon) & !is.na(lat)) #lat = N, long = E

# Convert the dataframe to an sf object using utm_N and utm_E as coordinates
elvesandjeger <- merged_data %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 25832)

# View the spatial dataframe
plot(st_geometry(elvesandjeger))
head(elvesandjeger)

#Write out data as a shapefile ##NOTE: dateTime field is altered to only date
st_write(elvesandjeger, "./Elvesandjeger/2025/elvesandjeger_2025.shp", append = FALSE)

#Write out only the data observations from August
august <- elvesandjeger %>%
  filter(month == "August")
str(august)
plot(st_geometry(august))

st_write(august,"./Elvesandjeger/2025/elvesandjeger_august_2025.shp", append = FALSE )
