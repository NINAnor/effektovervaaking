#Script for preparing the data into a workable dataframe
#Elvebreddedderkopp
#Author: Megan
#Date: October 2024

# Load the libraries
library(readxl)
library(dplyr)
library(sf)

# Define the file path to your Excel file
file_path <- here("./Stor-elvesbreddedderkopp/data/Arctosa-cinerea_Gaula2024.xlsx")

# Read each sheet and add a month column
data_juni <- read_excel(file_path, sheet = "juni") %>%
  mutate(month = "juni")

data_juli <- read_excel(file_path, sheet = "juli") %>%
  mutate(month = "juli")

data_aug <- read_excel(file_path, sheet = "aug") %>%
  mutate(month = "august")

data_sept <- read_excel(file_path, sheet = "sept") %>%
  mutate(month = "september")

# Combine all sheets into a single dataframe
merged_data <- bind_rows(data_juni, data_juli, data_aug, data_sept)

# View the merged dataframe
print(merged_data)
nrow(merged_data)
str(merged_data)
names(merged_data)

#Replace zeros in observation columns
merged_data <- merged_data %>%
  mutate(
    `Voksne hunner` = coalesce(`Voksne hunner`, 0),
    `Juvenile stadier` = coalesce(`Juvenile stadier`, 0)
  )

str(merged_data)

#Change column names so that they don't start with a number
merged_data <- merged_data %>%
  rename(
    voksne = `Voksne hunner`,
    juvenile = `Juvenile stadier`
  )

str(merged_data)

#Now the data is ready to be converted intro a spatial feature
#Remove any observations with no spatial information
merged_data <- merged_data %>%
  filter(!is.na(utm_N) & !is.na(utm_E))

# Convert the dataframe to an sf object using utm_N and utm_E as coordinates
edderkopp <- merged_data %>%
  st_as_sf(coords = c("utm_E", "utm_N"), crs = 25832)

# View the spatial dataframe
plot(st_geometry(edderkopp))

#Write out data as a shapefile ##NOTE: dateTime field is altered to only date
st_write(edderkopp, "./Stor-elvesbreddedderkopp/data/elvebreddedderkopp_obs_2024.shp")

#Write out only the data observations from August

unique(edderkopp$month)
sept <- edderkopp %>%
  filter(month == "september")
str(sept)

st_write(sept,"./elvebreddedderkopp_sept_2024.shp" )
