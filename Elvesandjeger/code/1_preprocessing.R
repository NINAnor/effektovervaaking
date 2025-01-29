#Elvesandjeger
#Script for preparing the data into a workable dataframe
#Author: Megan Nowell
#Date:2024

# Load the libraries
library(readxl)
library(dplyr)
library(sf)
library(here)

# Define the file path to your Excel file
file_path <- here("Elvesandjeger/data/Cicindela-maritima_Gaula2024.xlsx")

# Read each sheet and add a month column
data_january <- read_excel(file_path, sheet = "juni") %>%
  mutate(month = "juni")

data_february <- read_excel(file_path, sheet = "juli") %>%
  mutate(month = "juli")

data_march <- read_excel(file_path, sheet = "august") %>%
  mutate(month = "august")

data_april <- read_excel(file_path, sheet = "sept") %>%
  mutate(month = "september")

# Combine all sheets into a single dataframe
merged_data <- bind_rows(data_january, data_february, data_march, data_april)

# View the merged dataframe
print(merged_data)
nrow(merged_data)
str(merged_data)

#Replace zeros in observation columns
merged_data <- merged_data %>%
  mutate(
    `3st` = coalesce(`3st`, 0),
    `2st` = coalesce(`2st`, 0),
    `1st` = coalesce(`1st`, 0),
    Voksne = coalesce(Voksne, 0)
  )

str(merged_data)

#Change column names so that they don't start with a number
merged_data <- merged_data %>%
  rename(
    stad1 = `3st`,
    stad2 = `2st`,
    stad3 = `1st`
  )

str(merged_data)

#Now the data is ready to be converted intro a spatial feature
#Remove any observations with no spatial information
merged_data <- merged_data %>%
  filter(!is.na(utm_N) & !is.na(utm_E))

# Convert the dataframe to an sf object using utm_N and utm_E as coordinates
elvesandjeger <- merged_data %>%
  st_as_sf(coords = c("utm_E", "utm_N"), crs = 25832)

# View the spatial dataframe
plot(st_geometry(elvesandjeger))

#Write out data as a shapefile ##NOTE: dateTime field is altered to only date
st_write(elvesandjeger, "./elvesandjeger_2024.shp")

#Write out only the data observations from August
august <- elvesandjeger %>%
  filter(month == "august")
str(august)

st_write(august,"./elvesandjeger_august_2024.shp" )
