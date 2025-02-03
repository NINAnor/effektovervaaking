# Elvesandjeger
# Step 4: Convert observation data to long format and merge with all observations since 2015
#Author: Megan Nowell
# Data: 2024

library(tidyverse)

#Call in data from step 1: 1_prepare_data.R
obs24 <- st_read("./Elvesandjeger/data/elvesandjeger_2024.shp") #generated in step 1
df <- st_read("./Elvesandjeger/data/elvesandjeger_long.csv") #historical data

#Correct the northing and easting coordinates
obs24 <- obs24 %>%
  mutate(
    N = st_coordinates(.)[, "Y"], # Northing (Y coordinate)
    E = st_coordinates(.)[, "X"]  # Easting (X coordinate)
  )

head(obs24)
head(df)

#Data is a shapefile, but we don't need the geometry
df24 <- st_drop_geometry(obs24)
class(df24)
names(df24)

long_df24 <- df24 %>%
  # Rename columns to match new names
  rename(
    kommune = Kommune,
    locality = Lokalitet,
    utm = utm_sone,
    date = Dato,
    larva = Sum.larver
  ) %>%
  #And only keep the columns of interest
  dplyr::select(-`GPS.OH`, -`GPS.AL`)

head(long_df24)

long_df24 <- long_df24 %>%  
  # Create a new 'site' column as a shortened form of locality
  mutate(site = substr(locality, 1, 4)) %>%
  # Pivot stad1, stad2, stad3, and Voksne into long format
  pivot_longer(
    cols = c(stad1, stad2, stad3, larva, Voksne), # Explicitly specify the columns to pivot
    names_to = "stage",                   # Name of the new key column
    values_to = "obs"                     # Name of the new value column
  ) %>%
  # Add a 'year' column extracted from the date column
  mutate(
    year = as.integer(format(as.Date(date, "%d.%m.%Y"), "%Y")), # Extract year from date
    month = tolower(month) # Ensure month is lowercase to match target format
  )


head(long_df24)
head(df)

#Check data before merging
unique(df$locality)
unique(long_df24$locality)

unique(df$month)
unique(long_df24$month)

unique(df$stage)
unique(long_df24$stage)

unique(df$obs)
unique(long_df24$obs)

#Ensure columns are same format
str(df)
str(long_df24)

df <- df %>%
  mutate(E = as.numeric(E))

long_df24 <- long_df24 %>%
  mutate(E = as.numeric(E))

df <- df %>%
  mutate(N = as.numeric(N))

long_df24 <- long_df24 %>%
  mutate(N = as.numeric(N))

df <- df %>%
  mutate(obs = as.integer(obs))

long_df24 <- long_df24 %>%
  mutate(obs = as.integer(obs))

df <- df %>%
  mutate(year = as.integer(year))

long_df24 <- long_df24 %>%
  mutate(year = as.integer(year))

df <- df %>%
  mutate(date = as.Date(sub(" .*", "", date), format = "%d.%m.%Y"))

long_df24 <- long_df24 %>%
  mutate(date = as.Date(date)) # If it's already in "YYYY-MM-DD", this works directly

str(df)
str(long_df24)

# Merge the two dataframes on common columns
all.obs <- full_join(df, long_df24)
head(all.obs)

unique(all.obs$year) #Should include data from 2015 - 2024
class(all.obs)

#Write out newly merged dataframe
st_write(all.obs, "./Elvesandjeger/data/elvesandjeger_obs_2015-2024.csv")

