# Step 4 Stor-elvebreddedderkopp
#Convert observation data to long format and merge with all observations since 2015
# This step is only necessary for producing the figures in the report

library(tidyverse)

#Call in data from step 1: 1_prepare_data.R
obs24 <- st_read("./Stor-elvebreddedderkopp/data/elvebreddedderkopp_obs_2024.shp") #dataframe from step 1
df <- st_read("./Stor-elvebreddedderkopp/data/elvebreddedderkopp_obs_2020-2023.shp") #Data from previous years
names(obs24)
names(df)

#Correct the northing and easting coordinates
obs24 <- obs24 %>%
  mutate(
    utm_N = st_coordinates(.)[, "Y"], # Northing (Y coordinate)
    utm_E = st_coordinates(.)[, "X"]  # Easting (X coordinate)
  )

names(obs24)

#Data is a shapefile, but we don't need the geometry
df24 <- st_drop_geometry(obs24)
df <- st_drop_geometry(df)

long_df24 <- df24 %>%
  # Rename columns to match new names
  rename(
    locality = Lokaltt,
    utm_sone = utm_son,
    date = Dato,
    adult = voksne,
    juvenile = juvenil
  )

names(long_df24)

#And only keep the columns of interest
long_df24 <- long_df24 %>%
  dplyr::select(Kommune, locality, utm_sone, date, adult, juvenile, month, 
                utm_N, utm_E)
head(long_df24)

names(df)
long_df <- df %>%
  dplyr::select(Kommune, locality, site, utm_sone, date, adult, juvenile, month, year,  
                utm_N, utm_E)
head(df)

#Pivot data from 2024
long_df24 <- long_df24 %>%  
  # Create a new 'site' column as a shortened form of locality
  mutate(site = substr(locality, 1, 4)) %>%
  # Pivot into long format
  pivot_longer(
    cols = c(adult, juvenile), # Explicitly specify the columns to pivot
    names_to = "stage",                   # Name of the new key column
    values_to = "obs"                     # Name of the new value column
  ) %>%
  # Add a 'year' column extracted from the date column
  mutate(
    year = as.integer(format(as.Date(date, "%d.%m.%Y"), "%Y")), # Extract year from date
    month = tolower(month) # Ensure month is lowercase to match target format
  )


head(long_df24)

#Pivot df for previous years
long_df <- long_df %>%
  pivot_longer(
    cols = c(adult, juvenile), # Explicitly specify the columns to pivot
    names_to = "stage",                   # Name of the new key column
    values_to = "obs"   )
head(df)

#Check data before merging
unique(long_df$locality)
unique(long_df24$locality)

Encoding(long_df24$locality) #Norwegian letters are a problem in R and need to be changed
#long_df24$locality <- iconv(long_df24$locality, from = "unknown", to = "UTF-8")

long_df24 <- long_df24 %>%
mutate(locality = case_when(
  locality == "Kregnesøra" ~ "Kregnesteigen", # Rename Kregnesøra to Kregnesteigen
  locality == "Gravråk M" ~ "Gravråk",       # Remove 'M' from Gravråk M
  locality == "Fornes M" ~ "Fornes",         # Remove 'M' from Fornes M
  TRUE ~ locality # Keep other names unchanged
))

# Check that the two dataframes match before merging
unique(long_df$month)
unique(long_df24$month)

unique(long_df$stage)
unique(long_df24$stage)

unique(long_df$site)
unique(long_df24$site)

long_df <- long_df %>%
  mutate(site = case_when(
    site == "Storr" ~ "Stor", # Change Storr to Stor
    TRUE ~ site # Keep other site names unchanged
  ))

#Ensure columns are same format
str(long_df)
str(long_df24)


long_df <- long_df %>%
  mutate(
    utm_sone = ifelse(utm_sone == "32N", "32V", utm_sone), # Standardize utm_sone
    date = as.Date(date), # Convert date to Date class
    year = as.integer(year),
    month = tolower(month)                                 # Ensure month names are lowercase
  )



long_df24 <- long_df24 %>%
  mutate(
    month = tolower(month),
    date = as.Date(date)) 

columns <- c("Kommune", "locality", "site", "utm_sone", "date", 
             "month", "year", "utm_N", "utm_E", "stage", "obs")

long_df <- long_df %>% dplyr::select(all_of(columns))
long_df24 <- long_df24 %>% dplyr::select(all_of(columns))   

str(long_df)
str(long_df24)

# Merge the two dataframes on common columns
all.obs <- full_join(long_df, long_df24)
head(all.obs)

unique(all.obs$year) #Should have a dataframe with data from 2020- 2024
class(all.obs)

#Write out newly merged dataframe
write.csv(all.obs, "./Stor-elvebreddedderkopp/data/elvesbreddedderkopp_obs_2020-2024.csv")

