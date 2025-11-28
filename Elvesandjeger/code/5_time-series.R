#Create report figures of changes on elvesandjeger observations since 2015
#Note: figures don't render when using Norwegian characters

library(tidyverse)
library(sf)

setwd("path")

elvesandjeger
elvesandjeger_all <- st_read("./Elvesandjeger/elvesandjeger_obs_2015-2025.csv")

#Clean original data
str(elvesandjeger_all)
elvesandjeger_all <- elvesandjeger_all %>%
  mutate(obs = as.numeric(obs))
unique(elvesandjeger_all$locality)

elvesandjeger_all <- elvesandjeger_all %>%
  mutate(
    locality = case_when(
      locality %in% c("Gravr<e5>k", "GravrC%k M") ~ "Gravrak",
      locality %in% c("Storr<f8>nningen", "StorrC8nningen") ~ "Storronningen",
      TRUE ~ locality
    )
  )

# Check the result
unique(elvesandjeger_all$locality)

#Reshape the data and merge it with the previous records
str(elvesandjeger)

elvesandjeger_long <- elvesandjeger %>%
  pivot_longer(
    cols = starts_with("stad"),
    names_to = "stage",
    values_to = "obs"
  ) %>%
  filter(obs > 0) %>%  # Only keep records with observations
  mutate(
    kommune = NA_character_,
    utm = "32V",
    date = as.Date(dato),
    year = 2025,
    site = lokalitet,
    N = sf::st_coordinates(.)[,2],
    E = sf::st_coordinates(.)[,1],
    Nr = NA
    
  ) %>%
  dplyr::select(kommune, locality = lokalitet, utm, N, E, date, month, year, site, stage, obs, Nr)
head(elvesandjeger_long) 

str(elvesandjeger_long)
unique(elvesandjeger_long$month)

elvesandjeger_long <- elvesandjeger_long %>%
  mutate(
    N = as.character(N),
    E = as.character(E),
    date = as.character(date),
    year = as.character(year),
    month = tolower(month)
  )

elvesandjeger_df <- bind_rows(elvesandjeger_all, elvesandjeger_long)
head(elvesandjeger_df)
unique(elvesandjeger_df$year)
unique(elvesandjeger_df$locality)
unique(elvesandjeger_df$month)

elvesandjeger_df <- elvesandjeger_df %>%
  mutate(
    month = case_when(
      month %in% c("june") ~ "juni",
      month %in% c("july") ~ "juli",
      TRUE ~ month
    )
  )
unique(elvesandjeger_df$month)

# Calculate means and confidence intervals for each location and year
#Only looking at August observations since 2015 and excluding adults
aug_obs <- elvesandjeger_df %>%
  filter(month == "august") %>%
  filter(stage != "larva") %>%
  filter(stage != "voksne") %>%
  filter(stage != "Voksne") %>%
  filter(!is.na(year)) %>%
  filter(!is.na(locality)) %>%
  group_by(locality, year, stage) %>%
  summarise(
    Mean = mean(obs),
    Lower_CI = Mean - qt(0.975, df=n()-1) * sd(obs) / sqrt(n()),
    Upper_CI = Mean + qt(0.975, df=n()-1) * sd(obs) / sqrt(n())
  ) %>%
  ungroup()

unique(aug_obs$year)

#Line plot
# Plot with facets by both site and stage
aug_obs$year <- as.numeric(as.character(aug_obs$year))

ggplot(aug_obs, aes(x = year, y = Mean, group = stage)) +
  geom_line(size = 0.5, color = "black") + 
  geom_point(size = 2, color = "black") + 
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "black") +
  facet_grid(locality ~ stage) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_x_continuous(breaks = 2015:2025) +  # <- Force correct year order
  labs(
    x = "Cr",
    y = "Antall individer (august)"
  ) +
  coord_cartesian(ylim = c(0, NA))


###
obs <- elvesandjeger_df %>%
  filter(stage != "larva") %>%
  filter(stage != "voksne") %>%
  filter(stage != "Voksne") %>%
  filter(!is.na(year)) %>%
  filter(!is.na(locality)) %>%
  group_by(locality, year, stage) %>%
  summarise(
    Mean = mean(obs),
    Lower_CI = Mean - qt(0.975, df=n()-1) * sd(obs) / sqrt(n()),
    Upper_CI = Mean + qt(0.975, df=n()-1) * sd(obs) / sqrt(n())
  ) %>%
  ungroup()

unique(obs$year)
write.csv(obs, "./Elvesandjeger/2025/Elvesandjeger_2015-2025_long.csv")

#Line plot (ALL YEARS)
# Plot with facets by both site and stage
obs$year <- as.numeric(as.character(obs$year))

ggplot(obs, aes(x = year, y = Mean, group = stage)) +
  geom_line(size = 0.5, color = "black") + 
  geom_point(size = 2, color = "black") + 
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "black") +
  facet_grid(locality ~ stage) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_x_continuous(breaks = 2015:2025) +  # <- Force correct year order
  labs(
    x = "Cr",
    y = "Antall individer"
  ) +
  coord_cartesian(ylim = c(0, NA))

#########################################################
#Looks at change across months
# Line plots of number of individuals per year with error bars
# Calculate means and confidence intervals for each location and year
summary_obs <- long_obs %>%
  filter(year == 2024) %>%
  filter(stage != "larva") %>%
  group_by(site, month, stage) %>%
  summarize(
    Mean = mean(obs),
    Lower_CI = Mean - qt(0.975, df=n()-1) * sd(obs) / sqrt(n()),
    Upper_CI = Mean + qt(0.975, df=n()-1) * sd(obs) / sqrt(n())
  ) %>%
  ungroup()

unique(summary_obs$month)
months_in_order <- c("juni", "juli", "august", "september")

# Convert 'month' to a factor with specified levels and order
summary_obs <- summary_obs %>%
  mutate(month_number = factor(month, levels = months_in_order))

# Plot
ggplot(summary_obs, aes(x = month_number, y = Mean, group = stage)) +
  geom_line() +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +
  facet_wrap(site ~ stage, scales = 'free_y', labeller = labeller(site = site_labels, stage = stage_labels)) + # Facet by location, allow different y scales
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = 'Maaned 2024', y = 'Antall individer')




