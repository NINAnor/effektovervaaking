#Create report figures of changes on stor elvebreddedderkopp observations since 2015
#Note: figures don't render when using Norwegian characters
#Step 5 time-series. Convert data to long format first (step 4)

library(tidyverse)
library(sf)

long_obs <- st_read("./Stor-elvebreddedderkopp/data/elvebreddedderkopp_obs_2020-2024.csv")
long_obs <- all.obs #If already loaded from step 4
head(long_obs)
nrow(long_obs)

#Make sure columns are in correct format and spelling in consistent
str(long_obs)

long_obs <- long_obs %>%
  mutate(obs = as.integer(obs)) %>%
  mutate(year = as.numeric(year)) 


# Calculate means and confidence intervals for each location and year
#Only looking at Sep observations since 2020 and excluding adults
sep_obs <- long_obs %>%
  filter(month == "september") %>%
  group_by(site, year, stage) %>%
  summarise(
    Mean = mean(obs),
    Lower_CI = Mean - qt(0.975, df=n()-1) * sd(obs) / sqrt(n()),
    Upper_CI = Mean + qt(0.975, df=n()-1) * sd(obs) / sqrt(n())
  ) %>%
  ungroup()

# Plot
ggplot(sep_obs, aes(x = year, y = Mean, group = stage)) +
  geom_line() +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +
  facet_wrap(~ site + stage, ncol = 4) + # Facet by location and stage with shared y-axis
  coord_cartesian(ylim = c(0, NA)) + # Keep y-axis above 0
  scale_x_continuous(breaks = seq(min(sep_obs$year), max(sep_obs$year), by = 1)) + # Ensure all years are shown
  #theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + # Rotate x-axis text
  labs(x = 'Aar', y = 'Antall individer')



#Line plot
# Create a named vector to map site and stage names to desired titles
site_labels <- c(
  "Forn" = "Fornes",
  "Grav" = "Gravraak",
  "Stor" = "Storronningen",
  "Kreg" = "Kregnesteigen"
)


# Plot with facets by both site and stage
ggplot(sep_obs, aes(x = year, y = Mean, group = stage)) +
  geom_line(size = 0.5, color = "black") + # Lines
  geom_point(size = 2, color = "black") + # Points
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "black") + # Error bars
  facet_grid(site ~ stage, 
             labeller = labeller(site = site_labels, stage = stage_labels)) + # Facet by site and stage
  theme_minimal(base_size = 14) + # Minimal theme
  theme(
    strip.text = element_text(linewidth = 12, face = "bold"), # Bold facet labels
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) # Rotate x-axis labels
  ) +
  labs(
    x = "Cr", # X-axis label
    y = "Antall individer" # Y-axis label
  ) +
  coord_cartesian(ylim = c(0, NA)) # Ensure y-axis starts at 0

#########################################################
#Looks at change across months
# Line plots of number of individuals per year with error bars
# Calculate means and confidence intervals for each location and year
summary_obs <- long_obs %>%
  filter(year == 2024) %>%
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
  facet_wrap(site ~ stage, ncol= 4, scales = 'free_y', labeller = labeller(site = site_labels)) + # Facet by location, allow different y scales
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = 'Maaned 2024', y = 'Antall individer')




