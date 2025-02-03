# Create report figures of changes on elvesandjeger observations since 2015
# Step 5: 
#Note: figures don't render when using Norwegian characters

library(tidyverse)
library(sf)

long_obs <- st_read("./Elvesandjeger/data/elvesandjeger_obs_2015-2024.csv")
head(long_obs)
nrow(long_obs)

#Make sure columns are in correct format and spelling in consistent
str(long_obs)

long_obs <- long_obs %>%
  mutate(obs = as.integer(obs)) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(stage = str_replace(stage, "Voksne", "voksne")) %>%
  mutate(site = str_replace(site, "Storr", "Stor"))


# Calculate means and confidence intervals for each location and year
#Only looking at August observations since 2015 and excluding adults
aug_obs <- long_obs %>%
  filter(month == "august") %>%
  filter(stage != "larva") %>%
  filter(stage != "voksne") %>%
  group_by(site, year, stage) %>%
  summarise(
    Mean = mean(obs),
    Lower_CI = Mean - qt(0.975, df=n()-1) * sd(obs) / sqrt(n()),
    Upper_CI = Mean + qt(0.975, df=n()-1) * sd(obs) / sqrt(n())
  ) %>%
  ungroup()

# Plot
ggplot(aug_obs, aes(x = year, y = Mean, group = stage)) +
  geom_line() +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +
  facet_wrap(site ~ stage) + # Facet by location and stage with shared y-axis
  coord_cartesian(ylim = c(0, NA)) + # Keep y-axis above 0
  scale_x_continuous(breaks = seq(min(aug_obs$year), max(aug_obs$year), by = 1)) + # Ensure all years are shown
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + # Rotate x-axis text
  labs(x = 'Year', y = 'Antall individer')

#Ribbon plots
ggplot(aug_obs, aes(x = year, y = Mean, group = stage, color = stage, fill = stage)) +
  geom_line(size = 0.8) + # Lines for each stage
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, color = NA) + # Confidence intervals
  scale_y_log10() + # Logarithmic scale for y-axis
  facet_wrap(~ site, scales = "free_y") + # Facet by site
  theme_minimal(base_size = 15) + # Minimal theme with larger base font size
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), # Rotate x-axis labels
    strip.text = element_text(size = 12, face = "bold"), # Bold facet labels
    legend.position = "none" # Remove legend
  ) +
  labs(
    x = "Cr", # X-axis label (adjust as needed for language)
    y = "Antall individer" # Y-axis label
  )

#Line plot
# Create a named vector to map site and stage names to desired titles
site_labels <- c(
  "Forn" = "Fornes",
  "Grav" = "Gravraak",
  "Stor" = "Storronningen",
  "Kreg" = "Kregnesteigen"
)

stage_labels <- c(
  "larva" = "Larver",
  "stad1" = "Stadium 1",
  "stad2" = "Stadium 2",
  "stad3" = "Stadium 3",
  "voksne" = "Voksne"
)

# Plot with facets by both site and stage
ggplot(aug_obs, aes(x = year, y = Mean, group = stage)) +
  geom_line(size = 0.5, color = "black") + # Lines
  geom_point(size = 2, color = "black") + # Points
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "black") + # Error bars
  facet_grid(site ~ stage, 
             labeller = labeller(site = site_labels, stage = stage_labels)) + # Facet by site and stage
  theme_minimal(base_size = 14) + # Minimal theme
  theme(
    strip.text = element_text(size = 12, face = "bold"), # Bold facet labels
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
