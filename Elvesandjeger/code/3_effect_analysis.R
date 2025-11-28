#ELVESANDJEGER EFFECT ANALYSIS

library(dplyr)
library(sf)
library(DHARMa)
library(lme4)
library(MASS)
library(tidyr)
library(pscl)
library(ggplot2)
library(glmmTMB)

#ANALYZE THE DATA
setwd("path")

site.obs <- st_read("./elvesandjeger_results_2025_obs.csv")
str(site.obs)

years <- 20:24

site.obs <- site_dens_sf #If already loaded from previous

for (yr in years) {
  site.obs[[paste0("larvae", yr)]] <-
    round(
      site.obs[[paste0("S1_", yr)]] +
        site.obs[[paste0("S2_", yr)]] +
        site.obs[[paste0("S3_", yr)]]
    )
}

head(site.obs)
str(site.obs)

site.obs <- site.obs %>%
  mutate(
    larvae20 = round((S1_20 + S2_20 + S3_20) * area_m2),
    larvae21 = round((S1_21 + S2_21 + S3_21) * area_m2),
    larvae22 = round((S1_22 + S2_22 + S3_22) * area_m2),
    larvae23 = round((S1_23 + S2_23 + S3_23) * area_m2),
    larvae24 = round((S1_24 + S2_24 + S3_24) * area_m2),
    larvae25 = round((S1_25 + S2_25 + S3_25) * area_m2)
  )

# Remove Follstad sites which are newly cleared in 2024
site.obs <- site.obs %>%
  filter(!grepl("Follstad", Lokalitet))

#Fix the lokalitet names
site.obs <- site.obs %>%
  mutate(
    site_name = str_remove(Lokalitet, "\\s+\\d+[a-zA-Z]?$"),  # Removes " 56" or " 56a"
    site_name = str_replace_all(site_name, c("C8" = "o", "C%" = "a", "C&" = "ae",
                                             "C" = "O", "C" = "A", "C" = "Ae"))
  )

site.obs <- site.obs %>%
  mutate(
    Luket_25 = case_when(
      Metode_25 == "Luking_delvis" ~ "Delvis",
      TRUE ~ Luket_25  
    )
  )

print(site.obs)
################################
###Model the effects

# Baseline
poisson_model <- glm(larvae25 ~ Luket_24, family = poisson, data = site.obs)
summary(poisson_model)
plot <- plot(simulateResiduals(poisson_model)) 
sim <- simulateResiduals(poisson_model)
testDispersion(sim)

#Check for overdispersion: Overdispersion can be detected by dividing the residual deviance by the degrees of freedom. 
#If this quotient is much greater than one, the negative binomial distribution should be used. 
deviance_ratio <- poisson_model$deviance / poisson_model$df.residual
print(deviance_ratio)
#Very overdispersed = 12.8771

#Check for zero inflation
observed_zeros <- sum(site.obs$larvae25 == 0)

# Number of predicted zeros from the Poisson model
predicted_zeros <- sum(predict(poisson_model, type = "response") < 1e-10)

# Print observed vs predicted zeros
cat("Observed zeros:", observed_zeros, "\n")
cat("Predicted zeros:", predicted_zeros, "\n") 
#The data are zero-inflated because there are 72 zeros where none are predicted

#Try a negative binomial model
m_nb <- glm.nb(larvae25 ~ Luket_24, data = site.obs)
summary(m_nb)

sim_nb <- simulateResiduals(m_nb)
plot(sim_nb)
testDispersion(sim_nb)

#Data is overdispersed and zero-inflated
# Convert Luket_24 from character to a factor
site.obs$Luket_25 <- as.factor(site.obs$Luket_25)
site.obs$Luket_24 <- as.factor(site.obs$Luket_24)

# Set "Nei" as the reference category
site.obs$Luket_25 <- relevel(site.obs$Luket_25, ref = "Nei")
site.obs$Luket_24 <- relevel(site.obs$Luket_24, ref = "Nei")

# Refit the zero-inflated negative binomial model
zinb_model <- glmmTMB(
  larvae25 ~ Luket_24,               
  ziformula = ~1,                     
  family = nbinom2, 
  data = site.obs
)

summary(zinb_model)

#Reshape data
df <- site.obs %>%
  st_drop_geometry() %>%
  dplyr::select(
    site_name, siteID,
    matches("^Luket_"), 
    matches("^Antall_"),
    matches("^larvae[0-9]{2}$")
  )
head(df)

# List the year suffixes
years <- c("20","21","22","23","24","25")

# Pivot longer for each of the three types of variables
# Pivot Luket
luket_long <- df %>%
  pivot_longer(
    cols = matches("^Luket_[0-9]{2}$"),
    names_to = "year_suffix",
    names_prefix = "Luket_",
    values_to = "Luket"
  )
head(luket_long)

# Pivot Antall
antall_long <- df %>%
  pivot_longer(
    cols = matches("^Antall_[0-9]{2}$"),
    names_to = "year_suffix",
    names_prefix = "Antall_",
    values_to = "Antall"
  )
head(antall_long)

# Pivot larvae
larvae_long <- df %>%
  pivot_longer(
    cols = matches("^larvae[0-9]{2}$"),
    names_to = "year_suffix",
    names_prefix = "larvae",
    values_to = "Larvae"
  )

luket <- luket_long %>%
  dplyr::select(site_name, siteID, Luket, year_suffix)

antall <- antall_long %>%
  dplyr::select(siteID, Antall, year_suffix)

larvae <- larvae_long %>%
  dplyr::select(siteID, Larvae, year_suffix)


# Join them all
long_data <- luket %>%
  left_join(antall,  by = c("siteID", "year_suffix")) %>%
  left_join(larvae, by = c("siteID", "year_suffix"))

print(long_data)

# Final cleanup
long_data <- long_data %>%
  mutate(
    year = as.integer(paste0("20", year_suffix)),
    Luket = factor(Luket, levels = c("Nei", "Delvis", "Ja"))
  ) %>%
  dplyr::select(site_name, siteID, year, Luket, Antall, Larvae, everything())

long_data <- long_data %>%
  mutate(
    Luket = factor(ifelse(is.na(Luket), "Nei", as.character(Luket)),
                   levels = c("Nei", "Delvis", "Ja")),
    Larvae = ifelse(is.na(Larvae) | is.nan(Larvae), 0, Larvae)
  )


# Check the result
head(long_data)
str(long_data)
unique(long_data$Luket)
sum(is.na(long_data$Luket))

write.csv(long_data, "./Elvesandjeger/elvesandjeger_results_2025_obs_long.csv", row.names = FALSE)

long_data <- read.csv("./Elvesandjeger/elvesandjeger_results_2025_obs_long.csv")

#Test effect of clearing over time
m1 <- glmmTMB(
  Larvae ~ Luket + (1 | siteID),
  family = nbinom2,
  data = long_data
)
summary(m1)

long_data <- long_data %>%
  arrange(siteID, year) %>%
  group_by(siteID) %>%
  mutate(Larvae_lag1 = lag(Larvae)) %>%
  ungroup()

lag_model <- glmmTMB(
  Larvae ~ Larvae_lag1 * Luket + (1 | siteID),
  family = nbinom2,
  data = long_data
)

summary(lag_model)  # Not significant


#Determine the effect of clearing history
#How many times has a site been cleared
site_clearing_summary <- long_data %>%
  filter(Luket %in% c("Ja", "Delvis")) %>%
  group_by(siteID) %>%
  summarise(
    last_cleared_summary = max(year),
    times_cleared_summary = n(),
    .groups = "drop"
  )

long_data <- long_data %>%
  left_join(site_clearing_summary, by = "siteID") %>%
  mutate(
    years_since_clearing = ifelse(!is.na(last_cleared_summary) & year >= last_cleared_summary,
                                  year - last_cleared_summary,
                                  NA_real_)
  )
print(long_data)
write.csv(long_data, "./Elvesandjeger/2025/Elvesandjeger_clearing_2025_long.csv")

long_data %>%
  filter(year == 2025, !is.na(last_cleared_summary)) %>%
  ggplot(aes(x = factor(last_cleared_summary), y = Larvae)) +
  geom_boxplot() +
  facet_wrap(~ site_name)+
  scale_y_log10() +
  labs(x = "Siste luket", 
       y = "Antall larver (log transformert)") +
  theme_minimal()

#How many times has a site been cleared
# Plot for 2025
long_data %>%
  filter(year == 2025, !is.na(times_cleared_summary)) %>%
  ggplot(aes(x = as.factor(times_cleared_summary), y = Larvae)) +
  geom_boxplot() +
  facet_wrap(~site_name)+
  scale_y_log10() +
  labs(x = "Antall ganger luket", 
       y = "Antall larver i 2025 (log transformert)") +
  theme_minimal()

check <- long_data %>%
  filter(year == 2025, times_cleared_summary == 3)
check

unique(long_data$site_name)
#Explore other ways to visualise the data

#Boxplots
#2025
# Plot
ggplot(obs25_presence, aes(x = Luket, y = Larvae)) +
  geom_boxplot() +
  scale_y_log10() +
  #facet_wrap(~site_name)+
  labs(x = "Luking i 2025", 
       y = "Antall larver i 2025") +
  theme_minimal()

plot_data <- long_data %>%
  dplyr::select(site_name, siteID, year, Larvae, Luket) %>%
  pivot_wider(
    id_cols = c(site_name, siteID),
    names_from = year,
    values_from = c(Larvae, Luket),
    names_sep = "_"
  ) %>%
  # Rename for clarity
  rename(
    Larvae = Larvae_2025,
    Luket_prev = Luket_2024
  )

# Order categories logically
plot_data$Luket_prev <- factor(plot_data$Luket_prev, levels = c("Nei", "Delvis", "Ja"))

# Plot
ggplot(plot_data, aes(x = Luket_prev, y = Larvae)) +
  geom_boxplot() +
  scale_y_log10() +
  #facet_wrap(~site_name) +
  labs(
    x = "Luking i 2024",
    y = "Antall larver i 2025",
    #title = "Effekt av luking i 2024 pC% larver i 2025"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )

#Write out data
st_write(site.obs, "elvesandjeger_clearing_history_2020-2024.csv", delete_layer = TRUE)

