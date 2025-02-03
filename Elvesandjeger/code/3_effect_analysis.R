#Effektovervaaking - elvesandjeger
# Step 3: modelling of effect of lupine clearing on elvesandjeger populations
#author: Megan Nowell
#date: 2024

library(dplyr)
library(sf)
library(DHARMa)
library(lme4)
library(MASS)
library(tidyr)
library(pscl)
library(ggplot2)

#ANALYZE THE DATA
site.obs <- st_read("Elvesandjeger/data/elvesandjeger_results_2024_obs.csv")
str(site.obs)

site.obs <- site.obs %>%
  mutate(across(matches("^S[1-3]_|^A_"), as.numeric))
str(site.obs)

site.obs <- site.obs %>%
  mutate(larvae20 = S1_20+S2_20+S3_20) %>%
  mutate(larvae21 = S1_21+S2_21+S3_21) %>%
  mutate(larvae22 = S1_22+S2_22+S3_22) %>%
  mutate(larvae23 = S1_23+S2_23+S3_23) %>%
  mutate(larvae24 = S1_24+S2_24+S3_24)

site.obs <- site.obs %>%
  mutate(larvae20 = round(larvae20)) %>%
  mutate(larvae21 = round(larvae21)) %>%
  mutate(larvae22 = round(larvae22)) %>%
  mutate(larvae23 = round(larvae23)) %>%
  mutate(larvae24 = round(larvae24))

# Remove Follstad sites which are newly cleared in 2024
site.obs <- site.obs %>%
  filter(!grepl("Follstad", Lokalitet))

################################
###Model the effects

# Baseline
poisson_model <- glm(larvae24 ~ larvae24*Luket_24, family = poisson, data = site.obs)
summary(poisson_model)
plot <- plot(simulateResiduals(poisson_model)) #The data are not suited to a glm
sim <- simulateResiduals(poisson_model)
testDispersion(sim)

#Check for overdispersion: Overdispersion can be detected by dividing the residual deviance by the degrees of freedom. 
#If this quotient is much greater than one, the negative binomial distribution should be used. 
deviance_ratio <- poisson_model$deviance / poisson_model$df.residual
print(deviance_ratio)
#Very overdispersed = 30.8

#Check for zero inflation
observed_zeros <- sum(site.obs$larvae24 == 0)

# Number of predicted zeros from the Poisson model
predicted_zeros <- sum(predict(poisson_model, type = "response") < 1e-10)

# Print observed vs predicted zeros
cat("Observed zeros:", observed_zeros, "\n")
cat("Predicted zeros:", predicted_zeros, "\n") 
#The data are zero-inflated because there are 56 zeros where none are predicted

#Try a negative binomial model
nb_model <- glm.nb(larvae24 ~ larvae23 * Luket_24, data = site.obs)
summary(nb_model)

#Data is overdispersed and zero-inflated
# Convert Luket_24 from character to a factor
site.obs$Luket_24 <- as.factor(site.obs$Luket_24)
site.obs$Luket_23 <- as.factor(site.obs$Luket_23)

# Set "Nei" as the reference category
site.obs$Luket_24 <- relevel(site.obs$Luket_24, ref = "Nei")
site.obs$Luket_23 <- relevel(site.obs$Luket_23, ref = "Nei")

# Refit the zero-inflated negative binomial model
zinb_model <- zeroinfl(larvae24 ~ Luket_23, dist = "negbin", data = site.obs)
zinb_model

#Determine the effect of clearing history
#How many times has a site been cleared
site.obs <- site.obs %>%
  mutate(Luket_count = rowSums(across(c(Luket_20, Luket_21, Luket_22, Luket_23, Luket_24), ~ . == "Ja")))
str(site.obs)

#When was site last cleared
site.obs <- site.obs %>%
  mutate(last_cleared = case_when(
    Luket_24 %in% c("Ja", "Delvis") ~ 2024,
    Luket_23 %in% c("Ja", "Delvis") ~ 2023,
    Luket_22 %in% c("Ja", "Delvis") ~ 2022,
    Luket_21 %in% c("Ja", "Delvis") ~ 2021,
    Luket_20 %in% c("Ja", "Delvis") ~ 2020,
    TRUE ~ NA_real_
  )) %>%
  mutate(years_last_clearing = 2024 - last_cleared)
str(site.obs)

#How many times has a site been cleared
site.history <- site.obs %>%
  mutate(times_cleared = rowSums(across(c(Luket_20, Luket_21, Luket_22, Luket_23, Luket_24), 
                                        ~ . %in% c("Ja", "Delvis"))))
site.history <- site.history %>%
  filter(times_cleared > 0)

ggplot(site.history, aes(x = as.factor(times_cleared), y = larvae24)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(x = "Antall ganger luket", 
       y = "Antall larver i 2024 (log transformert)") +
  theme_minimal()

#When was the site last cleared
site.obs <- site.obs %>%
  mutate(last_cleared = as.factor(last_cleared))

n_cleared_model <- zeroinfl(larvae24 ~ last_cleared, dist = "negbin", data = site.obs)
n_cleared_model

#Explore other ways to visualise the data

#Boxplots
#Reorder the names in Luket_* columns and remove where Luket_* == NA
site.obs <- site.obs %>%
  mutate(across(starts_with("Luket_"), 
                ~ factor(.x, levels = c("Nei", "Delvis", "Ja"))))

#2024
obs24_presence <- site.obs %>%
  filter(!larvae24 == 0)
ggplot(obs24_presence, aes(x = Luket_24, y = larvae24)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(x = "Luket i 2024", 
       y = "Antall larver i 2024 (log transformert)") +
  theme_minimal()

#2023
obs23_presence <- site.obs %>%
  filter(!larvae23 == 0) %>%
  filter(!Luket_23 == 'NA')
ggplot(obs23_presence, aes(x = Luket_23, y = larvae23)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(x = "Luket i 2023", 
       y = "Antall larver i 2023 (log transformert)") +
  theme_minimal()

#2022
obs22_presence <- site.obs %>%
  filter(!larvae22 == 0) %>%
  filter(!Luket_22 == 'NA')
ggplot(obs22_presence, aes(x = Luket_22, y = larvae22)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(x = "Luket i 2022", 
       y = "Antall larver i 2022 (log transformert)") +
  theme_minimal()

#2021 ### Only showing NA
obs21_presence <- site.obs %>%
  filter(!larvae21 == 0)
ggplot(obs21_presence, aes(x = Luket_21, y = larvae21)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(x = "Luket i 2021", 
       y = "Antall larver i 2021 (log transformert)") +
  theme_minimal()


#Write out data
st_write(site.obs, "./Elvesandjeger/data/elvesandjeger_clearing_history_2020-2024.csv", delete_layer = TRUE)

