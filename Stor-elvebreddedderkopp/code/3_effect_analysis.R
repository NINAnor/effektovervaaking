#ELVEBREDDEDDERKOPP EFFECT ANALYSIS

library(dplyr)
library(sf)
library(DHARMa)
library(lme4)
library(MASS)
library(tidyr)
library(pscl)
library(ggplot2)

site.obs <- st_read("Stor-elvebreddedderkopp/data/elvebreddedderkopp_results_2024.csv")
head(site.obs)
str(site.obs)

site.obs <- site.obs %>%
  mutate(across(matches("^a_|^j_"), as.numeric))
str(site.obs)

# Remove Follstad sites which are newly cleared in 2024
unique(site.obs$Lokalitet)
site.obs <- site.obs %>%
  filter(!grepl("Follstad", Lokalitet))

################################
###Model the effects

# Baseline
p <- glm(a_24 ~ j_23*Luket_24, family = poisson, data = site.obs)
summary(p)
plot <- plot(simulateResiduals(p)) #The data are not suited to a glm
sim <- simulateResiduals(p)
testDispersion(sim)

#Check for overdispersion: Overdispersion can be detected by dividing the residual deviance by the degrees of freedom. 
#If this quotient is much greater than one, the negative binomial distribution should be used. 
deviance_ratio <- p$deviance / p$df.residual
print(deviance_ratio)
#1.5529 > 1.5 so switching to negative binomial

#Check for zero inflation
observed_zeros <- sum(site.obs$a_24 == 0)

# Number of predicted zeros from the Poisson model
predicted_zeros <- sum(predict(p, type = "response") < 1e-10)

# Print observed vs predicted zeros
cat("Observed zeros:", observed_zeros, "\n")
cat("Predicted zeros:", predicted_zeros, "\n") 
#The data are zero-inflated because there are 70 zeros where none are predicted

#Try a negative binomial model
nb_model <- glm.nb(a_24 ~ j_23 * Luket_24, data = site.obs)
summary(nb_model) # Doesn't work because can't establish a stable connection. Need to reduce complexity

#Data is overdispersed and zero-inflated
# Convert Luket_24 from character to a factor
site.obs$Luket_24 <- as.factor(site.obs$Luket_24)
site.obs$Luket_23 <- as.factor(site.obs$Luket_23)

# Set "Nei" as the reference category
site.obs$Luket_24 <- relevel(site.obs$Luket_24, ref = "Nei")
site.obs$Luket_23 <- relevel(site.obs$Luket_23, ref = "Nei")

# Refit the zero-inflated negative binomial model
luk_model_a <- zeroinfl(a_24 ~ Luket_24, dist = "negbin", data = site.obs)
luk_model_a

juv_model_j <- zeroinfl(j_24 ~ Luket_24, dist = "negbin", data = site.obs)
juv_model_j

juv_model <- zeroinfl(a_24 ~ j_23, dist = "negbin", data = site.obs)
juv_model

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

ggplot(site.history, aes(x = as.factor(times_cleared), y = a_24)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(x = "Antall ganger luket", 
       y = "Antall voksne i 2024 (log transformert)") +
  theme_minimal()

ggplot(site.history, aes(x = as.factor(times_cleared), y = j_24)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(x = "Antall ganger luket", 
       y = "Antall juveniler i 2024 (log transformert)") +
  theme_minimal()



#Boxplots
#Reorder the names in Luket_* columns and remove where Luket_* == NA
site.obs <- site.obs %>%
  mutate(across(starts_with("Luket_"), 
                ~ factor(.x, levels = c("Nei", "Delvis", "Ja"))))

#2024 adults
obs24_a <- site.obs %>%
  filter(!a_24 == 0)
ggplot(obs24_a, aes(x = Luket_24, y = a_24)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(x = "Luket i 2024", 
       y = "Antall voksne i 2024 (log transformert)") +
  theme_minimal()

#Juveniler 2024
obs24_j <- site.obs %>%
  filter(!j_24 == 0) 
ggplot(obs24_j, aes(x = Luket_24, y = j_24)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(x = "Luket i 2024", 
       y = "Antall juveniler i 2024 (log transformert)") +
  theme_minimal()


#Write out data
st_write(site.obs, "./Stor-elvebreddedderkopp/data/elvebreddedderkopp_clearing_history_2020-2024.csv", delete_layer = TRUE)

