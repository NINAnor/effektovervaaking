#Elvebreddedderkopp 2025

library(sf)        # spatial vector data
library(dplyr)     # data manipulation
library(ggplot2)   # plotting
library(terra)     # optional, alternative spatial tools
library(lme4)      # mixed models (if needed)
library(MASS)      # negative binomial models

# Define working directory
setwd("path")

spiders <- st_read("./Elevebreddedderkopp/2025/Elvebreddedderkopp_2025.shp")
sites <- st_read("./Lupinluking_2025/Lupinluking_2025_MidNat/Lupinluking_2025_MidNat.shp") %>%
  mutate(siteID = dense_rank(Lokalitet))
footprints <- st_read("../Digitizing/slitasje_2025.shp")

#Match the crs
sites <- st_transform(sites, st_crs(spiders))
footprints <- st_transform(footprints, st_crs(spiders))

names(spiders)
names(sites)
names(footprints)

sites <- sites %>%
  mutate(
    Luket_25 = ifelse(
      Luket_25 == "Ja" & Metode_25 == "Luking_delvis",
      "Delvis",
      Luket_25
    )
  )
unique(sites$Luket_25)

footprints_sf <- footprints %>%
  dplyr::select(Dist_binar) %>%
  rename(prints = Dist_binar)
print(footprints_sf)
footprints$prints <- as.integer(footprints$prints)
str(footprints)

#Calculate footprint intensity
spiders$spider_id <- seq_len(nrow(spiders))
# Find intersections between spiders and footprint cells
ints <- st_intersection(
  spiders %>% dplyr::select(spider_id),
  footprints_sf %>% dplyr::select(prints)   # keep only necessary attributes
)

# Sum 'prints' per spider polygon
fp_sum <- ints %>%
  st_drop_geometry() %>%   # remove geometry for clarity
  group_by(spider_id) %>%
  summarise(sum_prints = sum(prints, na.rm = TRUE))

spiders <- left_join(spiders, fp_sum, by = "spider_id")
print(spiders)



sites_slim <- sites %>%
  dplyr::select(
    Lokalitet,
    Antall_20, Luket_20,
    Antall_21, Luket_21,
    Antall_22, Luket_22,
    Antall_23, Luket_23,
    Antall_24, Luket_24,
    Antall_25, Luket_25, 
    siteID,
    geometry
  )
plot(st_geometry(sites_slim), col = 'red')
plot(st_geometry(spiders), add = TRUE, border = 'blue')

intersects <- lengths(st_intersects(spiders, sites_slim)) > 0
spiders$in_site <- intersects
sp_data <- st_join(spiders, sites_slim, join = st_intersects)
print(sp_data)
str(sp_data)

#Analyse the effect
#Select only the rows where there is both spider and clearing data
sp_clean <- sp_data %>%
  filter(in_site == TRUE)
table(sp_clean$Luket_24, useNA = "ifany")

sp_clean$Luket_24 <- factor(sp_clean$Luket_24)

sp_clean_pts <- sp_clean %>%
  st_centroid() %>%
  mutate(
    utm_E = st_coordinates(.)[,1],
    utm_N = st_coordinates(.)[,2]
  )
head(sp_clean_pts)

write.csv(sp_clean_pts, "./Elevebreddedderkopp/2025/elvebreddedderkopp_clearing_habitat_2025.csv" )
#Test for overdipsersion
m_voksne <- glm(voksne ~ Luket_24,
                data = sp_clean,
                family = poisson)
summary(m_voksne)

overdispersion <- sum(residuals(m_voksne, type = "pearson")^2) / m_voksne$df.residual
overdispersion #The data are very overdispersed

#Fit negative binomial model
sp_clean$Luket_24 <- relevel(factor(sp_clean$Luket_24), ref = "Nei")

m_voksne_nb <- glm.nb(voksne ~ Luket_24, data = sp_clean)
summary(m_voksne_nb)
overdispersion_nb <- sum(residuals(m_voksne_nb, type = "pearson")^2) / m_voksne_nb$df.residual
overdispersion_nb  #Model is a good fit now

m_juvenile_nb <- glm.nb(juvenile ~ Luket_24, data = sp_clean)
summary(m_juvenile_nb)


summary(m_juvenile_nb)
overdispersion_ju <- sum(residuals(m_juvenile_nb, type = "pearson")^2) / m_juvenile_nb$df.residual
overdispersion_ju

#Now test if other variables are having an effect
names(sp_clean)
sp_clean$substrat <- factor(sp_clean$substrat)
table(sp_clean$slitasje)
table(sp_clean$vegetasjon, useNA = "ifany")
unique(sp_clean$substrat)
table(sp_clean$substrat)


m_voksne_env <- glm.nb(
  voksne ~ substrat + slitasje + vegetasjon + Luket_24,
  data = sp_clean
)

summary(m_voksne_env)
sum(residuals(m_voksne_env, type = "pearson")^2) / m_voksne_env$df.residual


m_juvenile_env <- glm.nb(
  juvenile ~ substrat + slitasje + vegetasjon + Luket_24,
  data = sp_clean
)

summary(m_juvenile_env)
sum(residuals(m_juvenile_env, type = "pearson")^2) / m_juvenile_env$df.residual

#Plot the data
unique(sp_clean$dato)
sp_clean$Luket_25 <- factor(sp_clean$Luket_25, 
                            levels = c("Nei", "Delvis", "Ja"))

# Juvenile plot
ggplot(sp_clean, aes(x = Luket_24, y = juvenile)) +
  geom_boxplot(outlier.shape = 21, outlier.fill = "black") +
  scale_y_log10() +
  labs(
    x = "Luket i 2024",
    y = "Antall juveniler (2025, log-skala)",
    title = "Juveniler"
  ) +
  theme_bw(base_size = 20) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

sp_clean$slitasje <- factor(sp_clean$slitasje)
ggplot(sp_clean, aes(x = slitasje , y = juvenile)) +
  geom_boxplot(outlier.shape = 21, outlier.fill = "black") +
  scale_y_log10() +
  labs(
    x = "Slitasje",
    y = "Antall juveniler (2025, log-skala)",
    title = "Juveniler"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggplot(sp_clean, aes(x = substrat , y = juvenile)) +
  geom_boxplot(outlier.shape = 21, outlier.fill = "black") +
  scale_y_log10() +
  labs(
    x = "Substrat",
    y = "Antall juveniler (2025, log-skala)",
    title = "Juveniler"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

unique(sp_clean$vegetasjon)
ggplot(sp_clean, aes(x = vegetasjon , y = juvenile)) +
  geom_boxplot(outlier.shape = 21, outlier.fill = "black") +
  scale_y_log10() +
  labs(
    x = "Vegetasjonsdekning",
    y = "Antall juveniler (2025, log-skala)",
    title = "Juveniler"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

sp_clean$sum_prints.x <- as.numeric(sp_clean$sum_prints.x)
ggplot(sp_clean, aes(x = sum_prints.x , y = juvenile)) +
  geom_line(outlier.shape = 21, outlier.fill = "black") +
  scale_y_log10() +
  labs(
    x = "Forstyrelse",
    y = "Antall juveniler (2025, log-skala)",
    title = "Juveniler"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )
ggplot(sp_clean, aes(x = sum_prints.x, y = juvenile)) +
  geom_point() +
  scale_y_log10() +
  geom_smooth(method = "glm", method.args = list(family = "nb")) +
  theme_bw()

# Adult plot
ggplot(sp_clean, aes(x = Luket_24, y = voksne)) +
  geom_boxplot(outlier.shape = 21, outlier.fill = "black") +
  scale_y_log10() +
  labs(
    x = "Luket i 2024",
    y = "Antall voksne (2025, log-skala)",
    title = "Voksne"
  ) +
  theme_bw(base_size = 20) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggplot(sp_clean, aes(x = slitasje, y = voksne)) +
  geom_boxplot(outlier.shape = 21, outlier.fill = "black") +
  scale_y_log10() +
  labs(
    x = "Slitasje",
    y = "Antall voksne (2025, log-skala)",
    title = "Voksne"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggplot(sp_clean, aes(x = substrat, y = voksne)) +
  geom_boxplot(outlier.shape = 21, outlier.fill = "black") +
  scale_y_log10() +
  labs(
    x = "Substrat",
    y = "Antall voksne (2025, log-skala)",
    title = "Voksne"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggplot(sp_clean, aes(x = vegetasjon, y = voksne)) +
  geom_boxplot(outlier.shape = 21, outlier.fill = "black") +
  scale_y_log10() +
  labs(
    x = "Vegetasjonsdekning",
    y = "Antall voksne (2025, log-skala)",
    title = "Voksne"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )


#Is substrate correlated with lupin invasion
sp_clean$Antall_25_adj <- sp_clean$Antall_25 + 1

p <- ggplot(sp_clean, aes(x = substrat, y = Antall_25_adj)) +
  geom_boxplot(outlier.shape = 21, outlier.fill = "black") +
  scale_y_continuous(trans = "log10",
                     breaks = c(1, 10, 100, 1000),
                     labels = c("1", "10", "100", "1000")) +
  labs(
    x = "Substrat",
    y = "Antall lupin (log10(Antall_25 + 1))",
    title = "Antall_25 fordelt på substrat"
  ) +
  theme_bw(14)

print(p)


