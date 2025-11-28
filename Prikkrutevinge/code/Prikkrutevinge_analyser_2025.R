## Prikkrutevinge

library(readxl)
library(tidyverse)
library(lmerTest)
library(DHARMa)
library(emmeans)
library(vegan)
library(ggrepel)
library(ggplot2)
library(goeveg)
library(lmerTest)
library(MuMIn)
library(effects)

# importing data
dat <- readxl::read_excel("path/Effekt_prikkrutevinge_2025_cleaned.xlsx")

# Data calculations
#We want to calculate average vegetation height per plot, per year

dat <- dat %>% 
  group_by(year, plot_id2) %>% 
  rowwise() %>%
  mutate(vegheight_mean = mean(c_across(vegheight1:vegheight4), na.rm  = TRUE)) %>%
  ungroup() 

#We want to calculate the number of species in flower
dat <- dat %>% 
  group_by(year, plot_id2) %>%
  mutate(abundance_flowering = n_distinct(species[count_floral_stems > 0]))

# Calculate total number of flowers per plot and year
dat <- dat %>% 
  group_by(plot_id2,year) %>% 
  mutate(Total_count_flowers = sum(count_floral_stems, na.rm = TRUE))

# We want to separate monitoring plots from spinn plots for figures and analyses
dat <- dat %>%
  mutate(
    Spinnrute = if_else(
      grepl("S", plot_id2, ignore.case = TRUE),
      "Ja",
      "Nei"
    )
  )


#For each plot, we want to calculate how often it was mown between 2022 and 2025. 
#This requires some steps
#Remove species-level duplication by summarizing data at the plot-year level
plot_year_summary <- dat %>%
  group_by(plot_id2, year) %>%
  summarize(
    treatment_t0 = first(treatment_t0), # Or any appropriate rule (e.g., unique())
    treatment_t1 = first(treatment_t1),
    treatment_t2 = first(treatment_t2),
    treatment_t3 = first(treatment_t3),
    .groups = "drop"
  )

# Calculate the total treatments received for each plot
treatment_summary <- plot_year_summary %>%
  group_by(plot_id2) %>%
  mutate(
    treatment_received_until_2025 = sum(
      year == "2025" & c_across(starts_with("treatment_t")) == "slC%tt",
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# Merge the result back to the original dataset to retain all rows
dat <- dat %>%
  left_join(
    treatment_summary %>% select(year, plot_id2, treatment_received_until_2025),
    by = c("year", "plot_id2"))

dat <- dat %>%
  mutate(
    treatment_received_binary = if_else(
      year == 2022, 
      0,
      if_else(treatment_received_until_2025 > 0, 1, 0)
    )
  )

dat <- dat %>% 
  group_by(plot_id2,year) %>% 
  mutate(Presence_larvespinn = ifelse(count_spinn > 0, "Ja", "Nei"))

#Now we can use the number of treatments received until 2025 as a grouping variable in figures.

#For making the figures, we can aggregate and just keep the first species
dat_agg <- dat %>% 
  group_by(plot_id2, year) %>% 
  filter(!duplicated(plot_id2))

## but we also need to remove the circles
dat_agg <- dat_agg %>%
  filter(!grepl("sirkel",plot_id2,ignore.case=TRUE))


### What we want is to compare spinnruter with monitoring plots - in figures and statistics. 
# and to check if there are differences between monitoring plots with and without management

### Create figures
## figures for comparing spinn-ruter with monitoring plots

## Make figure for vegetation height
# Generate the plot
s.veg_height <- dat_agg %>% 
  group_by(year, Spinnrute) %>% 
  summarize(mean = mean(vegheight_mean), 
            sd = sd(vegheight_mean),
            n = n(),
            se = sd / sqrt(n)) %>% 
  ggplot(aes(x = as.factor(year), y = mean, fill = Spinnrute)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                width = 0.2, position = position_dodge(width=0.9)) +
  facet_wrap(~ Spinnrute) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank()) +
  guides(fill = guide_legend(title="")) +
  theme(legend.position="bottom") +
  scale_fill_manual(
    values = c("Ja" = "#004F71",      # blC%
               "Nei" = "#E57200"),    # oransje
    labels = c("Ja"  = "Spinnrute",
               "Nei" = "OvervC%kingsrute")
  ) +
  labs(x = "Cr", y = "VegetasjonshC8yde (cm)") +
  scale_y_continuous(expand = c(0, 0))


# Print the plot
s.veg_height

ggsave(plot = s.veg_height, filename = "path/prikkrutevinge_vegheight_spinn25.jpeg", width = 21, height = 14, units = "cm")

# test for differences

tapply(dat_agg$vegheight_mean,list(dat_agg$year,dat_agg$Spinnrute),mean)
tapply(dat_agg$vegheight_mean,list(dat_agg$year,dat_agg$Spinnrute),sd)

lm_height_s <- lmer(vegheight_mean ~ Spinnrute + factor(year) + (1|plot_id2), data=dat_agg)#
summary(lm_height_s)

# only monitoring plots
dat_height <- dat_agg %>% 
  filter(!grepl("S", as.character(plot_id2), ignore.case = TRUE)) %>% 
  select(polygon_id, plot_id2, year, vegheight_mean, treatment_received_until_2025, treatment_received_binary)

lm_height2 <- lm(data = dat_height[dat_height$year == 2025,], vegheight_mean ~ treatment_received_until_2025)
summary(lm_height2)

lm_height3 <- lm(data = dat_height[dat_height$year == 2025,], vegheight_mean ~ treatment_received_binary)
summary(lm_height3)


## Make figure for plantago
# Generate the plot
s.plantago <- dat_agg %>% 
  group_by(year, Spinnrute) %>% 
  summarize(mean = mean(count_plantago), 
            sd = sd(count_plantago),
            n = n(),
            se = sd / sqrt(n)) %>% 
  ggplot(aes(x = as.factor(year), y = mean, fill = Spinnrute)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                width = 0.2, position = position_dodge(width=0.9)) +
  facet_wrap(~ Spinnrute) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank()) +
  guides(fill = guide_legend(title="")) +
  theme(legend.position="bottom") +
  scale_fill_manual(
    values = c("Ja" = "#004F71",      # blC%
               "Nei" = "#E57200"),    # oransje
    labels = c("Ja"  = "Spinnrute",
               "Nei" = "OvervC%kingsrute")
  ) +
  labs(x = "Cr", y = "Antall smalkjemperosetter") +
  scale_y_continuous(expand = c(0, 0))

# Print the plot
s.plantago

ggsave(plot = s.plantago, filename = "path/prikkrutevinge_plantago_spinn25.jpeg", width = 21, height = 14, units = "cm")

# test for differences
tapply(dat_agg$count_plantago,list(dat_agg$year,dat_agg$Spinnrute),mean)
tapply(dat_agg$count_plantago,list(dat_agg$year,dat_agg$Spinnrute),sd)

lm_plantago_s <- glmer(count_plantago~Spinnrute + factor(year)+(1|plot_id2), data=dat_agg, family=poisson)
summary(lm_plantago_s)

# Generate the data for monitoring plots only
dat_host <- dat_agg %>% 
  filter(!grepl("S", as.character(plot_id2), ignore.case = TRUE)) %>% 
  select(polygon_id, plot_id2, year, count_plantago, treatment_received_until_2025,treatment_received_binary)

lm_host <- glmer(data = dat_host, count_plantago ~ as.factor(year) + (1| plot_id2),family = poisson)
summary(lm_host)

lm_host2 <- glm(data = dat_host[dat_host$year==2025,], count_plantago ~ treatment_received_binary,family = poisson)
summary(lm_host2)

lm_host3 <- glm(data = dat_host[dat_host$year==2025,], count_plantago ~ treatment_received_until_2025,family = poisson)
summary(lm_host3)


## Make figure for pollinator plants
# Generate the plot
s.pollinator <- dat_agg %>% 
  group_by(year, Spinnrute) %>% 
  summarize(mean = mean(Total_count_flowers), 
            sd = sd(Total_count_flowers),
            n = n(),
            se = sd / sqrt(n)) %>% 
  ggplot(aes(x = as.factor(year), y = mean, fill = Spinnrute)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                width = 0.2, position = position_dodge(width=0.9)) +
  facet_wrap(~ Spinnrute) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank()) +
  guides(fill = guide_legend(title="")) +
  theme(legend.position="bottom") +
  scale_fill_manual(
    values = c("Ja" = "#004F71",      # blC%
               "Nei" = "#E57200"),    # oransje
    labels = c("Ja"  = "Spinnrute",
               "Nei" = "OvervC%kingsrute")
  ) +
  labs(x = "Cr", y = "Antall blomsterstander av pollinatorplanter") +
  scale_y_continuous(expand = c(0, 0))

# Print the plot
s.pollinator

ggsave(plot = s.pollinator, filename = "path/prikkrutevinge_pollinators_spinn25.jpeg", width = 21, height = 14, units = "cm")

# test for differences
tapply(dat_agg$Total_count_flowers,list(dat_agg$year,dat_agg$Spinnrute),mean)
tapply(dat_agg$Total_count_flowers,list(dat_agg$year,dat_agg$Spinnrute),sd)

lm_pollinators_s <- lmer(Total_count_flowers ~ Spinnrute + factor(year) + (1|plot_id2), data=dat_agg)#
summary(lm_pollinators_s)

#Create data for monitoring plots only
dat_flower <- dat %>% 
  filter(!grepl("S", as.character(plot_id2), ignore.case = TRUE)) %>% #Remove spinn ruter
  group_by(year, plot_id2) %>% 
  summarize(total_flowers = sum(count_floral_stems, na.rm = TRUE), 
            treatment_received_until_2025 = first(treatment_received_until_2025),
            treatment_received_binary = first(treatment_received_binary), 
            polygon_id = first(polygon_id))

#Create model
lm_pollinator <- lmer(data = dat_pollinator, total_flowers ~ as.factor(year) + (1| plot_id2))
summary(lm_pollinator)

plot(lm_pollinator)
qqnorm(residuals(lm_pollinator))
plotQQunif(lm_pollinator)#test is not significant
plotResiduals(lm_pollinator)#significant but doesn't look too bad
# Obtain estimated marginal means (EMMs) for the interaction

lm_pollinator2 <- lm(data = dat_pollinator[dat_pollinator$year == 2025,], total_flowers ~ treatment_received_binary)
summary(lm_pollinator2)

lm_pollinator3 <- lm(data = dat_pollinator[dat_pollinator$year == 2025,], total_flowers ~ treatment_received_until_2025)
summary(lm_pollinator3)



## Make figure for standing dead biomass
s.litter <- dat_agg %>% 
  group_by(year, Spinnrute) %>% 
  summarize(mean = mean(cover_standing_litter), 
            sd = sd(cover_standing_litter),
            n = n(),
            se = sd / sqrt(n)) %>% 
  ggplot(aes(x = as.factor(year), y = mean, fill = Spinnrute)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                width = 0.2, position = position_dodge(width=0.9)) +
  facet_wrap(~ Spinnrute) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank()) +
  guides(fill = guide_legend(title="")) +
  theme(legend.position="bottom") +
  scale_fill_manual(
    values = c("Ja" = "#004F71",      # blC%
               "Nei" = "#E57200"),    # oransje
    labels = c("Ja"  = "Spinnrute",
               "Nei" = "OvervC%kingsrute")
  ) +
  labs(x = "Cr", y = "StC%ende dC8d biomasse (%)") +
  scale_y_continuous(expand = c(0, 0))

# Print the plot
s.litter

ggsave(plot = s.litter, filename = "path/prikkrutevinge_litter_spinn25.jpeg", width = 21, height = 14, units = "cm")

# test for differences
tapply(dat_agg$cover_standing_litter,list(dat_agg$year,dat_agg$Spinnrute),mean)
tapply(dat_agg$cover_standing_litter,list(dat_agg$year,dat_agg$Spinnrute),sd)

lm_litter_s <- lmer(cover_standing_litter ~ Spinnrute + factor(year) + (1|plot_id2), data=dat_agg)#
summary(lm_litter_s)


# Analysis standing dead biomass in monitoring plots only
dat_litter <- dat_agg %>% 
  filter(!grepl("S", as.character(plot_id2), ignore.case = TRUE)) %>% #Remove spinn ruter
  group_by(polygon_id, year, treatment_received_until_2025) %>% 
  select(polygon_id, plot_id2, year, cover_standing_litter, treatment_received_until_2025, treatment_received_binary)

#Create model
lm_litter <- lmer(data = dat_litter, cover_standing_litter ~ as.factor(year) + (1| plot_id2))
summary(lm_litter)

lm_litter2 <- lm(data = dat_litter[dat_litter$year == 2025,], cover_standing_litter ~ treatment_received_binary)
summary(lm_litter2)

lm_litter3 <- lm(data = dat_litter[dat_litter$year == 2025,], cover_standing_litter ~ treatment_received_until_2025)
summary(lm_litter3)

lm_litter3b <- lm(data = dat_litter[dat_litter$year == 2025,], cover_standing_litter ~ as.factor(treatment_received_until_2025))
summary(lm_litter3b)


## Make figure for cover of short vegetation
s.short <- dat_agg %>% 
  group_by(year, Spinnrute) %>% 
  summarize(mean = mean(cover_short_vegetation), 
            sd = sd(cover_short_vegetation),
            n = n(),
            se = sd / sqrt(n)) %>% 
  ggplot(aes(x = as.factor(year), y = mean, fill = Spinnrute)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                width = 0.2, position = position_dodge(width=0.9)) +
  facet_wrap(~ Spinnrute) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank()) +
  guides(fill = guide_legend(title="")) +
  theme(legend.position="bottom") +
  scale_fill_manual(
    values = c("Ja" = "#004F71",      # blC%
               "Nei" = "#E57200"),    # oransje
    labels = c("Ja"  = "Spinnrute",
               "Nei" = "OvervC%kingsrute")
  ) +
  labs(x = "Cr", y = "Kort vegetasjon (%)") +
  scale_y_continuous(expand = c(0, 0))

# Print the plot
s.short

ggsave(plot = s.short, filename = "path/prikkrutevinge_short_spinn25.jpeg", width = 21, height = 14, units = "cm")

# test for differences
tapply(dat_agg$cover_short_vegetation,list(dat_agg$year,dat_agg$Spinnrute),mean)
tapply(dat_agg$cover_short_vegetation,list(dat_agg$year,dat_agg$Spinnrute),sd)

lm_short_s <- lmer(cover_short_vegetation ~ Spinnrute + factor(year) + (1|plot_id2), data=dat_agg)#
summary(lm_short_s)

dat_short <- dat_agg %>% 
  filter(!grepl("S", as.character(plot_id2), ignore.case = TRUE)) %>% #Remove spinn ruter
  group_by(polygon_id, year, treatment_received_until_2025) %>% 
  select(polygon_id, plot_id2, year, cover_short_vegetation, treatment_received_until_2025, treatment_received_binary)

lm_short2 <- lm(data = dat_short[dat_short$year == 2025,], cover_short_vegetation ~ treatment_received_binary)
summary(lm_short2)

lm_short3 <- lm(data = dat_short[dat_short$year == 2025,], cover_short_vegetation ~ treatment_received_until_2025)
summary(lm_short3)

lm_short3b <- lm(data = dat_short[dat_short$year == 2025,], cover_short_vegetation ~ as.factor(treatment_received_until_2025))
summary(lm_short3b)


## Make figure for number of spinn in monitoring plots
s.spinn <- dat_agg %>% 
  group_by(year, Spinnrute) %>% 
  summarize(mean = sum(count_spinn)) %>% 
  ggplot(aes(x = as.factor(year), y = mean, fill = Spinnrute)) +
  geom_bar(position = "dodge", stat = "identity") +
  #geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
  #              width = 0.2, position = position_dodge(width=0.9)) +
  facet_wrap(~ Spinnrute) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_blank()) +
  guides(fill = guide_legend(title="")) +
  theme(legend.position="bottom") +
  scale_fill_manual(
    values = c("Ja" = "#004F71",      # blC%
               "Nei" = "#E57200"),    # oransje
    labels = c("Ja"  = "Spinnrute",
               "Nei" = "OvervC%kingsrute")
  ) +
  labs(x = "Cr", y = "Antall larvespinn") +
  scale_y_continuous(expand = c(0, 0))

# Print the plot
s.spinn

ggsave(plot = s.spinn, filename = "path/prikkrutevinge_spinn_spinn25.jpeg", width = 21, height = 14, units = "cm")


table(dat_agg$Spinnrute, dat_agg$year, dat_agg$Presence_larvespinn)


### ordinations and other analyses 2025
str(dat)

# I want to use a subset of the data, removing 2024 and also circles
# calculate number of species per plt
species.richness <- dat %>%
  distinct(plot_id2, year, species) %>%   # unike arter per plot og C%r
  count(plot_id2, year, name = "n_species")

dat <- dat %>%
  left_join(species.richness, by = c("plot_id2", "year"))


#For the NMDS, we need one datafile containing all the species relative abundances (long format), and one with the environmental variables
# we need to exclude 2024-data, because no species composition data were collected then
dat_env <- dat %>% 
  select(c(plot_id2, year, treatment_received_until_2025, treatment_received_binary,cover_field_layer, count_plantago, vegheight_mean, Spinnrute,n_species, Total_count_flowers)) %>% 
  distinct()

dat_env <- dat_env %>%
  filter(
    year != 2024,
    !grepl("sirkel",plot_id2,ignore.case=TRUE))

str(dat_env)
dat_env$year <- as.factor(dat_env$year)


## making the species dataset
# first removing 2024 and circles
dat_species_raw <- dat %>%
  filter(
    year != 2024,
    !grepl("sirkel",plot_id2,ignore.case=TRUE))

# then cleaning data
dat_species_wide <- dat_species_raw %>%
  group_by(plot_id2, year, species) %>%
  summarise(
    species_cover = mean(species_cover, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols    = c(plot_id2, year),
    names_from = species,
    values_from = species_cover,
    values_fill = 0   # fyller NA med 0 direkte
  ) %>%
  ungroup() %>%
  select(-plot_id2, -year)


#### Run NMDS ####

#Convert species matrix to relative abundances
dat_species_rel <-         
  decostand(dat_species_wide, method = "total")
#Export this dataset
#write.csv(dat_species_rel, "path/Data_Prikkrutevinge_2025_Species_rel.csv")

#Create bray-curtiss distance matrix
prikk_species_distmat <- 
  vegdist(dat_species_rel, method = "bray")

#create easy to view matrix and save (!!)
prikk_species_distmat <- 
  as.matrix(prikk_species_distmat, labels = T)
write.csv(prikk_species_distmat, "prikk_species_distmat_2023.csv")

#Run NMDS in Vegan
prikk_species_NMS <-
  metaMDS(prikk_species_distmat,
          distance = "bray",
          k =3,
          maxit = 999, 
          trymax = 500,
          wascores = TRUE)
#stress = not great but below 0.15 so OK

#check goodness of fit and stress
goodness(prikk_species_NMS)
stressplot(prikk_species_NMS)

stress.plot <- dimcheckMDS(prikk_species_distmat,
                           distance = "bray",
                           k =10,
                           trymax = 500) #stress is a little too high for comfort at 2 dimensions, better too use 3


#### Visualize the NMDS ####

#Create a gg plot
#Fit the environmental variables to the NMDS
prikk.envfit <- envfit(prikk_species_NMS, dat_env, permutations = 999, na.rm = TRUE) # this fits environmental vectors
prikk.spp.fit <- envfit(prikk_species_NMS, dat_species_rel, permutations = 999)

#create coordinates for sites
site.scrs <- as.data.frame(scores(prikk_species_NMS, display = "sites")) #save NMDS results into dataframe
site.scrs <- cbind(site.scrs, Spinn = dat_env$Spinnrute, Year = dat_env$year, No.treatments = dat_env$treatment_received_until_2025, If.treatment = dat_env$treatment_received_binary) #add grouping variable "Larvespinn" to dataframe
#site.scrs <- cbind(site.scrs, Site = rownames(site.scrs)) #add site names as variable if you want to display on plot

#create coordinates for species
spp.scrs <- as.data.frame(scores(prikk.spp.fit, display = "vectors")) #save species intrinsic values into dataframe
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs)) #add species names to dataframe
spp.scrs <- cbind(spp.scrs, pval = prikk.spp.fit$vectors$pvals) #add pvalues to dataframe so you can select species which are significant
spp.scrs<- cbind(spp.scrs, abrev = abbreviate(spp.scrs$Species, minlength = 6)) #abbreviate species names
sig.spp.scrs <- subset(spp.scrs, pval<=0.10) #subset data to show species significant at 0.05

#extract other environmental variables
env.scores.prikk <- as.data.frame(scores(prikk.envfit, display = "vectors")) #extracts relevant scores from envifit
env.scores.prikk <- cbind(env.scores.prikk, env.variables = rownames(env.scores.prikk)) #and then gives them their names

env.scores.prikk <- cbind(env.scores.prikk, pval = prikk.envfit$vectors$pvals) # add pvalues to dataframe
sig.env.scrs <- subset(env.scores.dune, pval<=0.05) #subset data to show variables significant at 0.05

#Create basic plot

#Create basic plot

nmds.plot.prikk <- ggplot(site.scrs, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(colour = factor(Spinn),
                 shape  = factor(Year)),
             size = 2) +
  coord_fixed() +
  theme_classic() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black", size = 1),
    legend.position = "right",
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.text    = element_text(size = 10)
  ) +
  labs(colour = "Spinnruter", shape = "Cr")+
  scale_colour_manual(
    values = c("Ja" = "#004F71",   # blC%
               "Nei" = "#E57200")  # oransje
  )

ggsave(plot = nmds.plot.prikk, filename = "path/prikkrutevinge_nmds_spinn25.jpeg", width = 21, height = 14, units = "cm")


# #add important species
# nmds.plot.prikk <- nmds.plot.prikk +
#   geom_point(data = sig.spp.scrs, aes(x = NMDS1, y=NMDS2), colour = "grey50") +
#   ggrepel::geom_text_repel(data = sig.spp.scrs, aes(x=NMDS1, y=NMDS2, label = Species), , colour = "grey50",  cex = 3, direction = "both", segment.size = 0.25, max.overlaps = Inf)


# checking differences in mean axes scores
# test for differences
tapply(site.scrs$NMDS1,list(site.scrs$Year,site.scrs$Spinn),mean)
tapply(site.scrs$NMDS1,list(site.scrs$Year,site.scrs$Spinn),sd)

lm_nmds1_s <- lm(NMDS1 ~ Spinn * factor(Year), data=site.scrs)#
summary(lm_nmds1_s)
lm_nmds1_sb <- lm(NMDS1 ~ Spinn + factor(Year), data=site.scrs)#
summary(lm_nmds1_sb)
lm_nmds1_sc <- lm(NMDS1 ~ factor(Year), data=site.scrs)#
summary(lm_nmds1_sc)

lm_nmds2_s <- lm(NMDS2 ~ Spinn * factor(Year), data=site.scrs)#
summary(lm_nmds2_s)
lm_nmds2_sb <- lm(NMDS2 ~ Spinn + factor(Year), data=site.scrs)#
summary(lm_nmds2_sb)
lm_nmds2_sc <- lm(NMDS2 ~ factor(Year), data=site.scrs)#
summary(lm_nmds2_sc)

lm_nmds3_s <- lm(NMDS3 ~ Spinn * factor(Year), data=site.scrs)#
summary(lm_nmds3_s)
lm_nmds3_sb <- lm(NMDS3 ~ Spinn + factor(Year), data=site.scrs)#
summary(lm_nmds3_sb)
lm_nmds3_sc <- lm(NMDS3 ~ factor(Year), data=site.scrs)#
summary(lm_nmds3_sc)

## species composition varies between years along all three axes, but there is no significant difference between spinnruter and monitoring plots - and no interactions
## but NB we know that sampling sizes are small



#rename the env.variables so it looks nicer and more norwegian in the plot
env.scores.prikk <- env.scores.prikk %>% 
  mutate(env.variables=fct_relevel(c("Antall ganger skjC8ttet","SkjC8ttet","Felsjikt", "Smalkjempe", "VegetasjonshC8yde", "Artsrikdom", "Antall blomsterstander")))

#add environmental vectors
nmds.plot.prikk <- nmds.plot.prikk+
  geom_segment(data = env.scores.prikk, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of env variables
  ggrepel::geom_text_repel(data = env.scores.prikk, aes(x=NMDS1, y=NMDS2, label = env.variables), cex = 4, direction = "both", segment.size = 0.25) #add labels for env variables



#add ellipses
# function for ellipsess 

veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

#data for ellipse, in this case using the management factor
df_ell.prikk.larvespinn <- data.frame() #sets up a data frame before running the function.
for(g in levels(as.factor(site.scrs$Spinn))){
  df_ell.prikk.larvespinn <- rbind(df_ell.prikk.larvespinn, cbind(as.data.frame(with(site.scrs [site.scrs$Spinn==g,],
                                                                                     veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,center=c(mean(NMDS1),mean(NMDS2))))) ,Larvespinn=g))
}


# data for labelling the ellipse
NMDS.mean.prikk=aggregate(site.scrs[ ,c("NMDS1", "NMDS2")], 
                          list(group = site.scrs$Spinn), mean)

# data for labelling the ellipse
NMDS.mean=aggregate(site.scrs[,c("NMDS1", "NMDS2")], 
                    list(group = site.scrs$Spinn), mean)

#add ellipses to plot, color by presence or absence of Larvespinn
df_ell.prikk.larvespinn$Spinn <- as.factor(df_ell.prikk.larvespinn$Spinn)
nmds.plot.prikk <- nmds.plot.prikk +
  geom_path(data = df_ell.prikk.larvespinn, 
            aes(x = NMDS1, y = NMDS2, group = factor(Spinn), colour = factor(Spinn))) #this is the ellipse, separate ones by Site. 


#Export this plot 
ggsave("Output/Figures/NMDS.2023.jpg",  plot = nmds.plot.prikk,
       scale = 1,
       width = 260,
       height = 320,
       units = c("mm")) 

#### Mixed model on effects of treatment ####

prikk.comm <- as.data.frame(site.scrs)

prikk.comm <- prikk.comm %>% 
  dplyr::mutate(Behandling = if_else(is.na(Behandling), "Foer", as.factor(Behandling)))

#design is unbalanced as none of the treatments is represented in Year. Suggest to this model first:
mod.behandling <- lm(NMDS1 ~ Larvespinn * Year, data = prikk.comm)
summary(mod.behandling)
plot(allEffects(mod.behandling))
plot(simulateResiduals(mod.behandling))

#And then do a seperate model for behandling only using 2023 data. 

mod.prikk <- lmer()


















## Make figure for vegetation height
dat_agg$polygon_id <- as.factor(dat_agg$polygon_id)
labels <- c("RT" = "Telefonstolpen", "RU" = "Utedassen")

# Generate the plot
p.veg_height <- dat_agg %>% 
  filter(!grepl("S", as.character(plot_id2), ignore.case = TRUE)) %>%
  group_by(polygon_id, year, treatment_received_until_2025) %>% 
  summarize(mean = mean(vegheight_mean), 
            sd = sd(vegheight_mean),
            n = n(),
            se = sd / sqrt(n)) %>% 
  ggplot(aes(x = as.factor(year), y = mean, fill = as.factor(treatment_received_until_2025))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, position = position_dodge(width=0.9)) +
  facet_wrap(~ polygon_id, labeller = labeller(polygon_id = labels)) +  # Apply custom labels
  theme_classic() +
  theme(strip.background = element_blank()) +
  guides(fill = guide_legend(title="Antall ganger skjC8tsel")) +
  theme(legend.position="bottom")+
  scale_fill_manual(values=c("#004F71","#E57200","#7A9A01","#93328E")) +
  labs(x = "Cr", y = "VegetasjonshC8yde (cm)") +
  scale_y_continuous(expand = c(0, 0))

# Print the plot
p.veg_height

ggsave(plot = p.veg_height, filename = "path/prikkrutevinge_veg_height25.jpeg", width = 21, height = 14, units = "cm")

# Make same figure for the number of smalkjempe
p.host <- dat_agg %>% 
  filter(!grepl("S", as.character(plot_id2), ignore.case = TRUE)) %>%
  group_by(polygon_id, year, treatment_received_until_2025) %>%
  summarize(mean = mean(count_plantago), 
            sd = sd(count_plantago),
            n = n(),
            se = sd / sqrt(n)) %>% 
  ggplot(aes(x = as.factor(year), y = mean, fill = as.factor(treatment_received_until_2025))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, position = position_dodge(width=0.9)) +
  facet_wrap(~ polygon_id, labeller = labeller(polygon_id = labels)) +  # Apply custom labels
  theme_classic() +
  theme(strip.background = element_blank()) +
  guides(fill = guide_legend(title="Antall ganger skjC8tsel")) +
  theme(legend.position="bottom")+
  scale_fill_manual(values=c("#004F71","#E57200","#7A9A01","#93328E")) +
  labs(x = "Cr", y = "Antall smalkjemperosetter") +
  scale_y_continuous(expand = c(0, 0))

p.host


ggsave(plot = p.host, filename = "path/prikkrutevinge_smalkjemperosetter25.jpeg", width = 21, height = 14, units = "cm")

# Make same figure for number of flowering stems
p.pollinator <- dat %>% 
  filter(!grepl("S", as.character(plot_id2), ignore.case = TRUE)) %>% #Remove spinn ruter
  group_by(year, plot_id2) %>% 
  summarize(total_flowers = sum(count_floral_stems, na.rm = TRUE), 
            treatment_received_until_2025 = first(treatment_received_until_2025), 
            polygon_id = first(polygon_id)) %>% 
  group_by(polygon_id, year, treatment_received_until_2025) %>% 
  summarize(mean = mean(total_flowers), 
            sd = sd(total_flowers),
            n = n(),
            se = sd / sqrt(n)) %>% 
  ggplot(aes(x = as.factor(year), y = mean, fill = as.factor(treatment_received_until_2025))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, position = position_dodge(width=0.9)) +
  facet_wrap(~ polygon_id, labeller = labeller(polygon_id = labels)) +  # Apply custom labels
  theme_classic() +
  theme(strip.background = element_blank()) +
  guides(fill = guide_legend(title="Antall ganger skjC8tsel")) +
  theme(legend.position="bottom")+
  scale_fill_manual(values=c("#004F71","#E57200","#7A9A01","#93328E")) +
  labs(x = "Cr", y = "Antall blomsterstand av pollinatorplanter") +
  scale_y_continuous(expand = c(0, 0))

p.pollinator

ggsave(plot = p.pollinator, filename = "path/prikkrutevinge_pollinator_planter25.jpeg", width = 21, height = 14, units = "cm")

# Same figure for standing dead litter

p.litter <- dat_agg %>% 
  filter(!grepl("S", as.character(plot_id2), ignore.case = TRUE)) %>% #Remove spinn ruter
  group_by(polygon_id, year, treatment_received_until_2025) %>% 
  summarize(mean = mean(cover_standing_litter), 
            sd = sd(cover_standing_litter),
            n = n(),
            se = sd / sqrt(n)) %>% 
  ggplot(aes(x = as.factor(year), y = mean, fill = as.factor(treatment_received_until_2025))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, position = position_dodge(width=0.9)) +
  facet_wrap(~ polygon_id, labeller = labeller(polygon_id = labels)) +  # Apply custom labels
  theme_classic() +
  theme(strip.background = element_blank()) +
  guides(fill = guide_legend(title="Antall ganger skjC8tsel")) +
  theme(legend.position="bottom")+
  scale_fill_manual(values=c("#004F71","#E57200","#7A9A01","#93328E")) +
  labs(x = "Cr", y = "Dekning stC%ende dC8d biomasse (%)") +
  scale_y_continuous(expand = c(0, 0))

p.litter

ggsave(plot = p.litter, filename = "path/prikkrutevinge_standing_litter25.jpeg", width = 21, height = 14, units = "cm")


### table showing how many plots with how many treatments - for each locality

table(dat$plot_id2,dat$treatment_received_until_2025)


### Statistics

## vegetation height
dat_height <- dat_agg %>% 
  filter(!grepl("S", as.character(plot_id2), ignore.case = TRUE)) %>% 
  select(polygon_id, plot_id2, year, vegheight_mean, treatment_received_until_2025, treatment_received_binary)

mean(dat_height$vegheight_mean[dat_height$year==2022])
sd(dat_height$vegheight_mean[dat_height$year==2022])

lm_height <- lmer(data = dat_height, vegheight_mean ~ as.factor(year) + (1| plot_id2))
summary(lm_height)

plot(lm_height)
qqnorm(residuals(lm_height))
plotQQunif(lm_height)#test is not significant, but looks a bit wonky visually
plotResiduals(lm_height)#significant but doesn't look too bad
# Obtain estimated marginal means (EMMs) for the interaction
emm_interaction <- emmeans(lm_height, ~polygon_id + as.factor(year))
# Perform pairwise comparisons
pairwise_comparisons <- contrast(emm_interaction, method = "pairwise")
# View results
summary(pairwise_comparisons)

lm_height2 <- lm(data = dat_height[dat_height$year == 2025,], vegheight_mean ~ treatment_received_until_2025)
summary(lm_height2)

lm_height3 <- lm(data = dat_height[dat_height$year == 2025,], vegheight_mean ~ treatment_received_binary)
summary(lm_height3)

## Analyses for smalkjempe

#Create data to run model on

# Generate the data
dat_host <- dat_agg %>% 
  filter(!grepl("S", as.character(plot_id2), ignore.case = TRUE)) %>% 
  select(polygon_id, plot_id2, year, count_plantago, treatment_received_until_2025,treatment_received_binary)

mean(dat_host$count_plantago[dat_host$year==2022])
sd(dat_host$count_plantago[dat_host$year==2022])

lm_host <- glmer(data = dat_host, count_plantago ~ as.factor(year) + (1| plot_id2),family = poisson)
summary(lm_host)

plot(lm_host)
qqnorm(residuals(lm_host))
plotQQunif(lm_host)#ok with poisson-distribution
plotResiduals(lm_host)#ok, n.s.

lm_host2 <- glm(data = dat_host[dat_host$year==2025,], count_plantago ~ treatment_received_binary,family = poisson)
summary(lm_host2)

lm_host3 <- glm(data = dat_host[dat_host$year==2025,], count_plantago ~ treatment_received_until_2025,family = poisson)
summary(lm_host3)


# Analysis for number of flowering stems

#Create data
dat_flower <- dat %>% 
  filter(!grepl("S", as.character(plot_id2), ignore.case = TRUE)) %>% #Remove spinn ruter
  group_by(year, plot_id2) %>% 
  summarize(total_flowers = sum(count_floral_stems, na.rm = TRUE), 
            treatment_received_until_2025 = first(treatment_received_until_2025),
            treatment_received_binary = first(treatment_received_binary), 
            polygon_id = first(polygon_id))

mean(dat_pollinator$total_flowers[dat_pollinator$year==2022])
sd(dat_pollinator$total_flowers[dat_pollinator$year==2022])

#Create model
lm_pollinator <- lmer(data = dat_pollinator, total_flowers ~ as.factor(year) + (1| plot_id2))
summary(lm_pollinator)

plot(lm_pollinator)
qqnorm(residuals(lm_pollinator))
plotQQunif(lm_pollinator)#test is not significant
plotResiduals(lm_pollinator)#significant but doesn't look too bad
# Obtain estimated marginal means (EMMs) for the interaction

lm_pollinator2 <- lm(data = dat_pollinator[dat_pollinator$year == 2025,], total_flowers ~ treatment_received_binary)
summary(lm_pollinator2)

lm_pollinator3 <- lm(data = dat_pollinator[dat_pollinator$year == 2025,], total_flowers ~ treatment_received_until_2025)
summary(lm_pollinator3)


# Analysis standing dead biomass
dat_litter <- dat_agg %>% 
  filter(!grepl("S", as.character(plot_id2), ignore.case = TRUE)) %>% #Remove spinn ruter
  group_by(polygon_id, year, treatment_received_until_2025) %>% 
  select(polygon_id, plot_id2, year, cover_standing_litter, treatment_received_until_2025, treatment_received_binary)

mean(dat_litter$cover_standing_litter[dat_litter$year==2022])
sd(dat_litter$cover_standing_litter[dat_litter$year==2022])


#Create model
lm_litter <- lmer(data = dat_litter, cover_standing_litter ~ as.factor(year) + (1| plot_id2))
summary(lm_litter)

plot(lm_litter)
qqnorm(residuals(lm_litter))
plotQQunif(lm_litter)#not looking good
plotResiduals(lm_litter)#definately not looking good
# Obtain estimated marginal means (EMMs) for the interaction
emm_interaction <- emmeans(lm_litter, ~polygon_id + as.factor(year) * treatment_received_until_2024)
# Perform pairwise comparisons
pairwise_comparisons <- contrast(emm_interaction, method = "pairwise")
# View results
summary(pairwise_comparisons)

lm_litter2 <- lm(data = dat_litter[dat_litter$year == 2025,], cover_standing_litter ~ treatment_received_binary)
summary(lm_litter2)

lm_litter3 <- lm(data = dat_litter[dat_litter$year == 2025,], cover_standing_litter ~ treatment_received_until_2025)
summary(lm_litter3)

lm_litter3b <- lm(data = dat_litter[dat_litter$year == 2025,], cover_standing_litter ~ as.factor(treatment_received_until_2025))
summary(lm_litter3b)



