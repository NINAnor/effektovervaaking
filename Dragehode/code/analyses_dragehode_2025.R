#This document contains scripts to performed data analyses for the project EffektovervC%king - dragehode in 2025. 
#Downloading, cleaning and merging of data in Folder survey123-vegetation-data, scripts 1_From_survey123_to_dirty_table, 2_Table diagnostics and cleaning, and 3_data_cleaning_and_dataset_merging.

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(vegan)
library(car)
library(goeveg)
library(ggrepel)
library(scales)
library(forcats)
library(ARTool)
library(ggpubr)
library(reshape2)
library(here)
library(data.table)


df <- readxl::read_excel("path/Dragehode_2025_cleaned.xlsx")

#Calculate species richness
df <- df %>% 
  group_by(year, plot_id_full) %>% 
  mutate(species_richness = as.numeric(n_distinct(species)), 
         count_dragehode_total = count_dragehode_seedlings + count_dragehode_vegetative + count_dragehode_fertile)

df$species_cover <- as.numeric(df$species_cover)

#Plot species richness
p.richness <- df %>% 
  group_by(year, treatment) %>% 
  distinct(plot_id_full, .keep_all = TRUE) %>% 
  summarize(mean_richness = mean(species_richness), 
            sd_richness = sd(species_richness), 
            n_richness = n()) %>% 
  mutate(se_richness = sd_richness / sqrt(n_richness), 
         upper_ci_richness = mean_richness + qt(1 - (0.05 / 2), n_richness - 1) * se_richness, 
         lower_ci_richness = mean_richness - qt(1 - (0.05 / 2), n_richness - 1) * se_richness) %>% 
  ggplot(aes(x = treatment, y = mean_richness, fill = treatment))+
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_errorbar(aes(ymin = mean_richness - sd_richness, ymax = mean_richness + sd_richness), width = 0.1) +
  theme_classic() +
  theme(strip.background = element_blank())+
  guides(fill = guide_legend(title="SkjC8tsel")) +
  scale_fill_manual(values=c("#004F71","#E57200","#008C95")) +
  labs(x = "SkjC8tsel", y = "Antall karplantearter") +
  scale_y_continuous(expand = c(0, 0))

ggsave(plot = p.richness, filename = "path/species_richness_25.jpeg", width = 12, height = 8, units = "cm")

#Do a simple anova (non-parametric transformed rank tests give the same results)
aov_richness <- df %>% 
  group_by(year, treatment) %>% 
  distinct(plot_id_full, .keep_all = TRUE) %>% 
  aov(species_richness ~ as.factor(treatment) * as.factor(year), data = .)

summary(aov_richness)


#Species richness nectar plants

#First, let's create a list of nectar plants

nectar_species <- c("Hypericum_perforatum",
                    "Achillea_millefolium", 
                    "Alchemilla_sp",
                    "Linaria_vulgaris", 
                    "Stellaria_graminea", 
                    "Ajuga_pyramidalis", 
                    "Galium_verum", 
                    "Galeopsis_tetrahit", 
                    "Lathyrus_linifolius", 
                    "Campanula_rotundifolia", 
                    "Viola_riviniana", 
                    "Knautia_arvensis", 
                    "Hylotelephium_maximum", 
                    "Dracocephalum_ruyschiana", 
                    "Hypericum_maculatum", 
                    "Lathyrus_pratensis", 
                    "Viscaria_vulgaris", 
                    "Viola_canina_subsp._canina", 
                    "Veronica_chamaedrys", 
                    "Anthriscus_sylvestris", 
                    "Potentilla_argentea", 
                    "Lotus_corniculatus", 
                    "Arabidopsis_thaliana", 
                    "Galium_boreale", 
                    "Viola_tricolor", 
                    "Vicia_cracca", 
                    "Anemone_nemorosa", 
                    "Scleranthus_perennis", 
                    "Veronica_officinalis",
                    "Centaurea_jacea")

#Create plot

p.nectar_dekning <- df %>% 
  filter(species %in% nectar_species)  %>% 
  group_by(year, treatment, plot_id_full) %>% 
  summarize(sum_cover = sum(as.numeric(species_cover))) %>% 
  ungroup() %>% 
  group_by(year, treatment) %>% 
  summarize(mean = mean(as.numeric(sum_cover)), 
            sd = sd(as.numeric(sum_cover)), 
            n = n()) %>% 
  mutate(se = sd / sqrt(n), 
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se, 
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se) %>% 
  ggplot(aes(x = treatment, y = mean, fill = treatment))+
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
  theme_classic() +
  theme(strip.background = element_blank())+
  guides(fill = guide_legend(title="SkjC8tsel")) +
  scale_fill_manual(values=c("#004F71","#E57200")) +
  labs(x = "SkjC8tsel", y = "Dekning pollinatorplanter (%)") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50), oob=rescale_none)

ggsave(plot = p.nectar_dekning, filename = "path/nectar_dekning_25.jpeg", width = 12, height = 8, units = "cm")

#Do anova
aov_nectar <- df %>% 
  filter(species %in% nectar_species)  %>% 
  group_by(year, treatment, plot_id_full) %>% 
  summarize(sum_cover = sum(as.numeric(species_cover))) %>% 
  aov(sum_cover ~ as.factor(treatment) * as.factor(year), data = .)

summary(aov_nectar)


#Dragehode counts

p.dragehode <- df %>% 
  filter(presence_dragehode == "present") %>% 
  group_by(year, treatment) %>% 
  distinct(plot_id_full, .keep_all = TRUE) %>% 
  summarize(mean = mean(count_dragehode_total), 
            sd = sd(count_dragehode_total), 
            n = n()) %>% 
  mutate(se = sd / sqrt(n), 
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se, 
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se) %>% 
  ggplot(aes(x = treatment, y = mean, fill = treatment))+
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
  theme_classic() +
  theme(strip.background = element_blank())+
  guides(fill = guide_legend(title="SkjC8tsel")) +
  scale_fill_manual(values=c("#004F71","#E57200")) +
  labs(x = "SkjC8tsel", y = "Antall dragehodeplanter") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 26), oob=rescale_none)

ggsave(plot = p.dragehode, filename = "path/dragehode_count_25.jpeg", width = 12, height = 8, units = "cm")

#Do a simple ANOVA

aov_count <- df %>% 
  filter(presence_dragehode == "present") %>% 
  group_by(year, treatment) %>% 
  distinct(plot_id_full, .keep_all = TRUE)%>% 
  aov(count_dragehode_total ~ as.factor(treatment) * as.factor(year), data = .)

summary(aov_count)


#Dragehode cover
p.dragehode_dekning <- p.dragehodedekning <- df %>% 
  filter(presence_dragehode == "present" 
         & species == "Dracocephalum_ruyschiana")  %>% 
  group_by(year, treatment) %>% 
  summarize(mean = mean(as.numeric(species_cover)), 
            sd = sd(as.numeric(species_cover)), 
            n = n()) %>% 
  mutate(se = sd / sqrt(n), 
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se, 
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se) %>% 
  ggplot(aes(x = treatment, y = mean, fill = treatment))+
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
  theme_classic() +
  theme(strip.background = element_blank())+
  guides(fill = guide_legend(title="SkjC8tsel")) +
  scale_fill_manual(values=c("#004F71","#E57200")) +
  labs(x = "SkjC8tsel", y = "Dekning dragehode (%)") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 26), oob=rescale_none)

ggsave(plot = p.dragehode_dekning, filename = "path/dragehode_cover_25.jpeg", width = 12, height = 8, units = "cm")

#Do a simple anova

aov_dekning <- df %>% 
  filter(presence_dragehode == "present" 
         & species == "Dracocephalum_ruyschiana")  %>% 
  group_by(year, treatment) %>% 
  aov(species_cover ~ as.factor(treatment) * as.factor(year), data = .)
summary(aov_dekning)


#Create a combined plot

p.dragehode_all <- ggarrange(p.richness, p.nectar_dekning, p.dragehode, p.dragehode_dekning, labels = c("a", "b", "c", "d"), common.legend = TRUE, legend = "none", ncol = 2, nrow = 2)

ggsave(plot = p.dragehode_all, filename = "path/dragehode_all_25.jpeg", width = (24), height = (16), units = "cm")


#Dragehode count per class
p.dragehode_classes <- df %>% 
  group_by(year, treatment) %>% 
  distinct(plot_id_full, .keep_all = TRUE) %>% 
  pivot_longer(cols = c(count_dragehode_seedlings:count_dragehode_fertile), 
               names_to = "class", 
               values_to = "count") %>% 
  # Reverse order of class levels to achieve desired stacking
  mutate(class = factor(class, levels = c("count_dragehode_seedlings",
                                          "count_dragehode_vegetative",
                                          "count_dragehode_fertile"))) %>% 
  arrange(year, treatment, class) %>%  # Arrange to enforce stacking order
  group_by(year, treatment) %>% 
  ggplot(aes(x = treatment, y = count, fill = class)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  facet_wrap(~ year) +
  theme_classic() +
  theme(strip.background = element_blank()) +
  guides(fill = guide_legend(title = "StC8rrelsesklasse")) +
  scale_fill_manual(values = c( "#2DCCD3","#7A9A01","#93328E"),
                    labels = c("smC%planter", "vegetative", "fertile")) + 
  labs(x = "SkjC8tsel", y = "Antall dragehodeplanter") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,50))

ggsave(plot = p.dragehode_classes, filename = "path/dragehode_classes_25.jpeg", width = (12*1.5), height = (8*1.5), units = "cm")

df %>% 
  group_by(year, plot_id_full) %>% 
  filter(species == "Dracocephalum_ruyschiana") %>% 
  summarize(count_dragehode_total)

#Overview of total numbers by year, per class

counts <- df %>% 
  group_by(year) %>% 
  distinct(plot_id_full, .keep_all = TRUE) %>% 
  pivot_longer(cols = c(count_dragehode_seedlings:count_dragehode_fertile, count_dragehode_total), 
               names_to = "class", 
               values_to = "count") %>% 
  group_by(year, treatment, class) %>% 
  summarize(sum = sum(count), 
            n = n())

df %>% 
  group_by(year) %>% 
  distinct(plot_id_full, .keep_all = TRUE) %>% 
  pivot_longer(cols = c(count_dragehode_seedlings), 
               names_to = "class", 
               values_to = "count") %>% 
  group_by(year, class) %>% 
  summarize(sum = sum(count), 
            n = n())



#For further (ordination) analyses, we want a file that only contains plotID, species, and their cover - in wide format
df_nmds <- dcast(df, plot_id_full + year + treatment + presence_dragehode ~ species, value.var = "species_cover",
                 fun.aggregate = sum, fill = 0)

#Create species dataset
df_species <- df_nmds %>% 
  select(!c(plot_id_full, year, treatment, presence_dragehode))

#Create grouping dataset
df_groups <- df_nmds %>% 
  select(plot_id_full, year, treatment, presence_dragehode)

# Calculating relative abundance and creating new dataframe with relative abundance data
df_species_rel <-         
  decostand(df_species, method = "total")

#Do NMDS
nmds_result <- metaMDS(df_species_rel, distance = "bray", k = 2, trymax = 100) #stress = 0.18, good enough

#Add treatment (slC%tt), dragehode and year information to NMDS results
nmds_scores <- as.data.frame(scores(nmds_result)$sites)
nmds_scores$Treatment <- df_groups$treatment
nmds_scores$Year <- df_groups$year
nmds_scores$Plot_ID <- df_groups$plot_id_full
nmds_scores$Dragehode <- df_groups$presence_dragehode
nmds_scores$Group <- paste(nmds_scores$Treatment, nmds_scores$Year, sep = "_")

#Create dataframe for species scores
species_scores <- as.data.frame(scores(nmds_result)$species)
species_scores$Species <- rownames(species_scores)

#Create arrows 
centroids <- nmds_scores %>%
  group_by(Treatment, Year) %>%
  summarise(Centroid1 = mean(NMDS1), Centroid2 = mean(NMDS2)) %>%
  arrange(Treatment, Year)

arrows_data <- centroids %>%
  group_by(Treatment) %>%
  mutate(Centroid1_next = lead(Centroid1),
         Centroid2_next = lead(Centroid2)) %>%
  filter(!is.na(Centroid1_next)) 

#Create figure
p.dragehode_nmds <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Treatment)) +
  geom_point(aes(shape = as.factor(Year)), size = 3, alpha = 0.5) +  # Add points for sites
  stat_ellipse(aes(group = Group, linetype = as.factor(Year)), type = "t") +  # Add ellipses
  geom_segment(data = arrows_data,
               aes(x = Centroid1, y = Centroid2,
                   xend = Centroid1_next, yend = Centroid2_next, color = Treatment),
               arrow = arrow(length = unit(0.2, "cm")), size = 1) +# Add arrows for years
  geom_point(data = centroids,
             aes(x = Centroid1, y = Centroid2, fill = factor(Year), shape = factor(Year)),
             size = 2, show.legend = FALSE) +# Add points for centroids
  # Customize plot appearance
  labs(color = "SkjC8tsel", shape = "Cr", linetype = "Cr") +
  theme_classic() +
  scale_color_manual(values=c("#004F71","#E57200","#008C95")) +   # Points colors by Year
  scale_fill_manual(values=c("#004F71","#E57200","#008C95"))     # Centroid points colored by Year

ggsave(plot = p.dragehode_nmds, filename = "path/dragehode_nmds_2025.jpeg", width = (20), height = (16), units = "cm")


#Do permanova analysis

# Bray-Curtis dissimilarity matrix
bray_curtis <- vegdist(df_species, method = "bray")

# Run PERMANOVA considering Treatment, Year, and their interaction
permanova_interaction <- adonis(bray_curtis ~ year * treatment, data = df_nmds)

# Print the results
print(permanova_interaction)
