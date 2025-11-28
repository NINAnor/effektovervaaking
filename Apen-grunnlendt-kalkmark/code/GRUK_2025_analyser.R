### Dataanalyser GRUK-effekt 2025 ### 

## laster biblioteker
library(sciplot)
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(stringr)
library(cowplot)

# defining a colour palette
nina.colours <- c("#004F71","#008C95","#E57200","#93328E","#7A9A01","#A2AAAD","#2DCCD3","#FFB25B")

## henter inn data
gruk25_plots <- read_excel("path/Data/GRUK_effekt_data_2025_updated.xlsx",sheet="plots")
gruk25_plotspecies <- read_excel("path/Data/GRUK_effekt_data_2025_updated.xlsx",sheet="species.long")
gruk25_circles <- read_excel("path/Data/GRUK_effekt_data_2025_updated.xlsx",sheet="circles")
gruk25_circlespecies <- read_excel("path/Data/GRUK_effekt_data_2025_updated.xlsx",sheet="species.circles")

## henter inn lister over rC8dlistearter og habitatspesialister
artsliste_rl <- read_excel("path/Data/GRUK_artslister.xlsx",sheet="RL")
artsliste_hs <- read_excel("path/Data/GRUK_artslister.xlsx",sheet="HS")
artsliste_fa <- read_excel("path/Data/GRUK_artslister.xlsx",sheet="FA") # NB denne inneholder arter per 2023, og er ikke kompatibel med vC%re analyser... Vinterkarse, syrin osv. 

## tar ut skogsrutene pC% Padda
gruk25_plots <- gruk25_plots %>%
  filter(!str_starts(Plot.mapping.unit, "T4"))

gruk25_circles <- gruk25_circles %>%
  filter(!str_starts(Circle.mapping.unit, "T4"))

## for C% svare pC% spC8rsmC%let om effekter av tiltakene bruker vi fC8lgende variabler

#### Naturmangfoldvariabler 
## 1. Antall arter
## 2. Antall habitatspesialister
## 3. Antall rC8dlistearter
## 4. Samlet mengde habitatspesialister
## 4. Samlet mengde rC8dlistearter


#### PC%virkningsvariabler
## 1. Antall fremmede arter 
##  a. i rutene
##  b. i sirklene
## 2. Dekning av fremmede arter 
##  a. i rutene
##  b. i sirklene
## 3. Dekning av vedplanter 
##  a. i sirklene
## 4. Dekning av slitasje og kjC8respor 
##  a. i sirklene
## 5. Dekning av problemarter
##  a. i sirklene
##  b. som funksjon av 
##      i. tiltak
##      ii .slitasje og kjC8respor


## beregner antall arter og mengde av artene per kombinasjon av rute og tidspunkt

## Antall arter
datspeciesnr <- gruk25_plotspecies %>%
  group_by(Rute,Tid_num) %>%
  summarise(nr_species = n_distinct(Latinsk_navn, na.rm=TRUE))

gruk25_plots <- gruk25_plots %>% 
  unique() %>%
  left_join(datspeciesnr)

## Antall rC8dlistearter og total abundanse av rC8dlistearter
rl_summary <- gruk25_plotspecies %>%
  semi_join(artsliste_rl, by = "Latinsk_navn") %>%   # beholder bare rC8dlistearter
  group_by(Rute, Tid_num) %>%                # grupper per rute og tid
  summarise(
    n_rl = n_distinct(Latinsk_navn),                 # antall unike rC8dlistearter
    abundans_rl = sum(Dekning, na.rm = TRUE),  # total abundans
    .groups = "drop"
  )

gruk25_plots <- gruk25_plots %>%
  left_join(rl_summary, by = c("Rute", "Tid_num"))

## Antall habitatspesialister og total abundanse av habitatspesialister
hs_summary <- gruk25_plotspecies %>%
  semi_join(artsliste_hs, by = "Latinsk_navn") %>%   # beholder bare habitatspesialister
  group_by(Rute, Tid_num) %>%                # grupper per rute og tid
  summarise(
    n_hs = n_distinct(Latinsk_navn),                 # antall unike habitatspesialister
    abundans_hs = sum(Dekning, na.rm = TRUE),  # total abundans
    .groups = "drop"
  )

gruk25_plots <- gruk25_plots %>%
  left_join(hs_summary, by = c("Rute", "Tid_num"))

## Antall fremmede arter og total abundanse. Denne lista inneholder fremmede arter jf. vC%r nedtrekksmeny i S123, dvs. fremmede arter etter Fremmedartslista 2018.
fa_summary <- gruk25_plotspecies %>%
  semi_join(artsliste_fa, by = "Latinsk_navn") %>%   # beholder bare fremmede arter
  group_by(Rute, Tid_num) %>%                # grupper per rute og tid
  summarise(
    n_fa = n_distinct(Latinsk_navn),                 # antall unike fremmede arter
    abundans_fa = sum(Dekning, na.rm = TRUE),  # total abundans
    .groups = "drop"
  )

gruk25_plots <- gruk25_plots %>%
  left_join(fa_summary, by = c("Rute", "Tid_num"))


## legger inn 0 i stedet for NA i de rutene hvor vi faktisk har ruteanalyser, men hvor arter (rC8dliste, hab.spes, fremmedarter) ikke forekommer
gruk25_plots <- gruk25_plots %>%
  mutate(
    n_rl = case_when(
      is.na(n_rl) & !is.na(nr_species) ~ 0,   # sett 0 nC%r ingen RL-arter, men arter finnes
      TRUE ~ n_rl
    ),
    abundans_rl = case_when(
      is.na(abundans_rl) & !is.na(nr_species) ~ 0,  # samme logikk for abundans
      TRUE ~ abundans_rl
    ),
    n_hs = case_when(
      is.na(n_hs) & !is.na(nr_species) ~ 0,   # sett 0 nC%r ingen HS-arter, men arter finnes
      TRUE ~ n_hs
    ),
    abundans_hs = case_when(
      is.na(abundans_hs) & !is.na(nr_species) ~ 0,  # samme logikk for abundans
      TRUE ~ abundans_hs
    ),
    n_fa = case_when(
      is.na(n_fa) & !is.na(nr_species) ~ 0,   # sett 0 nC%r ingen RL-arter, men arter finnes
      TRUE ~ n_fa
    ),
    abundans_fa = case_when(
      is.na(abundans_fa) & !is.na(nr_species) ~ 0,  # samme logikk for abundans
      TRUE ~ abundans_fa
    )
  )


### Lager figurer for hver av pC%virkningsfaktorene separat
### Dvs. ett sett av figurer for BleikC8ya og TorvC8ya, der fokus er bekjempelse av gravbergknapp
### Og ett sett av figurer for Padda, MalmC8ya og HusbergC8ya, der fokus er bekjempelse av gjengroing

#### ---- Bekjempelse av gravbergknapp ---- 
## Det gir egentlig ikke mening C% ha med Nakholmen her, ettersom bare C)n rute har vC&rt under duk, og ingen av de fem nye rutene fra 2025 er med behandling
## Henter ut lokalitetene BleikC8ya og TorvC8ya

gruk25_plots_gbk <- gruk25_plots %>%
  filter(Lokalitet == "TorvC8ya" | Lokalitet == "BleikC8ya")

gruk25_circles_gbk <- gruk25_circles %>%
  filter(Lokalitet == "TorvC8ya" | Lokalitet == "BleikC8ya")

## splitter TorvC8ya i to dellokaliteter, S og N, fordi behandlingen der og overvC%kingsfrekvensen varierer
gruk25_plots_gbk <- gruk25_plots_gbk %>%
  mutate(
    Lokalitet_ny = case_when(
      Lokalitet == "TorvC8ya" & str_starts(Rute, "TN") ~ "TorvC8yaN",
      Lokalitet == "TorvC8ya" & str_starts(Rute, "TS") ~ "TorvC8yaS",
      TRUE ~ Lokalitet
    )
  )

gruk25_plots_gbk <- gruk25_plots_gbk %>%
  mutate(
    Tid = case_when(
      Lokalitet_ny == "TorvC8yaN" & Tid == "Etter5" ~ "Etter2",
      TRUE ~Tid),
    Tid_num = case_when(
      Lokalitet_ny == "TorvC8yaN" & Tid_num == 5 ~2,
      TRUE ~ Tid_num
    )
  )

gruk25_circles_gbk <- gruk25_circles_gbk %>%
  mutate(
    Lokalitet_ny = case_when(
      Lokalitet == "TorvC8ya" & str_starts(Rute, "TN") ~ "TorvC8yaN",
      Lokalitet == "TorvC8ya" & str_starts(Rute, "TS") ~ "TorvC8yaS",
      TRUE ~ Lokalitet
    )
  )

gruk25_circles_gbk <- gruk25_circles_gbk %>%
  mutate(
    Tid = case_when(
      Lokalitet_ny == "TorvC8yaN" & Tid == "Etter5" ~ "Etter2",
      TRUE ~Tid),
    Tid_num = case_when(
      Lokalitet_ny == "TorvC8yaN" & Tid_num == 5 ~2,
      TRUE ~ Tid_num
    )
  )


## summerer variablene per kombinasjon av rute og tid, gjennomsnitt og standardavvik, til figurer
gbk_summary_data <- gruk25_plots_gbk %>%
  group_by(Tid_num,Lokalitet_ny) %>%
  summarise(
    mean_n_rl = mean(n_rl, na.rm = TRUE),
    se_n_rl = sd(n_rl, na.rm = TRUE) / sqrt(n()),
    mean_ab_rl = mean(abundans_rl,na.rm=TRUE),
    se_ab_rl = sd(abundans_rl,na.rm=TRUE) / sqrt(n()),
    mean_n_hs = mean(n_hs, na.rm = TRUE),
    se_n_hs = sd(n_hs, na.rm = TRUE) / sqrt(n()),
    mean_ab_hs = mean(abundans_hs,na.rm=TRUE),
    se_ab_hs = sd(abundans_hs,na.rm=TRUE) / sqrt(n()),
    mean_n_fa = mean(n_fa, na.rm = TRUE),
    se_n_fa = sd(n_fa, na.rm = TRUE) / sqrt(n()),
    mean_ab_fa = mean(abundans_fa,na.rm=TRUE),
    se_ab_fa = sd(abundans_fa,na.rm=TRUE) / sqrt(n()),
    mean_n_sr = mean(nr_species, na.rm = TRUE),
    se_n_sr = sd(nr_species, na.rm = TRUE) / sqrt(n()),
    mean_dekning_felt = mean(Field.layer,na.rm=TRUE),
    se_dekning_felt = sd(Field.layer,na.rm=TRUE) / sqrt(n())
  )

## figurer: antall arter og dekning av feltsjikt
gbk_ant_arter <- ggplot(gbk_summary_data, aes(x = Lokalitet_ny, y = mean_n_sr, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_n_sr - se_n_sr, ymax = mean_n_sr + se_n_sr),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 20)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Antall", fill = "") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet_ny, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  guides(fill = "none")+
  labs(title = "Artsrikdom") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            # legg stripene utenfor panelet
    axis.text.x = element_blank(),          # fjern dupliserte x-etiketter
    axis.ticks.x = element_blank()
  )

gbk_feltsjikt <- ggplot(gbk_summary_data, aes(x = Lokalitet_ny, y = mean_dekning_felt, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_dekning_felt - se_dekning_felt, ymax = mean_dekning_felt + se_dekning_felt),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 100)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Dekning (%)", fill = "Tid") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet_ny, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  labs(title = "Feltsjikt") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            # legg stripene utenfor panelet
    axis.text.x = element_blank(),          # fjern dupliserte x-etiketter
    axis.ticks.x = element_blank()
  )

gbk_veg <- plot_grid(gbk_ant_arter,gbk_feltsjikt,ncol = 2)
ggsave("GBK_veg.jpeg", plot = gbk_veg, path = "path/Analyser/GRUK_effekt_2025", width = 10, height = 6, dpi = 300)


## figurer: antall og dekning av habitatspesialister
gbk_ant_hs <- ggplot(gbk_summary_data, aes(x = Lokalitet_ny, y = mean_n_hs, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_n_hs - se_n_hs, ymax = mean_n_hs + se_n_hs),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 5)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Antall", fill = "Tid") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet_ny, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  guides(fill = "none")+
  labs(title = "Antall habitatspesialister") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            # legg stripene utenfor panelet
    axis.text.x = element_blank(),          # fjern dupliserte x-etiketter
    axis.ticks.x = element_blank()
  )

gbk_ab_hs <- ggplot(gbk_summary_data, aes(x = Lokalitet_ny, y = mean_ab_hs, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_ab_hs - se_ab_hs, ymax = mean_ab_hs + se_ab_hs),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 25)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Dekning (%)", fill = "Tid") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet_ny, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  labs(title = "Dekning habitatspesialister") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            # legg stripene utenfor panelet
    axis.text.x = element_blank(),          # fjern dupliserte x-etiketter
    axis.ticks.x = element_blank()
  )

gbk_hs <- plot_grid(gbk_ant_hs,gbk_ab_hs,ncol = 2)
ggsave("GBK_HS.jpeg", plot = gbk_hs, path = "path/Analyser/GRUK_effekt_2025", width = 10, height = 6, dpi = 300)


## figurer: antall og dekning av rC8dlistearter
gbk_ant_rl <- ggplot(gbk_summary_data, aes(x = Lokalitet_ny, y = mean_n_rl, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_n_rl - se_n_rl, ymax = mean_n_rl + se_n_rl),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 4)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Antall", fill = "Tid") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet_ny, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  guides(fill = "none")+
  labs(title = "Antall rC8dlistearter") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            # legg stripene utenfor panelet
    axis.text.x = element_blank(),          # fjern dupliserte x-etiketter
    axis.ticks.x = element_blank()
  )

gbk_ab_rl <- ggplot(gbk_summary_data, aes(x = Lokalitet_ny, y = mean_ab_rl, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_ab_rl - se_ab_rl, ymax = mean_ab_rl + se_ab_rl),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 20)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Dekning (%)", fill = "Tid") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet_ny, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  labs(title = "Dekning rC8dlistearter") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            # legg stripene utenfor panelet
    axis.text.x = element_blank(),          # fjern dupliserte x-etiketter
    axis.ticks.x = element_blank()
  )

gbk_rl <- plot_grid(gbk_ant_rl,gbk_ab_rl,ncol = 2)
ggsave("GBK_RL.jpeg", plot = gbk_rl, path = "path/Analyser/GRUK_effekt_2025", width = 10, height = 6, dpi = 300)


## figurer: antall og dekning av fremmedarter i rutene
gbk_ant_fa <- ggplot(gbk_summary_data, aes(x = Lokalitet_ny, y = mean_n_fa, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_n_fa - se_n_fa, ymax = mean_n_fa + se_n_fa),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 3)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Antall", fill = "Tid") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet_ny, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  guides(fill = "none")+
  labs(title = "Antall fremmede arter") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            # legg stripene utenfor panelet
    axis.text.x = element_blank(),          # fjern dupliserte x-etiketter
    axis.ticks.x = element_blank()
  )

gbk_ab_fa <- ggplot(gbk_summary_data, aes(x = Lokalitet_ny, y = mean_ab_fa, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_ab_fa - se_ab_fa, ymax = mean_ab_fa + se_ab_fa),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 70)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Dekning (%)", fill = "Tid") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet_ny, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  labs(title = "Dekning fremmede arter") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            # legg stripene utenfor panelet
    axis.text.x = element_blank(),          # fjern dupliserte x-etiketter
    axis.ticks.x = element_blank()
  )

gbk_fa <- plot_grid(gbk_ant_fa,gbk_ab_fa,ncol = 2)
ggsave("GBK_FA.jpeg", plot = gbk_fa, path = "path/Analyser/GRUK_effekt_2025", width = 10, height = 6, dpi = 300)


### i sirklene
## summerer variablene per kombinasjon av sirkel og tid, gjennomsnitt og standardavvik, til figurer
gbk_summary_data_circles <- gruk25_circles_gbk %>%
  group_by(Tid_num,Lokalitet_ny) %>%
  summarise(
    mean_utfordrende_arter = mean(utfordrende_arter, na.rm = TRUE),
    se_utfordrende_arter = sd(utfordrende_arter, na.rm = TRUE) / sqrt(n()),
    mean_slitasje = mean(slitasje,na.rm=TRUE),
    se_slitasje = sd(slitasje,na.rm=TRUE) / sqrt(n()),
    mean_vedplanter = mean(vedplanter,na.rm=TRUE),
    se_vedplanter = sd(vedplanter,na.rm=TRUE) / sqrt(n()),
    mean_fremmede = mean(Circle.invasive.species,na.rm=TRUE),
    se_fremmede = sd(Circle.invasive.species,na.rm=TRUE) / sqrt(n()),
    mean_problem = mean(Circle.problematic.species,na.rm=TRUE),
    se_problem = sd(Circle.problematic.species,na.rm=TRUE) / sqrt(n()))

# figur: dekning av fremmedarter i sirklene
gbk_ab_fa_c <- ggplot(gbk_summary_data_circles, aes(x = Lokalitet_ny, y = mean_fremmede, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_fremmede - se_fremmede, ymax = mean_fremmede + se_fremmede),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 75)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Dekning (%)", fill = "Tid") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet_ny, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  guides(fill = "none")+
  labs(title = "Dekning fremmede arter") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            # legg stripene utenfor panelet
    axis.text.x = element_blank(),          # fjern dupliserte x-etiketter
    axis.ticks.x = element_blank()
  )

# figur: dekning av problemarter i sirklene
gbk_problem_c <- ggplot(gbk_summary_data_circles, aes(x = Lokalitet_ny, y = mean_problem, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_problem - se_problem, ymax = mean_problem + se_problem),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 5)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Dekning (%)", fill = "Tid") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet_ny, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  labs(title = "Dekning problemarter") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            # legg stripene utenfor panelet
    axis.text.x = element_blank(),          # fjern dupliserte x-etiketter
    axis.ticks.x = element_blank()
  )


gbk_fa_c <- plot_grid(gbk_ab_fa_c,gbk_problem_c,ncol = 2)
ggsave("GBK_FA.SIRKLER.jpeg", plot = gbk_fa_c, path = "path/Analyser/GRUK_effekt_2025", width = 10, height = 6, dpi = 300)



#### ---- Bekjempelse av gjengroing ---- #### 
## Henter ut lokalitetene MalmC8ya, Padda og HusbergC8ya
gruk25_plots_busk <- gruk25_plots %>%
  filter(Lokalitet == "MalmC8ya" | Lokalitet == "Padda" | Lokalitet == "HusbergC8ya")

gruk25_circles_busk <- gruk25_circles %>%
  filter(Lokalitet == "MalmC8ya" | Lokalitet == "Padda" | Lokalitet == "HusbergC8ya")

## summerer variablene per kombinasjon av rute og tid, gjennomsnitt og standardavvik, til figurer
busk_summary_data <- gruk25_plots_busk %>%
  group_by(Tid_num,Lokalitet) %>%
  summarise(
    mean_n_rl = mean(n_rl, na.rm = TRUE),
    se_n_rl = sd(n_rl, na.rm = TRUE) / sqrt(n()),
    mean_ab_rl = mean(abundans_rl,na.rm=TRUE),
    se_ab_rl = sd(abundans_rl,na.rm=TRUE) / sqrt(n()),
    mean_n_hs = mean(n_hs, na.rm = TRUE),
    se_n_hs = sd(n_hs, na.rm = TRUE) / sqrt(n()),
    mean_ab_hs = mean(abundans_hs,na.rm=TRUE),
    se_ab_hs = sd(abundans_hs,na.rm=TRUE) / sqrt(n()),
    mean_n_fa = mean(n_fa, na.rm = TRUE),
    se_n_fa = sd(n_fa, na.rm = TRUE) / sqrt(n()),
    mean_ab_fa = mean(abundans_fa,na.rm=TRUE),
    se_ab_fa = sd(abundans_fa,na.rm=TRUE) / sqrt(n()),
    mean_n_sr = mean(nr_species, na.rm = TRUE),
    se_n_sr = sd(nr_species, na.rm = TRUE) / sqrt(n()),
    mean_dekning_felt = mean(Field.layer,na.rm=TRUE),
    se_dekning_felt = sd(Field.layer,na.rm=TRUE) / sqrt(n())
  )


## figurer: antall arter og dekning av feltsjikt
busk_ant_arter <- ggplot(busk_summary_data, aes(x = Lokalitet, y = mean_n_sr, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_n_sr - se_n_sr, ymax = mean_n_sr + se_n_sr),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 20)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Antall", fill = "") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  guides(fill = "none")+
  labs(title = "Artsrikdom") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            # legg stripene utenfor panelet
    axis.text.x = element_blank(),          # fjern dupliserte x-etiketter
    axis.ticks.x = element_blank()
  )

busk_feltsjikt <- ggplot(busk_summary_data, aes(x = Lokalitet, y = mean_dekning_felt, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_dekning_felt - se_dekning_felt, ymax = mean_dekning_felt + se_dekning_felt),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 100)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Dekning (%)", fill = "Tid") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  labs(title = "Feltsjikt") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            # legg stripene utenfor panelet
    axis.text.x = element_blank(),          # fjern dupliserte x-etiketter
    axis.ticks.x = element_blank()
  )

busk_veg <- plot_grid(busk_ant_arter,busk_feltsjikt,ncol = 2)
ggsave("BUSK_veg.jpeg", plot = busk_veg, path = "path/Analyser/GRUK_effekt_2025", width = 10, height = 6, dpi = 300)


# figurer: antall og dekning av habitatspesialister
busk_ant_hs <- ggplot(busk_summary_data, aes(x = Lokalitet, y = mean_n_hs, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_n_hs - se_n_hs, ymax = mean_n_hs + se_n_hs),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 5)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Antall", fill = "Tid") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  guides(fill = "none")+
  labs(title = "Antall habitatspesialister") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            # legg stripene utenfor panelet
    axis.text.x = element_blank(),          # fjern dupliserte x-etiketter
    axis.ticks.x = element_blank()
  )

busk_ab_hs <- ggplot(busk_summary_data, aes(x = Lokalitet, y = mean_ab_hs, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_ab_hs - se_ab_hs, ymax = mean_ab_hs + se_ab_hs),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 50)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Dekning (%)", fill = "Tid") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  labs(title = "Dekning habitatspesialister") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            # legg stripene utenfor panelet
    axis.text.x = element_blank(),          # fjern dupliserte x-etiketter
    axis.ticks.x = element_blank()
  )

busk_hs <- plot_grid(busk_ant_hs,busk_ab_hs,ncol = 2)
ggsave("BUSK_HS.jpeg", plot = busk_hs, path = "path/Analyser/GRUK_effekt_2025", width = 10, height = 6, dpi = 300)


## figurer: antall og dekning av rC8dlistearter
busk_ant_rl <- ggplot(busk_summary_data, aes(x = Lokalitet, y = mean_n_rl, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_n_rl - se_n_rl, ymax = mean_n_rl + se_n_rl),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 4)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Antall", fill = "Tid") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  guides(fill = "none")+
  labs(title = "Antall rC8dlistearter") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            # legg stripene utenfor panelet
    axis.text.x = element_blank(),          # fjern dupliserte x-etiketter
    axis.ticks.x = element_blank()
  )

busk_ab_rl <- ggplot(busk_summary_data, aes(x = Lokalitet, y = mean_ab_rl, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_ab_rl - se_ab_rl, ymax = mean_ab_rl + se_ab_rl),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 30)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Dekning (%)", fill = "Tid") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  labs(title = "Dekning rC8dlistearter") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            # legg stripene utenfor panelet
    axis.text.x = element_blank(),          # fjern dupliserte x-etiketter
    axis.ticks.x = element_blank()
  )

busk_rl <- plot_grid(busk_ant_rl,busk_ab_rl,ncol = 2)
ggsave("BUSK_RL.jpeg", plot = busk_rl, path = "path/Analyser/GRUK_effekt_2025", width = 10, height = 6, dpi = 300)


### i sirklene
## summerer variablene per kombinasjon av sirkel og tid, gjennomsnitt og standardavvik, til figurer
busk_summary_data_circles <- gruk25_circles_busk %>%
  group_by(Tid_num,Lokalitet) %>%
  summarise(
    mean_utfordrende_arter = mean(utfordrende_arter, na.rm = TRUE),
    se_utfordrende_arter = sd(utfordrende_arter, na.rm = TRUE) / sqrt(n()),
    mean_slitasje = mean(slitasje,na.rm=TRUE),
    se_slitasje = sd(slitasje,na.rm=TRUE) / sqrt(n()),
    mean_vedplanter = mean(vedplanter,na.rm=TRUE),
    se_vedplanter = sd(vedplanter,na.rm=TRUE) / sqrt(n()),
    mean_fremmede = mean(Circle.invasive.species,na.rm=TRUE),
    se_fremmede = sd(Circle.invasive.species,na.rm=TRUE) / sqrt(n()),
    mean_problem = mean(Circle.problematic.species,na.rm=TRUE),
    se_problem = sd(Circle.problematic.species,na.rm=TRUE) / sqrt(n()))

## figur: dekning av fremmedarter i sirklene
busk_ab_fa_c <- ggplot(busk_summary_data_circles, aes(x = Lokalitet, y = mean_fremmede, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_fremmede - se_fremmede, ymax = mean_fremmede + se_fremmede),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 25)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Dekning (%)", fill = "Tid") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  guides(fill = "none")+
  labs(title = "Dekning fremmede arter") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            # legg stripene utenfor panelet
    axis.text.x = element_blank(),          # fjern dupliserte x-etiketter
    axis.ticks.x = element_blank()
  )

## figur: dekning av vedplanter i sirklene
busk_vedplanter_c <- ggplot(busk_summary_data_circles, aes(x = Lokalitet, y = mean_vedplanter, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_vedplanter - se_vedplanter, ymax = mean_vedplanter + se_vedplanter),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 75)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Dekning (%)", fill = "Tid") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  labs(title = "Dekning vedplanter") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            
    axis.text.x = element_blank(),          
    axis.ticks.x = element_blank()
  )


busk_vedplanter_figur <- plot_grid(busk_vedplanter_c,busk_ab_fa_c,ncol = 2)
ggsave("BUSK.SIRKLER.jpeg", plot = busk_vedplanter_figur, path = "path/Analyser/GRUK_effekt_2025", width = 10, height = 6, dpi = 300)


## figur: dekning av problemarter i sirklene
busk_problem_c <- ggplot(busk_summary_data_circles, aes(x = Lokalitet, y = mean_problem, fill = as.factor(Tid_num))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_problem - se_problem, ymax = mean_problem + se_problem),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0, 50)) +  # Set y-axis limits
  labs(x = "Lokalitet", y = "Dekning (%)", fill = "Tid") +
  scale_fill_manual(values = nina.colours) +
  facet_wrap(~ Lokalitet, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  labs(title = "Dekning problemarter") + 
  theme(plot.title = element_text(hjust = 0.5,size = 14,face = "bold"))+
  theme(
    strip.placement = "outside",            
    axis.text.x = element_blank(),          
    axis.ticks.x = element_blank()
  )


#### ---- Sammenheng mellom slitasje og problemarter ---- ####
## Her har vi bare data over tid fra Padda
## Henter ut T2-data fra Padda fra sirkeldatasettet
Pfiltered_data <- gruk25_circles %>%
  filter(
    Lokalitet == "Padda",
    !str_starts(Circle.mapping.unit, "T4"))

## summerer variablene per kombinasjon av sirkel og tid, gjennomsnitt og standardavvik, til figurer
Psummary_data <- Pfiltered_data %>%
  group_by(Tid_num,Circle.weeding) %>%
  summarise(
    mean_utfordrende = mean(utfordrende_arter, na.rm = TRUE),
    se_utfordrende = sd(utfordrende_arter, na.rm = TRUE) / sqrt(n()),
    mean_slitasje = mean(slitasje, na.rm = TRUE),
    se_slitasje = sd(slitasje, na.rm = TRUE) / sqrt(n())
  )

## i datasettet er tid0 = fC8r tiltak, forsommer 2022, 
## tid1 = etter tiltak, hC8st22, tid2 = forsommer 2023 (etter tiltak, fC8r luking), 
## tid3 = hC8st 2023 (delvis luket), tid4 = forsommer 2024 (delvis luket), tid5 = sensommer 2025 (luket)

## figur: dekning av utfordrende arter (fremmede + problem) som funksjon av luking og tid
## synliggjC8r betydningen av C% luke for C% unngC% etablering av problemarter
Psummary_data$Circle.weeding <- ifelse(
  is.na(Psummary_data$Circle.weeding),
  "Nei",                         # <-- NA blir "Nei"
  ifelse(Psummary_data$Circle.weeding == 1, "Ja", "Nei")
)

Psummary_data$Circle.weeding <- factor(
  Psummary_data$Circle.weeding,
  levels = c("Nei", "Ja")
)


utfordrende_Padda <- ggplot(Psummary_data, aes(x = Tid_num, y = mean_utfordrende, fill = as.factor(Circle.weeding))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +  # Creates grouped bars
  geom_errorbar(
    aes(ymin = mean_utfordrende - se_utfordrende, ymax = mean_utfordrende + se_utfordrende),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_x_continuous(breaks = 0:5) +  
  scale_y_continuous(limits = c(0, 50)) +  # Set y-axis limits
  labs(x = "Tidspunkt etter tiltak", y = "Dekning utfordrende arter i sirkel (%)", fill = "Luking") +
  scale_fill_manual(values = nina.colours) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))

ggsave("Padda_utfordrende.jpeg", plot = utfordrende_Padda, path = "path/Analyser/GRUK_effekt_2025", width = 10, height = 6, dpi = 300)

## Vi kan forvente at det er en tidsforskyvning mellom slitasjen og dekningen av utfordrende arter
## (fC8rst blir det bar jord, sC% koloniserer artene)

## tester derfor sammenhengen mellom problemartsdekning i t og slitasje i t - 1 og t - 2.

## lager variabler for dekning av utfordrende arter for tid t+1 og t+2
Pfiltered_data_lag <- Pfiltered_data %>%
  arrange(Rute, Tid_num) %>%
  group_by(Rute) %>%
  mutate(
    utfordrende_arter_t1 = lead(utfordrende_arter),  # verdien ved tid t+1
    utfordrende_arter_t2 = lead(utfordrende_arter,n=2),# verdien ved tid t + 2
    luking_t1 = lead(Circle.weeding), 
    slitasje_t = slitasje                              # verdien ved tid t
  ) %>%
  ungroup()

library(lme4)

## slitasje tid t, dekning av utfordrende arter t + 1
mod_lmm <- lmer(utfordrende_arter_t1 ~ slitasje_t + (1|Rute), data = Pfiltered_data_lag)
summary(mod_lmm) # den er ikke signifikant, men tendens til positiv. 

## slitasje tid t, dekning av utfordrende arter t + 2
mod_lmm2 <- lmer(utfordrende_arter_t2 ~ slitasje_t + (1|Rute), data = Pfiltered_data_lag)
summary(mod_lmm2) # veldig signifikant

# figur som viser sammenhengen
slitasje_problemarter <- ggplot(Pfiltered_data_lag, aes(x = slitasje_t, y = utfordrende_arter_t2,)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Slitasje ved tid t", y = "Dekning av utfordrende arter ved tid t+2")+
  scale_fill_manual(values = nina.colours) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.text.y = element_text(angle = 0, hjust = 1,size=12))+
  theme(axis.title = element_text(size=16))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))

ggsave("Padda_slitasje.jpeg", plot = slitasje_problemarter, path = "path/Analyser/GRUK_effekt_2025", width = 10, height = 6, dpi = 300)

