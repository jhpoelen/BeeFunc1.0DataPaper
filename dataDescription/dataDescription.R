##Packages
library(tidyverse)
library(fmsb)
library(devtools)
library(ggradar)

##import data
BeeFuncFiles <- list.files(pattern="^BeeFunc.*\\.csv$")
BeeFuncData <- lapply(BeeFuncFiles, read.csv2)
names(BeeFuncData) <- str_remove(basename(BeeFuncFiles), "\\.csv$")
list2env(BeeFuncData, envir = .GlobalEnv)

CLFrance <- read.csv2("CHECKLIST_FRANCE_RTU_EN.csv", sep=";", header=TRUE)
trait_groups <- read.csv2("trait_groups.csv",  sep=",", header=TRUE)

##PALETTES

palette_family <- c("Andrenidae" = "#cb4335",
                    "Apidae" = "#e67e22",
                    "Colletidae" = "#f1c40f",
                    "Halictidae" = "#27ae60",
                    "Megachilidae" = "#5dade2",
                    "Melittidae" = "#9b59b6")

palette_traits <- c("Body length" = "#c9211e",
                    "ITD" ="#900C3F",
                    "Tongue length"="#C70039",
                    "Wing length" ="#FF4500",
                    "Dry mass" ="#FF5733",
                    "Hair length" ="#ff967b",
                    "Phenology" ="#1b4f72",
                    "Voltinism"="#2874a6",
                    "Sociality" = "#3498db",
                    "Lectism" ="#145a32",
                    "Pollination" ="#229954",
                    "Parasitism" = "#27ae60",
                    "Nesting" ="#7dcea0",
                    "Foraging distance" = "#9fe977",
                    "Biogeographical status" ="#f6dd57",
                    "Altitude" ="#FF8C00",
                    "Ecological affinity"="#0b5345",
                    "IUCN Status"="#935116",
                    "Habitat" ="#ffc107")




## Number of species for France

nb_spc_fr <- CLFrance %>%
  distinct(VALID_NAME) %>%
  summarise(nb_spc_fr = n()) %>%
  pull()

nb_spc_familles <- CLFrance %>%
  group_by(FAMILY) %>%
  summarise(nb_spc_familles =n())

nb_spc_genres <- CLFrance %>%
  group_by(GENUS) %>%
  summarise(nb_spc_genres = n())

## Number of species per trait
NB_SPC_ALTITUDE <- BeeFuncAltitude %>%
  distinct(VALID_NAME) %>%
  summarise(nb_spc_altitude = n()) %>%
  pull()

NB_SPC_VOLTINISME <- BeeFuncVoltinism %>%
  distinct(VALID_NAME) %>%
  summarise(nb_spc_voltinisme = n()) %>%
  pull()

NB_SPC_PHENOLOGIE <- BeeFuncPhenology %>%
  distinct(VALID_NAME) %>%
  summarise(nb_spc_masse = n()) %>%
  pull()

NB_SPC_SOCIABILITE <- BeeFuncSociality %>%
  distinct(VALID_NAME) %>%
  summarise(nb_spc_sociabilite = n()) %>%
  pull()

NB_SPC_STATUT_BIOGEOGRAPHIQUE <- BeeFuncBiogeographicalStatus %>%
  distinct(VALID_NAME) %>%
  summarise(nb_spc_statut_biogeographique = n()) %>%
  pull()

NB_SPC_STATUT_IUCN <- BeeFuncIUCNStatus %>%
  distinct(VALID_NAME) %>%
  summarise(nb_spc_iucn = n()) %>%
  pull()

NB_SPC_LONGUEUR_CORPS <- BeeFuncBodyLength %>%
  distinct(VALID_NAME) %>%
  summarise(nb_spc_corps = n()) %>%
  pull()

NB_SPC_LONGUEUR_LANGUE <- BeeFuncTongueLength %>%
  distinct(VALID_NAME) %>%
  summarise(nb_spc_langue = n()) %>%
  pull()

NB_SPC_ITD <- BeeFuncITD %>%
  distinct(VALID_NAME) %>%
  summarise(nb_spc_itd = n()) %>%
  pull()

NB_SPC_AFFINITE_ECOLOGIQUE <- BeeFuncEcologicalAffinity %>%
  distinct(VALID_NAME) %>%
  summarise(nb_spc_affinite_ecologique = n()) %>%
  pull()

NB_SPC_LECTISME <- BeeFuncLectism %>%
  distinct(VALID_NAME) %>%
  summarise(nb_spc_lectisme = n()) %>%
  pull()

NB_SPC_POLLINISATION <- BeeFuncPollination %>%
  distinct(VALID_NAME) %>%
  summarise(nb_spc_pollinisation = n()) %>%
  pull()

NB_SPC_PARASITISME <- BeeFuncParasitism %>%
  distinct(VALID_NAME) %>%
  summarise(nb_spc_parasite = n()) %>%
  pull()

NB_SPC_HABITAT <- BeeFuncHabitat %>%
  distinct(VALID_NAME) %>%
  summarise(nb_spc_habitat = n()) %>%
  pull()

NB_SPC_NESTTYPE <- BeeFuncNestType %>%
  distinct(VALID_NAME) %>%
  summarise(nb_spc_nesting = n()) %>%
  pull()

## COMPLETENESS FOR EACH TRAIT ----
CPTD_ALTITUDE <- round(NB_SPC_ALTITUDE/nb_spc_fr*100,2)
CPTD_VOLTINISME <- round(NB_SPC_VOLTINISME/nb_spc_fr*100,2)
CPTD_PHENOLOGIE <- round(NB_SPC_PHENOLOGIE/nb_spc_fr*100,2)
CPTD_SOCIABILITE <- round(NB_SPC_SOCIABILITE/nb_spc_fr*100,2)
CPTD_STATUT_BIOGEOGRAPHIQUE <- round(NB_SPC_STATUT_BIOGEOGRAPHIQUE/nb_spc_fr*100,2)
CPTD_STATUT_IUCN <- round(NB_SPC_STATUT_IUCN/nb_spc_fr*100,2)
CPTD_LONGUEUR_CORPS <- round(NB_SPC_LONGUEUR_CORPS/nb_spc_fr*100,2)
CPTD_LONGUEUR_LANGUE <- round(NB_SPC_LONGUEUR_LANGUE/nb_spc_fr*100,2)
CPTD_ITD <- round(NB_SPC_ITD/nb_spc_fr*100,2)
CPTD_AFFINITE_ECOLOGIQUE <- round(NB_SPC_AFFINITE_ECOLOGIQUE/nb_spc_fr*100,2)
CPTD_LECTISME <- round(NB_SPC_LECTISME/nb_spc_fr*100,2)
CPTD_POLLINISATION <- round(NB_SPC_POLLINISATION/nb_spc_fr*100,2)
CPTD_PARASITISME <- round(NB_SPC_PARASITISME/216*100,2)
CPTD_HABITAT <- round(NB_SPC_HABITAT/nb_spc_fr*100,2)
CPTD_NESTTYPE <- round(NB_SPC_NESTTYPE/nb_spc_fr*100,2)

### Creating vectors ----
NB_SPC <- c(NB_SPC_LONGUEUR_CORPS, NB_SPC_ITD, NB_SPC_LONGUEUR_LANGUE, NB_SPC_PHENOLOGIE, NB_SPC_VOLTINISME, NB_SPC_SOCIABILITE, NB_SPC_LECTISME, NB_SPC_POLLINISATION, NB_SPC_PARASITISME, NB_SPC_STATUT_BIOGEOGRAPHIQUE, NB_SPC_ALTITUDE, NB_SPC_AFFINITE_ECOLOGIQUE, NB_SPC_HABITAT, NB_SPC_STATUT_IUCN, NB_SPC_NESTTYPE)
CPTD <- c(CPTD_LONGUEUR_CORPS, CPTD_ITD, CPTD_LONGUEUR_LANGUE, CPTD_PHENOLOGIE, CPTD_VOLTINISME, CPTD_SOCIABILITE, CPTD_LECTISME, CPTD_POLLINISATION, CPTD_PARASITISME, CPTD_STATUT_BIOGEOGRAPHIQUE, CPTD_ALTITUDE, CPTD_AFFINITE_ECOLOGIQUE, CPTD_HABITAT, CPTD_STATUT_IUCN, CPTD_NESTTYPE)
row_names <- c("Body length", "ITD", "Tongue length", "Phenology", "Voltinism", "Sociality", "Lectism", "Pollination", "Parasitism", "Biogeographical status", "Altitude", "Ecological affinity", "Habitat", "IUCN Status", "Nesting")

### Creation of the table ----
trait_completeness_TAB_temp <- data.frame(NB_SPC, CPTD) %>%
  rename("Number of reported species" = "NB_SPC") %>%
  rename("Percentage of completeness" = "CPTD")
row.names(trait_completeness_TAB_temp) <- row_names

trait_completeness_TAB_temp <- trait_completeness_TAB_temp %>%
  rownames_to_column(var="Trait")

trait_completeness_TAB_temp <- left_join(trait_completeness_TAB_temp, trait_groups, by="Trait")


### graph trait completeness points----
trait_completeness_TAB_temp_plot_points <- trait_completeness_TAB_temp %>%
  arrange(Trait_Type, `Percentage of completeness`) %>%  # Tri au sein de chaque groupe par `Percentage of completeness`
  mutate(Trait = factor(Trait, levels = unique(Trait))) %>%  # Définition de l'ordre des niveaux de Trait
  ggplot(aes(x = Trait, y = `Percentage of completeness`, color = Trait)) +
  geom_segment(aes(xend = Trait, yend = 0), color = "grey") +  # Augmenter la largeur ici
  geom_point(size = 4) +
  scale_colour_manual(values=palette_traits) +
  coord_flip() +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10, face="bold"),  # Taille du texte des étiquettes de facettes
    strip.background = element_rect(fill = "lightblue", color = "grey90"),  # Couleur de fond et bordure des facettes
    strip.placement = "outside"
  ) +
  xlab("") +
  ylab("Completeness (%)") +
  facet_wrap(~Trait_Type, ncol = 1, scales = "free_y")


### COMPLETENESS FOR SPECIES

### SPECIES----
##List trait files
trait_files <- list.files(pattern ="BeeFunc.*\\.csv", full.names = TRUE)

##Put them in a list
trait_files_list <- lapply(trait_files, read_csv2)

## Create the completeness table for each species
CPTD_SPC <- CLFrance %>%
  mutate(count = 0)

# Browse each functional trait dataframe in the list and count species occurrences
for(trait_files in trait_files_list) {
  # Vérifier quelles espèces de CL_FRANCE_temp sont présentes dans le fichier de traits
  species_present <- CPTD_SPC$VALID_NAME %in% trait_files$VALID_NAME
  # Mettre dans CPTD_SPC
  CPTD_SPC$count <- CPTD_SPC$count + species_present
}

species_completeness_TAB <- CPTD_SPC %>%
  mutate(percentage = round(count/20*100,2)) %>%
  select(VALID_NAME, count, percentage) %>%
  filter(VALID_NAME != "") %>%
  rename("Number of trait reported" = "count") %>%
  rename("Percentage of completeness" = "percentage") %>%
  rename("Species name" = "VALID_NAME")
species_completeness_TAB



### COMPLETENESS FOR FAMILIES
##FAMILIES----
# Number of species per family

nbSpcFam <- CLFrance %>%
  group_by(FAMILY) %>%
  mutate(nbSpc = n()) %>%
  select(FAMILY, nbSpc) %>%
  distinct() %>%
  ungroup()


### Create a file ----
### Creation of a function to extract the data
filesFam <- list.files(pattern ="^BeeFunc.*\\.csv$", full.names = TRUE)

##storage list
data_list_family <- list()

##loop to retrieve columns from all files
for (file in filesFam) {
  data <- read.csv2(file)
  data_subset <- data %>%
    select(VALID_NAME, FAMILY) %>%
    distinct()
  if (nrow(data_subset) > 0) {
    data_subset$source_file <- basename(file)
    data_list_family[[length(data_list_family) + 1]] <- data_subset
  }
}

## concatenate
combined_data_family <- do.call(rbind, data_list_family)

spcList <- combined_data_family %>%
  select(VALID_NAME) %>%
  distinct()

### Number of species per family
spc_par_fam <- combined_data_family %>%
  group_by(FAMILY) %>%
  summarise(nb_par_fam = n_distinct(VALID_NAME))

### Completeness of traits for families
CPTD_fam <- left_join(spc_par_fam, nb_spc_familles, by = "FAMILY") %>%
  mutate(percentage=round(nb_par_fam/nb_spc_familles*100,2))

### Calculation of number of species per family and trait ----
nb_spc_famille_trait <- combined_data_family %>%
  group_by(FAMILY, source_file) %>%
  summarize(nb_spc =n_distinct(VALID_NAME))

nb_spc_famille_trait <- left_join(nb_spc_famille_trait, nbSpcFam, by="FAMILY") %>%
  mutate(proportion=round(nb_spc / nbSpc*100,2))


nbSpcFamTraitWide <- nb_spc_famille_trait %>%
  select(FAMILY, source_file, nb_spc) %>%
  pivot_wider(names_from = FAMILY, values_from = nb_spc) %>%
  mutate_all(~ replace_na(., 0)) %>%
  mutate(Total = rowSums(across(where(is.numeric))))


### Percentage of species by family and trait ----

propSpcFamTraitWide <- nb_spc_famille_trait %>%
  select(FAMILY, source_file, proportion) %>%
  pivot_wider(names_from = FAMILY, values_from = proportion) %>%
  mutate_all(~ replace_na(., 0))

### radar chart for family by trait
propSpcFamTraitWideRadar <- propSpcFamTraitWide %>%
  rename("groups" = "source_file")


palette_traits <- c("Body length" = "#c9211e",
                    "ITD" ="#900C3F",
                    "Tongue length"="#C70039",
                    "Wing length" ="#FF4500",
                    "Dry mass" ="#FF5733",
                    "Hair length" ="#ff967b",
                    "Phenology" ="#1b4f72",
                    "Voltinism"="#2874a6",
                    "Sociality" = "#3498db",
                    "Lectism" ="#145a32",
                    "Pollination" ="#229954",
                    "Parasitism" = "#27ae60",
                    "Nesting" ="#7dcea0",
                    "Foraging distance" = "#9fe977",
                    "Biogeographical status" ="#f6dd57",
                    "Altitude" ="#FF8C00",
                    "Ecological affinity"="#0b5345",
                    "IUCN Status"="#935116",
                    "Habitat" ="#ffc107")

## Radar Altitude
radarAltitudeFam <- ggradar(
  propSpcFamTraitWideRadar %>% filter(groups=="BeeFuncAltitude.csv"), 
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = "#FF8C00",
  background.circle.colour = "white",
  fill=TRUE,
  gridline.mid.colour = "grey",
  axis.label.size=8
)

## Radar Biogeographic status

radarBiogeoStatusFam <- ggradar(
  propSpcFamTraitWideRadar %>% filter(groups == "BeeFuncBiogeographicalStatus.csv"), 
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = "#FFD700",
  background.circle.colour = "white",
  fill=TRUE,
  gridline.mid.colour = "grey",
  axis.label.size=8
)

## Radar BodyLength

radarBodyLengthFam <- ggradar(
  propSpcFamTraitWideRadar %>% filter(groups == "BeeFuncBodyLength.csv"), 
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = "#c9211e",
  background.circle.colour = "white",
  fill=TRUE,
  gridline.mid.colour = "grey",
  axis.label.size=8
)

## Radar EcologicalAffinity

radarEcologicalAffinityFam <- ggradar(
  propSpcFamTraitWideRadar %>% filter(groups=="BeeFuncEcologicalAffinity.csv"), 
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = "#0b5345",
  background.circle.colour = "white",
  fill=TRUE,
  gridline.mid.colour = "grey",
  axis.label.size=8
)

## Radar ITD

radarITDFam <- ggradar(
  propSpcFamTraitWideRadar %>% filter(groups == "BeeFuncITD.csv"), 
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = "#900C3F",
  background.circle.colour = "white",
  fill=TRUE,
  gridline.mid.colour = "grey",
  axis.label.size=8)


## Radar IUCN STATUS

radarIUCNStatusFam <- ggradar(
  propSpcFamTraitWideRadar %>% filter(groups=="BeeFuncIUCNStatus.csv"), 
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = "#935116",
  background.circle.colour = "white",
  fill=TRUE,
  gridline.mid.colour = "grey",
  axis.label.size=8)


## Radar Lectism

radarLectismFam <- ggradar(
  propSpcFamTraitWideRadar %>% filter(groups == "BeeFuncLectism.csv"), 
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = "#145a32",
  background.circle.colour = "white",
  fill=TRUE,
  gridline.mid.colour = "grey",
  axis.label.size=8)


## Radar Parasitism

radarParasitismFam <- ggradar(
  propSpcFamTraitWideRadar %>% filter(groups == "BeeFuncParasitism.csv"), 
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = "#27ae60",
  background.circle.colour = "white",
  fill=TRUE,
  gridline.mid.colour = "grey",
  axis.label.size=8)


## Radar Phenology

radarPhenologyFam <- ggradar(
  propSpcFamTraitWideRadar %>% filter(groups == "BeeFuncPhenology.csv"), 
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = "#1b4f72",
  background.circle.colour = "white",
  fill=TRUE,
  gridline.mid.colour = "grey",
  axis.label.size=8)

## Radar Pollination

radarPollinationFam <- ggradar(
  propSpcFamTraitWideRadar %>% filter(groups == "BeeFuncPollination.csv"), 
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = "#229954",
  background.circle.colour = "white",
  fill=TRUE,
  gridline.mid.colour = "grey",
  axis.label.size=8)

## Radar Sociality

radarSocialityFam <- ggradar(
  propSpcFamTraitWideRadar %>% filter(groups=="BeeFuncSociality.csv"), 
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = "#3498db",
  background.circle.colour = "white",
  fill=TRUE,
  gridline.mid.colour = "grey",
  axis.label.size=8)


## Radar Voltinism

radarVoltinismFam <- ggradar(
  propSpcFamTraitWideRadar %>% filter(groups=="BeeFuncVoltinism.csv"), 
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = "#2874a6",
  background.circle.colour = "white",
  fill=TRUE,
  gridline.mid.colour = "grey",
  axis.label.size=8)


## Radar TongueLength

radarTongueLengthFam <- ggradar(
  propSpcFamTraitWideRadar %>% filter(groups=="BeeFuncTongueLength.csv"), 
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = "#C70039",
  background.circle.colour = "white",
  fill=TRUE,
  gridline.mid.colour = "grey",
  axis.label.size=8)


## Radar Habitat

radarHabitat <- ggradar(
  propSpcFamTraitWideRadar %>% filter(groups=="BeeFuncHabitat.csv"), 
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = "#ffc107",
  background.circle.colour = "white",
  fill=TRUE,
  gridline.mid.colour = "grey",
  axis.label.size=8)


radarAltitudeFam
radarBiogeoStatusFam
radarBodyLengthFam
radarEcologicalAffinityFam
radarITDFam
radarIUCNStatusFam
radarLectismFam
radarParasitismFam
radarPhenologyFam
radarPollinationFam
radarSocialityFam
radarVoltinismFam
radarTongueLengthFam
radarHabitat



##For nesting
### Create a file ----
### Creation of a function to extract the data
nesting <- list.files(pattern="^BeeFuncNest.*\\.csv$")


##storage list
data_list_nesting <- list()

##loop to retrieve columns from all files
for (file in nesting) {
  data <- read.csv2(file)
  data_subset <- data %>%
    select(VALID_NAME, FAMILY) %>%
    distinct()
  if (nrow(data_subset) > 0) {
    data_subset$source_file <- basename(file)
    data_list_nesting[[length(data_list_nesting) + 1]] <- data_subset
  }
}

## concatenate
combined_data_nesting <- do.call(rbind, data_list_nesting)

### Number of species per family
spc_par_nesting <- combined_data_nesting %>%
  group_by(FAMILY) %>%
  summarise(nb_par_fam = n_distinct(VALID_NAME))

### Completeness of traitfor families
CPTD_nesting <- left_join(spc_par_nesting, nb_spc_familles, by = "FAMILY") %>%
  mutate(percentage=round(nb_par_fam/nb_spc_familles*100,2))

### Calculation of number of species per family and trait ----
nb_spc_famille_nesting <- combined_data_nesting %>%
  group_by(FAMILY, source_file) %>%
  summarize(nb_spc =n_distinct(VALID_NAME))

nb_spc_famille_nesting <- left_join(nb_spc_famille_nesting, nbSpcFam, by="FAMILY") %>%
  mutate(proportion=round(nb_spc / nbSpc*100,2))


nbSpcFamNestingWide <- nb_spc_famille_nesting %>%
  select(FAMILY, source_file, nb_spc) %>%
  pivot_wider(names_from = FAMILY, values_from = nb_spc) %>%
  mutate_all(~ replace_na(., 0)) %>%
  mutate(Total = rowSums(across(where(is.numeric))))


### Percentage of species by family and trait ----

propSpcFamNestingtWide <- nb_spc_famille_trait %>%
  select(FAMILY, source_file, proportion) %>%
  pivot_wider(names_from = FAMILY, values_from = proportion) %>%
  mutate_all(~ replace_na(., 0))

##radar chart

CPTDNestingRadar <- CPTD_nesting %>%
  select(FAMILY, percentage) %>%
  pivot_wider(names_from = FAMILY, values_from = percentage) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(groups = "nesting") %>%
  select(groups, Andrenidae, Apidae, Colletidae, Halictidae, Megachilidae, Melittidae)


radarNestingFam <- ggradar(
  CPTDNestingRadar, 
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = "#7dcea0",
  background.circle.colour = "white",
  fill=TRUE,
  gridline.mid.colour = "grey",
  axis.label.size=8)

radarNestingFam




### COMPLETENESS OF GENERA
### GENERA ----
# Nombre d'espèces par genre

nbSpcGen <- CLFrance %>%
  group_by(GENUS) %>%
  mutate(nbSpc = n()) %>%
  select(GENUS, nbSpc) %>%
  distinct() %>%
  ungroup()


### Create a file ----
### Creation of a function to extract the data
filesGen <- list.files(pattern ="^BeeFunc.*\\.csv$", full.names = TRUE)

##liste de stockage
data_list_genus <- list()

##loop to retrieve columns from all files
for (file in filesGen) {
  data <- read.csv2(file)
  data_subset <- data %>%
    select(VALID_NAME, GENUS) %>%
    distinct()
  if (nrow(data_subset) > 0) {
    data_subset$source_file <- basename(file)
    data_list_genus[[length(data_list_genus) + 1]] <- data_subset
  }
}

## concatenate
combined_data_genus <- do.call(rbind, data_list_genus)

### Number of species per genus

spc_par_gen <- combined_data_genus %>%
  select(VALID_NAME, GENUS) %>%
  distinct() %>%
  group_by(GENUS) %>%
  mutate(nbSpcGen = n()) %>%
  ungroup()


### Trait completeness for genera
CPTD_gen <- left_join(spc_par_gen, nb_spc_genres, by = "GENUS") %>%
  mutate(percentage=round(nbSpcGen/nb_spc_genres*100,2)) %>%
  select(GENUS, nbSpcGen, nb_spc_genres, percentage) %>%
  distinct()

### Calculation of number of species per genus and trait ----
nb_spc_genre_trait <- combined_data_genus %>%
  group_by(GENUS, source_file) %>%
  summarize(nb_spc =n_distinct(VALID_NAME))

nb_spc_genre_trait <- left_join(nb_spc_genre_trait, nbSpcGen, by="GENUS") %>%
  mutate(proportion=round(nb_spc / nbSpc*100,2))


nbSpcGenTraitWide <- nb_spc_genre_trait %>%
  select(GENUS, source_file, nb_spc) %>%
  pivot_wider(names_from = source_file, values_from = nb_spc) %>%
  mutate_all(~ replace_na(., 0)) %>%
  mutate(Total = rowSums(across(where(is.numeric))))


### Percentage of species by family and trait ----

propSpcGenTraitWide <- nb_spc_genre_trait %>%
  select(GENUS, source_file, proportion) %>%
  pivot_wider(names_from = source_file, values_from = proportion) %>%
  mutate_all(~ replace_na(., 0))


### TOTAL COMPLETENESS OF THE DATABASE

beeFuncFiles <- list.files(pattern ="^BeeFunc.*\\.csv$", full.names = TRUE)

totalInformation <- beeFuncFiles %>%
  map_dfr(~ read.csv2(.x, colClasses = "character") %>%
            mutate(source_file = basename(.x))) %>%
  select(VALID_NAME, FAMILY, GENUS, source_file)

nbTotalInformation <- nrow(totalInformation)

uniqueSpcTrait <- totalInformation %>%
  select(VALID_NAME, source_file) %>%
  distinct() %>%
  nrow()

#total completeness of BeeFunc (based on 983 species for every trait except for parasitic interactions, for which only the 216 parasitic species in France are considered)
totalCompleteness <- round(uniqueSpcTrait/((983*19)+(216*1))*100,2)

nbSpcBeeFunc <- totalInformation %>%
  select(VALID_NAME) %>%
  distinct() %>%
  nrow()


nbInfoAndrenidae <- totalInformation %>%
  filter(FAMILY == "Andrenidae") %>%
  distinct() %>%
  nrow()

nbInfoApidae <- totalInformation %>%
  filter(FAMILY == "Apidae") %>%
  distinct() %>%
  nrow()

nbInfoColletidae <- totalInformation %>%
  filter(FAMILY == "Colletidae") %>%
  distinct() %>%
  nrow()

nbInfoHalictidae <- totalInformation %>%
  filter(FAMILY == "Halictidae") %>%
  distinct() %>%
  nrow()

nbInfoMegachilidae <- totalInformation %>%
  filter(FAMILY == "Megachilidae") %>%
  distinct() %>%
  nrow()

nbInfoMelittidae <- totalInformation %>%
  filter(FAMILY == "Melittidae") %>%
  distinct() %>%
  nrow()

completenessAndrenidae <- round(nbInfoAndrenidae/(20*194)*100,2)
completenessApidae <- round(nbInfoApidae/(20*287)*100,2)
completenessColletidae <- round(nbInfoColletidae/(20*83)*100,2)
completenessHalictidae <- round(nbInfoHalictidae/(20*186)*100,2)
completenessMegachilidae <- round(nbInfoMegachilidae/(20*209)*100,2)
completenessMelittidae <- round(nbInfoMelittidae/(20*17)*100,2)

Families <- c("Andrenidae", "Apidae", "Colletidae", "Halictidae", "Megachilidae", "Melittidae")
completenessFamiliesList <- c(completenessAndrenidae,completenessApidae,completenessColletidae,completenessHalictidae,completenessMegachilidae,completenessMelittidae)

completenessFamilies <- data.frame(Families, completenessFamiliesList) %>%
  rename("Level of completeness (%)" = "completenessFamiliesList")

completenessGenus <- totalInformation %>%
  group_by(GENUS) %>%
  mutate(nbInfoGenus = n_distinct(VALID_NAME, source_file)) %>%
  ungroup()

completenessGenus <- left_join(completenessGenus, nbSpcGen, by = "GENUS") %>%
  mutate(completeGenus = nbSpc *20) %>%
  mutate(completenessGenus = round(nbInfoGenus/completeGenus*100,2)) %>%
  select(GENUS, completenessGenus) %>%
  distinct() %>%
  arrange(GENUS) %>%
  filter(!is.na(GENUS)) %>%
  rename("Genus" = "GENUS") %>%
  rename("Level of completeness (%)" = "completenessGenus")


#radar chart for parasitic species only (146 Apidae, 32 Halictidae, 38 Megachilidae)

parasiteInfos <- BeeFuncParasitism %>%
  group_by(FAMILY) %>%
  mutate(nbReportedSpecies = n_distinct(VALID_NAME)) %>%
  select(FAMILY, nbReportedSpecies) %>%
  distinct()

parasiteFamille <- c(146, 32, 38)
FAMILY <- c("Apidae", "Halictidae", "Megachilidae")
parasiteFrance <- data.frame(FAMILY, parasiteFamille)

radarChartParasitic <- parasiteInfos %>%
  left_join(parasiteFrance, by ="FAMILY") %>%
  mutate(proportion=round(nbReportedSpecies/parasiteFamille*100, 2)) %>%
  select(FAMILY, proportion) %>%
  pivot_wider(names_from = FAMILY, values_from = proportion) %>%
  mutate(trait = "parasitismOnly") %>%
  select(trait, Apidae, Halictidae, Megachilidae)

radarParasitismFamOnly <- ggradar(
  radarChartParasitic,
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = "#27ae60",
  background.circle.colour = "white",
  fill=TRUE,
  gridline.mid.colour = "grey",
  axis.label.size=8)


#bibliographic references

### Extract all bibliographic sources from all files in the database
BeeFuncFiles <- list.files(pattern ="^BeeFunc.*\\.csv$", full.names = TRUE)

BeeFuncFiles <- list.files(pattern="^BeeFunc.*\\.csv$")
BeeFuncData <- lapply(BeeFuncFiles, read.csv2)
names(BeeFuncData) <- str_remove(basename(BeeFuncFiles), "\\.csv$")
list2env(BeeFuncData, envir = .GlobalEnv)


BeeFunc_all_sources <- BeeFuncFiles %>%
  map_dfr(~ read.csv2(.x, colClasses = "character") %>%
            select(SOURCE))


BeeFunc_all_sources <- as_tibble(BeeFunc_all_sources)




# create a vector with unique sources
BeeFunc_unique_sources <- BeeFunc_all_sources %>%
  distinct()

# list sources from the literature
BeeFunc_all_litterature <- BeeFunc_all_sources %>%
  filter(!str_detect(SOURCE, "Personnal communication"))

BeeFunc_unique_litterature <- BeeFunc_all_litterature %>%
  distinct()

# list sources from experts

BeeFunc_all_experts <- BeeFunc_all_sources %>%
  filter(str_detect(SOURCE, "Personnal communication")) %>%
  filter(!str_detect(SOURCE, "Genoud, D. 2024. Personnal communication."))

BeeFunc_unique_experts <- BeeFunc_all_experts %>%
  distinct()

# list the appearances of David Genoud's expert opinion
BeeFunc_all_DGe <- BeeFunc_all_sources %>%
  filter(str_detect(SOURCE, "Genoud, D. 2024. Personnal communication."))

BeeFunc_unique_DGe <- BeeFunc_all_DGe %>%
  distinct()

# create vectors
reference_type <- c("Total", "Scientific litterature", "Expert opinion", "Personnal communication")
number_of_trait_info <- c(nrow(BeeFunc_all_sources), nrow(BeeFunc_all_litterature), nrow(BeeFunc_all_DGe), nrow(BeeFunc_all_experts))
number_of_references <- c(nrow(BeeFunc_unique_sources), nrow(BeeFunc_unique_litterature), nrow(BeeFunc_unique_DGe), nrow(BeeFunc_unique_experts))

# create the summary table
BeeFunc_litterature_tab <- data.frame(reference_type, number_of_trait_info, number_of_references)
BeeFunc_litterature_tab <- BeeFunc_litterature_tab %>%
  mutate(Proportion =round((number_of_trait_info/nrow(BeeFunc_all_sources))*100, 2)) %>%
  rename("Reference type" ="reference_type") %>%
  rename("Number of trait information" = "number_of_trait_info") %>%
  rename("Number of references" = "number_of_references")

## Most frequently cited sources
citationRate <- BeeFunc_all_sources %>%
  group_by(SOURCE) %>%
  mutate(numberOfCitation = n()) %>%
  ungroup() %>%
  distinct()
