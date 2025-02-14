#import packages
library(tidyverse)

#data import
baseBeeFuncFiles <- list.files(pattern="^baseBeeFunc.*\\.csv$")
BeeFuncData <- lapply(baseBeeFuncFiles, read.csv2)
names(BeeFuncData) <- str_remove(basename(baseBeeFuncFiles), "\\.csv$")
list2env(BeeFuncData, envir = .GlobalEnv)

abeillesTXRF <- read.csv2("TXRF17_ABEILLES.csv", header=TRUE)
abeillesTXRF <- abeillesTXRF %>%
  mutate(across(everything(),as.character))



##Creating files

BeeFuncBodyLength <- baseBeeFuncLength %>%
  filter(ANATOMICAL_ENTITY == " body") %>%
  mutate(BeeFuncID = paste0("bfBod", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
  mutate(across(everything(), str_squish)) %>%
  filter(str_count(LB_NOM, "\\S+") != 3) %>%
  filter(str_count(LB_NOM, "\\S+") != 3) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))
write.csv2(BeeFuncBodyLength, file="BeeFuncBodyLength.csv", row.names = FALSE)

BeeFuncITD <- baseBeeFuncLength %>%
  filter(DIMENSION == " intertegular distance") %>%
  mutate(BeeFuncID = paste0("bfITD", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
  mutate(across(everything(), str_squish)) %>%
  filter(str_count(LB_NOM, "\\S+") != 3) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))
write.csv2(BeeFuncITD, file="BeeFuncITD.csv", row.names = FALSE)

BeeFuncTongueLength <- baseBeeFuncLength %>%
  filter(ANATOMICAL_ENTITY == " body > head > tongue") %>%
  mutate(BeeFuncID = paste0("bfTon", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
  mutate(across(everything(), str_squish)) %>%
  filter(str_count(LB_NOM, "\\S+") != 3) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))
write.csv2(BeeFuncTongueLength, file="BeeFuncTongueLength.csv", row.names = FALSE)

BeeFuncPhenology <- baseBeeFuncPhenology %>%
  mutate(BeeFuncID = paste0("bfPhe", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
  mutate(across(everything(), str_squish)) %>%
  filter(str_count(LB_NOM, "\\S+") != 3) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))
write.csv2(BeeFuncPhenology, file="BeeFuncPhenology.csv", row.names = FALSE)

BeeFuncVoltinism <- baseBeeFuncVoltinism %>%
  mutate(BeeFuncID = paste0("bfVol", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
  mutate(across(everything(), str_squish)) %>%
  filter(str_count(LB_NOM, "\\S+") != 3) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))
write.csv2(BeeFuncVoltinism, file="BeeFuncVoltinism.csv", row.names = FALSE)

BeeFuncSociality <- baseBeeFuncSociality %>%
  mutate(BeeFuncID = paste0("bfSoc", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
  mutate(across(everything(), str_squish)) %>%
  filter(str_count(LB_NOM, "\\S+") != 3) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))
write.csv2(BeeFuncSociality, file="BeeFuncSociality.csv", row.names = FALSE)

BeeFuncBiogeographicalStatus <- baseBeeFuncBiogeographicalStatus %>%
  mutate(BeeFuncID = paste0("bfBio", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
  mutate(across(everything(), str_squish)) %>%
  filter(str_count(LB_NOM, "\\S+") != 3) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))
write.csv2(BeeFuncBiogeographicalStatus, file="BeeFuncBiogeographicalStatus.csv", row.names = FALSE)

BeeFuncAltitude <- baseBeeFuncAltitude %>%
  mutate(BeeFuncID = paste0("bfAlt", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
  mutate(across(everything(), str_squish)) %>%
  filter(str_count(LB_NOM, "\\S+") != 3) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))
write.csv2(BeeFuncAltitude, file="BeeFuncAltitude.csv", row.names = FALSE)

BeeFuncEcologicalAffinity <- baseBeeFuncEcologicalAffinity %>%
  mutate(BeeFuncID = paste0("bfEco", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
  mutate(across(everything(), str_squish)) %>%
  filter(str_count(LB_NOM, "\\S+") != 3) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))
write.csv2(BeeFuncEcologicalAffinity, file="BeeFuncEcologicalAffinity.csv", row.names = FALSE)

BeeFuncIUCNStatus <- baseBeeFuncIUCNstatus %>%
  mutate(BeeFuncID = paste0("bfIUCN", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
  mutate(across(everything(), str_squish)) %>%
  filter(str_count(LB_NOM, "\\S+") != 3) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))
write.csv2(BeeFuncIUCNStatus, file="BeeFuncIUCNStatus.csv", row.names = FALSE)

BeeFuncNestDepth <- baseBeeFuncNestDepth %>%
  mutate(BeeFuncID = paste0("bfNestDep", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
  mutate(across(everything(), str_squish)) %>%
  filter(str_count(LB_NOM, "\\S+") != 3) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))
write.csv2(BeeFuncNestDepth, file="BeeFuncNestDepth.csv", row.names = FALSE)

BeeFuncNestMaterials <- baseBeeFuncNestMaterials %>%
  mutate(BeeFuncID = paste0("bfNestMat", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
  mutate(across(everything(), str_squish)) %>%
  filter(str_count(LB_NOM, "\\S+") != 3) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))
write.csv2(BeeFuncNestMaterials, file="BeeFuncNestMaterials.csv", row.names = FALSE)

BeeFuncNestType <- baseBeeFuncNestNestType %>%
  mutate(BeeFuncID = paste0("bfNestTyp", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
  mutate(across(everything(), str_squish)) %>%
  filter(str_count(LB_NOM, "\\S+") != 3) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))
write.csv2(BeeFuncNestType, file="BeeFuncNestType.csv", row.names = FALSE)

BeeFuncNestCellNumber <- baseBeeFuncNestCells %>%
  mutate(BeeFuncID = paste0("bfNestCel", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
  mutate(across(everything(), str_squish)) %>%
  filter(str_count(LB_NOM, "\\S+") != 3) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))
write.csv2(BeeFuncNestCellNumber, file="BeeFuncNestCellNumber.csv", row.names = FALSE)

BeeFuncNestSlope <- baseBeeFuncNestingSlope %>%
  mutate(BeeFuncID = paste0("bfNestSlo", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
  mutate(across(everything(), str_squish)) %>%
  filter(str_count(LB_NOM, "\\S+") != 3) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))
write.csv2(BeeFuncNestSlope, file="BeeFuncNestSlope.csv", row.names = FALSE)

BeeFuncNestSubstrate <- baseBeeFuncNestSubstrate %>%
  mutate(BeeFuncID = paste0("bfNestSub", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
  mutate(across(everything(), str_squish)) %>%
  filter(str_count(LB_NOM, "\\S+") != 3) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))
write.csv2(BeeFuncNestSubstrate, file="BeeFuncNestSubstrate.csv", row.names = FALSE)

BeeFuncHabitat <- baseBeeFuncHabitat %>%
  mutate(BeeFuncID = paste0("bfHab", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
  mutate(across(everything(), str_squish)) %>%
  filter(str_count(LB_NOM, "\\S+") != 3) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))
write.csv2(BeeFuncHabitat, "BeeFuncHabitat.csv", row.names = FALSE)

#insert VALID_NAME corresponding to LB_NAME
BeeFunc <- list.files(pattern ="^BeeFunc.*\\.csv$")
BeeFunc <- lapply(BeeFunc, function(file) {
  data <- read.csv(file, sep=";", header=TRUE)
  data$CD_NOM <- as.character(data$CD_NOM)
  abeillesTXRF$CD_NOM <- as.character(abeillesTXRF$CD_NOM)
  result <- data %>% left_join(abeillesTXRF %>% select(NOM_VALIDE, CD_NOM), by="CD_NOM")
  result <- result %>% relocate(NOM_VALIDE, .after = 1) %>%
    rename("VALID_NAME" = "NOM_VALIDE")
  assign(gsub(".csv$", "", file), result, envir = .GlobalEnv)
})

# #INTERACTIONS
interactionsBase <- read.csv2("BDC_INTERSP_EN.csv", sep=";", header=TRUE)

interactionsBaseEn <- interactionsBase %>%
  select(-CD_REF,-NOM_COMPLET_HTML, -VALID_NAME, -ACRONYME, -RELATION, -CD_REF_02, -NOM_COMPLET_HTML_02, -CD_SIG, -LB_ADM_TR, -NIVEAU_ADMIN, -CD_ISO3166_1, -CD_ISO3166_2, -CD_SUP, -CD_SUP_, -SPECIFICITE, -CD_ETAT_SEXE_1, -LB_ETAT_SEXE_1, -OBO_SEXE_1, -LB_ETAT_STADE_1, -OBO_STADE_1, -OBO_SPECIFICITE, -CD_ETAT_STADE_2, -LB_ETAT_STADE_2, -OBO_STADE_2) %>%
  mutate(across(.cols = everything(), ~str_remove_all(., "<i>"))) %>%
  mutate(across(.cols = everything(), ~str_remove_all(., "</i>"))) %>%
  mutate(across(.cols = everything(), ~str_remove_all(., "<br />")))

#import valid names
interactionsBaseEn <- interactionsBaseEn %>%
  left_join(abeillesTXRF %>% select(NOM_VALIDE, CD_NOM), by ="CD_NOM")

interactionsBaseEn <- interactionsBaseEn %>%
  relocate(NOM_VALIDE, .before = everything()) %>%
  rename("VALID_NAME" = "NOM_VALIDE")

InterBeeFuncLectism <- interactionsBaseEn %>%
  filter(RELATION_EN == "acquires nutrients from") %>%
  distinct() %>%
  mutate(BeeFuncID = paste0("bfLec", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
mutate(across(everything(), str_squish)) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))

InterBeeFuncParasitism <- interactionsBaseEn %>%
  filter(RELATION_EN == "kleptoparasite of") %>%
  distinct() %>%
mutate(BeeFuncID = paste0("bfPar", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
mutate(across(everything(), str_squish)) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))

InterBeeFuncPollination <- interactionsBaseEn %>%
  filter(RELATION_EN == "pollinates") %>%
mutate(BeeFuncID = paste0("bfPol", row_number())) %>%
  relocate(BeeFuncID, .before = everything()) %>%
mutate(across(everything(), str_squish)) %>%
  mutate(across(everything(), ~ replace_na(.x, "")))

## correct recent taxonomy (Ropars et al, in prep)
BeeFunc <- mget(ls(pattern = "^BeeFunc"), envir = .GlobalEnv)
BeeFunc <- BeeFunc[sapply(BeeFunc, is.data.frame)]

BeeFunc <- lapply(BeeFunc, function(taxo) {
  taxo %>%
    mutate(VALID_NAME = case_when (
      VALID_NAME == "Bombus wurflenii Radoszkowski, 1859" ~ "Bombus mastrucatus Gerstäcker, 1869",
      VALID_NAME == "Bombus perezi (Schulthess-Rechberg, 1886)" ~ "Bombus vestalis (Geoffroy in Fourcroy, 1785)",
      VALID_NAME == "Bombus pereziellus (Skorikov, 1922)" ~ "Bombus muscorum (Linnaeus, 1758)",
      VALID_NAME == "Lasioglossum sabulosum (Warncke, 1986)" ~ "Lasioglossum monstrificum (Morawitz, 1891)",
      VALID_NAME == "Amegilla magnilabris (Fedtschenko, 1875)" ~ "Amegilla savignyi (Lepeletier, 1841)",
      VALID_NAME == "Andrena hispania Warncke, 1967" ~ "Andrena morio Brullé, 1832",
      VALID_NAME == "Andrena similis Smith, 1849" ~ "Andrena russula Lepeletier de Saint-Fargeau, 1841",
      VALID_NAME == "Anthophora salviae (Panzer, 1802)" ~ "Anthophora crinipes Smith, 1854",
      VALID_NAME == "Bombus mocsaryi Kriechbaumer, 1877" ~ "Bombus laesus Morawitz, 1875",
      VALID_NAME == "Chelostoma proximum Schletterer, 1889" ~ "Chelostoma rapunculi (Lepeletier, 1841)",
      VALID_NAME == "Eucera alternans (Brullé, 1832)" ~ "Eucera rufa (Lepeletier, 1841)",
      VALID_NAME == "Eucera eucnemidea Dours, 1873" ~ "Eucera grisea Fabricius, 1793",
      VALID_NAME == "Lasioglossum discum (Smith, 1853)" ~ "Lasioglossum discus (Smith, 1853)",
      VALID_NAME == "Megachile picicornis Morawitz, 1877" ~ "Megachile marginata Smith, 1853",
      VALID_NAME == "Megachile pilidens Alfken, 1924" ~ "Megachile argentata (Fabricius, 1793)",
      VALID_NAME == "Megachile schmiedeknechti Costa, 1884" ~ "Megachile argentata (Fabricius, 1793)",
      VALID_NAME == "Bombus wurflenii mastrucatus Gerstacker, 1869" ~ "Bombus mastrucatus Gerstäcker, 1869",
      VALID_NAME == "Coelioxys haemorrhoa Forster, 1853" ~ "Coelioxys haemorrhous Förster, 1853",
      TRUE ~ VALID_NAME
    )) %>%
    filter(VALID_NAME != "Megachile lucidifrons Ferton, 1905",
           VALID_NAME != "Megachile albocristata Smith, 1853",
           VALID_NAME != "Andrena curtula Pérez, 1903",
           VALID_NAME != "Andrena espanola Warncke, 1967",
           VALID_NAME != "Bombus inexspectatus (Tkalců, 1963)",
           VALID_NAME != "Halictus fumatipennis Blüthgen, 1923",
           VALID_NAME != "Nomada gredosiana Schwarz & Gusenleitner, 2013",
           VALID_NAME != "Amegilla salviae (Morawitz, 1876)",
           VALID_NAME != "Andrena avara auct. non Warncke, 1967",
           VALID_NAME != "Andrena erythrocnemis Morawitz, 1871",
           VALID_NAME != "Andrena gallica Schmiedeknecht, 1883",
           VALID_NAME != "Anthophora borealis Morawitz, 1865",
           VALID_NAME != "Anthophora lanata (Klug, 1845)",
           VALID_NAME != "Anthophora rutilans Dours, 1869",
           VALID_NAME != "Anthophora ventilabris Lepeletier, 1841",
           VALID_NAME != "Halictus tetrazonius (Klug in Germar, 1817)",
           VALID_NAME != "Hoplitis cretaea (Tkalců, 1992)",
           VALID_NAME != "Hylaeus stigmorhinus (Pérez, 1895)",
           VALID_NAME != "Lasioglossum aegyptiellum (Strand, 1909)",
           VALID_NAME != "Megachile atlantica Benoist, 1934",
           VALID_NAME != "Megachile bioculata Pérez, 1902",
           VALID_NAME != "Nomada ferghanica Morawitz, 1875",
           VALID_NAME != "Nomada rostrata Herrich-Schäffer, 1839",
           VALID_NAME != "Osmia notata (Fabricius, 1804)",
           VALID_NAME != "Tetralonia scabiosae Mocsáry, 1881",
           VALID_NAME != "Megachile baetica (Gerstäcker, 1869)",
           VALID_NAME != "Halictus tetrazonius (Klug in Germar, 1817)",
           VALID_NAME != "Anthophora borealis Morawitz, 1865",
           VALID_NAME != "Hoplitis marchali (Perez, 1902)",
           VALID_NAME != "Osmia notata (Fabricius, 1804)",
           VALID_NAME != "Megachile baetica (Gerstacker, 1869)",
           VALID_NAME != "Nomada gredosiana Schwarz & Gusenleitner, 2013",
           VALID_NAME != "Nomada rostrata Herrich-Schaffer, 1839",
           VALID_NAME != "Megachile concinna Smith, 1879",
           VALID_NAME != "Andrena agnata Warncke, 1967")
})

list2env(BeeFunc, .GlobalEnv)


## correct recent taxonomy (Ropars et al, in prep) for interactions
InterBeeFunc <- mget(ls(pattern = "^InterBeeFunc"), envir = .GlobalEnv)
InterBeeFunc <- InterBeeFunc[sapply(InterBeeFunc, is.data.frame)]

InterBeeFunc <- lapply(InterBeeFunc, function(taxo) {
  taxo %>%
    mutate(VALID_NAME = case_when (
      VALID_NAME == "Bombus wurflenii Radoszkowski, 1859" ~ "Bombus mastrucatus Gerstäcker, 1869",
      VALID_NAME == "Bombus perezi (Schulthess-Rechberg, 1886)" ~ "Bombus vestalis (Geoffroy in Fourcroy, 1785)",
      VALID_NAME == "Bombus pereziellus (Skorikov, 1922)" ~ "Bombus muscorum (Linnaeus, 1758)",
      VALID_NAME == "Lasioglossum sabulosum (Warncke, 1986)" ~ "Lasioglossum monstrificum (Morawitz, 1891)",
      VALID_NAME == "Amegilla magnilabris (Fedtschenko, 1875)" ~ "Amegilla savignyi (Lepeletier, 1841)",
      VALID_NAME == "Andrena hispania Warncke, 1967" ~ "Andrena morio Brullé, 1832",
      VALID_NAME == "Andrena similis Smith, 1849" ~ "Andrena russula Lepeletier de Saint-Fargeau, 1841",
      VALID_NAME == "Anthophora salviae (Panzer, 1802)" ~ "Anthophora crinipes Smith, 1854",
      VALID_NAME == "Bombus mocsaryi Kriechbaumer, 1877" ~ "Bombus laesus Morawitz, 1875",
      VALID_NAME == "Chelostoma proximum Schletterer, 1889" ~ "Chelostoma rapunculi (Lepeletier, 1841)",
      VALID_NAME == "Eucera alternans (Brullé, 1832)" ~ "Eucera rufa (Lepeletier, 1841)",
      VALID_NAME == "Eucera eucnemidea Dours, 1873" ~ "Eucera grisea Fabricius, 1793",
      VALID_NAME == "Lasioglossum discum (Smith, 1853)" ~ "Lasioglossum discus (Smith, 1853)",
      VALID_NAME == "Megachile picicornis Morawitz, 1877" ~ "Megachile marginata Smith, 1853",
      VALID_NAME == "Megachile pilidens Alfken, 1924" ~ "Megachile argentata (Fabricius, 1793)",
      VALID_NAME == "Megachile schmiedeknechti Costa, 1884" ~ "Megachile argentata (Fabricius, 1793)",
      VALID_NAME == "Bombus wurflenii mastrucatus Gerstacker, 1869" ~ "Bombus mastrucatus Gerstäcker, 1869",
      VALID_NAME == "Coelioxys haemorrhoa Forster, 1853" ~ "Coelioxys haemorrhous Förster, 1853",
      TRUE ~ VALID_NAME
    )) %>%
    filter(VALID_NAME != "Megachile lucidifrons Ferton, 1905",
           VALID_NAME != "Megachile albocristata Smith, 1853",
           VALID_NAME != "Andrena curtula Pérez, 1903",
           VALID_NAME != "Andrena espanola Warncke, 1967",
           VALID_NAME != "Bombus inexspectatus (Tkalců, 1963)",
           VALID_NAME != "Halictus fumatipennis Blüthgen, 1923",
           VALID_NAME != "Nomada gredosiana Schwarz & Gusenleitner, 2013",
           VALID_NAME != "Amegilla salviae (Morawitz, 1876)",
           VALID_NAME != "Andrena avara auct. non Warncke, 1967",
           VALID_NAME != "Andrena erythrocnemis Morawitz, 1871",
           VALID_NAME != "Andrena gallica Schmiedeknecht, 1883",
           VALID_NAME != "Anthophora borealis Morawitz, 1865",
           VALID_NAME != "Anthophora lanata (Klug, 1845)",
           VALID_NAME != "Anthophora rutilans Dours, 1869",
           VALID_NAME != "Anthophora ventilabris Lepeletier, 1841",
           VALID_NAME != "Halictus tetrazonius (Klug in Germar, 1817)",
           VALID_NAME != "Hoplitis cretaea (Tkalců, 1992)",
           VALID_NAME != "Hylaeus stigmorhinus (Pérez, 1895)",
           VALID_NAME != "Lasioglossum aegyptiellum (Strand, 1909)",
           VALID_NAME != "Megachile atlantica Benoist, 1934",
           VALID_NAME != "Megachile bioculata Pérez, 1902",
           VALID_NAME != "Nomada ferghanica Morawitz, 1875",
           VALID_NAME != "Nomada rostrata Herrich-Schäffer, 1839",
           VALID_NAME != "Osmia notata (Fabricius, 1804)",
           VALID_NAME != "Tetralonia scabiosae Mocsáry, 1881",
           VALID_NAME != "Megachile baetica (Gerstäcker, 1869)",
           VALID_NAME != "Halictus tetrazonius (Klug in Germar, 1817)",
           VALID_NAME != "Anthophora borealis Morawitz, 1865",
           VALID_NAME != "Hoplitis marchali (Perez, 1902)",
           VALID_NAME != "Osmia notata (Fabricius, 1804)",
           VALID_NAME != "Megachile baetica (Gerstacker, 1869)",
           VALID_NAME != "Nomada gredosiana Schwarz & Gusenleitner, 2013",
           VALID_NAME != "Nomada rostrata Herrich-Schaffer, 1839",
           VALID_NAME != "Megachile concinna Smith, 1879",
           VALID_NAME != "Andrena agnata Warncke, 1967")
})

InterBeeFunc <- lapply(InterBeeFunc, function(taxo) {
  taxo %>%
    mutate(VALID_NAME_02 = case_when (
      VALID_NAME_02 == "Bombus wurflenii Radoszkowski, 1859" ~ "Bombus mastrucatus Gerstäcker, 1869",
      VALID_NAME_02 == "Bombus perezi (Schulthess-Rechberg, 1886)" ~ "Bombus vestalis (Geoffroy in Fourcroy, 1785)",
      VALID_NAME_02 == "Bombus pereziellus (Skorikov, 1922)" ~ "Bombus muscorum (Linnaeus, 1758)",
      VALID_NAME_02 == "Lasioglossum sabulosum (Warncke, 1986)" ~ "Lasioglossum monstrificum (Morawitz, 1891)",
      VALID_NAME_02 == "Amegilla magnilabris (Fedtschenko, 1875)" ~ "Amegilla savignyi (Lepeletier, 1841)",
      VALID_NAME_02 == "Andrena hispania Warncke, 1967" ~ "Andrena morio Brullé, 1832",
      VALID_NAME_02 == "Andrena similis Smith, 1849" ~ "Andrena russula Lepeletier de Saint-Fargeau, 1841",
      VALID_NAME_02 == "Anthophora salviae (Panzer, 1802)" ~ "Anthophora crinipes Smith, 1854",
      VALID_NAME_02 == "Bombus mocsaryi Kriechbaumer, 1877" ~ "Bombus laesus Morawitz, 1875",
      VALID_NAME_02 == "Chelostoma proximum Schletterer, 1889" ~ "Chelostoma rapunculi (Lepeletier, 1841)",
      VALID_NAME_02 == "Eucera alternans (Brullé, 1832)" ~ "Eucera rufa (Lepeletier, 1841)",
      VALID_NAME_02 == "Eucera eucnemidea Dours, 1873" ~ "Eucera grisea Fabricius, 1793",
      VALID_NAME_02 == "Lasioglossum discum (Smith, 1853)" ~ "Lasioglossum discus (Smith, 1853)",
      VALID_NAME_02 == "Megachile picicornis Morawitz, 1877" ~ "Megachile marginata Smith, 1853",
      VALID_NAME_02 == "Megachile pilidens Alfken, 1924" ~ "Megachile argentata (Fabricius, 1793)",
      VALID_NAME_02 == "Megachile schmiedeknechti Costa, 1884" ~ "Megachile argentata (Fabricius, 1793)",
      VALID_NAME_02 == "Bombus wurflenii mastrucatus Gerstacker, 1869" ~ "Bombus mastrucatus Gerstäcker, 1869",
      VALID_NAME == "Coelioxys haemorrhoa Forster, 1853" ~ "Coelioxys haemorrhous Förster, 1853",
      TRUE ~ VALID_NAME_02
    )) %>%
    filter(VALID_NAME_02 != "Megachile lucidifrons Ferton, 1905",
           VALID_NAME_02 != "Megachile albocristata Smith, 1853",
           VALID_NAME_02 != "Andrena curtula Pérez, 1903",
           VALID_NAME_02 != "Andrena espanola Warncke, 1967",
           VALID_NAME_02 != "Bombus inexspectatus (Tkalců, 1963)",
           VALID_NAME_02 != "Halictus fumatipennis Blüthgen, 1923",
           VALID_NAME_02 != "Nomada gredosiana Schwarz & Gusenleitner, 2013",
           VALID_NAME_02 != "Amegilla salviae (Morawitz, 1876)",
           VALID_NAME_02 != "Andrena avara auct. non Warncke, 1967",
           VALID_NAME_02 != "Andrena erythrocnemis Morawitz, 1871",
           VALID_NAME_02 != "Andrena gallica Schmiedeknecht, 1883",
           VALID_NAME_02 != "Anthophora borealis Morawitz, 1865",
           VALID_NAME_02 != "Anthophora lanata (Klug, 1845)",
           VALID_NAME_02 != "Anthophora rutilans Dours, 1869",
           VALID_NAME_02 != "Anthophora ventilabris Lepeletier, 1841",
           VALID_NAME_02 != "Halictus tetrazonius (Klug in Germar, 1817)",
           VALID_NAME_02 != "Hoplitis cretaea (Tkalců, 1992)",
           VALID_NAME_02 != "Hylaeus stigmorhinus (Pérez, 1895)",
           VALID_NAME_02 != "Lasioglossum aegyptiellum (Strand, 1909)",
           VALID_NAME_02 != "Megachile atlantica Benoist, 1934",
           VALID_NAME_02 != "Megachile bioculata Pérez, 1902",
           VALID_NAME_02 != "Nomada ferghanica Morawitz, 1875",
           VALID_NAME_02 != "Nomada rostrata Herrich-Schäffer, 1839",
           VALID_NAME_02 != "Osmia notata (Fabricius, 1804)",
           VALID_NAME_02 != "Tetralonia scabiosae Mocsáry, 1881",
           VALID_NAME_02 != "Megachile baetica (Gerstäcker, 1869)",
           VALID_NAME_02 != "Halictus tetrazonius (Klug in Germar, 1817)",
           VALID_NAME_02 != "Anthophora borealis Morawitz, 1865",
           VALID_NAME_02 != "Hoplitis marchali (Perez, 1902)",
           VALID_NAME_02 != "Osmia notata (Fabricius, 1804)",
           VALID_NAME_02 != "Megachile baetica (Gerstacker, 1869)",
           VALID_NAME_02 != "Nomada gredosiana Schwarz & Gusenleitner, 2013",
           VALID_NAME_02 != "Nomada rostrata Herrich-Schaffer, 1839",
           VALID_NAME_02 != "Megachile concinna Smith, 1879",
           VALID_NAME_02 != "Andrena agnata Warncke, 1967")
})

list2env(InterBeeFunc, .GlobalEnv)


# Final interaction files
BeeFuncLectism <- InterBeeFuncLectism
BeeFuncParasitism <- InterBeeFuncParasitism
BeeFuncPollination <- InterBeeFuncPollination


#creation of final files
write.csv2(BeeFuncLectism, "BeeFuncLectism.csv", row.names = FALSE)
write.csv2(BeeFuncParasitism, "BeeFuncParasitism.csv", row.names = FALSE)
write.csv2(BeeFuncPollination, "BeeFuncPollination.csv", row.names = FALSE)
write.csv2(BeeFuncBodyLength, file="BeeFuncBodyLength.csv", row.names = FALSE)
write.csv2(BeeFuncITD, file="BeeFuncITD.csv", row.names = FALSE)
write.csv2(BeeFuncTongueLength, file="BeeFuncTongueLength.csv", row.names = FALSE)
write.csv2(BeeFuncPhenology, file="BeeFuncPhenology.csv", row.names = FALSE)
write.csv2(BeeFuncVoltinism, file="BeeFuncVoltinism.csv", row.names = FALSE)
write.csv2(BeeFuncSociality, file="BeeFuncSociality.csv", row.names = FALSE)
write.csv2(BeeFuncBiogeographicalStatus, file="BeeFuncBiogeographicalStatus.csv", row.names = FALSE)
write.csv2(BeeFuncAltitude, file="BeeFuncAltitude.csv", row.names = FALSE)
write.csv2(BeeFuncEcologicalAffinity, file="BeeFuncEcologicalAffinity.csv", row.names = FALSE)
write.csv2(BeeFuncIUCNStatus, file="BeeFuncIUCNStatus.csv", row.names = FALSE)
write.csv2(BeeFuncNestDepth, file="BeeFuncNestDepth.csv", row.names = FALSE)
write.csv2(BeeFuncNestMaterials, file="BeeFuncNestMaterials.csv", row.names = FALSE)
write.csv2(BeeFuncNestType, file="BeeFuncNestType.csv", row.names = FALSE)
write.csv2(BeeFuncNestCellNumber, file="BeeFuncNestCellNumber.csv", row.names = FALSE)
write.csv2(BeeFuncNestSlope, file="BeeFuncNestSlope.csv", row.names = FALSE)
write.csv2(BeeFuncNestSubstrate, file="BeeFuncNestSubstrate.csv", row.names = FALSE)
write.csv2(BeeFuncHabitat, "BeeFuncHabitat.csv", row.names = FALSE)
