# Carga de los LAS


library(petroreadr)
library(tidyverse)



path <- "../Force-2020-Machine-Learning-competition/lithology_competition/data/las_files_Lithostrat_data/"

files <-data.frame(filename = list.files(path = path, "*.las", recursive = TRUE))
files <- dplyr::filter(files, grepl(".las", filename)) %>%  pull(filename)
logs <- read_las(file.path(path, files), verbose = TRUE)$data

# lista con los pozos cargados
pozos_las <- unique(logs$WELL)


# Editamos los nombres de los pozos para unirlos con datos de la base de de datos de Noruega.

logs <- logs %>% separate(WELL, sep = " ", into = c("well", "del")) %>% 
  mutate(well = ifelse(substring(well, nchar(well), nchar(well)) == "R", substring(well, 1, nchar(well) - 1), well),
         well = ifelse(substring(well, nchar(well), nchar(well)) == "S", substring(well, 1, nchar(well) - 1), well))






