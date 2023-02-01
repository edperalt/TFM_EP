# Carga de los LAS


library(petroreadr)
library(tidyverse)


# esta direccion es para correr el archivo independiente
# path <- "../Force-2020-Machine-Learning-competition/lithology_competition/data/las_files_Lithostrat_data/"

# Direccion para correr el script desde un Rmd o quarto 
path <- "../../Force-2020-Machine-Learning-competition/lithology_competition/data/las_files_Lithostrat_data/"

files <-data.frame(filename = list.files(path = path, "*.las", recursive = TRUE))
files <- dplyr::filter(files, grepl(".las", filename)) %>%  pull(filename)
logs <- read_las(file.path(path, files), verbose = TRUE)$data




# Editamos los nombres de los pozos para unirlos con datos de la base de de datos de Noruega.

logs <- logs %>% separate(WELL, sep = " ", into = c("well", "del")) %>% 
  mutate(well = ifelse(substring(well, nchar(well), nchar(well)) == "R", substring(well, 1, nchar(well) - 1), well),
         well = ifelse(substring(well, nchar(well), nchar(well)) == "S", substring(well, 1, nchar(well) - 1), well))

# salvamos en disco para acceder a ellos mas rápido
write_rds(logs, "data/logs_force2020.RDS")


######## lista de pozos en force2020

force2020_pozos <- unique(logs$well)

# tope de las formaciones tomadas de NPD

nor_fm <- read_csv("../npd/NPD_Lithostratigraphy_member_formations_all_wells.csv") %>% 
  select(1:7) %>% 
  rename("well" = "Well identifier") %>% 
  mutate(well = ifelse(substring(well, nchar(well), nchar(well)) == "R", substring(well, 1, nchar(well) - 1),   well),
         well = ifelse(substring(well, nchar(well), nchar(well)) == "S", substring(well, 1, nchar(well) - 1),   well),
         well = ifelse(substring(well, nchar(well), nchar(well)) == "A", substring(well, 1, nchar(well) - 1),   well),
         well = ifelse(substring(well, nchar(well), nchar(well)) == " ", substring(well, 1, nchar(well) - 1), well),
         well = ifelse(well == "25/2-13 T4", "25/2-13", well),
         well = ifelse(well == "16/11-1 ST3", "16/11-1", well)
  ) 



## agregamos las formaciones al dataframe con los registros

temp <- logs %>% mutate(fm = "otro")


for (i in 1:nrow(nor_fm)) {
  the_well <- nor_fm[[i,"well"]]
  the_fm <- nor_fm[[i,"Surface"]]
  md <- nor_fm[[i,"MD"]]
  temp <- temp %>% 
    mutate(fm = ifelse((well == the_well & DEPT >= md), the_fm,fm )
    )
}

## creamos una lista de las formaciones que conforman el grupo Brent

brent_group <- c("Ness Fm. Top", "Etive Fm. Top", "Rannoch Fm. Top","Tarbert Fm. Top","Broom Fm. Top")


logs <- temp %>% 
  mutate(brent = ifelse(fm %in% brent_group, TRUE, FALSE ))


wells_brent <- unique(logs %>% filter(brent == TRUE) %>% pull(well))


# Cargamos un diccionario que tiene los codigos de los distintos tipos de roca 

tipo_de_roca <- read_csv("../npd/lith_codes.csv", 
                         col_types = cols(order = col_skip()))

# finalmente nos quedams con la seccion de regisro del grupo Brent y con los codigos de la lithologia

logs_brent <- logs %>% 
  filter(well %in% wells_brent, 
         brent == TRUE) %>% 
  left_join(tipo_de_roca,
                 by = c("FORCE_2020_LITHOFACIES_LITHOLOGY" = "lith_code"))


# saveRDS(logs_brent, "data/logs_brent.rds")

## datos de core

core_data<- read_excel("data/RealPore wells status log.xlsx") %>% 
  separate(Wellbore, sep = " ", into = c("well", "del")) %>% 
  mutate(well = ifelse(substring(well, nchar(well), nchar(well)) == "R", substring(well, 1, nchar(well) - 1), well),
         well = ifelse(substring(well, nchar(well), nchar(well)) == "S", substring(well, 1, nchar(well) - 1), well))
core_well <- unique(core_data$well)


brent_cored <- core_data %>% 
  filter(well %in% wells_brent) %>% 
  rename(top = `Core sample - top depth`,
         base = `Core sample -  bottom depth`)

temp <- logs_brent %>% mutate(core=1)

for (i in 1:nrow(brent_cored)) {
  the_well  <- brent_cored[[i,"well"]]
  top_core  <- brent_cored[[i,"top"]]
  base_core <- brent_cored[[i,"base"]]
  
  temp <- temp %>% 
    mutate(core = ifelse(DEPT >= top_core & DEPT <=base_core, TRUE,FALSE )
    )
}

final_logs <- temp

final_logs %>% filter(core== TRUE) %>% count(well)
