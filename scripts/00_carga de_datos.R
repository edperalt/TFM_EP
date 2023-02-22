# Carga de los LAS
library(readxl)
library(readr)
library(petroreadr)
library(tidyverse)


##### Carga de datos en formato las ##### 

# esta direccion es para correr el archivo independiente
path <- "../Force-2020-Machine-Learning-competition/lithology_competition/data/las_files_Lithostrat_data/"

# Direccion para correr el script desde un Rmd o quarto 
#path <- "../../Force-2020-Machine-Learning-competition/lithology_competition/data/las_files_Lithostrat_data/"

files <-data.frame(filename = list.files(path = path, "*.las", recursive = TRUE))
files <- dplyr::filter(files, grepl(".las", filename)) %>%  pull(filename)
logs <- read_las(file.path(path, files), verbose = TRUE)$data




# Editamos los nombres de los pozos para unirlos con datos de la base de de datos de Noruega.

logs <- logs %>% separate(WELL, sep = " ", into = c("well", "del")) %>% 
  mutate(well = ifelse(substring(well, nchar(well), nchar(well)) == "R", substring(well, 1, nchar(well) - 1), well),
         well = ifelse(substring(well, nchar(well), nchar(well)) == "S", substring(well, 1, nchar(well) - 1), well)) %>%  select(-del)


# salvamos en disco para acceder a ellos mas rápido
# write_rds(logs, "data/logs_force2020.RDS")
logs <- read_rds("data/logs_force2020.RDS")


##### Lista de pozos en force2020 ##### 

force2020_pozos <- unique(logs$well)

##### Coordenadas de pozos en el concurso      ##### 

coord_all_wells <-
  logs %>%
  drop_na(x_loc, DEPT) %>%
  group_by(well) %>%
  filter(DEPT == min(DEPT)) %>%
  select(well, x_loc,  y_loc, z_loc) %>% 
  ungroup()

write_rds(coord_all_wells, "data/coord_all_wells.rds")
# coord_all_wells<- read_rds("data/coord_all_wells.rds")

##### tope de las formaciones tomadas de NPD. ##### 

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



##### agregamos las formaciones al dataframe con los registros ##### 

temp <- logs %>% mutate(fm = "otro")


for (i in 1:nrow(nor_fm)) {
  the_well <- nor_fm[[i,"well"]]
  the_fm <- nor_fm[[i,"Surface"]]
  md <- nor_fm[[i,"MD"]]
  temp <- temp %>% 
    mutate(fm = ifelse((well == the_well & DEPT >= md), the_fm,fm )
    )
}

##### lista formaciones grupo Brent.  ##### 

brent_group <- c("Ness Fm. Top", "Etive Fm. Top", "Rannoch Fm. Top","Tarbert Fm. Top","Broom Fm. Top")


logs <- temp %>% 
  mutate(brent = ifelse(fm %in% brent_group, TRUE, FALSE ))


#####  pozos Brent.  ##### 

wells_brent <- unique(logs %>% filter(brent == TRUE) %>% pull(well))
# write_rds(wells_brent, "data/data_load/wells_brent.rds")
wells_brent <-read_rds( "data/data_load/wells_brent.rds")

#####  diccionario tipos de roca.  #####  

tipo_de_roca <- read_csv("../npd/lith_codes.csv", 
                         col_types = cols(order = col_skip()))


logs_brent <- logs %>% 
  filter(well %in% wells_brent) %>% 
  left_join(tipo_de_roca,
                 by = c("FORCE_2020_LITHOFACIES_LITHOLOGY" = "lith_code")) %>% 
  select(-c(del, FORCE_2020_LITHOFACIES_CONFIDENCE))

# saveRDS(logs_brent, "data/logs_brent.rds")
# logs_brent <- read_rds("data/logs_brent.rds")

# finalmente agregamos una columna que nos indica la sección con core dentro de la formación Brent
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

temp <- logs_brent %>% mutate(core=FALSE)

for (i in 1:nrow(brent_cored)) {
  the_well  <- brent_cored[[i,"well"]]
  top_core  <- brent_cored[[i,"top"]]
  base_core <- brent_cored[[i,"base"]]
  
  temp <- temp %>% 
    mutate(core = ifelse(DEPT >= top_core & DEPT <=base_core & well == the_well, TRUE,core )
    )
}

final_logs <- temp

print(paste0("There are ", 
final_logs %>% mutate(core_brent = ifelse(core == TRUE & brent == TRUE, TRUE, FALSE) ) %>% 
  filter(core_brent == TRUE) %>% pull(well) %>% unique() %>% length()
, " wells with core from the BRENT formation "))

# salvamos el dataframe con los registros y la columna que indica cuando hay core en el Brent

final_logs<- final_logs %>% 
  rename(core_all = core) %>% 
  mutate(core = ifelse(brent == T & core_all== T, T, F))

saveRDS(final_logs, "data/logs_core_rds.rds")

final_logs<- read_rds("data/logs_core_rds.rds")
# lista de pozos con nucleos


cored_brent_wells <-
  final_logs %>% mutate(core_brent = ifelse(core == TRUE &
                                              brent == TRUE, TRUE, FALSE)) %>%
  filter(core_brent == TRUE) %>% pull(well) %>% unique()

write_rds(cored_brent_wells, "data/cored_brent_wells.rds")

well_names <- unique(final_logs$well)

well_names <- final_logs %>% 
  group_by(well) %>% 
  filter(brent ==TRUE) %>% 
  summarise(top = min(-z_loc, na.rm = TRUE)) %>% 
  arrange(top) %>% pull(well)   # select wells organized by depth 

los_pozos <- tibble(well = well_names,
                    well_id = seq(1, length(well_names)))


final_logs <- final_logs %>% 
  left_join(los_pozos, by= "well") %>% 
  group_by(well) %>% 
  arrange(DEPT) %>% 
  ungroup()

seabed <- final_logs %>% 
  group_by(well) %>% 
  summarise(surface = min(DEPT, na.rm = TRUE)) %>% 
  left_join(los_pozos, by= "well")

well_top_base_md <- final_logs %>% 
  group_by(well) %>% 
  summarise(top_md = min(DEPT,na.rm = TRUE),
            td_md = max(DEPT, na.rm = TRUE),
            # top_ss = max(z_loc, na.rm = TRUE),
            # td_ss = min(z_loc, na.rm = TRUE)
  ) %>% 
  left_join(los_pozos, by= "well") %>% 
  pivot_longer(-c(well, well_id)) %>% 
  group_by(well) %>% 
  arrange(value) %>% 
  ungroup()

top_brent_md <- final_logs %>%
  filter(brent== TRUE) %>% 
  group_by(well) %>% 
  summarise(top_brent = min(DEPT, na.rm = TRUE )) %>% 
  left_join(los_pozos, by= "well") %>% 
  arrange(top_brent) %>% 
  ungroup()

base_brent_md <- final_logs %>%
  filter(brent== TRUE) %>% 
  group_by(well) %>% 
  summarise(bas_brent = max(DEPT, na.rm = TRUE )) %>% 
  left_join(los_pozos, by= "well")  %>% 
  arrange(bas_brent) %>% 
  ungroup()

label_pos <- tibble(well = well_names,
                    pos = c(rep(c(1000,1300,1500, 1700,1900),7),1000, 1300)) %>% 
  left_join(los_pozos, by="well")

write_rds(label_pos, "../data/label_pos.rds")

well_top_base_ss <- final_logs %>% 
  group_by(well) %>% 
  summarise(top_ss = max(z_loc, na.rm = TRUE),
            td_ss = min(z_loc, na.rm = TRUE)
  ) %>% 
  left_join(los_pozos, by= "well") %>% 
  pivot_longer(-c(well, well_id)) %>% 
  group_by(well) %>% 
  arrange(value) %>% 
  ungroup()

write_rds(well_top_base_ss, "../data/well_top_base_ss.rds") 

top_brent_ss <- final_logs %>%
  filter(brent== TRUE) %>% 
  group_by(well) %>% 
  filter(z_loc == max(z_loc, na.rm = TRUE)) %>%
  select(well, z_loc, x_loc, y_loc) %>% 
  left_join(los_pozos, by= "well") %>% 
  arrange(desc(z_loc)) %>% 
  ungroup() 

saveRDS(top_brent_ss, "../data/top_brent_ss.rds")

# top_brent_ss <- final_logs %>%
#   filter(brent== TRUE) %>% 
#   group_by(well) %>% 
#   summarise(top_brent_ss = min(-z_loc, na.rm = TRUE )) %>% 
#   left_join(los_pozos, by= "well") %>% 
#   arrange(top_brent_ss) %>% 
#   ungroup() 

base_brent_ss <- final_logs %>%
  filter(brent== TRUE) %>% 
  group_by(well) %>% 
  summarise(bas_brent_ss = max(-z_loc, na.rm = TRUE )) %>% 
  left_join(los_pozos, by= "well")  %>% 
  arrange(bas_brent_ss) %>% 
  ungroup()

write_rds(base_brent_ss, "data/base_brent_ss.rds")

cored_brent <- final_logs %>% filter(brent == TRUE, 
                                     core == TRUE) 
saveRDS(cored_brent, "data/cored_brent.rds")











