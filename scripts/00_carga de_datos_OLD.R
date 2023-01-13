library(petroreadr)
library(tidyverse)


library(readr)
xdata <- read_delim("../Force-2020-Machine-Learning-competition/lithology_competition/data/train.csv", 
                    delim = ";", escape_double = FALSE, col_types = cols(FORCE_2020_LITHOFACIES_LITHOLOGY = col_integer(), 
                                                                         FORCE_2020_LITHOFACIES_CONFIDENCE = col_integer()), 
                    trim_ws = TRUE)
pozos_train <- unique(xdata$WELL)

test_data <-  read_delim("../Force-2020-Machine-Learning-competition/lithology_competition/data/leaderboard_test_features.csv", 
                                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)






library(petroreadr)
library(tidyverse)



path <- "../Force-2020-Machine-Learning-competition/lithology_competition/data/las_files_Lithostrat_data/"

files <-data.frame(filename = list.files(path = path, "*.las", recursive = TRUE))
files <- dplyr::filter(files, grepl(".las", filename)) %>%  pull(filename)
logs <- read_las(file.path(path, files), verbose = TRUE)$data

pozos_las <- unique(logs$WELL)
pozos_las == pozos_train

summary(logs)
logs <- logs %>% separate(WELL, sep = " ", into = c("well", "del")) %>% 
  mutate(well = ifelse(substring(well, nchar(well), nchar(well)) == "R", substring(well, 1, nchar(well) - 1), well),
         well = ifelse(substring(well, nchar(well), nchar(well)) == "S", substring(well, 1, nchar(well) - 1), well))

unique(temp$well)

unique(logs$WELL)

logs %>% drop_na(x_loc) %>% 
  ggplot(aes(x_loc, y_loc, group = WELL))+
  # geom_path(size =2)+
  geom_path(data = xdata, aes(X_LOC, Y_LOC), color = "red", size =3)+
  geom_path(data = test_data, aes(X_LOC, Y_LOC), color = "blue", size =3)+
  coord_fixed()+
  labs(x="",
       y="",
       title = "TFM Edmundo Peralta",
       subtitle = "pozos disponibles")+
  theme_bw()



logs %>% drop_na(DEPT) %>% 
  ggplot(aes(DEPT, DEPTH_MD, group= well))+
  geom_point()
  






# 
# xlogs <- logs %>% 
#   rename(zone_codes = `Zoneloglinkedto'SARB_Tops_OPCO_OFFICIALUPTOSR-81'`,
#          perm = PERM_FINAL_M4_2022,
#          poro = XPHIT,
#          litho = LI_F,
#          rrt_2022 = RRT_F) %>% 
#   mutate(zone = case_when(
#     zone_codes == 19 ~ "Arab_A0",
#     zone_codes == 21 ~ "Arab_A1",
#     zone_codes == 23 ~ "Arab_A2",
#     zone_codes == 24 ~ "Arab_A2_dense" ,
#     zone_codes == 25 ~ "Arab_B" ,
#     zone_codes == 27 ~ "Arab_C1" ,
#     zone_codes == 29 ~ "Arab_C2" ,
#     zone_codes == 31 ~ "Arab_C3" ,
#     zone_codes == 32 ~ "Arab_C3_dense" ,
#     zone_codes  > 32 ~ "Arab_D" ,
#     zone_codes  > 39 ~ "other" ,
#     TRUE   ~ "other" 
#   ),
#   lithology = case_when(
#     litho == 0 ~ "Anhidrite",
#     litho == 2 ~ "Dolomite",
#     litho == 3 ~ "Limestone"
#   ),
#   rrt = case_when(
#     rrt_2022 ==  1 ~ "D1",
#     rrt_2022 ==  2 ~ "D2",
#     rrt_2022 ==  3 ~ "D3",
#     rrt_2022 ==  4 ~ "D4",
#     rrt_2022 ==  5 ~ "D5",
#     rrt_2022 ==  6 ~ "D6",
#     rrt_2022 ==  7 ~ "L1",
#     rrt_2022 ==  8 ~ "L2",
#     rrt_2022 ==  9 ~ "G5",
#     rrt_2022 == 10 ~ "L3",
#     rrt_2022 == 13 ~ "L4",
#     rrt_2022 == 16 ~ "WP3",
#     rrt_2022 == 19 ~ "WP5",
#     rrt_2022 == 20 ~ "L0",
#     rrt_2022 == 21 ~ "A"
#     
#   )) %>% 
#   filter(zone != "other")

logs
