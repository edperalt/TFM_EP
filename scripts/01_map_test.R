
library(plotly)
library(readr)
# load library tidyverse
library(tidyverse)

# create data for world coordinates using 
# map_data() function
world_coordinates <- map_data("world")
norway_coord <- world_coordinates %>% filter(region %in% "Norway") %>% 
  filter(long >0, long <7,
         lat <63, lat > 57)

# wells from NPD

temp <- read_csv("../npd/wlbPoint.csv") 
nor_wells <- temp %>% select("wlbWell", "wlbField", "wlbWellType", "wlbCompletionYear", 
                             "wlbNsDecDeg", "wlbEwDesDeg", "wlbNsUtm", "wlbEwUtm", "wlbUtmZone", 
                             "wlbEwCode", "wlbNsCode", "wlbFormationWithHc1","wlbFormationWithHc2",
                             "wlbFormationWithHc3")


well_name <- unique(logs$well)

tfm_wells <- nor_wells %>% filter(wlbWell %in% well_name)

missing_wells <- setdiff(well_name, unique(tfm_wells$wlbWell))

missing_wells

# fields from NPD

temp <- read_csv("../npd/fldArea.csv") 

the_fields <- temp %>% 
  select("fldName","fldDiscoveryYear", "fldHcType") 


nor_fields <- tfm_wells %>% 
  group_by(wlbField) %>% 
  summarise(wlbNsDecDeg= first(wlbNsDecDeg),
            wlbEwDesDeg= first(wlbEwDesDeg),
            wlbFormationWithHc1= first(wlbFormationWithHc1),
            wlbNsUtm = first(wlbNsUtm),
            wlbEwUtm= first(wlbEwUtm))

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

temp <- logs %>% mutate(fm = "otro")




for (i in 1:nrow(nor_fm)) {
  the_well <- nor_fm[[i,"well"]]
  the_fm <- nor_fm[[i,"Surface"]]
  md <- nor_fm[[i,"MD"]]
  temp <- temp %>% 
    mutate(fm = ifelse((well == the_well & DEPT >= md), the_fm,fm )
           )
}

#saveRDS(temp, "data/logs.rds")

# unique(temp$fm)

temp %>% 
  filter(fm != "otro") %>% 
  ggplot(aes(RDEP))+
  geom_histogram()+
  scale_x_log10()+
  facet_wrap(vars(fm), 
             scales = "free_y"
             )


temp %>% 
  filter(fm == "Ness Fm. Top") %>% 
  ggplot(aes(GR, DEPT))+
  geom_path()+
  scale_y_reverse()+
  # scale_x_log10()+
  facet_wrap(vars(well), scales = "free_y")
  

temp %>% 
  ggplot(aes(fm))+
  geom_histogram(stat = "count")+
  facet_wrap(vars(well))


ness_w <- temp %>% filter(fm == "Ness Fm. Top") %>% 
  group_by(well) %>% 
  summarise(x=first(x_loc),
            y=first(y_loc)) %>% 
  pull(well)

ness_wells <- nor_wells %>% filter(wlbWell %in% ness_w)


# ekofisk
# 56°32′57.11″N 3°12′35.95″E
fields <- tibble(field = c("Ekofisk", "Gulfaks"),
                 latitud = c(56.5, 61.35),
                 longitud = c(3.21, 2.45))
# create world map using ggplot() function
ggplotly(
ggplot() +
  
  # geom_map() function takes world coordinates 
  # as input to plot world map
  geom_map(
    data = norway_coord, map = norway_coord,
    aes(long, lat, map_id = region),
    color = "white", fill = "darkgreen"
  )+
  geom_point(
    data = nor_fields, aes(wlbEwDesDeg,wlbNsDecDeg ), size = 3, color = "red"
  )+
  geom_point(
    data= tfm_wells, aes(wlbEwDesDeg,wlbNsDecDeg ), color = "blue")+
  geom_point(
    data = ness_wells, aes(wlbEwDesDeg,wlbNsDecDeg), color= "yellow", size =1)
  )


nor_fields %>%drop_na(wlbFormationWithHc1) %>% count(wlbFormationWithHc1) %>% 
  arrange(desc(n))

nor_wells %>% drop_na(wlbFormationWithHc1) 
  group_by(wlbWell) %>% 
  summarise(wlbFormationWithHc1= first(wlbFormationWithHc1)) %>% 
  count(wlbFormationWithHc1) %>% 
  arrange(desc(n))


