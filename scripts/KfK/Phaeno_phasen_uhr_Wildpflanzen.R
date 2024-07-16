#create wildpflanzen metadata 
#Naturraeume_KfK <- read_sf("data/Naturraum_KfK.gpkg") %>% select(c(NAME,NATNR))
uhr_meta_wildpflanzen <- tibble(pflanze_id=c(116,102,132,129,130,103,129,112,123,132),
                                pflanze=c("Huflattich","Busch-Windröschen","Stiel-Eiche","Schwarzer Holunder","Sommer-Linde","Eberesche","Schwarzer Holunder","Hänge-Birke","Rotbuche","Stiel-Eiche"),
                                phase_id=c(5,5,4,5,5,62,62,31,32,32),
                                jahreszeit=c("Vorfrühling","Erstfrühling","Vollfrühling","Frühsommer","Hochsommer","Spätsommer","Frühherbst","Vollherbst","Spätherbst","Winter"))



Daten_Wildpflanzen <- tbl(con,"Megaframe_Jahresmelder") %>%
  filter(bundesland %in% "Rheinland-Pfalz" & 
           pflanze_id %in% !!uhr_meta_wildpflanzen$pflanze_id &
           phase_id %in% !!uhr_meta_wildpflanzen$phase_id ) %>% 
  collect()


stats_wildpflanzen <- Daten_Wildpflanzen %>%
  group_by(bundesland,refjahr,pflanze_id,phase_id) %>% 
  summarise(doy=mean(jultag),count=n()) %>% 
  left_join(Daten_Wildpflanzen %>% select(phase_id,pflanze_id,phase,pflanze) %>% unique(),by=c("pflanze_id","phase_id"))

write.csv(stats_wildpflanzen,"scripts/KfK/Wildpflanzen_BL_Rheinland-Pfalz.csv",row.names = FALSE,fileEncoding = "UTF8")

Daten_Wildpflanzen <- tbl(con,"Megaframe_Jahresmelder") %>%
  filter(bundesland %in% "Saarland" & 
           pflanze_id %in% !!uhr_meta_wildpflanzen$pflanze_id &
           phase_id %in% !!uhr_meta_wildpflanzen$phase_id ) %>% 
  collect()
  
  
  stats_wildpflanzen <- Daten_Wildpflanzen %>%
  group_by(bundesland,refjahr,pflanze_id,phase_id) %>% 
  summarise(doy=mean(jultag),count=n()) %>% 
  left_join(Daten_Wildpflanzen %>% select(phase_id,pflanze_id,phase,pflanze) %>% unique(),by=c("pflanze_id","phase_id"))

write.csv(stats_wildpflanzen,"scripts/KfK/Wildpflanzen_BL_Saarland.csv",row.names = FALSE,fileEncoding = "UTF8")



#Naturraeume



lala <- Daten_Wildpflanzen %>% st_as_sf(coords=c("lon","lat")) %>%  st_set_crs("WGS84") 
lala <- st_transform(lala,"EPSG:25832","WGS84")
Daten_wildpflanzen_naturraum <- st_join(lala,Naturraeume_KfK,join=st_within)

Stats_wildpflanzen_naturraum <- Daten_wildpflanzen_naturraum %>% group_by(NATNR,refjahr,pflanze_id,phase_id) %>% 
  summarise(doy=mean(jultag),count=n())  

Stats_wildpflanzen_naturraum_clean <- Stats_wildpflanzen_naturraum %>% 
  as_tibble() %>% select(!(geometry)) %>% 
  left_join(Daten_Wildpflanzen %>%  select(phase_id,pflanze_id,phase,pflanze) %>% unique(),
            by=c("pflanze_id","phase_id")) %>% left_join(as_tibble(Naturraeume_KfK) %>% select(!geom),by="NATNR") %>% 
  select(NAME,NATNR,refjahr,pflanze_id,phase_id,doy,count,phase,pflanze) %>% drop_na()

write.csv(Stats_wildpflanzen_naturraum_clean,"scripts/KfK/Wildpflanzen_NR_Rheinland-Pfalz.csv",row.names = FALSE,fileEncoding = "UTF8")

