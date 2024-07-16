con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "data/Phenodb_202407.sqlite3")

dbListTables(con)
Naturraeume_KfK <- read_sf("data/Naturraum_KfK.gpkg") %>% select(c(NAME,NATNR))



uhr_meta <- tibble(pflanze_id=c(113,109,310,129,130,311,129,132,132,132),
                   pflanze=c("Hasel","Forsythie","Apfel","Schwarzer Holunder","Sommer-Linde","Apfel, frühe Reife","Schwarzer Holunder","Stiel-Eiche","Stiel-Eiche","Stiel-Eiche"),
                   phase_id=c(5,5,5,5,5,29,62,62,31,32),
                   jahreszeit=c("Vorfrühling","Erstfrühling","Vollfrühling","Frühsommer","Hochsommer","Spätsommer","Frühherbst","Vollherbst","Spätherbst","Winter"),
                   alt_pflanze_id=c(127,350,132,121,360,103,119,122,103,313),
                   alt_pflanze=c("Schneeglöckchen","Stachelbeere","Stiel-Eiche","Robinie","Rote Johannisbeere","Eberesche","Kornelkirsche","Rosskastanie","Eberesche","Apfel, späte Reife"),
                   alt_phase_id=c(5,4,4,5,29,62,62,62,32,32) ) %>% mutate(order=1:length(pflanze))



Daten_uhr <- tbl(con,"Megaframe_Jahresmelder") %>%
  filter(bundesland %in% "Rheinland-Pfalz" & 
           pflanze_id %in% !!uhr_meta$pflanze_id &
           phase_id %in% !!uhr_meta$phase_id ) %>% 
  collect()


stats <- Daten_uhr %>%
  group_by(bundesland,refjahr,pflanze_id,phase_id) %>% 
  summarise(doy=mean(jultag),count=n()) %>% 
  left_join(Daten_uhr %>% select(phase_id,pflanze_id,phase,pflanze) %>% unique(),by=c("pflanze_id","phase_id"))

write.csv(stats,"scripts/KfK/Kulturpflanzen_BL_Rheinland-Pfalz.csv",row.names = FALSE,fileEncoding = "UTF8")

Daten_uhr <- tbl(con,"Megaframe_Jahresmelder") %>%
  filter(bundesland %in% "Saarland" & 
           pflanze_id %in% !!uhr_meta$pflanze_id &
           phase_id %in% !!uhr_meta$phase_id ) %>% 
  collect()


stats <- Daten_uhr %>%
  group_by(bundesland,refjahr,pflanze_id,phase_id) %>% 
  summarise(doy=mean(jultag),count=n()) %>% 
  left_join(Daten_uhr %>% select(phase_id,pflanze_id,phase,pflanze) %>% unique(),by=c("pflanze_id","phase_id"))
write.csv(stats,"scripts/KfK/Kulturpflanzen_BL_Saarland.csv",row.names = FALSE,fileEncoding = "UTF8")


#Naturraeume

lala <- Daten_uhr %>% st_as_sf(coords=c("lon","lat")) %>%  st_set_crs("WGS84") 
lala <- st_transform(lala,"EPSG:25832","WGS84")
Daten_kulturpflanzen_naturraum <- st_join(lala,Naturraeume_KfK,join=st_within)

Stats_kulturpflanzen_naturraum <- Daten_kulturpflanzen_naturraum %>% group_by(NATNR,refjahr,pflanze_id,phase_id) %>% 
  summarise(doy=mean(jultag),count=n())  

Stats_kulturpflanzen_naturraum_clean <- Stats_kulturpflanzen_naturraum %>% as_tibble() %>% select(!(geometry)) %>%  left_join(Daten_uhr %>% 
                                                     select(phase_id,pflanze_id,phase,pflanze) %>% unique(),
                                                     by=c("pflanze_id","phase_id")) %>% left_join(as_tibble(Naturraeume_KfK) %>% select(!geom),by="NATNR") %>% 
      select(NAME,NATNR,refjahr,pflanze_id,phase_id,doy,count,phase,pflanze) %>% drop_na()

write.csv(Stats_kulturpflanzen_naturraum_clean,"scripts/KfK/Kulturpflanzen_NR_Rheinland-Pfalz.csv",row.names = FALSE,fileEncoding = "UTF8")

