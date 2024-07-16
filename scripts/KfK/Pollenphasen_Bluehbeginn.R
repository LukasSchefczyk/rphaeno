#Naturraeume_KfK <- read_sf("data/Naturraum_KfK.gpkg") %>% select(c(NAME,NATNR))
uhr_meta_pollenphasen <- tibble(pflanze_id=c(113,128,112,104,135,203,101),
                                pflanze=c("Hasel","Schwarz-Erle","Hänge-Birke","Esche","Wiesen-Fuchsschwanz","Winterroggen","Beifuß"),
                                phase_id=c(5,5,5,5,5,5,5))

Daten_Pollenphasen <- tbl(con,"Megaframe_Jahresmelder") %>%
  filter(bundesland %in% "Rheinland-Pfalz" & 
           pflanze_id %in% !!uhr_meta_pollenphasen$pflanze_id &
           phase_id %in% !!uhr_meta_pollenphasen$phase_id ) %>% 
  collect()

stats_pollenphasen <- Daten_Pollenphasen %>%
  group_by(bundesland,refjahr,pflanze_id,phase_id) %>% 
  summarise(doy=mean(jultag),count=n()) %>% 
  left_join(Daten_Pollenphasen %>% select(phase_id,pflanze_id,phase,pflanze) %>% unique(),by=c("pflanze_id","phase_id"))

write.csv(stats_pollenphasen,"scripts/KfK/Pollenphasen_BL_Rheinland-Pfalz.csv",row.names = FALSE,fileEncoding = "UTF8")

Daten_Pollenphasen <- tbl(con,"Megaframe_Jahresmelder") %>%
  filter(bundesland %in% "Saarland" & 
           pflanze_id %in% !!uhr_meta_pollenphasen$pflanze_id &
           phase_id %in% !!uhr_meta_pollenphasen$phase_id ) %>% 
  collect()

stats_pollenphasen <- Daten_Pollenphasen %>%
  group_by(bundesland,refjahr,pflanze_id,phase_id) %>% 
  summarise(doy=mean(jultag),count=n()) %>% 
  left_join(Daten_Pollenphasen %>% select(phase_id,pflanze_id,phase,pflanze) %>% unique(),by=c("pflanze_id","phase_id"))

write.csv(stats_pollenphasen,"scripts/KfK/Pollenphasen_BL_Saarland.csv",row.names = FALSE,fileEncoding = "UTF8")
