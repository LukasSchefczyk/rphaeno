## Joining Datatables
source("scripts/prep/functions_db.R")
library(RSQLite)
library(DBI)
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "temp/dbtest.sqlite3" )

dbListTables(con)

con2 <- DBI::dbConnect(RSQLite::SQLite(), dbname = "temp/test8.sqlite3")

dbListTables(con2)
con <- con2

Stationen <- tbl(con2, "Stationen") 
Pflanze <- tbl(con2, "Pflanze") 
Daten <- tbl(con2,"Daten")
Daten_Jahresmelder <- tbl(con2,"Daten_Jahresmelder")
Phasendefinition <- tbl(con2,"Phasendefinition")
Phase <- Phasendefinition %>%  select(phase_id,phase) %>% arrange(phase_id) %>% distinct() %>%  collect() 
megaframe_Jahresmelder <- tbl(con2,"Megaframe_Jahresmelder")

Daten_rlp <- megaframe_Jahresmelder %>% filter(bundesland %in% c("Rheinland-Pfalz")) %>% collect()

copy_to(con, daten_rlp, "daten_rlp",
        temporary = FALSE,
        overwrite=TRUE,
        indexes = list("stations_id","stationsname","bundesland",
                       "naturraumgruppe_code","naturraumgruppe",
                       "naturraum_code","naturraum","objekt_id","objekt" ))

#### Megaframe rauslesen oder reinschreiben als "echte" Tabelle sprengt RAM in der vm hier... 
#megaframe2 <-  megaframe %>% collect()
if(0){
copy_to(con,megaframe, "megaframe",
        temporary = FALSE,
        overwrite=TRUE,
        indexes = list("stations_id","stationsname","bundesland",
                       "naturraumgruppe_code","naturraumgruppe",
                       "naturraum_code","naturraum","objekt_id","objekt" ))
}

#DBI::dbDisconnect(con)  

megaframe <- tbl(con,"Megaframe")
Daten_rlp <- megaframe %>% 
  filter(bundesland %in% c("Rheinland-Pfalz")) %>% 
  collect()

Kettenheim <- megaframe %>% filter(stationsname=="Kettenheim") %>% collect()
Kettenheim %>% filter(phase_id==5,pflanze_id==210) %>%  ggplot() + aes(x=refjahr,y=jultag) + geom_line()




df <- Daten_rlp %>% filter(phase_id==5 & stationshoehe>200 & refjahr == 2021 & pflanze_id==215)

df %>%  nr_df_to_sf(col2geom="naturraum_code",geomcol = "naturraum_code",clip=NULL) %>% 
  ggplot() +
  geom_sf(aes(fill=jultag,geometry=geom),show.legend = TRUE) +
  geom_sf(data= df %>% 
            station_df_to_sf() )



#phaenologische Uhr Phasen 
# Vorfrühling	Hasel (Blühbeginn)	1992 - Vorjahr
# Erstfrühling	Forsythie (Blühbeginn)	2001 - Vorjahr
# Vollfrühling	Apfel (Blühbeginn)	1992 - Vorjahr
# Frühsommer	Schwarzer Holunder (Blühbeginn)	1992 - Vorjahr
# Hochsommer	Sommer-Linde (Blühbeginn)	1992 - Vorjahr
# Spätsommer	Frühreifende Äpfel (Pflückreife)	1992 - Vorjahr
# Frühherbst	Schwarzer Holunder (Fruchtreife)	1992 - Vorjahr
# Vollherbst	Stiel-Eiche (Früchte)	2011 - Vorjahr
# Spätherbst	Stiel-Eiche (Blattverfärbung)	2011 - Vorjahr
# Winter	Stiel-Eiche (Blattfall)	2011 - Vorjahr



Megaframe_Jahresmelder <- tbl(con,"Megaframe_Jahresmelder") 

Stationen  <- tbl(con,"Stationen")

Pflanze <- tbl(con,"Pflanze") 

Phasendefinition <- tbl(con,"Phasendefinition") 

tbl(con,"Mais_Spezifizierung") 

tbl(con,"Weinrebe_Spezifizierung") 

tbl(con,"Ruebe_Spezifizierung")

Phaenologie_Besonderheiten_Zeitreihen<- tbl(con,"Phaenologie_Besonderheiten_Zeitreihen")

Notizen <- tbl(con,"Notizen")


# Pflanze %>% collect() %>%
#   filter(str_detect(pflanze,paste(c("Hasel","Forsythie","Apfel","Holunder","Sommer-Linde","Stiel-Eiche"),collapse = "|")))
# 
# c(109,113,129,130,132,310,311)
# 
# c("Forsythie","Hasel","Schwarzer Holunder","Sommer-Linde","Stiel-Eiche","Apfel","Apfel, frühe Reife")

uhr_meta <- tibble(pflanze_id=c(113,109,310,129,130,311,129,132,132,132),
       pflanze=c("Hasel","Forsythie","Apfel","Schwarzer Holunder","Sommer-Linde","Apfel, frühe Reife","Schwarzer Holunder","Stiel-Eiche","Stiel-Eiche","Stiel-Eiche"),
       phase_id=c(5,5,5,5,5,29,62,62,31,32),
       jahreszeit=c("Vorfrühling","Erstfrühling","Vollfrühling","Frühsommer","Hochsommer","Spätsommer","Frühherbst","Vollherbst","Spätherbst","Winter"),
       alt_pflanze_id=c(127,350,132,121,360,103,119,122,103,313),
       alt_pflanze=c("Schneeglöckchen","Stachelbeere","Stiel-Eiche","Robinie","Rote Johannisbeere","Eberesche","Kornelkirsche","Rosskastanie","Eberesche","Apfel, späte Reife"),
       alt_phase_id=c(5,4,4,5,29,62,62,62,32,32),
       ) %>% mutate(order=1:length(pflanze))


Daten_uhr <- tbl(con,"Megaframe_Jahresmelder") %>%
  filter(bundesland %in% "Rheinland-Pfalz" & pflanze_id %in% !!uhr_meta$pflanze_id & phase_id %in% !!uhr_meta$phase_id) %>% 
  collect()

Daten_uhr %>%  filter(pflanze_id==uhr_meta$pflanze_id[1] & phase_id==uhr_meta$phase_id[1]) %>% 
  summarise(mean_jultag=mean(jultag),order=uhr_meta$order[1])

plot_data_uhr <- Daten_uhr %>% left_join(uhr_meta ,by=c("pflanze_id","phase_id"))  %>% 
  filter(refjahr>=1961 & refjahr <=2021) %>%  
  group_by(order) %>%
  drop_na(order) %>% 
  summarise(mean_jultag=mean(jultag),count=n()) %>%
  right_join(uhr_meta,by="order") %>% 
  print_all
