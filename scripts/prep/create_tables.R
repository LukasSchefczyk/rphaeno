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
