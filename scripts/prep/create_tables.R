## Joining Datatables
source("scripts/prep/functions_db.R")
library(RSQLite)
library(DBI)
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "temp/dbtest.sqlite3" )

dbListTables(con)

con2 <- DBI::dbConnect(RSQLite::SQLite(), dbname = "temp/test5.sqlite3")

dbListTables(con2)
con <- con2

Stationen <- tbl(con2, "Stationen") 
Pflanze <- tbl(con2, "Pflanze") 
Daten <- tbl(con2,"Daten")
Phasendefinition <- tbl(con2,"Phasendefinition")
Phase <- Phasendefinition %>%  select(phase_id,phase) %>% arrange(phase_id) %>% distinct() %>%  collect() 

station_s <- tbl(con2,"Phaenologie_Stationen_Sofortmelder")
station_j <- tbl(con2,"Phaenologie_Stationen_Jahresmelder")
station_s_only <-anti_join(station_s,station_j,by="stations_id") %>% mutate(Melder="Sofortmelder") %>% collect()
station_j_only <-anti_join(station_j,station_s,by="stations_id") %>% mutate(Melder="Jahresmelder") %>% collect()
station_sj <- semi_join(station_j,station_s,by="stations_id") %>% mutate(Melder="Beides") %>% collect()
lala <- bind_rows(station_s_only,station_sj,station_j_only)

create_megaframe <- function (con=con) {
  
  #con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname )
  con <- con
  Stationen <- tbl(con, "Stationen") 
  Pflanze <- tbl(con, "Pflanze") 
  Daten <- tbl(con,"Daten")
  Phasendefinition <- tbl(con,"Phasendefinition")
  Phase <- tbl(con,"Phase")
  Obst_spezi <- tbl(con, "Obst_Spezifizierung")
  Mais_spezi <- tbl(con, "Mais_Spezifizierung")
  megaframe <- left_join(Daten,Stationen,by="stations_id") %>%
  left_join(Phasendefinition,by=c("pflanze_id","phase_id")) %>%
  left_join(Phase,by=c("phase","phase_id")) %>%
  left_join(Pflanze,by=c("pflanze_id","pflanze")) 
  
  #DBI::dbDisconnect(con)
  
  return(megaframe)
}

create_view_in_db <- function(df,viewname,con=con) {
  #con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname )
  con <- con 
  require(dbplyr)
  #df <- tibble(...)
  df %>% 
    show_query() %>%  
    capture.output() %>% 
    #erste Zeile vom SQL befehl austauschen gegen create view dbplyr kann keine views erstellen
    `[<-`(1, paste0("CREATE VIEW ",viewname," AS")) %>%
    paste(collapse = " \n ") -> viewquery
  DBI::dbExecute(con, viewquery)
  #DBI::dbDisconnect(con)
}

megaframe <- create_megaframe(con2) %>% create_view_in_db("megatest",con2)



megaframe %>% show_query() %>%  
capture.output() %>% 
  #erste Zeile vom SQL befehl austauschen gegen create view dbplyr kann keine views erstellen
  `[<-`(1, "CREATE VIEW megaframe AS") %>%
  paste(collapse = " \n ") -> viewquery


DBI::dbExecute(con, viewquery)

megaframe <- tbl(con2,"Megaframe")


Daten_rlp <- megaframe %>% filter(bundesland %in% c("Rheinland-Pfalz")) %>% collect()

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
