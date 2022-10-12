## Joining Datatables

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "temp/dbtest.sqlite3" )

dbListTables(con)

con2 <- DBI::dbConnect(RSQLite::SQLite(), dbname = "temp/temp.sqlite3")

dbListTables(con2)


Stationen <- tbl(con, "Stationen") 
Pflanze <- tbl(con, "Pflanze") 
Daten <- tbl(con,"Daten")
Phasendefinition <- tbl(con,"Phasendefinition")


create_megaframe <- function () {
  #con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname )
  Stationen <- tbl(con, "Stationen") 
  Pflanze <- tbl(con, "Pflanze") 
  Daten <- tbl(con,"Daten")
  Phasendefinition <- tbl(con,"Phasendefinition")
  Obst_spezi <- tbl(con, "Obst_Spezifizierung")
  Mais_spezi <- tbl(con, "Mais_Spezifizierung")
  megaframe <- left_join(Daten,Stationen,by="stations_id") %>%
  left_join(Phasendefinition,by=c("objekt_id","phasen_id")) %>% 
  left_join(Pflanze,by=c("objekt_id","objekt")) 
  
  return(megaframe)
}
megaframe <- create_megaframe()

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
