### Functions for Phenodatabase 

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

create_view_in_db <- function(df_query,viewname,con=con) {
  #con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname )
  con <- con 
  require(dbplyr)
  #df <- tibble(...)
  df_query %>% 
    show_query() %>%  
    capture.output() %>% 
    #erste Zeile vom SQL befehl austauschen gegen create view dbplyr kann keine views erstellen
    `[<-`(1, paste0("CREATE VIEW ",viewname," AS")) %>%
    paste(collapse = " \n ") -> viewquery
  DBI::dbExecute(con, viewquery)
  #DBI::dbDisconnect(con)
}
