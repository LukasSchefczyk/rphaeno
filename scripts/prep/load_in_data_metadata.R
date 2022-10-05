library(dbplyr)

temp_dir="temp/"
dbname <- "temp/dbtest.sqlite3"
filelistmeta_distinct <- filelistmeta %>%
  mutate(filepath=glue("{temp_dir}{relpath}{file}"))  %>% 
  distinct(file,.keep_all = TRUE) %>% 
  mutate(tablename=str_replace_all(file,c("PH_Beschreibung_"="",".txt"="")),
         dbname=dbname) 
  


#All non Phasendefinition files 
filelistmeta_distinct %>%
  filter(str_detect(tablename, 'Phasendefinition',negate = TRUE)) %>% 
  pwalk(function(...) { 

    df <- tibble(...)
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = df$dbname )
    indexes <- switch(df$tablename,
                      "Phaenologie_Besonderheiten_Zeitreihen" = list("objekt_id","phasen_id"),
                      "Phaenologie_Qualitaetsbyte" = list("qualitaetsbyte"),
                      "Phaenologie_Qualitaetsniveau" = list("qualitaetsniveau"),
                      "Phaenologie_Stationen_Jahresmelder" = list("stations_id","stationsname","bundesland",
                                                                  "naturraumgruppe_code","naturraumgruppe",
                                                                  "naturraum_code","naturraum" ),
                      "Phaenologie_Stationen_Sofortmelder" = list("stations_id","stationsname","bundesland",
                                                                  "naturraumgruppe_code","naturraumgruppe",
                                                                  "naturraum_code","naturraum" ),
                      "Pflanze" = list("objekt_id"),
                      "Phase" =list("phasen_id","phase"),
                      list("objekt_id","phasen_id")
                      )
    
    #load in table "raw" 
    lala <- read_csv2(file=df$filepath,col_names = TRUE,locale = locale(encoding = "ISO-8859-1"),
                      col_select=-contains(c("...","eor")),show_col_types = FALSE) %>% 
      #cleaning tableheaders to all to lowercase and remove whitespace between words
      #remove whitespaces in columnnames 
      rename_with(~tolower(gsub(" ", "_", .x, fixed = TRUE))) %>%  
      #fix type of dbl column to int column 
      mutate(across(where(is.double), as.integer))
    
    if(df$tablename=="Phase") {
      lala <- lala %>%  rename(phasen_id=phase_id)
    }
    if(df$tablename =="Pflanze") {
      #Fix for data in 2 rows ... WTF DWD :D
      lala <- lala %>%  mutate(objekt_latein=lead(objekt_id,1)) %>%
        drop_na() %>% 
        mutate(objekt_id=as.integer(objekt_id))
    }
    
    copy_to(con, lala, df$tablename,
            temporary = FALSE,
            overwrite=TRUE,
            indexes = indexes
    )
    DBI::dbDisconnect(con)

}
)



con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname )
# Combine Phasendefinition Metadata into one df per reporter and both into Phasendefinition 
indexes <- list("objekt_id","phasen_id")
for ( reportertype in c("Jahresmelder","Sofortmelder","melder") ) {
  abfrage <- switch(reportertype,
                    "Jahresmelder" = "Phasendefinition_Jahresmelder",
                    "Sofortmelder" = "Phasendefinition_Sofortmelder",
                    "melder"       = "Phasendefinition"
                   )
    
  data <- filelistmeta_distinct %>%
    filter(str_detect(tablename, abfrage ,negate = FALSE)) %>% select(filepath) %>%  
    map(function(x) read_csv2(file=x,col_names = TRUE,locale = locale(encoding = "ISO-8859-1"),
              col_select=-contains(c("...","eor")),show_col_types = FALSE)) %>% 
    reduce(rbind) %>%  
    #cleaning tableheaders to all to lowercase and remove whitespace between words
    #remove whitespaces in columnnames 
    rename_with(~tolower(gsub(" ", "_", .x, fixed = TRUE))) %>%  
    #fix type of dbl column to int column 
    mutate(across(where(is.double), as.integer))  %>%  
    #remove dulicates
    distinct()
  
  copy_to(con, data, abfrage,
          temporary = FALSE,
          overwrite=TRUE,
          indexes = indexes
  )
  

}
DBI::dbDisconnect(con)  


##Notizen in extra Tabelle laden 
#TODO ANY_OF Funktioniert nicht in der PIPE 
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname )
filelistdaten %>%  
  filter(str_detect(file,"Notiz")) %>%  
  filter(str_detect(file,"Spezifizierung",negate = TRUE)) %>% 
  transmute(filepath=glue("{temp_dir}{relpath}{file}")) %>% 
  map(function(x) {read_csv2(file=x,col_names = TRUE,locale = locale(encoding = "ISO-8859-1"),
                            col_select=-contains(c("...","eor")),show_col_types = FALSE) %>% 
        rename(any_of(c(qualitaetsniveau="QUALITAETS_NIVEAU",qualitaetsniveau="QUALITAETSNIVEAU")))
      }) %>% 
  reduce(rbind)



