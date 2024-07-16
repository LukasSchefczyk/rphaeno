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
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname )
notizen <- filelistdaten %>%  
  filter(str_detect(file,"Notiz")) %>%  
  filter(str_detect(file,"Spezifizierung",negate = TRUE)) %>% 
  transmute(filepath=glue("{temp_dir}{relpath}{file}")) %>% 
  map(function(x) {read_csv2(file=x,skip=2,locale = locale(encoding = "ISO-8859-1"),
                             col_select=-contains(c("...","eor")),show_col_types = FALSE,
                             col_names=c("stations_id","referenzjahr", "qualitaetsniveau", "objekt_id", 
                                         "objekt", "phasen_id", "phase", "notizen","eor","...1"))}) %>% 
  
  reduce(rbind) %>% 
  #fix type of dbl column to int column 
  mutate(across(where(is.double), as.integer)) 

copy_to(con, notizen, "Notizen",
        temporary = FALSE,
        overwrite=TRUE,
        indexes = indexes
)
DBI::dbDisconnect(con)  

### Spezifizierung notiz
spezi_notiz <- filelistdaten %>%  
  filter(str_detect(file,"Notiz")) %>%  
  filter(str_detect(file,"Spezifizierung",negate = FALSE)) %>% 
  transmute(filepath=glue("{temp_dir}{relpath}{file}")) %>% 
  map(function(x) {read_csv2(file=x,skip=2,locale = locale(encoding = "ISO-8859-1"),
                             col_select=-contains(c("...","eor")),show_col_types = FALSE,
                             col_names=c("stations_id","referenzjahr", "objekt_id", 
                                         "objekt", "sorte_id","sorte", "notizen","eor","...1"))}) %>% 
  
  reduce(rbind) %>% 
  #fix type of dbl column to int column 
  mutate(across(where(is.double), as.integer))


copy_to(con, spezi_notiz, "Spezifizierung_Notizen",
        temporary = FALSE,
        overwrite=TRUE,
        indexes = c("stations_id","objekt_id","sorte_id")
)
DBI::dbDisconnect(con) 


###Spezifizierung
tabname <- filelistdaten %>%  
  filter(str_detect(file,"Spezifizierung")) %>%  
  filter(str_detect(file,"Notiz",negate = TRUE)) %>% 
  select(file) %>%  
  mutate(file=str_replace(file,".txt",""),file = map(str_split(file, "_"), tail, 2)) %>%
  unnest_wider(file, names_repair = ~paste0("dir", seq_along(.) - 1)) %>%  
  transmute(tabname=glue("{dir0}_{dir1}"))

###TODO  über die 4 Spezfizerungsdateien loopen und in tabnames in die db schreiben
# Pwalk funktioniert nicht
#spezi <- 
if(0) {
filelistdaten %>%  
  filter(str_detect(file,"Spezifizierung")) %>%  
  filter(str_detect(file,"Notiz",negate = TRUE)) %>% 
  transmute(filepath=glue("{temp_dir}{relpath}{file}")) %>% 
  bind_cols(tabname) %>%
  mutate(dbname=dbname)%>% 
  #slice(1) %>% 
  ##########  FIX ME bleibt haengen , oder auch nicht WTF... manchmal gehts manchmal nicht .... 
  pwalk(function(...) {
    
    df <- tibble(...)
    
    lala <- 
      read_csv2(
        file = df$filepath,
        skip = 0,
        locale = locale(encoding = "ISO-8859-1"),
        col_select = -contains(c("...", "eor")),
        show_col_types = FALSE,
        col_names = TRUE
      ) %>%
      mutate(across(where(is.double), as.integer)) %>%
      #remove whitespaces in columnnames
      rename_with( ~ tolower(gsub(" ", "_", .x, fixed = TRUE)))
    
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = df$dbname )
    copy_to(con, lala, df$tabname,
            temporary = FALSE,
            overwrite=TRUE,
            indexes = c("stations_id","objekt_id","sorte_id")
    )
    DBI::dbDisconnect(con) 
  } )  
### FIX ME ^^^^^^^^
}### For futureme to fix maybe ... have fun

spezi <- filelistdaten %>%  
  filter(str_detect(file,"Spezifizierung")) %>%  
  filter(str_detect(file,"Notiz",negate = TRUE)) %>% 
  transmute(filepath=glue("{temp_dir}{relpath}{file}")) %>% 
  bind_cols(tabname) %>%
  mutate(dbname=dbname)


read_spezi <- function(...) {
  
  df <- tibble(...)
  
  lala <- 
    read_csv2(
      file = df$filepath,
      skip = 1,
      locale = locale(encoding = "ISO-8859-1"),
      col_select = -contains(c("...", "eor")),
      show_col_types = FALSE,
      col_names = TRUE
    ) %>%
    mutate(across(where(is.double), as.integer)) %>%
    #remove whitespaces in columnnames
    rename_with( ~ tolower(gsub(" ", "_", .x, fixed = TRUE)))
  
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = df$dbname )
  copy_to(con, lala, df$tabname,
          temporary = FALSE,
          overwrite=TRUE,
          indexes = c("stations_id","objekt_id","sorte_id")
  )
  DBI::dbDisconnect(con) 
}

#for (i in length(spezi[[1]]) ) {
 for (i in 3) {
   read_spezi(spezi[i,])
  
}
