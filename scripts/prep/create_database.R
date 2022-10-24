## Create Phenodatabase from DWD function

create_database <- function (dbname="temp/temp.sqlite3",temp_dir="temp/",
                             plant=NULL,downloaddata=TRUE,meta_spezifizierung=TRUE,meta_beschreibung=TRUE,
                             keepdldata=TRUE) {
#### libraries ####   
  require(tidyverse)
  require(rvest)
  require(glue)
  require(lubridate)
  require(RSQLite)
  require(DBI)
  require(dbplyr)
  
  if(!is.atomic(plant)) stop("plant should be simple character vector like c(\"Apfel\",\"Eiche\"")
  if( !dbname %>% str_detect("sqlite") ) stop("dbname should be sqlite3 database and end with .sqlite3")
  if( !dir.exists(temp_dir) ) dir.create(temp_dir,recursive = TRUE)
  
  source("scripts/prep/functions_db.R")
  
#### get filelist ####
  
  basepath <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/"
  url="https://opendata.dwd.de/weather/tree.html"
  
  html <- read_html(url)
  pathlist <- html %>%  
    html_elements("a") %>%  
    html_attr("href") %>% 
    str_subset("phenology") %>%  
    str_subset("observations") %>%  as_tibble_col(column_name="paths")
  
  get_file_names_from_url <- function(x) {
    html <- read_html(x)
    
    file <- html %>%
      html_elements("a") %>%
      html_attr("href") %>%
      str_subset("../", negate = TRUE) %>%
      as_tibble_col(column_name = "file")
    
    meta <- html %>%
      html_elements("pre") %>%
      html_text2()  %>%
      str_split("\n") %>%
      unlist() %>%
      str_subset("../", negate = TRUE) %>% # remove ../ from list
      str_match("\\s\\s*(.*?)\\s*\r") %>%  # match/get everything between string1(space \\s) and string2 ("\r")
      .[, 2] %>%
      as_tibble_col(column_name = "meta") %>%
      drop_na() %>%
      separate(meta, c("lm_datum", "size"), sep = "\\s\\s+") %>% 
      mutate(
        lm_datum = dmy_hm(lm_datum),
        size = as.integer(size),
        path = x
      )
    
    bind_cols(file, meta) %>%
      relocate(path, .after = file) %>%
      
      ### add columns
      ### reporter (annual_reporter or immediate reporter) / jahresmelder | sofortmelder
      ### period ( historical or recent) / periode 
      ### group  / gruppe
      mutate(relpath=str_replace(path,pattern=basepath,replacement = "")) %>%
      mutate(relpath2=str_sub(relpath,start=1,end=nchar(relpath)-1)) %>% 
      separate(relpath2,c("reporter","group","periode"),sep="/",remove=FALSE) %>% 
      select(-relpath2)
    #relpath split into 3 vars 
    #remove last / with str_sub for splitting into 3 instead of 4 groups
  }
  
  #get filelist 
  filelistfull <- map_dfr(pathlist$paths, get_file_names_from_url)
  
  #Generate Filelists for each task in advance
   
  filelistmeta <- filelistfull %>%  filter(str_detect(file,"PH_Beschreibung")) 
  filelistdaten <- filelistfull %>%  
    filter(str_detect(file,"PH_Beschreibung|.pdf|Spezifizierung|Notiz",negate=TRUE)) %>% 
    filter(str_detect(file,paste0(plant,collapse = "|")))
  filelistspezi <- filelistfull %>%  filter(str_detect(file,"Spezifizierung|Notiz"))
  notizen <- filelistspezi  %>%  
    filter(str_detect(file,"Notiz")) %>%
    #Spezifzierungsnotizen rauswerfen
    filter(str_detect(file,"Spezifizierung",negate = TRUE))
  #gruppen <- filelistdaten %>%  distinct(group)
  filelistbeschreibung <- filelistfull %>%  filter(str_detect(file,".pdf")) %>% 
    semi_join(filelistdaten %>%  distinct(group),by="group")
  #remove duplicates with same filename 
  #build filelist to download from above to apply filtering , plant=NULL dont effect filtering 
  #filelist <- filelistfull %>%   distinct(file,.keep_all = TRUE) #full filelist download
  filelist <- bind_rows(filelistmeta,
                        filelistdaten,
                        if(meta_spezifizierung) filelistspezi else NULL,
                        notizen,
                        if(meta_beschreibung) filelistbeschreibung else NULL) %>%  distinct(file,.keep_all=TRUE)
  
  
#### download files 
  
  #input should be the filelist from get_filelist 
  
  #todo 
  #variable to filter by plant to reduce download
  #variable to filter by reporter
  #variable to filter periode?? or just whole ts as option and user should crop it 
  #check if data is already downloaded
  #check wether newer data is available, 
  #check if database is already there, if overwrite from user
  #
  
  download_files <- function (df) {
    #define safe download function so that map doesn't stop when deadlink
    #temp_dir="temp/"
    safe_download <- safely(~ download.file(.x , .y, mode = "w"))
    
    links <- df %>%  mutate(df.links=glue("{path}{file}"),
                            df.outfile=glue("{temp_dir}{relpath}{file}"),
                            df.relpath=glue("{temp_dir}{relpath}")) %>%
      select(df.links,df.outfile,df.relpath)### 
    
    
    #create folders 
    walk(links %>% select(df.relpath) %>%  pull(),dir.create,recursive=TRUE,showWarnings=FALSE)
    #download links
    walk2(links %>% select(df.links) %>%  pull(), links %>% select(df.outfile) %>%  pull(), safe_download)
    
  }
  
if(downloaddata) download_files(filelist)

  #download_files(filelistmeta)
  

  
#### Load in Metadata Files ####
column_fix_names<- c(pflanze_id = "objekt_id", pflanze = "objekt",
                     pflanze_englisch ="objekt_englisch",pflanze_latein="objekt_latein",
                     phase_id = "phasen_id",refjahr="ReferenzJahr",
                     lat="geograph.breite",lon="geograph.laenge")
  
  
    
  filelistmeta_distinct <- filelistmeta %>%
    mutate(filepath=glue("{temp_dir}{relpath}{file}"))  %>% 
    distinct(file,.keep_all = TRUE) %>% 
    mutate(tablename=str_replace_all(file,c("PH_Beschreibung_"="",".txt"="")),
           dbname=dbname) 
  
  
  ### All non Phasendefinition files 
  filelistmeta_distinct %>%
    filter(str_detect(tablename, 'Phasendefinition',negate = TRUE)) %>% 
    pwalk(function(...) { 
      
      df <- tibble(...)
      con <-  DBI::dbConnect(RSQLite::SQLite(), dbname = df$dbname )
      indexes <- switch(df$tablename,
                        "Phaenologie_Besonderheiten_Zeitreihen" = list("pflanze_id","phase_id"),
                        "Phaenologie_Qualitaetsbyte" = list("qualitaetsbyte"),
                        "Phaenologie_Qualitaetsniveau" = list("qualitaetsniveau"),
                        "Phaenologie_Stationen_Jahresmelder" = list("stations_id","stationsname","bundesland",
                                                                    "naturraumgruppe_code","naturraumgruppe",
                                                                    "naturraum_code","naturraum" ),
                        "Phaenologie_Stationen_Sofortmelder" = list("stations_id","stationsname","bundesland",
                                                                    "naturraumgruppe_code","naturraumgruppe",
                                                                    "naturraum_code","naturraum" ),
                        "Pflanze" = list("pflanze_id"),
                        "Phase" =list("phase_id","phase"),
                        list("objekt_id","phase_id")
      )
      
      #load in table "raw" 
      lala <- read_delim(file=df$filepath,col_names = TRUE,
                         locale = locale(encoding = "ISO-8859-1",decimal_mark = "."),
                         col_select=-contains(c("...","eor")),show_col_types = FALSE,
                         delim=";",trim_ws = TRUE,n_max = 99000000) %>%
        #cleaning tableheaders to all to lowercase and remove whitespace between words
        #remove whitespaces in columnnames 
        rename_with(~tolower(gsub(" ", "_", .x, fixed = TRUE))) %>%  
        #fix type of dbl column to int column 
        mutate(across(c(where(is.double),-starts_with("geograph.")), as.integer)) 
      
      
      #if(df$tablename=="Phase") {
      #  lala <- lala %>%  rename(phasen_id=phase_id)
      #}
      if(df$tablename =="Pflanze") {
        #Fix for data in 2 rows ... WTF DWD :D
        lala <- lala %>%  mutate(objekt_latein=lead(objekt_id,1)) %>%
          drop_na() #%>% 
        # mutate(objekt_id=as.integer(objekt_id))
      }
      
      #change type to int and rename columnnames 
        lala <- lala %>%  
        mutate(across(ends_with(c("_id","_code")),as.integer)) %>% 
        rename(any_of(column_fix_names))
      
      copy_to(con, lala, df$tablename,
              temporary = FALSE,
              overwrite=TRUE,
              indexes = indexes
      )
      DBI::dbDisconnect(con)
    }
    )
  
  
  #### Combine Stationen into Stationen Table ####  
  con <-  DBI::dbConnect(RSQLite::SQLite(), dbname = dbname )
  Jahresmelder <- tbl(con, "Phaenologie_Stationen_Jahresmelder") %>%  collect()
  Sofortmelder <- tbl(con, "Phaenologie_Stationen_Sofortmelder") %>%  collect()
  #Stationen <- rbind(Jahresmelder,Sofortmelder)  %>%  distinct(stations_id,.keep_all = TRUE)
  #adding Meldertyp to Stationen table
  station_s_only <-anti_join(Sofortmelder,Jahresmelder,by="stations_id") %>% mutate(melder="Sofortmelder")
  station_j_only <-anti_join(Jahresmelder,Sofortmelder,by="stations_id") %>% mutate(melder="Jahresmelder") 
  station_sj <- semi_join(Jahresmelder,Sofortmelder,by="stations_id") %>% mutate(melder="Beides")
  Stationen <- bind_rows(station_s_only,station_sj,station_j_only) %>% 
    arrange(stations_id) %>%  #rename(lat=geograph.breite,lon=geograph.laenge) %>% 
    #mutate(datum_stationsaufloesung=ymd(dmy(datum_stationsaufloesung))) %>%
    rename(any_of(column_fix_names))
  
  copy_to(con, Stationen, "Stationen",
          temporary = FALSE,
          overwrite=TRUE,
          indexes = list("stations_id","stationsname","bundesland",
                         "naturraumgruppe_code","naturraumgruppe",
                         "naturraum_code","naturraum" ))
  DBI::dbDisconnect(con) 
  #rm(list=c("Jahresmelder","Sofortmelder","Stationen"))  
  
  
  
  
  ### Combine Phasendefinition Metadata into one df per reporter and both into Phasendefinition
  con <-  DBI::dbConnect(RSQLite::SQLite(), dbname = dbname )
  indexes <- list("pflanze_id","phase_id")
  for ( reportertype in c("Jahresmelder","Sofortmelder","melder") ) {
    abfrage <- switch(reportertype,
                      "Jahresmelder" = "Phasendefinition_Jahresmelder",
                      "Sofortmelder" = "Phasendefinition_Sofortmelder",
                      "melder"       = "Phasendefinition"
    )
    
    data <- filelistmeta_distinct %>%
      filter(str_detect(tablename, abfrage ,negate = FALSE)) %>% select(filepath) %>%  
      map(function(x) read_csv2(file=x,col_names = TRUE,locale = locale(encoding = "ISO-8859-1"),
                                col_select=-contains(c("...","eor")),show_col_types = FALSE,
                                n_max = 99000000)) %>% 
      reduce(rbind) %>%  
      #cleaning tableheaders to all to lowercase and remove whitespace between words
      #remove whitespaces in columnnames 
      rename_with(~tolower(gsub(" ", "_", .x, fixed = TRUE))) %>%  
      #fix type of dbl column to int column 
      mutate(across(where(is.double), as.integer))  %>%  
      #remove dulicates
      distinct() %>%  
      rename(any_of(column_fix_names))
    
    copy_to(con, data, abfrage,
            temporary = FALSE,
            overwrite=TRUE,
            indexes = indexes
    )
    
    
  }
  DBI::dbDisconnect(con)  
  
  
  ##Notizen in extra Tabelle laden 
  con <-  DBI::dbConnect(RSQLite::SQLite(), dbname = dbname )
  notizendb <- notizen %>% 
    transmute(filepath=glue("{temp_dir}{relpath}{file}")) %>% 
    map(function(x) {read_csv2(file=x,skip=2,locale = locale(encoding = "ISO-8859-1"),
                               col_select=-contains(c("...","eor")),show_col_types = FALSE,
                               col_names=c("stations_id","refjahr", "qualitaetsniveau", "objekt_id", 
                                           "objekt", "phasen_id", "phase", "notizen","eor","...1"),
                               n_max = 99000000)}) %>% 
    
    reduce(rbind) %>% 
    #fix type of dbl column to int column 
    mutate(across(where(is.double), as.integer)) %>% 
    rename(any_of(column_fix_names))
  
  copy_to(con, notizendb, "Notizen",
          temporary = FALSE,
          overwrite=TRUE,
          indexes = indexes
  )
  DBI::dbDisconnect(con)    
  
  
  if(meta_spezifizierung) {
  ### Spezifizierung notiz
  con <-  DBI::dbConnect(RSQLite::SQLite(), dbname = dbname )
  spezi_notiz <- filelistspezi %>%  
    filter(str_detect(file,"Notiz")) %>%  
    filter(str_detect(file,"Spezifizierung",negate = FALSE)) %>% 
    transmute(filepath=glue("{temp_dir}{relpath}{file}")) %>% 
    map(function(x) {read_csv2(file=x,skip=2,locale = locale(encoding = "ISO-8859-1"),
                               col_select=-contains(c("...","eor")),show_col_types = FALSE,
                               col_names=c("stations_id","refjahr", "objekt_id", 
                                           "objekt", "sorte_id","sorte", "notizen","eor","...1"),
                               n_max = 99000000)}) %>% 
    
    reduce(rbind) %>% 
    #fix type of dbl column to int column 
    mutate(across(where(is.double), as.integer)) %>% 
    rename(any_of(column_fix_names))
  
  
  copy_to(con, spezi_notiz, "Spezifizierung_Notizen",
          temporary = FALSE,
          overwrite=TRUE,
          indexes = c("stations_id","pflanze_id","sorte_id")
  )
  DBI::dbDisconnect(con) 
  
  
  
  ###Spezifizierung
  tabname <- filelistspezi %>%  
    filter(str_detect(file,"Spezifizierung")) %>%  
    filter(str_detect(file,"Notiz",negate = TRUE)) %>% 
    select(file) %>%  
    mutate(file=str_replace(file,".txt",""),file = map(str_split(file, "_"), tail, 2)) %>%
    unnest_wider(file, names_repair = ~paste0("dir", seq_along(.) - 1)) %>%  
    transmute(tabname=glue("{dir0}_{dir1}"))
  
  spezi <- filelistspezi %>%  
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
        col_names = TRUE,
        n_max = 99000000
        
      ) %>%
      mutate(across(where(is.double), as.integer)) %>%
      #remove whitespaces in columnnames
      rename_with( ~ tolower(gsub(" ", "_", .x, fixed = TRUE))) %>% 
      rename(any_of(column_fix_names))
    
    con <-  DBI::dbConnect(RSQLite::SQLite(), dbname = df$dbname )
    copy_to(con, lala, df$tabname,
            temporary = FALSE,
            overwrite=TRUE,
            indexes = c("stations_id","pflanze_id","sorte_id")
    )
    DBI::dbDisconnect(con) 
  }
  ## Obst spezifizierung gefixt mit n_max wert statt default Inf
  for (i in 1:length(spezi[[1]]) ) {
    read_spezi(spezi[i,])
    
  }
  }#if(meta_spezifizierung)

#### Load in Data ####   
  
  data <- filelistdaten %>% mutate(filepath=glue("{temp_dir}{relpath}{file}")) %>% 
    select(filepath) %>%  
    map(function(x) read_csv2(file=x,skip=1,locale = locale(encoding = "ISO-8859-1"),
                              col_names= c("stations_id","refjahr","qualitaetsniveau","objekt_id","phasen_id",
                                           "eintrittsdatum","eintrittsdatum_qb","jultag","eor","...1"),
                              col_select=-contains(c("...","eor")),show_col_types = FALSE,
                              n_max = 99000000)) %>% 
    reduce(rbind) %>%  
    #cleaning tableheaders to all to lowercase and remove whitespace between words
    #remove whitespaces in columnnames 
    rename_with(~tolower(gsub(" ", "_", .x, fixed = TRUE))) %>%  
    #fix type of dbl column to int column 
    mutate(across(where(is.double), as.integer)) %>% 
    rename(any_of(column_fix_names))
  #%>%  
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname )
  copy_to(con, data, "Daten",
          temporary = FALSE,
          overwrite=TRUE,
          indexes = indexes
  )
  
#### Create megaframe View in database 
  
  create_megaframe(con) %>% create_view_in_db("Megaframe",con)
  
  DBI::dbDisconnect(con)
  
#### Deleting Temp Data #### 
if(!keepdldata) {
  unlink(temp_dir, recursive = TRUE)
}
# Cleaning Ram 
invisible(gc())
  
}#create_database 

#create_database(downloaddata = FALSE)
#create_database(dbname="data/test.sqlite3",temp_dir = "lala/",keepdldata = TRUE ,downloaddata=TRUE,meta_spezifizierung = FALSE,plant=c("Birne"))
create_database(dbname="temp/test5.sqlite3",temp_dir = "temp/",keepdldata = TRUE ,downloaddata=FALSE,meta_spezifizierung = TRUE)



