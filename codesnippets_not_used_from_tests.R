#### Code Snippets vom ausprobieren


test <- list.files(basepathftp,full.names=TRUE,recursive=TRUE)



library(curl)
url = "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/"
h = new_handle(dirlistonly=TRUE)
con = curl(url, "r", h)
tbl = read.table(con, stringsAsFactors=TRUE, fill=TRUE)
close(con)
head(tbl)
html <- read_html(basepath)

html %>%  html_elements("h1")


lala <- read_html("https://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/annual_reporters/crops/historical/")

lala %>%  html_elements("a") %>%  html_attr("href") %>%  str_subset(".txt")  %>%  as_tibble_col(column_name = "files")


get_file_names_from_url <- function(x) {
  read_html(x) %>% 
    html_elements("a") %>%
    html_attr("href") %>% 
    str_subset(".txt")  %>% 
    as_tibble_col(column_name = "files")
}

map_dfr(pathlist2 %>%  pull(paths), get_file_names_from_url)

filelistfull <- pathlist %>% 
  mutate(files=map(paths, get_file_names_from_url),
         filemeta= map(paths,get_meta_filedata)                     ) %>%
  unnest(cols=files) %>% 
  unnest_wider(filemeta)


str_match( ">\\s*(.*?)\\s*\r")          

lulu <- lala %>%  html_elements("pre") %>%  html_text2()

lulu %>%  str_split("\n") %>%  unlist() %>%  str_subset(">") %>%  str_match(">\\s*(.*?)\\s*\r") %>% .[,2] %>% as_tibble_col(column_name = "meta")

get_meta_filedata <- function(x) {
  read_html(x) %>% 
    html_elements("pre") %>%
    html_text2()  %>% 
    str_split("\n") %>% 
    unlist() %>% 
    str_subset(">") %>%
    str_match(">\\s*(.*?)\\s*\r") %>%
    .[,2] %>% 
    as_tibble_col(column_name = "meta") %>%
    separate(meta,c("lm_datum","size"), sep = "\\s\\s+") %>% 
    mutate(lm_datum=dmy_hm(lm_datum),
           size=as.integer(size))
  
}


x <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/annual_reporters/crops/recent/"
x <- pathlist$paths[7]

file <- read_html(x) %>% 
  html_elements("a") %>%
  html_attr("href") %>% 
  # str_subset(".txt")  %>%
  str_subset("../",negate=TRUE) %>% 
  as_tibble_col(column_name = "file")

meta <- read_html(x) %>% 
  html_elements("pre") %>%
  html_text2()  %>% 
  str_split("\n") %>% 
  unlist() %>% 
  #str_subset(">") %>%
  str_subset("../",negate = TRUE) %>%
  str_match("\\s\\s*(.*?)\\s*\r") %>%
  .[,2] %>% 
  as_tibble_col(column_name = "meta") %>%
  drop_na() %>% 
  separate(meta,c("lm_datum","size"), sep = "\\s\\s+") %>% 
  mutate(lm_datum=dmy_hm(lm_datum),
         size=as.integer(size),
         path=x) 

bind_cols(file,meta) %>% 
  relocate(path,.before=file)



con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "temp/dbtest.sqlite3")

copy_to(con, lala, "phasendefinition",
        temporary = FALSE,
        overwrite=TRUE,
        indexes = list(
          "Objekt_id", 
          "Phasen_id"
        )
)


phase <- tbl(con, "phasendefinition")



"Phaenologie_Besonderheiten_Zeitreihen"                     
"Phaenologie_Qualitaetsbyte"                                
"Phaenologie_Qualitaetsniveau"                              
"Phaenologie_Stationen_Jahresmelder"                        
"Phasendefinition_Jahresmelder_Landwirtschaft_Kulturpflanze"
"Pflanze"                                                   
"Phase"                                                     
"Phasendefinition_Jahresmelder_Feldarbeit"                  
"Phasendefinition_Jahresmelder_Weidegang"                   
"Phasendefinition_Jahresmelder_Obst"                        
"Phasendefinition_Jahresmelder_Weinrebe"                    
"Phasendefinition_Jahresmelder_Wein"                        
"Phasendefinition_Jahresmelder_Wildwachsende_Pflanze"       
"Phaenologie_Stationen_Sofortmelder"                        
"Phasendefinition_Sofortmelder_Landwirtschaft_Kulturpflanze"
"Phasendefinition_Sofortmelder_Obst"                        
"Phasendefinition_Sofortmelder_Wildwachsende_Pflanze"



filelistmeta_distinct %>% slice(1) 
#%>% read_csv(file=.$filepath,col_names = TRUE,col_types = NULL,locale = default_locale())

file <- "temp/annual_reporters/crops/historical/PH_Beschreibung_Phaenologie_Besonderheiten_Zeitreihen.txt"
read_csv2(file=file,col_names = TRUE,locale = locale(encoding = "ISO-8859-1"))
lala <- read_csv2(file=file,col_names = TRUE,locale = locale(encoding = "Latin1"),col_select = 1:7)  



#######
#
PH_Beschreibung_Phaenologie_Besonderheiten_Zeitreihen.txt
file <- "temp/annual_reporters/crops/historical/PH_Beschreibung_Phaenologie_Besonderheiten_Zeitreihen.txt"
lala <- read_csv2(file=file,col_names = TRUE,locale = locale(encoding = "Latin1"),col_select = 1:7)  
file <- filelistmeta_distinct %>% slice(2) %>% pull(filepath)
lala <- read_csv2(file=file,col_names = TRUE,locale = locale(encoding = "Latin1"),col_select=-contains(c("...","eor")),show_col_types = FALSE)  
file <- filelistmeta_distinct %>% slice(3) %>% pull(filepath)
lala <- read_csv2(file=file,col_names = TRUE,locale = locale(encoding = "Latin1"))
lala <- read_csv2(file=file,col_names = TRUE,locale = locale(encoding = "Latin1"),col_select=-contains(c("...","eor")),show_col_types = FALSE)  

file <- filelistmeta_distinct %>% slice(6) %>% pull(filepath)
lala <- read_csv2(file=file,col_names = TRUE,locale = locale(encoding = "Latin1"),col_select=-contains(c("...","eor")),show_col_types = FALSE)  

file <- filelistmeta_distinct %>% slice(11) %>% pull(filepath)
lulu <- read_csv2(file=file,col_names = TRUE,locale = locale(encoding = "Latin1"),col_select=-contains(c("...","eor")),show_col_types = FALSE)  


file <- filelistmeta_distinct %>% slice(16) %>% pull(filepath)
lala <- read_csv2(file=file,col_names = TRUE,locale = locale(encoding =  "ISO-8859-1"),col_select=-contains(c("...","eor")),show_col_types = FALSE)  





con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "temp/dbtest.sqlite3")

copy_to(con, lala, "phasendefinition",
        temporary = FALSE,
        overwrite=TRUE,
        indexes = list(
          "Objekt_id", 
          "Phasen_id"
        )
)


phase <- tbl(con, "phasendefinition")

