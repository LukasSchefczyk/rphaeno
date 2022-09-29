# Download Filelist  

#https://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/

library(tidyverse)
library(rvest)
library(glue)
library(lubridate)

basepath <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/"
basepathftp <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/"
#reporter <- c("annual_reporters","immediate_reporters")
reporter <- "annual_reporters"

 
 
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
      mutate(relpath=str_sub(relpath,1,nchar(relpath)-1)) %>% 
      separate(relpath,c("reporter","group","periode"),sep="/",remove=FALSE) 
      #suffixpath split into 3 vars , doesnt remain in tbl
   
}



filelistfull <- map_dfr(pathlist$paths, get_file_names_from_url)
#ToDo Do i need these three seperate??!
filelistbeschreibung <- filelistfull %>%  filter(str_detect(file,".pdf")) 
filelistmeta <- filelistfull %>%  filter(str_detect(file,"PH_Beschreibung")) 
filelistdaten <- filelistfull %>%  filter(str_detect(file,"PH_Beschreibung|.pdf",negate=TRUE)) 



lala <- filelistfull$path[1]

filelistfull  