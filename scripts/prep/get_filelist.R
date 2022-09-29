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

pathlist2 <- pathlist[3:4,]
#Todo
#map over urls get filelists, merge together to tbl


get_file_names_from_url2 <- function(x) {
   file <- read_html(x) %>%
      html_elements("a") %>%
      html_attr("href") %>%
      # str_subset(".txt")  %>%
      str_subset("../", negate = TRUE) %>%
      as_tibble_col(column_name = "file")
   
   
   meta <- read_html(x) %>%
      html_elements("pre") %>%
      html_text2()  %>%
      str_split("\n") %>%
      unlist() %>%
      #str_subset(">") %>%
      str_subset("../", negate = TRUE) %>%
      str_match("\\s\\s*(.*?)\\s*\r") %>%  # match between string1(space \\s) and string2 ("\r")
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
      relocate(path, .before = file)
}


filelistfull <- map_dfr(pathlist$paths, get_file_names_from_url2)
  