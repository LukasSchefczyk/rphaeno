# Download Filelist  

#https://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/

library(tidyverse)
library(rvest)
library(glue)

basepath <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/"
basepathftp <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/"
#reporter <- c("annual_reporters","immediate_reporters")
reporter <- "annual_reporters"

 
 
 url="https://opendata.dwd.de/weather/tree.html"
 html <- read_html(url)
 html %>%  
   html_elements("a") %>%  
   html_attr("href") %>% 
   str_subset("phenology") %>%  
   str_subset("observations")
 
 #Todo
 #map over urls get filelists, merge together to tbl

 