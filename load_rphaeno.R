#source this file to load phenoscripts into R
#library dependencies
require(tidyverse)
require(rvest)
require(ggnewscale)
require(ggtext)
require(ggrepel)
require(glue)
require(lubridate)
require(RSQLite)
require(DBI)
require(dbplyr)
require(sf)

# main scripts
source("scripts/prep/functions_db.R")
source("scripts/prep/create_database.R")

#create_database(dbname="data/Phenodb_2023.sqlite3",temp_dir = "temp/",keepdldata = TRUE ,downloaddata=TRUE,meta_spezifizierung = TRUE,meta_beschreibung=TRUE)
#create_database(dbname="data/Phenodb_2023.sqlite3",temp_dir = "temp/",keepdldata = TRUE ,downloaddata=FALSE,meta_spezifizierung = TRUE,meta_beschreibung=TRUE)

