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

