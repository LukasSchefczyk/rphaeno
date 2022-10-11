#Load in Main Data Files 

## Kulturpflanze_Ruebe_akt  / Kulturpflanze_Ruebe_hist  header groß geschrieben... WTF DWD :D ...


#filelist ohne metadaten 
#filelistdaten


data <- filelistdaten %>% mutate(filepath=glue("{temp_dir}{relpath}{file}")) %>% 
         select(filepath) %>%  
  map(function(x) read_csv2(file=x,skip=1,locale = locale(encoding = "ISO-8859-1"),
                            col_names= c("stations_id","referenzjahr","qualitaetsniveau","objekt_id","Phasen_id",
                                         "eintrittsdatum","eintrittsdatum_qb","jultag","eor","...1"),
                            col_select=-contains(c("...","eor")),show_col_types = FALSE)) %>% 
  reduce(rbind) %>%  
  #cleaning tableheaders to all to lowercase and remove whitespace between words
  #remove whitespaces in columnnames 
  rename_with(~tolower(gsub(" ", "_", .x, fixed = TRUE))) %>%  
  #fix type of dbl column to int column 
  mutate(across(where(is.double), as.integer)) 
  #%>%  
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname )
copy_to(con, data, "Daten",
        temporary = FALSE,
        overwrite=TRUE,
        indexes = indexes
        )
DBI::dbDisconnect(con)
rm(data)