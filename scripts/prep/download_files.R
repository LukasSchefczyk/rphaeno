#download files 
#input should be the filelist from get_filelist 

#todo 
#variable to filter by plant to reduce download
#variable to filter by reporter
#variable to filter periode?? or just whole ts as option and user should crop it 

download_files <- function (df) {
  #define safe download function so that map doesn't stop when deadlink
  temp_dir="temp/"
  safe_download <- safely(~ download.file(.x , .y, mode = "wb"))
  #TODO -> df als links outfile relpath# df %>%  mutate(links=) ### 
  
  links <- df %>% glue_data("{path}{file}")
  outfile <- df %>%  glue_data("{relpath}{file}")
  relpath <- df %>%  glue_data("{temp_dir}{relpath}")
  #create folders 
  dir.create()
    map2(links, outfile, safe_download)
  
}

df <- filelistfull %>%  slice(1:2)
safe_download <- safely(~ download.file(.x , .y, mode = "wb"))
df %>% map2(glue("{path}{file}"), glue("{relpath}{file}"), safe_download)
