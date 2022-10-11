#download files 
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
  temp_dir="temp/"
  safe_download <- safely(~ download.file(.x , .y, mode = "wb"))
  
   links <- df %>%  mutate(df.links=glue("{path}{file}"),
                 df.outfile=glue("{temp_dir}{relpath}{file}"),
                 df.relpath=glue("{temp_dir}{relpath}")) %>%
          select(df.links,df.outfile,df.relpath)### 
  

  #create folders 
  walk(links %>% select(df.relpath) %>%  pull(),dir.create,recursive=TRUE,showWarnings=FALSE)
  #download links
  walk2(links %>% select(df.links) %>%  pull(), links %>% select(df.outfile) %>%  pull(), safe_download)
  
}

#download_files(filelist)
#download_files(filelistmeta)
