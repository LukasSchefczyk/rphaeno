# Create sf objects from Lat lon data,
#import of Borders of state and nature  from gpkg 
#Namen cleanen von Naturraum etc , Alte Pheno skripts anschauen

library(sf)
gfile <- system.file("data/Naturraum_Grenzen_DE.gpkg", package = "sf", mustWork = TRUE)
gfile <- "data/Naturraum_Grenzen_DE.gpkg"
sf::st_layers(gfile)
bundesland_sf <- read_sf("data/Naturraum_Grenzen_DE.gpkg","bundesland")
nrg_sf <- read_sf("data/Naturraum_Grenzen_DE.gpkg","naturraumgruppe")
nr_sf <- read_sf("data/Naturraum_Grenzen_DE.gpkg","naturraum")
lk_sf <- read_sf("data/Naturraum_Grenzen_DE.gpkg","landkreis")

# ggplot(lk %>% filter(bundesland=="Rheinland-Pfalz")) + geom_sf(aes(fill=BEZ),size=0.2,color="grey") +
# geom_sf(data= bundesland %>% filter(name=="Rheinland-Pfalz"),fill="NA" ,color = "black",size=1) +
# geom_sf(data=lele %>% filter(is.na(datum_stationsaufloesung)))+
#  theme_void()
# 
# ggplot(lk) + geom_sf(aes(fill=BEZ),size=0.2,color="grey") +
#   geom_sf(data= bundesland,fill="NA" ,color = "black",size=1) +
#   geom_sf(data=Stationen %>% collect() %>% station_df_to_sf() %>% filter(is.na(datum_stationsaufloesung)))+
#   theme_void()



nr_sf_chr <- nr_sf %>% pull(naturraum) %>% as_tibble() %>% rename(naturraum=value)
db_nr_chr <- Stationen %>% select(naturraum) %>% distinct() %>% collect()


lala <- db_nr_chr %>% left_join(nr_sf %>% select(naturraum,geom),by="naturraum") %>% st_sf %>% st_set_crs("EPSG:25832")
ggplot(lala) + 
  geom_sf(aes(fill=naturraum_code,geometry=geom),show.legend = FALSE)

db_nrg_chr <- Stationen %>% select(naturraumgruppe) %>% distinct() %>% collect()
nrg_sf_chr <- nrg_sf %>% pull(naturraumgruppe) %>% as_tibble() %>% rename(naturraumgruppe=value)
lele <- db_nrg_chr %>% left_join(nrg_sf %>% select(naturraumgruppe_code,geom),by="naturraumgruppe_code") %>% st_sf %>% st_set_crs("EPSG:25832")

ggplot(lele) + 
  geom_sf(aes(fill=naturraumgruppe_code,geometry=geom),show.legend = FALSE)


bld <- "Rheinland-Pfalz" ; bld <- c("Rheinland-Pfalz","Saarland")
lala %>% filter(bundesland %in% bld) %>% collect() %>% 
  nr_df_to_sf(col2geom="naturraum_code",geomcol = "naturraum_code",clip=bld) %>% 
  ggplot() +
  geom_sf(aes(fill=naturraum_code,geometry=geom),show.legend = FALSE) +
  geom_sf(data=lala %>% filter(bundesland %in% bld) %>% collect() %>% station_df_to_sf() ) 

bld <- c("Rheinland-Pfalz","Saarland")
lala %>% filter(bundesland %in% bld) %>% collect() %>% 
  filter(str_detect(naturraum,regex("saar",ignore_case = TRUE))) %>% 
  nr_df_to_sf(col2geom="naturraum_code",geomcol = "naturraum_code",clip=NULL) %>% 
  ggplot() +
  geom_sf(aes(fill=naturraum_code,geometry=geom),show.legend = FALSE) +
  geom_sf(data=lala %>% filter(bundesland %in% bld) %>% collect() %>%
            filter(str_detect(naturraum,regex("saar",ignore_case = TRUE))) %>%  station_df_to_sf() ) #+ 
 # geom_sf(data=sf::read_sf("data/Naturraum_Grenzen_DE.gpkg","bundesland") %>%  filter(name %in% bld) , fill=NA, size=0.8)

#write table to gpkg 
#st_write(station_df_to_sf(lulu),"rlpstations.gpkg")
