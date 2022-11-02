### Functions for Phenodatabase 

create_megaframe <- function (con=con) {
  
  #con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname )
  con <- con
  Stationen <- tbl(con, "Stationen") 
  Pflanze <- tbl(con, "Pflanze") 
  Daten <- tbl(con,"Daten")
  Phasendefinition <- tbl(con,"Phasendefinition")
  Phase <- tbl(con,"Phase")
  Obst_spezi <- tbl(con, "Obst_Spezifizierung")
  Mais_spezi <- tbl(con, "Mais_Spezifizierung")
  megaframe <- left_join(Daten,Stationen,by="stations_id") %>%
    left_join(Phasendefinition,by=c("pflanze_id","phase_id")) %>%
    left_join(Phase,by=c("phase","phase_id")) %>%
    left_join(Pflanze,by=c("pflanze_id","pflanze")) 
  
  #DBI::dbDisconnect(con)
  
  return(megaframe)
}

create_view_in_db <- function(df_query,viewname,con=con) {
  #con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname )
  con <- con 
  require(dbplyr)
  #df <- tibble(...)
  df_query %>% 
    show_query() %>%  
    capture.output() %>% 
    #erste Zeile vom SQL befehl austauschen gegen create view dbplyr kann keine views erstellen
    `[<-`(1, paste0("CREATE VIEW ",viewname," AS")) %>%
    paste(collapse = " \n ") -> viewquery
  DBI::dbExecute(con, viewquery)
  #DBI::dbDisconnect(con)
}

remove_view_from_db <- function(viewname,con=con) {
  delquery <- paste0("DROP VIEW ",viewname)
  DBI::dbExecute(con, delquery)
}


station_df_to_sf <- function (...,src_crs=NULL,tar_crs=NULL,coords_col=NULL,remove=TRUE) {
  df <- tibble(...)
  if(is.null(src_crs)) src_crs <- "EPSG:4326"
  if(is.null(tar_crs)) tar_crs <- "EPSG:25832"
  if(is.null(coords_col)) coords_col <- c("lon","lat") 
  df2 <- st_as_sf(df,coords = coords_col,remove=remove) %>% 
    st_set_crs(.,st_crs(src_crs)) %>% 
    st_transform(.,st_crs(tar_crs))
  return(df2)
}

print_all <- function (...) {
  print(...,n=Inf,width=Inf)
}


nr_df_to_sf <- function(...,col2geom=NULL,geomcol=NULL,clip=NULL) {
  geomcol <- geomcol
  col2geom <- col2geom
  checknr <- c("naturraum","naturraum_code","naturraumgruppe","naturraumgruppe_code")
  if(is.null(geomcol) || !(geomcol %in% checknr)) stop(paste("geomcol needs to be one of",paste(checknr,collapse = ","))) 
  if(is.null(col2geom)) stop("col2geom needs to be defined")                  
  df <- tibble(...)
  #check for nr column in df
  if (geomcol %in% c("naturraum","naturraum_code") ) {
  sf <- sf::read_sf("data/Naturraum_Grenzen_DE.gpkg","naturraum")
  }
  if (geomcol %in% c("naturraumgruppe_code","naturraumgruppe_code") ) {
  sf <- sf::read_sf("data/Naturraum_Grenzen_DE.gpkg","naturraumgruppe")
  }
  if (geomcol %in% c("bundesland")) {
    sf <- sf::read_sf("data/Naturraum_Grenzen_DE.gpkg","bundesland")  
  }
  if(!is.null(clip)) {
    #st_union input to make one polygon if there are multipolygons then st_intersect to cut/clip polygon
    if(!is.character(clip)) stop("clip should be a character vector like c(\"Rheinland-Pfalz\")")
    bld <- sf::read_sf("data/Naturraum_Grenzen_DE.gpkg","bundesland") %>% filter(name %in% clip) %>% st_union
    sf <- suppressWarnings(st_intersection(sf,bld))
  }
  df %>% 
    #setNames to emulate named vector e.g. c("naturraum"="naturraum")
    {if(is.null(clip))
          left_join(.,sf %>% select({{geomcol}},geom),by=setNames(col2geom,geomcol)) 
      else 
          full_join(.,sf %>% select({{geomcol}},geom),by=setNames(col2geom,geomcol))} %>% 
    st_sf %>% st_set_crs("EPSG:25832")

}


