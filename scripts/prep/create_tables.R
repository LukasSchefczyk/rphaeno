## Joining Datatables
library(RSQLite)
library(DBI)
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "temp/dbtest.sqlite3" )

dbListTables(con)

con2 <- DBI::dbConnect(RSQLite::SQLite(), dbname = "temp/temp.sqlite3")

dbListTables(con2)


Stationen <- tbl(con2, "Stationen") 
Pflanze <- tbl(con2, "Pflanze") 
Daten <- tbl(con2,"Daten")
Phasendefinition <- tbl(con2,"Phasendefinition")
Phasen <- Phasendefinition %>%  select(phasen_id,phase) %>% arrange(phasen_id) %>% distinct() %>%  collect() 

station_s <- tbl(con2,"Phaenologie_Stationen_Sofortmelder")
station_j <- tbl(con2,"Phaenologie_Stationen_Jahresmelder")
station_s_only <-anti_join(station_s,station_j,by="stations_id") %>% mutate(Melder="Sofortmelder") %>% collect()
station_j_only <-anti_join(station_j,station_s,by="stations_id") %>% mutate(Melder="Jahresmelder") %>% collect()
station_sj <- semi_join(station_j,station_s,by="stations_id") %>% mutate(Melder="Beides") %>% collect()
lala <- bind_rows(station_s_only,station_sj,station_j_only)

create_megaframe <- function () {
  #con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname )
  Stationen <- tbl(con, "Stationen") 
  Pflanze <- tbl(con, "Pflanze") 
  Daten <- tbl(con,"Daten")
  Phasendefinition <- tbl(con,"Phasendefinition")
  Phase <- tbl(con,"Phase")
  Obst_spezi <- tbl(con, "Obst_Spezifizierung")
  Mais_spezi <- tbl(con, "Mais_Spezifizierung")
  megaframe <- left_join(Daten,Stationen,by="stations_id") %>%
  left_join(Phasendefinition,by=c("objekt_id","phasen_id")) %>%
  left_join(Phase,by=c("phase","phasen_id")) %>%
  left_join(Pflanze,by=c("objekt_id","objekt")) 
  
  return(megaframe)
}
megaframe <- create_megaframe()

megaframe %>% show_query() %>%  
capture.output() %>% 
  `[<-`(1, "CREATE VIEW megaframe AS") %>%
  paste(collapse = " \n ") -> viewquery


DBI::dbExecute(con, viewquery)

#View erstellen 
#nrow <- dbExecute(con, paste0("CREATE VIEW test AS",
#                              "Random statements"))

dbExecute(con, paste0("CREATE VIEW test AS SELECT
`stations_id`,
`referenzjahr`,
`qualitaetsniveau`,
`LHS`.`objekt_id` AS `objekt_id`,
`phasen_id`,
`eintrittsdatum`,
`eintrittsdatum_qb`,
`jultag`,
`stationsname`,
`geograph.breite`,
`geograph.laenge`,
`stationshoehe`,
`naturraumgruppe_code`,
`naturraumgruppe`,
`naturraum_code`,
`naturraum`,
`datum_stationsaufloesung`,
`bundesland`,
`LHS`.`objekt` AS `objekt`,
`phase`,
`phasendefinition`,
`bbch_code`,
`hinweis_bbch`,
`objekt_englisch`,
`objekt_latein`
FROM (
  SELECT
  `stations_id`,
  `referenzjahr`,
  `qualitaetsniveau`,
  `LHS`.`objekt_id` AS `objekt_id`,
  `LHS`.`phasen_id` AS `phasen_id`,
  `eintrittsdatum`,
  `eintrittsdatum_qb`,
  `jultag`,
  `stationsname`,
  `geograph.breite`,
  `geograph.laenge`,
  `stationshoehe`,
  `naturraumgruppe_code`,
  `naturraumgruppe`,
  `naturraum_code`,
  `naturraum`,
  `datum_stationsaufloesung`,
  `bundesland`,
  `objekt`,
  `phase`,
  `phasendefinition`,
  `bbch_code`,
  `hinweis_bbch`
  FROM (
    SELECT
    `LHS`.`stations_id` AS `stations_id`,
    `referenzjahr`,
    `qualitaetsniveau`,
    `objekt_id`,
    `phasen_id`,
    `eintrittsdatum`,
    `eintrittsdatum_qb`,
    `jultag`,
    `stationsname`,
    `geograph.breite`,
    `geograph.laenge`,
    `stationshoehe`,
    `naturraumgruppe_code`,
    `naturraumgruppe`,
    `naturraum_code`,
    `naturraum`,
    `datum_stationsaufloesung`,
    `bundesland`
    FROM `Daten` AS `LHS`
    LEFT JOIN `Stationen` AS `RHS`
    ON (`LHS`.`stations_id` = `RHS`.`stations_id`)
  ) AS `LHS`
  LEFT JOIN `Phasendefinition` AS `RHS`
  ON (
    `LHS`.`objekt_id` = `RHS`.`objekt_id` AND
    `LHS`.`phasen_id` = `RHS`.`phasen_id`
  )
) AS `LHS`
LEFT JOIN `Pflanze` AS `RHS`
ON (`LHS`.`objekt_id` = `RHS`.`objekt_id` AND `LHS`.`objekt` = `RHS`.`objekt`)" )) 






dbExecute(con, paste0("CREATE VIEW test AS SELECT
`stations_id`,
`referenzjahr`,
`qualitaetsniveau`,
`LHS`.`objekt_id` AS `objekt_id`,
`phasen_id`,
`eintrittsdatum`,
`eintrittsdatum_qb`,
`jultag`,
`stationsname`,
`geograph.breite`,
`geograph.laenge`,
`stationshoehe`,
`naturraumgruppe_code`,
`naturraumgruppe`,
`naturraum_code`,
`naturraum`,
`datum_stationsaufloesung`,
`bundesland`,
`LHS`.`objekt` AS `objekt`,
`phase`,
`phasendefinition`,
`bbch_code`,
`hinweis_bbch`,
`objekt_englisch`,
`objekt_latein`
FROM (
  SELECT
  `stations_id`,
  `referenzjahr`,
  `qualitaetsniveau`,
  `LHS`.`objekt_id` AS `objekt_id`,
  `LHS`.`phasen_id` AS `phasen_id`,
  `eintrittsdatum`,
  `eintrittsdatum_qb`,
  `jultag`,
  `stationsname`,
  `geograph.breite`,
  `geograph.laenge`,
  `stationshoehe`,
  `naturraumgruppe_code`,
  `naturraumgruppe`,
  `naturraum_code`,
  `naturraum`,
  `datum_stationsaufloesung`,
  `bundesland`,
  `objekt`,
  `phase`,
  `phasendefinition`,
  `bbch_code`,
  `hinweis_bbch`
  FROM (
    SELECT
    `LHS`.`stations_id` AS `stations_id`,
    `referenzjahr`,
    `qualitaetsniveau`,
    `objekt_id`,
    `phasen_id`,
    `eintrittsdatum`,
    `eintrittsdatum_qb`,
    `jultag`,
    `stationsname`,
    `geograph.breite`,
    `geograph.laenge`,
    `stationshoehe`,
    `naturraumgruppe_code`,
    `naturraumgruppe`,
    `naturraum_code`,
    `naturraum`,
    `datum_stationsaufloesung`,
    `bundesland`
    FROM `Daten` AS `LHS`
    LEFT JOIN `Stationen` AS `RHS`
    ON (`LHS`.`stations_id` = `RHS`.`stations_id`)
  ) AS `LHS`
  LEFT JOIN `Phasendefinition` AS `RHS`
  ON (
    `LHS`.`objekt_id` = `RHS`.`objekt_id` AND
    `LHS`.`phasen_id` = `RHS`.`phasen_id`
  )
) AS `LHS`
LEFT JOIN `Pflanze` AS `RHS`
ON (`LHS`.`objekt_id` = `RHS`.`objekt_id` AND `LHS`.`objekt` = `RHS`.`objekt`)" )) 



Daten_rlp <- megaframe %>% filter(bundesland %in% c("Rheinland-Pfalz")) %>% collect()

copy_to(con, daten_rlp, "daten_rlp",
        temporary = FALSE,
        overwrite=TRUE,
        indexes = list("stations_id","stationsname","bundesland",
                       "naturraumgruppe_code","naturraumgruppe",
                       "naturraum_code","naturraum","objekt_id","objekt" ))

#### Megaframe rauslesen oder reinschreiben als "echte" Tabelle sprengt RAM in der vm hier... 
#megaframe2 <-  megaframe %>% collect()
if(0){
copy_to(con,megaframe, "megaframe",
        temporary = FALSE,
        overwrite=TRUE,
        indexes = list("stations_id","stationsname","bundesland",
                       "naturraumgruppe_code","naturraumgruppe",
                       "naturraum_code","naturraum","objekt_id","objekt" ))
}


#DBI::dbDisconnect(con)  
