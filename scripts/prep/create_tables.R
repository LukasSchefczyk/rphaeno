## Joining Datatables
source("scripts/prep/functions_db.R")
library(RSQLite)
library(DBI)
library(ggnewscale)
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "temp/dbtest.sqlite3" )

dbListTables(con)

con2 <- DBI::dbConnect(RSQLite::SQLite(), dbname = "temp/test8.sqlite3")

dbListTables(con2)
con <- con2

Stationen <- tbl(con2, "Stationen") 
Pflanze <- tbl(con2, "Pflanze") 
Daten <- tbl(con2,"Daten")
Daten_Jahresmelder <- tbl(con2,"Daten_Jahresmelder")
Phasendefinition <- tbl(con2,"Phasendefinition")
Phase <- Phasendefinition %>%  select(phase_id,phase) %>% arrange(phase_id) %>% distinct() %>%  collect() 
megaframe_Jahresmelder <- tbl(con2,"Megaframe_Jahresmelder")

Daten_rlp <- megaframe_Jahresmelder %>% filter(bundesland %in% c("Rheinland-Pfalz")) %>% collect()

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

megaframe <- tbl(con,"Megaframe")
Daten_rlp <- megaframe %>% 
  filter(bundesland %in% c("Rheinland-Pfalz")) %>% 
  collect()

Kettenheim <- megaframe %>% filter(stationsname=="Kettenheim") %>% collect()
Kettenheim %>% filter(phase_id==5,pflanze_id==210) %>%  ggplot() + aes(x=refjahr,y=jultag) + geom_line()




df <- Daten_rlp %>% filter(phase_id==5 & stationshoehe>200 & refjahr == 2021 & pflanze_id==215)

df %>%  nr_df_to_sf(col2geom="naturraum_code",geomcol = "naturraum_code",clip=NULL) %>% 
  ggplot() +
  geom_sf(aes(fill=jultag,geometry=geom),show.legend = TRUE) +
  geom_sf(data= df %>% 
            station_df_to_sf() )



#phaenologische Uhr Phasen 
# Vorfrühling	Hasel (Blühbeginn)	1992 - Vorjahr
# Erstfrühling	Forsythie (Blühbeginn)	2001 - Vorjahr
# Vollfrühling	Apfel (Blühbeginn)	1992 - Vorjahr
# Frühsommer	Schwarzer Holunder (Blühbeginn)	1992 - Vorjahr
# Hochsommer	Sommer-Linde (Blühbeginn)	1992 - Vorjahr
# Spätsommer	Frühreifende Äpfel (Pflückreife)	1992 - Vorjahr
# Frühherbst	Schwarzer Holunder (Fruchtreife)	1992 - Vorjahr
# Vollherbst	Stiel-Eiche (Früchte)	2011 - Vorjahr
# Spätherbst	Stiel-Eiche (Blattverfärbung)	2011 - Vorjahr
# Winter	Stiel-Eiche (Blattfall)	2011 - Vorjahr



Megaframe_Jahresmelder <- tbl(con,"Megaframe_Jahresmelder") 

Stationen  <- tbl(con,"Stationen")

Pflanze <- tbl(con,"Pflanze") 

Phasendefinition <- tbl(con,"Phasendefinition") 

tbl(con,"Mais_Spezifizierung") 

tbl(con,"Weinrebe_Spezifizierung") 

tbl(con,"Ruebe_Spezifizierung")

Phaenologie_Besonderheiten_Zeitreihen<- tbl(con,"Phaenologie_Besonderheiten_Zeitreihen")

Notizen <- tbl(con,"Notizen")


# Pflanze %>% collect() %>%
#   filter(str_detect(pflanze,paste(c("Hasel","Forsythie","Apfel","Holunder","Sommer-Linde","Stiel-Eiche"),collapse = "|")))
# 
# c(109,113,129,130,132,310,311)
# 
# c("Forsythie","Hasel","Schwarzer Holunder","Sommer-Linde","Stiel-Eiche","Apfel","Apfel, frühe Reife")

uhr_meta <- tibble(pflanze_id=c(113,109,310,129,130,311,129,132,132,132),
       pflanze=c("Hasel","Forsythie","Apfel","Schwarzer Holunder","Sommer-Linde","Apfel, frühe Reife","Schwarzer Holunder","Stiel-Eiche","Stiel-Eiche","Stiel-Eiche"),
       phase_id=c(5,5,5,5,5,29,62,62,31,32),
       jahreszeit=c("Vorfrühling","Erstfrühling","Vollfrühling","Frühsommer","Hochsommer","Spätsommer","Frühherbst","Vollherbst","Spätherbst","Winter"),
       alt_pflanze_id=c(127,350,132,121,360,103,119,122,103,313),
       alt_pflanze=c("Schneeglöckchen","Stachelbeere","Stiel-Eiche","Robinie","Rote Johannisbeere","Eberesche","Kornelkirsche","Rosskastanie","Eberesche","Apfel, späte Reife"),
       alt_phase_id=c(5,4,4,5,29,62,62,62,32,32),
       ) %>% mutate(order=1:length(pflanze))


Daten_uhr <- tbl(con,"Megaframe_Jahresmelder") %>%
  filter(bundesland %in% "Rheinland-Pfalz" & pflanze_id %in% !!uhr_meta$pflanze_id & phase_id %in% !!uhr_meta$phase_id) %>% 
  collect()

Daten_uhr %>%  filter(pflanze_id==uhr_meta$pflanze_id[1] & phase_id==uhr_meta$phase_id[1]) %>% 
  summarise(mean_jultag=mean(jultag),order=uhr_meta$order[1])

plot_data_uhr <- Daten_uhr %>% left_join(uhr_meta ,by=c("pflanze_id","phase_id"))  %>% 
  filter(refjahr>=1961 & refjahr <=2021) %>%  
  group_by(order) %>%
  drop_na(order) %>% 
  summarise(mean_jultag=mean(jultag),count=n()) %>%
  right_join(uhr_meta,by="order") %>% 
  #right_join(Daten_uhr %>% select(phase_id,phase) %>%  distinct(),by=c("alt_phase_id" = "phase_id")) %>% 
  #rename(alt_phase=phase) %>% 
  right_join(Daten_uhr %>% select(phase_id,phase) %>%  distinct(),by="phase_id") 
  
   
plot_data_uhr_2 <- Daten_uhr %>% left_join(uhr_meta ,by=c("pflanze_id","phase_id"))  %>% 
  filter(refjahr>=1991 & refjahr <=2021) %>%  
  group_by(order) %>%
  drop_na(order) %>% 
  summarise(mean_jultag=mean(jultag),count=n()) %>%
  right_join(uhr_meta,by="order") %>% 
  #right_join(Daten_uhr %>% select(phase_id,phase) %>%  distinct(),by=c("alt_phase_id" = "phase_id")) %>% 
  #rename(alt_phase=phase) %>% 
  right_join(Daten_uhr %>% select(phase_id,phase) %>%  distinct(),by="phase_id") 


# Compute percentages
p <- plot_data_uhr %>%  mutate(fraction= mean_jultag/365/sum(mean_jultag/365),
                          ymax=cumsum(fraction),
                          ymin=c(0, head(ymax, n=-1))) %>% 
  ggplot() + aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=mean_jultag) +
  geom_rect() +
  coord_polar(theta="y",start=0) + 
  xlim(c(-1, 4))
png("plots/pietest.png",width = 192*5,height=108*5,type="cairo-png")
print(p)
dev.off()


p <- plot_data_uhr %>%  mutate(fraction= mean_jultag/365/sum(mean_jultag/365),
                               ymax=cumsum(fraction),
                               ymin=c(0, head(ymax, n=-1))) %>% 
  ggplot(aes(x=1:10,y=fraction, fill=mean_jultag))  +
  geom_bar(stat="identity") +
  coord_polar(theta="y",start=0) + 
  xlim(c(-3, 2))
ggsave("plots/pietest2.png",device = "png")



#add overlapping days to beginning of year
pdata <- bind_rows(plot_data_uhr %>%  slice(n()) %>%  mutate(order=0,mean_jultag=0), plot_data_uhr) %>% 
  #calc percentages
  mutate(fraction= mean_jultag/365,
                            ymax=cumsum(fraction),
                            ymin=c(0, head(ymax, n=-1)))

pdata <- bind_rows(plot_data_uhr %>%  slice(n()) %>%  mutate(order=10,mean_jultag=0),
                   plot_data_uhr,
                   plot_data_uhr %>%  slice(n()) %>%  mutate(order=11,mean_jultag=365)) %>% 
  #calc percentages
  mutate(diff_mean_jultag=lead(mean_jultag)-mean_jultag) %>% 
  drop_na(diff_mean_jultag) %>% 
  mutate(fraction= diff_mean_jultag,
         ymax=cumsum(fraction),
         ymin=c(0, head(ymax, n=-1)),
         labelPosition=(ymax + ymin) / 2)

pdata2 <- bind_rows(plot_data_uhr_2 %>%  slice(n()) %>%  mutate(order=10,mean_jultag=0),
                    plot_data_uhr_2,
                    plot_data_uhr_2 %>%  slice(n()) %>%  mutate(order=11,mean_jultag=365)) %>% 
  #calc percentages
  mutate(diff_mean_jultag=lead(mean_jultag)-mean_jultag) %>% 
  drop_na(diff_mean_jultag) %>% 
  mutate(fraction= diff_mean_jultag,
         ymax=cumsum(fraction),
         ymin=c(0, head(ymax, n=-1)),
         labelPosition=(ymax + ymin)/2)

caldata <- tibble(daymonth=days_in_month(1:12),monthabb=month.abb) %>% 
  mutate(ymax=cumsum(daymonth),ymin=c(0, head(ymax, n=-1))) %>% 
  mutate(order=11) %>% 
  mutate(labelPosition=(ymax + ymin) / 2)

# make outerlabels
# Winterlabel together
  
p <- pdata %>% 
  ggplot() + aes(ymax=ymax, ymin=ymin, xmax=8, xmin=4.05, fill=factor(order)) +
  geom_rect(show.legend = FALSE) +
  geom_text(x=(8+4.05)/2, aes(y=labelPosition, label=round(diff_mean_jultag)), size=3.5) +
  geom_rect(data=pdata2,aes(ymax=ymax, ymin=ymin, xmax=4, xmin=0, fill=factor(order)),show.legend = FALSE) +
  geom_text(data=pdata2,x=(4+0)/2, aes(y=labelPosition, label=round(diff_mean_jultag)), size=3.5) +
  scale_fill_manual(values=c("10"="blue","1"="darkgreen","2"="green","3"="lightgreen",
                             "4"="red","5"="orange","6"="magenta","7"="yellow","8"="gold",
                             "9"="lightyellow","11"="white")) +
  geom_rect(data=caldata,aes(ymax=ymax, ymin=ymin, xmax=-4, xmin=0,fill=factor(order)),colour="black",show.legend = FALSE) +
  geom_text(data=caldata, x=-0.9, aes(y=labelPosition, label=monthabb), size=3.5) +
  coord_polar(theta="y",start=0) + 
  xlim(c(-5, 8)) +
  theme_void()

ggsave("plots/pietest2.png",device = "png")

