## Plots für Phenobericht 
source("load_rphaeno.R")

##doppelt-phänologische Uhr der Wildpflanzen für RLP
#letzte 30 Jahre (1994-2023) gegenüber 1951-1980 und 1961-1990

#create wildpflanzen metadata 
uhr_meta_wildpflanzen <- tibble(pflanze_id=c(116,102,132,129,130,103,129,112,123,132),
                                pflanze=c("Huflattich","Busch-Windröschen","Stiel-Eiche","Schwarzer Holunder","Sommer-Linde","Eberesche","Schwarzer Holunder","Hänge-Birke","Rotbuche","Stiel-Eiche"),
                                phase_id=c(5,5,4,5,5,62,62,31,32,32),
                                jahreszeit=c("Vorfrühling","Erstfrühling","Vollfrühling","Frühsommer","Hochsommer","Spätsommer","Frühherbst","Vollherbst","Spätherbst","Winter")) %>%
  mutate(order=1:length(pflanze))


#Stielleiche vor 1991 DWD interpoliert von KfK leitphasen dokument 
mean_jultag_stieleiche_fix <- c(304,300,297,303,304,304,299,308,299,302,309,310,304,303,303,303,307,305,306,308,302,303,306,309,311,306,306,307,306,313,307,308,311,313,311,302,312,304,305,305)
refjahr_stieleiche_fix <- 1951:1990

stieleichefix <- as_tibble(cbind(refjahr=refjahr_stieleiche_fix,jultag=mean_jultag_stieleiche_fix)) %>%
  mutate(stations_id=99999,pflanze_id=132,phase_id=32,stationsname="DWDinterpoliert_RLP",bundesland="Rheinland-Pfalz",phase="herbstlicher Blattfall",pflanze="Stiel-Eiche")

#rotbuche fix  Rotbuche mittlerer jultag Blattfall minus Blattverfaerbung jährlich 1991-2018 
#Mittlwert 1991-2018 Der Differenz bilden 
#auf 1951-1990 auf Blattverfärbung addieren.

temp <- tbl(con,"Megaframe_Jahresmelder") %>%
  filter(bundesland %in% "Rheinland-Pfalz" & 
           pflanze_id == 123 &
           phase_id %in% c(32,31) ) %>% 
  collect()

rotbuche_diff_mean_1991_2018 <- temp %>% 
  group_by(refjahr,phase_id) %>% 
  summarise(mean_jultag=mean(jultag)) %>%
  filter(refjahr>=1991 & refjahr <= 2019) %>%
  ungroup() %>% 
  pivot_wider(values_from = "mean_jultag",names_from = "phase_id") %>% 
  mutate(diff=`32`- `31`) %>%
  summarise(mean_diff=mean(diff)) %>% 
  pull()

rotbuchefix <- temp %>% filter(phase_id==31,refjahr<=1990)  %>% 
  group_by(phase_id,refjahr) %>% 
  summarise(mean_jultag=mean(jultag)) %>%
  mutate(phase_id=32,jultag=(mean_jultag+rotbuche_diff_mean_1991_2018)) %>% 
  mutate(stations_id=99998,pflanze_id=123,phase_id=32,stationsname="DWDinterpoliert_RLP",bundesland="Rheinland-Pfalz",phase="herbstlicher Blattfall",pflanze="Rotbuche")


#load db and data for rlp
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "temp/test8.sqlite3")

dbListTables(con)

Daten_uhr <- tbl(con,"Megaframe_Jahresmelder") %>%
  filter(bundesland %in% "Rheinland-Pfalz" & 
           pflanze_id %in% !!uhr_meta_wildpflanzen$pflanze_id &
           phase_id %in% !!uhr_meta_wildpflanzen$phase_id ) %>% 
  collect() %>% 
  #fix für Stieleiche 
  full_join(stieleichefix) %>% 
  full_join(rotbuchefix)


#Prep Data 
innenperiode="1961-2021"
aussenperiode="1994-2023"


add_year_order <- function(df) {
  #df <- tibble(...) 
  #check length of order 
  len <- length(df$order)
  bind_rows(df %>%  slice(n()) %>%  mutate(order=len,mean_jultag=0),
            df,
            df %>%  slice(n()) %>%  mutate(order=len+1,mean_jultag=365)) 
  
}
make_ring_data <- function(df,uhr_meta=uhr_meta,zeitraum,ring) {
  df %>% left_join(uhr_meta ,by=c("pflanze_id","phase_id"))  %>% 
    filter(refjahr>=as.numeric(str_split(zeitraum,"-",simplify = TRUE)[1]) & 
             refjahr<=as.numeric(str_split(zeitraum,"-",simplify = TRUE)[2]) ) %>%
    group_by(order) %>%
    drop_na(order) %>% 
    summarise(mean_jultag=mean(jultag,na.rm=TRUE),count=n(),ring=ring,zeitraum=zeitraum) %>%
    ###hier noch einbauen wenn count zu niedrig alt planze nehmen
    right_join(uhr_meta,by="order") %>% 
    right_join(Daten_uhr %>% select(phase_id,phase) %>%  distinct(),by="phase_id")  %>% 
    add_year_order() %>% 
    mutate(diff_mean_jultag=lead(mean_jultag)-mean_jultag) %>% 
    drop_na(diff_mean_jultag) %>% 
    mutate(fraction= diff_mean_jultag,
           ymax=cumsum(fraction),
           ymin=c(0, head(ymax, n=-1)),
           labelPosition=(ymax + ymin) / 2) 
  
}

lubridate::month(1:12,
                 label = TRUE,
                 abbr = TRUE,
                 locale = "German")

caldata <- tibble(daymonth=days_in_month(1:12),
                  monthabb=month(1:12,label = TRUE,abbr = TRUE,locale = "German")) %>% 
  mutate(ymax=cumsum(daymonth),ymin=c(0, head(ymax, n=-1))) %>% 
  mutate(order=11) %>% 
  mutate(labelPosition=(ymax + ymin) / 2)

plot_data_uhr <- bind_rows(make_ring_data(Daten_uhr,uhr_meta_wildpflanzen,zeitraum="1951-1980",ring="innen"),
                           make_ring_data(Daten_uhr,uhr_meta_wildpflanzen,zeitraum="1994-2023",ring="aussen"))



## Labels erstellen/fixen
pdata <- plot_data_uhr %>% 
  # mutate(biglabel=as.Date("2021-01-01") + mean_jultag) %>% 
  #  mutate(biglabel=glue("{jahreszeit}<br>{pflanze}<br>({phase})<br>{day(biglabel)}.{month(biglabel)} / <span style='color:orange'>{day(biglabel)}.{month(biglabel)}</span>" )) %>% 
  mutate(biglabel=as.Date("2021-01-01") + mean_jultag) %>% 
  group_by(order) %>% 
  mutate(biglabeldatum = paste0(glue("{day(biglabel)}.{month(biglabel)}"), collapse = "/"))  %>% 
  ungroup() %>% 
  mutate(biglabeldatum=str_extract(biglabeldatum,"^([^/]*/){1}[^/]*")) %>% 
  separate(biglabeldatum,c("biglabaussen","biglabinnen"),"/") %>%
  mutate(biglabel=glue("{jahreszeit}<br>{pflanze}<br>({phase})<br>{biglabaussen} / <span style='color:orange'>{biglabinnen}</span>" )) %>% 
  select(-biglabinnen,-biglabaussen) %>% 
  group_by(order,ring) %>% 
  mutate(smalllabel=as.character(round(sum(diff_mean_jultag))),
         smalllabel=if_else(order==10,paste0(zeitraum,"\n",smalllabel," Tage"),smalllabel)) %>%  
  ungroup()

smalllabelwinter <- plot_data_uhr %>% group_by(ring) %>% filter(ymin==0 | ymax==365) %>% summarise(summe=sum(diff_mean_jultag))  
pdata <- pdata %>% 
  mutate(
    smalllabel=case_when(
      ymax==365 && ring=="aussen" ~ smalllabelwinter %>%
        filter(ring=="aussen") %>%
        select(summe) %>% pull %>% as.character() ,
      ymax==365 && ring=="innen" ~ smalllabelwinter %>%
        filter(ring=="innen") %>%
        select(summe) %>% pull %>% as.character(),
      ymin==0 ~ NA_character_,
      TRUE ~ smalllabel
    )) %>%  
  mutate(labelPosition=if_else(ymax==365,365,labelPosition)) %>% 
  mutate(biglabelPosition=labelPosition,
         biglabel=if_else(ymin==0,glue(""),biglabel)) 


#make biglabel from data from both rings
colorring <- c("10"="#2F6EBD","1"="#50612A","2"="#C4D69A","3"="#EAF2DD","4"="#F15A15","5"="#E64A3E","6"="#DC675E","7"="#F7F60E","8"="#E8F5A3","9"="#FBFEC5","11"="#FFFFFF")
# values=c("10"="blue","1"="darkgreen","2"="green","3"="lightgreen",
#          "4"="red","5"="orange","6"="magenta","7"="yellow","8"="gold",
#          "9"="lightyellow","11"="white")
p <- pdata %>% filter(ring=="aussen") %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=8, xmin=4.05, fill=factor(order) ) )   +
  geom_rect(show.legend = FALSE) +
  geom_text(x=(8+4.05)/2, aes(y=labelPosition, label=smalllabel), size=3) +
  #geom_text(x=12.5,xlim = c(NA,12),aes(y=biglabelPosition, label=biglabel),size=2.5,show.legend = FALSE)+
  geom_richtext(x=12.5,aes(y=biglabelPosition, label=biglabel),size=2.5,show.legend = FALSE, fill = NA, label.color = NA)+
  #geom_text_repel(show.legend = FALSE)+
  geom_rect(data=pdata %>% filter(ring=="innen"),aes(ymax=ymax, ymin=ymin, xmax=4, xmin=0, fill=factor(order)),show.legend = FALSE) +
  geom_text(data=pdata %>% filter(ring=="innen"),x=(4+0)/2, aes(y=labelPosition, label=smalllabel), size=3) +
  scale_fill_manual(values=colorring) +
  geom_rect(data=caldata,aes(ymax=ymax, ymin=ymin, xmax=-4, xmin=0,fill=factor(order)),colour="black",show.legend = FALSE) +
  geom_text(data=caldata, x=-0.9, aes(y=labelPosition, label=monthabb), size=3) +
  coord_polar(theta="y",start=0) + 
  xlim(c(-5, 12)) +
  labs(title = "Phaenologische Jahreszeiten für Rheinland-Pfalz", 
       subtitle = glue("äußerer Ring zeigt das Mittel 1994-2023<br><span style='color:orange'>innerer Ring zeigt das Mittel 1951-1980</span>"),
       caption = "Daten: DWD      Darstellung: LfU Rheinland-Pfalz") +
  theme_void() + 
  theme(plot.subtitle = element_markdown(hjust = 0.5),
        plot.title=element_markdown(hjust=0.5))

ggsave("plots/PhenoUhr_2Ringe_1951-1980_1994-2023_Wildpflanze.png",device = "png",width=2100,height=2100 ,units = "px")




#######################
################
#################
########## 1961-1990


plot_data_uhr <- bind_rows(make_ring_data(Daten_uhr,uhr_meta_wildpflanzen,zeitraum="1961-1990",ring="innen"),
                           make_ring_data(Daten_uhr,uhr_meta_wildpflanzen,zeitraum="1994-2023",ring="aussen"))



## Labels erstellen/fixen
pdata <- plot_data_uhr %>% 
  # mutate(biglabel=as.Date("2021-01-01") + mean_jultag) %>% 
  #  mutate(biglabel=glue("{jahreszeit}<br>{pflanze}<br>({phase})<br>{day(biglabel)}.{month(biglabel)} / <span style='color:orange'>{day(biglabel)}.{month(biglabel)}</span>" )) %>% 
  mutate(biglabel=as.Date("2021-01-01") + mean_jultag) %>% 
  group_by(order) %>% 
  mutate(biglabeldatum = paste0(glue("{day(biglabel)}.{month(biglabel)}"), collapse = "/"))  %>% 
  ungroup() %>% 
  mutate(biglabeldatum=str_extract(biglabeldatum,"^([^/]*/){1}[^/]*")) %>% 
  separate(biglabeldatum,c("biglabaussen","biglabinnen"),"/") %>%
  mutate(biglabel=glue("{jahreszeit}<br>{pflanze}<br>({phase})<br>{biglabaussen} / <span style='color:orange'>{biglabinnen}</span>" )) %>% 
  select(-biglabinnen,-biglabaussen) %>% 
  group_by(order,ring) %>% 
  mutate(smalllabel=as.character(round(sum(diff_mean_jultag))),
         smalllabel=if_else(order==10,paste0(zeitraum,"\n",smalllabel," Tage"),smalllabel)) %>%  
  ungroup()

smalllabelwinter <- plot_data_uhr %>% group_by(ring) %>% filter(ymin==0 | ymax==365) %>% summarise(summe=sum(diff_mean_jultag))  
pdata <- pdata %>% 
  mutate(
    smalllabel=case_when(
      ymax==365 && ring=="aussen" ~ smalllabelwinter %>%
        filter(ring=="aussen") %>%
        select(summe) %>% pull %>% as.character() ,
      ymax==365 && ring=="innen" ~ smalllabelwinter %>%
        filter(ring=="innen") %>%
        select(summe) %>% pull %>% as.character(),
      ymin==0 ~ NA_character_,
      TRUE ~ smalllabel
    )) %>%  
  mutate(labelPosition=if_else(ymax==365,365,labelPosition)) %>% 
  mutate(biglabelPosition=labelPosition,
         biglabel=if_else(ymin==0,glue(""),biglabel)) 


#make biglabel from data from both rings
colorring <- c("10"="#2F6EBD","1"="#50612A","2"="#C4D69A","3"="#EAF2DD","4"="#F15A15","5"="#E64A3E","6"="#DC675E","7"="#F7F60E","8"="#E8F5A3","9"="#FBFEC5","11"="#FFFFFF")
# values=c("10"="blue","1"="darkgreen","2"="green","3"="lightgreen",
#          "4"="red","5"="orange","6"="magenta","7"="yellow","8"="gold",
#          "9"="lightyellow","11"="white")
p <- pdata %>% filter(ring=="aussen") %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=8, xmin=4.05, fill=factor(order) ) )   +
  geom_rect(show.legend = FALSE) +
  geom_text(x=(8+4.05)/2, aes(y=labelPosition, label=smalllabel), size=3) +
  #geom_text(x=12.5,xlim = c(NA,12),aes(y=biglabelPosition, label=biglabel),size=2.5,show.legend = FALSE)+
  geom_richtext(x=12.5,aes(y=biglabelPosition, label=biglabel),size=2.5,show.legend = FALSE, fill = NA, label.color = NA)+
  #geom_text_repel(show.legend = FALSE)+
  geom_rect(data=pdata %>% filter(ring=="innen"),aes(ymax=ymax, ymin=ymin, xmax=4, xmin=0, fill=factor(order)),show.legend = FALSE) +
  geom_text(data=pdata %>% filter(ring=="innen"),x=(4+0)/2, aes(y=labelPosition, label=smalllabel), size=3) +
  scale_fill_manual(values=colorring) +
  geom_rect(data=caldata,aes(ymax=ymax, ymin=ymin, xmax=-4, xmin=0,fill=factor(order)),colour="black",show.legend = FALSE) +
  geom_text(data=caldata, x=-0.9, aes(y=labelPosition, label=monthabb), size=3) +
  coord_polar(theta="y",start=0) + 
  xlim(c(-5, 12)) +
  labs(title = "Phaenologische Jahreszeiten für Rheinland-Pfalz", 
       subtitle = glue("äußerer Ring zeigt das Mittel 1994-2023<br><span style='color:orange'>innerer Ring zeigt das Mittel 1961-1990</span>"),
       caption = "Daten: DWD      Darstellung: LfU Rheinland-Pfalz") +
  theme_void() + 
  theme(plot.subtitle = element_markdown(hjust = 0.5),
        plot.title=element_markdown(hjust=0.5))

ggsave("plots/PhenoUhr_2Ringe_1961-1990_1994-2023_Wildpflanze.png",device = "png",width=2100,height=2100 ,units = "px")









#Zeitreihen folgender Phasen für RLP

Phasendefinition <- tbl(con,"Phasendefinition")


#Apfelblüte

daten <- tbl(con,"Megaframe_Jahresmelder") %>%
  filter(bundesland %in% "Rheinland-Pfalz" & 
           pflanze_id %in% c(310,311,312,313) &
           phase_id == 5) %>% 
  collect()
daten2 <- daten %>%
  mutate(pflanze_id=310) %>%  
  group_by(pflanze_id,refjahr) %>%
  summarise(mean_jultag=mean(jultag),min_jultag=min(jultag),max_jultag=max(jultag),
            median_jultag=median(jultag),mean_Melderanzahl=mean(n_distinct(stations_id)),
            phase="Bluehbeginn",pflanze="Apfel") %>% 
  ungroup() %>% 
  mutate(refjahr = as.Date(as.character(refjahr),format="%Y")) %>%
  complete(refjahr = seq.Date(min(refjahr), max(refjahr), by="year"))

for (var in c("mean_jultag")) {
  png(filename=paste0("plots/Blüte_Beginn_3xApfel_Zeitreihe_",var,".png"),type="cairo-png",width=2000,height=1000)
  
  print(ggplot(data=daten2, aes(x=refjahr,y=.data[[var]])) +  
          geom_line(aes(color=pflanze),lwd=1.2,na.rm = TRUE) +
          geom_point() + 
          geom_smooth(method=lm,se = FALSE)+
          scale_y_continuous(limits = c(90, 150), breaks = seq(90, 150, 10)) +
                    theme_grey(base_size = 22) +
          theme(   legend.key.size=unit(2, 'cm'),legend.background=element_blank(),
                   legend.key.height=unit(1.2, 'cm'))+
          labs(x="Jahr",
               y="Mittlerer julianischer Tag",
               color="Pflanze",
               title="Mittlerer julianischer Tag des Blütenbeginns des Apfels in Rheinland-Pfalz ")
  )#print
  dev.off()
}

#Blüte Winterraps blüte beginn
daten <- prep_data_for_timeseries(205,5,"Rheinland-Pfalz")
for (var in c("mean_jultag")) {
  png(filename=paste0("plots/Blüte_Beginn_Winterraps_Zeitreihe_",var,".png"),type="cairo-png",width=2000,height=1000)
  
  print(ggplot(data=daten, aes(x=refjahr,y=.data[[var]])) +  
          geom_line(aes(color=pflanze),lwd=1.2,na.rm = TRUE) +
          geom_point() +
          geom_smooth(method=lm,se = FALSE)+
          scale_y_continuous(limits = c(90, 150), breaks = seq(90, 150, 10)) +
          theme_grey(base_size = 22) +
          theme(   legend.key.size=unit(2, 'cm'),legend.background=element_blank(),
                   legend.key.height=unit(1.2, 'cm')) +
          labs(x="Jahr",
               y="Mittlerer julianischer Tag",
               color="Pflanze",
               title="Mittlerer julianischer Tag des Blütenbeginns des Winterraps in Rheinland-Pfalz ")
  )#print
  dev.off()
}
# Blüte Ende Winterraps in RLP nicht vorhanden
#daten <- prep_data_for_timeseries(205,7,"Rheinland-Pfalz")
#for (var in c("mean_jultag")) {
#  png(filename=paste0("plots/Blüte_Ende_Winterraps_Zeitreihe_",var,".png"),type="cairo-png",width=2000,height=1000)
#  
#  print(ggplot(data=daten, aes(x=refjahr,y=.data[[var]])) +  
#          geom_line(aes(color=pflanze),lwd=1.2,na.rm = TRUE) +
#          geom_point() + 
#          theme_grey(base_size = 22) +
#          theme(   legend.key.size=unit(2, 'cm'),legend.background=element_blank(),
#                   legend.key.height=unit(1.2, 'cm')) + ylim(-50,275)+
#          labs(x="Jahr",
#               y="Mittlerer julianischer Tag",
#               color="Pflanze",
#               title="Mittlerer julianischer Tag des Blütenendes des Winterraps in Rheinland-Pfalz ")
#  )#print
#  dev.off()
#}

#Blattaustrieb Rotbuche
daten <- prep_data_for_timeseries(123,4,"Rheinland-Pfalz")
for (var in c("mean_jultag")) {
  png(filename=paste0("plots/Blattentfaltung_Rotbuche_Zeitreihe_",var,".png"),type="cairo-png",width=2000,height=1000)
  
  print(ggplot(data=daten, aes(x=refjahr,y=.data[[var]])) +  
          geom_line(aes(color=pflanze),lwd=1.2,na.rm = TRUE) +
          geom_point() + 
          geom_smooth(method=lm,se = FALSE)+
          scale_y_continuous(limits = c(90, 150), breaks = seq(90, 150, 10)) +
          theme_grey(base_size = 22) +
          theme(   legend.key.size=unit(2, 'cm'),legend.background=element_blank(),
                   legend.key.height=unit(1.2, 'cm'))+
          labs(x="Jahr",
               y="Mittlerer julianischer Tag",
               color="Pflanze",
               title="Mittlerer julianischer Tag der Blattentfaltung der Rotbuche in Rheinland-Pfalz ")
  )#print
  dev.off()
}


#Blattaustrieb Stieleiche
daten <- prep_data_for_timeseries(132,4,"Rheinland-Pfalz")
for (var in c("mean_jultag")) {
  png(filename=paste0("plots/Blattentfaltung_Stieleiche_Zeitreihe_",var,".png"),type="cairo-png",width=2000,height=1000)
  
  print(ggplot(data=daten, aes(x=refjahr,y=.data[[var]])) +  
          geom_line(aes(color=pflanze),lwd=1.2,na.rm = TRUE) +
          geom_point() + 
          geom_smooth(method=lm,se = FALSE)+
          scale_y_continuous(limits = c(90, 150), breaks = seq(90, 150, 10)) +
          theme_grey(base_size = 22) +
          theme(   legend.key.size=unit(2, 'cm'),legend.background=element_blank(),
                   legend.key.height=unit(1.2, 'cm')) +
          labs(x="Jahr",
               y="Mittlerer julianischer Tag",
               color="Pflanze",
               title="Mittlerer julianischer Tag der Blattentfaltung der Stieleiche in Rheinland-Pfalz ")
  )#print
  dev.off()
}


#Maigrün Fichte
daten <- prep_data_for_timeseries(107,8,"Rheinland-Pfalz")
for (var in c("mean_jultag")) {
  png(filename=paste0("plots/Maitrieb_Fichte_Zeitreihe_",var,".png"),type="cairo-png",width=2000,height=1000)
  
  print(ggplot(data=daten, aes(x=refjahr,y=.data[[var]])) +  
          geom_line(aes(color=pflanze),lwd=1.2,na.rm = TRUE) +
          geom_point() + 
          geom_smooth(method=lm,se = FALSE)+
          scale_y_continuous(limits = c(90, 150), breaks = seq(90, 150, 10)) +
          theme_grey(base_size = 22) +
          theme(   legend.key.size=unit(2, 'cm'),legend.background=element_blank(),
                   legend.key.height=unit(1.2, 'cm')) +
          labs(x="Jahr",
               y="Mittlerer julianischer Tag",
               color="Pflanze",
               title="Mittlerer julianischer Tag des Maitriebs der Fichte in Rheinland-Pfalz ")
  )#print
  dev.off()
}
#Maigrün Kiefer

daten <- tbl(con,"Megaframe_Jahresmelder") %>%
  filter(bundesland %in% "Rheinland-Pfalz" & 
           pflanze_id == 118 &
           phase_id == 8) %>% 
  collect()
daten2 <- daten %>% 
  group_by(pflanze_id,refjahr) %>%
  summarise(mean_jultag=mean(jultag),min_jultag=min(jultag),max_jultag=max(jultag),
            median_jultag=median(jultag),mean_Melderanzahl=mean(n_distinct(stations_id)),
            phase_id=phase_id,phase,pflanze) %>% 
  ungroup() %>% 
  mutate(refjahr = as.Date(as.character(refjahr),format="%Y")) %>%
  complete(refjahr = seq.Date(min(refjahr), max(refjahr), by="year"))

for (var in c("mean_jultag")) {
  png(filename=paste0("plots/Maitrieb_Kiefer_Zeitreihe_",var,".png"),type="cairo-png",width=2000,height=1000)
  
  print(ggplot(data=daten2, aes(x=refjahr,y=.data[[var]])) +  
          geom_line(aes(color=pflanze),lwd=1.2,na.rm = TRUE) +
          geom_point() +
          geom_smooth(method=lm,se = FALSE)+
          scale_y_continuous(limits = c(90, 150), breaks = seq(90, 150, 10)) +
          theme_grey(base_size = 22) +
          theme(   legend.key.size=unit(2, 'cm'),legend.background=element_blank(),
                   legend.key.height=unit(1.2, 'cm')) +
          labs(x="Jahr",
               y="Mittlerer julianischer Tag",
               color="Pflanze",
               title="Mittlerer julianischer Tag des Maitriebs der Kiefer in Rheinland-Pfalz ")
  )#print
  dev.off()
}


prep_data_for_timeseries <- function(pflanzeid,phaseid,land) {
  daten <- tbl(con,"Megaframe_Jahresmelder") %>%
    filter(bundesland %in% land & 
             pflanze_id == pflanzeid &
             phase_id == phaseid) %>% 
    collect()
  return(
    daten %>% 
    group_by(pflanze_id,refjahr) %>%
    summarise(mean_jultag=mean(jultag),min_jultag=min(jultag),max_jultag=max(jultag),
              median_jultag=median(jultag),mean_Melderanzahl=mean(n_distinct(stations_id)),
              phase_id=phase_id,phase,pflanze) %>% 
    ungroup() %>% 
    mutate(refjahr = as.Date(as.character(refjahr),format="%Y")) %>%
    complete(refjahr = seq.Date(min(refjahr), max(refjahr), by="year"))
    )

} 

