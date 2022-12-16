## ---- Libraries ----
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(lubridate)
library(dplyr)
library(RSQLite)
library(tidyr)
library(ggmap)
library(sf)
library(viridis)
library(patchwork)
library(RColorBrewer)



### ---- Data Load in ---- 
# Vorher per "Hand" / HeidiSQL Export / QGIS + Naturraum wfs  zum Geopackage gemacht
#aus alten Daten hergestellt  
db <- "Data/Phenologie_Naturraum_RLP_072022.gpkg"
st_layers(db)
naturraum_rlp_geom <- read_sf(db,layer="Naturraum3")
naturraum4_rlp_geom <- read_sf(db,layer="Naturraum4")
plants_phase <- read_sf(db,layer="plants_phase")
stations_rlp <- read_sf(db,layer="stations_rlp")
reports_rlp <- read_sf(db, layer="reports_rlp")


### ---- Defining Constants and Stuff ----
mainpath="G:/Skripte/Pheno"
plotpath=paste0(mainpath,"/Plots/")

#ids der Pflanzen von Interesse
ids = c(101,104,112,113,128,135,203)
jultagalsmonat <- c(31,59,90,120,151,181,212,243,273,304,334,365)
jultagbreaks <- c(0,jultagalsmonat)

PairedPalette = colorRampPalette(brewer.pal(12, "Paired"))

habfrage <- c("hoehe < 200"
              ,"hoehe < 300"
              ,"hoehe >= 300"
              ,"hoehe >= 200 & hoehe < 400"
              ,"hoehe >= 400 & hoehe < 600"
              ,"hoehe >= 600 ")

hklassen <- 1:6 

hlabels <- c("Höhe < 200m"
             ,"Höhe < 300m"
             ,"Höhe >= 300m"
             ,"200m <= Höhe < 400m"
             ,"400m <= Höhe < 600m"
             ,"Höhe >= 600m ")
hfacetlabel <- hlabels ; names(hfacetlabel) <- hklassen

Statlabeltitle <- c("Mittlerer","Frühester","Spätester","Medianer","Mittlere Melderanzahl")
names(Statlabeltitle) <- c("mean_jultag","min_jultag","max_jultag","median_jultag","mean_Melderanzahl")


### ---- Data Cleanup ---- 
#Fix Encoding von Text weil SCHEISS ENCODING SCHEISS WEIL WINDOWS UND SO
#Fix versteh ich nicht macht keinen Sinn funktioniert aber... durch rumprobieren gefixt... 
#aber nur plants_phase ist betroffen... vllt beim einlesen in die SQL Datenbank vergessen die Tabelle auf utf8 zu encoden?
plants_phase <- plants_phase %>% mutate(obj=iconv(obj ,from="UTF8" , to="UTF8"),phase_def=iconv(phase_def ,from="UTF8" , to="UTF8"),BBCH_code=iconv(BBCH_code ,from="UTF8" , to="UTF8"))

plant_names <- plants_phase %>% select(obj_id,obj) %>%  summarise(plant_id=unique(obj_id),plant_name=unique(obj)) %>%  filter(plant_id %in% ids)  %>%  arrange(plant_id) %>%  mutate(plant_name=trimws(plant_name,"r"))
plant_names_labeller <- plant_names$plant_name ; names(plant_names_labeller) <- plant_names$plant_id

#alter weg von sqlite oder csv geometry erzeugen und transformieren 
#stations_rlp <- st_as_sf(stations_rlp2,coords = c("geol","geob")) %>% st_set_crs(.,st_crs(4326)) %>% st_transform(.,st_crs(25832))

rlpgrenze <- naturraum_rlp_geom %>% select(geom)%>% st_make_valid() %>% st_union()


### ----Data Wrangling and Plotting ----
## DF Bluehphase für ausgewählte Pflanzen
### Haupttabelle 
df <- reports_rlp %>% 
  select(id,refJahr,plant_id,phase_id,jultag) %>% filter(phase_id==5,plant_id %in% ids) %>% 
  left_join(stations_rlp %>% select(id,naturraumgruppeCode,hoehe,geom),by=c("id" = "id")) %>% 
  left_join(plant_names,by=c("plant_id"="plant_id"))

###
#Hoehenklassen 
#eigentlich mit reclassify oder cut zu machen aber weil überschneidene Bereiche sind von Hand
for (hklassen in 1:6) {
  
  tempdf <- df %>%
    filter(eval(parse(text=habfrage[hklassen]))) %>% mutate(Hoehenstufenklasse=hklassen)
  
  assign(paste0("tempdf",hklassen),tempdf)
}
dfhoehe <- rbind(tempdf1,tempdf2,tempdf3,tempdf4,tempdf5,tempdf6)
rm(tempdf1,tempdf2,tempdf3,tempdf4,tempdf5,tempdf6)



### Zeitreihe Bluehpase 

df_Zeitreihe_Bluehphase <- df %>%
  group_by(plant_id,refJahr) %>% summarise(mean_jultag=mean(jultag),min_jultag=min(jultag),max_jultag=max(jultag),median_jultag=median(jultag),mean_Melderanzahl=mean(n_distinct(id)))

df_Zeitreihe_Bluehphase_cleaned <- df_Zeitreihe_Bluehphase %>% filter(plant_id==203) %>% mutate(refJahr = as.Date(as.character(refJahr),format="%Y")) %>%
  complete(refJahr = seq.Date(min(refJahr), max(refJahr), by="year")) %>% full_join(df_Zeitreihe_Bluehphase %>% mutate(refJahr = as.Date(as.character(refJahr),format="%Y"))) %>% left_join(plant_names,by=c("plant_id"="plant_id"))

for (var in c("mean_jultag","median_jultag","min_jultag","max_jultag","mean_Melderanzahl")) {
  png(filename=paste0(plotpath,"Bluehphase_Zeitreihe_",var,".png"),type="cairo-png",width=2000,height=1000)
  
  print(ggplot(data=df_Zeitreihe_Bluehphase_cleaned, aes(x=refJahr,y=.data[[var]])) +  
          geom_line(aes(color=plant_name),lwd=1.2,na.rm = TRUE) +
          geom_point() + 
          theme_grey(base_size = 22) +
          theme(   legend.key.size=unit(2, 'cm'),legend.background=element_blank(),
                   legend.key.height=unit(1.2, 'cm')) + ylim(-50,275)+
          labs(x="Jahr",color="Pflanze")
  )#print
  dev.off()
}

#test in ein plot 

for (var in c("mean_jultag","median_jultag","min_jultag","max_jultag","mean_Melderanzahl")) {
  
  
  assign(paste0("tempvar_",var), ggplot(data=df_Zeitreihe_Bluehphase_cleaned, aes(x=refJahr,y=.data[[var]])) +  
           geom_line(aes(color=plant_name),lwd=1.2,na.rm = TRUE) +
           geom_point() + 
           theme_grey(base_size = 32) +
           theme(   legend.key.size=unit(2, 'cm'),legend.background=element_blank(),
                    legend.key.height=unit(1.2, 'cm')) + ylim(-50,275)+
           labs(x="Jahr",color="Pflanze"))
}

png(filename=paste0(plotpath,"Bluehphase_Zeitreihe_Stats_zusammen.png"),type="cairo-png",width=2000,height=2000)
print(tempvar_mean_jultag / tempvar_median_jultag / tempvar_max_jultag / tempvar_min_jultag/ tempvar_mean_Melderanzahl )
dev.off()
rm(tempvar_mean_jultag,tempvar_median_jultag,tempvar_min_jultag,tempvar_max_jultag,tempvar_mean_Melderanzahl)



### Höhenstufen Plots 

dfhoehe_group <- dfhoehe %>%
  group_by(plant_id,refJahr,Hoehenstufenklasse) %>% summarise(mean_jultag=mean(jultag),max_jultag=max(jultag),min_jultag=min(jultag),median_jultag=median(jultag),mean_Melderanzahl=mean(n_distinct(id))) %>% 
  left_join(plant_names,by=c("plant_id"="plant_id"))


facetlabs <- hlabels
names(facetlabs) <- as.character(1:6)
#%>% filter(refJahr > 1990)

for (var in c("mean_jultag","median_jultag","min_jultag","max_jultag","mean_Melderanzahl")) {
  png(filename=paste0(plotpath,"Hoehenstufen_Bluehphase_Zeitreihe_",var,".png"),type="cairo-png",width=3000,height=1000)
  print(
    ggplot(data=dfhoehe_group , aes(x=refJahr,y=.data[[var]],fill=factor(Hoehenstufenklasse))) +  
      geom_line(aes(color=as.factor(plant_name)),lwd=1.2,na.rm = TRUE) +
      geom_point() +
      ylim(if(var=="mean_Melderanzahl"){c(0,150)}else{c(-50,275)}) +
      theme_grey(base_size = 32) +
      theme(  legend.key = element_rect(fill = NA)  ,legend.background=element_blank(),
              legend.key.height=unit(1.2, 'cm'),legend.key.size=unit(2, 'cm')) +
      #labs(caption = habfrage[5]) +
      facet_wrap(~ Hoehenstufenklasse,ncol=5,labeller = labeller(Hoehenstufenklasse=facetlabs))+
      guides(fill="none") + 
      labs(x="Jahr",color="Pflanze")
    #+ scale_fill_manual(values=1:6,labels = habfrage) +
    #  guides(fill = guide_legend(override.aes = list(shape = as.character(1:5),size=7)))
    
  )#print
  dev.off()
}

### Hoehenstufe Beifuss und Hasel  in einem 
for (var in c("mean_Melderanzahl","mean_jultag")) {
  png(filename=paste0(plotpath,"Hoehenstufe_Zeitreihe_Beifuss_Hasel_Bluehphase_",var,".png"),type="cairo-png",width=2000,height=1000)
  
  print(
    ggplot(data=dfhoehe_group %>% filter(plant_name %in% c("Beifuß","Hasel")), aes(x=refJahr,y=.data[[var]],group=Hoehenstufenklasse)) +  
      geom_line(data=dfhoehe_group %>%filter(plant_name %in% c("Beifuß")),aes(col=factor(Hoehenstufenklasse),linetype=factor(plant_name))) +
      geom_line(data=dfhoehe_group %>%filter(plant_name %in% c("Hasel")),aes(col=factor(Hoehenstufenklasse),linetype=factor(plant_name))) +
      geom_point(data=dfhoehe_group %>%filter(plant_name %in% c("Beifuß")),aes(col=factor(Hoehenstufenklasse))) +
      geom_point(data=dfhoehe_group %>%filter(plant_name %in% c("Hasel")),aes(col=factor(Hoehenstufenklasse)))+
      ylim(if(var=="mean_Melderanzahl"){c(0,150)}else{c(-50,275)}) +
      
      theme_grey(base_size = 22) +
      theme(   legend.key.size=unit(2, 'cm'),legend.background=element_blank(),
               legend.key.height=unit(1.3, 'cm')) +
      labs(x="Jahr",col='Hoehenstufenklasse',linetype="Pflanze") +
      scale_color_discrete(labels=hlabels)
    
    #+  labs(caption = Hoehenstufeninterval[klasse]) 
    
  )#print
  dev.off()
}




### Naturraumgruppen und Höhenstufen

dfhoehenr_group <- dfhoehe %>% 
  group_by(plant_id,refJahr,Hoehenstufenklasse,naturraumgruppeCode) %>%
  summarise(mean_jultag=mean(jultag),max_jultag=max(jultag),min_jultag=min(jultag),median_jultag=median(jultag),mean_Melderanzahl=mean(n_distinct(id))) %>% 
  left_join(stations_rlp %>% select(naturraumgruppe,naturraumgruppeCode)) %>%  left_join(plant_names)

#for (var in c("mean_jultag","median_jultag","min_jultag","max_jultag","mean_Melderanzahl")) {
for (var in c("mean_Melderanzahl")) {
  #%>% filter(refJahr > 1990)
  for (hoehenstufe in 1:5) {
    #var="mean_jultag"
    png(filename=paste0(plotpath,"/Hoehenstufe_",hoehenstufe,"_Naturraumgruppen_Bluehphase_Zeitreihe_",var,".png"),type="cairo-png",width=4000,height=2000)
    print(
      ggplot(data=dfhoehenr_group %>% filter(Hoehenstufenklasse==hoehenstufe), aes(x=refJahr,y=.data[[var]],fill=factor(Hoehenstufenklasse))) +  
        geom_line(aes(color=as.factor(plant_name)),lwd=1.2,na.rm = TRUE) +
        geom_point() +
        ylim(if(var=="mean_Melderanzahl"){c(0,50)}else{c(-50,275)}) +
        theme_grey(base_size = 32) +
        theme(  legend.key = element_rect(fill = NA)  ,legend.background=element_blank(),
                legend.key.height=unit(1.2, 'cm'),legend.key.size=unit(2, 'cm')) +
        #labs(caption = habfrage[5]) +
        facet_wrap(vars(naturraumgruppe) ,ncol=5)+
        #guides(fill="none")+
        labs(x="Jahr",color="Pflanze",fill="Höhenstufe") +
        scale_fill_manual(values=hoehenstufe ,labels = hlabels[hoehenstufe]) +
        guides(fill = guide_legend(override.aes = list(size=1,col="white")))
      #labeller = labeller(naturraumgruppeCode=XXX)
    )#print
    dev.off()
  }#for hoehenstufe
}#for var





### ---- Karten Plotten ----
### ---- Übersichtskarte  Naturraum Karte mit Melder und Höhe ----
png(filename=paste0(plotpath,"Karte_Naturraeume_Melder_Hoehe_RLP.png"),type="cairo-png",width=4000,height=2000)
print(
  ggplot() + 
    #coord_sf(crs=sf::st_crs(25832)) +  
    geom_sf(data=naturraum_rlp_geom,aes(fill=natnr)) +
    scale_fill_discrete(labels=naturraum_rlp_geom$name)+
    geom_sf(data=stations_rlp,aes(size=hoehe),alpha=0.5)+
    theme_grey(base_size = 50) +
    theme(legend.key.size=unit(2, 'cm'),legend.background=element_blank(),
          legend.key.height=unit(1.3, 'cm')) +
    labs(fill="Naturraum",size = "Höhe in m")
)#print
dev.off()


### ---- Karten Nach Höhenstufen ----
### pro Höhenstufe Karte Mittlere Jultag für jede Pflanze und Naturraum mit Melder als Label / png 


dfhoehenr_group_mean <- dfhoehe %>% filter(refJahr >= 1991 & refJahr <= 2020 ) %>% group_by(plant_id,Hoehenstufenklasse,naturraumgruppeCode) %>%
  summarise(mean_jultag=mean(jultag),max_jultag=max(jultag),min_jultag=min(jultag),median_jultag=median(jultag),mean_Melderanzahl=mean(n_distinct(id))) %>% 
  left_join(stations_rlp %>%  as.data.frame() %>%  select(naturraumgruppe,naturraumgruppeCode) %>%  distinct(),by="naturraumgruppeCode")  %>%  left_join(plant_names)

dfhoehenr_group_mean_geometry <- naturraum_rlp_geom %>% select("natnr",geom) %>% mutate_at("natnr", as.numeric)
for (var in c("mean_jultag","median_jultag","min_jultag","max_jultag")) {
  #for (var in c("mean_jultag")) { 
  for (hoehenstufe in 1:5) {
    dftempplot <-   dfhoehenr_group_mean_geometry %>% 
      full_join(dfhoehenr_group_mean %>% filter(Hoehenstufenklasse==hoehenstufe),by=c("natnr"="naturraumgruppeCode"))  %>%
      na.omit() # %>% mutate(mean_jultag_bin=cut(mean_jultag, breaks=seq(0,240,30)))
    
    png(filename=paste0(plotpath,"Karte_Bluehbeginn_Naturraum_",var,"_Hoehenstufe_",hoehenstufe,".png"),type="cairo-png",width=3000,height=1800)
    print(
      ggplot(dftempplot) + 
        geom_sf(data=naturraum_rlp_geom , fill = "grey", color = "black")+
        geom_sf(data=dftempplot,aes(fill=.data[[var]]),size=1,color="black")+
        #scale_fill_manual(palette = "viridis") +
        geom_sf(data=rlpgrenze, fill="NA" ,color = "black",size=1.2) +
        geom_sf_text(aes(label=mean_Melderanzahl),size=12,col="grey80",fontface="bold",show.legend = TRUE) +
        scale_fill_stepsn(n.breaks = 29,colours = PairedPalette(29),limits=c(-30,260))+
        #scale_fill_stepsn(n.breaks = 15, colours = viridis(15))+
        #scale_fill_stepsn(n.breaks = length(breaks), colours = viridis(length(breaks)),limits=range(breaks))+
        #scale_fill_binned() +
        #scale_fill_fermenter(palette = "viridis",na.value="grey") +
        theme_bw(base_size = 40) +
        theme(legend.key.size=unit(2, 'cm'),legend.background=element_blank(),
              legend.key.height=unit(3.3, 'cm')) +
        labs(fill="Blühbeginn\nMittlere Julianischer Tag",size = "Höhe in m",x="",y="",
             title=paste0("Mittlerer Blühbeginn in ",hlabels[hoehenstufe]," nach Pflanzenarten und Naturräumen\n","und mittleren Melderanzahl pro Naturraum"),
        )+
        #geom_sf_label(aes(label=Melderanzahl),size=12,col="black",fontface="bold") +
        guides(fill = guide_colourbar(barheight = 90))+
        #scale_colour_manual(values=NA) +              
        #   guides(colour=guide_legend("Keine Melder", override.aes=list(colour="grey")))
        facet_wrap(vars(plant_id),labeller = labeller(plant_id=plant_names_labeller),nrow=2)
      
    )#print
    dev.off()
  } #for hoehenstufe
} #for var 



#### Karte Nach Naturräumen , Höhenstufen und Stationen

##Stationen
dfhoehenr_group_mean_station <- dfhoehe %>% filter(refJahr >= 1991 & refJahr <= 2020 ) %>% group_by(id,plant_id,Hoehenstufenklasse,naturraumgruppeCode) %>%
  summarise(mean_jultag=mean(jultag),max_jultag=max(jultag),min_jultag=min(jultag),median_jultag=median(jultag),mean_Melderanzahl=mean(n_distinct(id))) %>% 
  left_join(stations_rlp %>%  as.data.frame() %>%  select(naturraumgruppe,naturraumgruppeCode) %>%  distinct())  %>%  left_join(plant_names)

dfhoehenr_group_mean_geometry_station <- naturraum_rlp_geom %>% select("natnr",geom) %>% mutate_at("natnr", as.numeric)

#Flaeche 
dfhoehenr_group_mean_flaeche <- dfhoehe %>% group_by(plant_id,Hoehenstufenklasse,naturraumgruppeCode) %>%
  summarise(mean_jultag=mean(jultag),max_jultag=max(jultag),min_jultag=min(jultag),median_jultag=median(jultag),mean_Melderanzahl=mean(n_distinct(id))) %>% 
  left_join(stations_rlp %>%  as.data.frame() %>%  select(naturraumgruppe,naturraumgruppeCode) %>%  distinct(),by="naturraumgruppeCode")  %>%  left_join(plant_names)

dfhoehenr_group_mean_flaeche_geometry <- naturraum_rlp_geom %>% select("natnr",geom) %>% mutate_at("natnr", as.numeric)

#for (var in c("mean_jultag","median_jultag","min_jultag","max_jultag")) {
for (var in c("mean_jultag")) { 
  for (hoehenstufe in 1:5) {
    
    dftempplot <-  dfhoehenr_group_mean_geometry_station %>% 
      full_join(dfhoehenr_group_mean_station %>% filter(Hoehenstufenklasse==hoehenstufe),by=c("natnr"="naturraumgruppeCode"))  %>%
      na.omit() %>% left_join(st_transform(stations_rlp,st_crs(25832)) %>% as.data.frame() %>%  select(id,geom_station=geom),by="id")
    
    test <- dftempplot %>% group_by(natnr,plant_id) %>% summarise(MelderanzahlNatnr=sum(mean_Melderanzahl)) %>%  as.data.frame() %>%  select(natnr,MelderanzahlNatnr,plant_id)
    dftempplot <- dftempplot %>% left_join(test,by=c("natnr"="natnr","plant_id"="plant_id")) %>% left_join(stations_rlp %>%  as.data.frame() %>%  select(id,hoehe))
    
    ##Flaeche 
    dffuerflaeche <-  dfhoehenr_group_mean_flaeche_geometry %>% 
      full_join(dfhoehenr_group_mean_flaeche %>% filter(Hoehenstufenklasse==hoehenstufe),by=c("natnr"="naturraumgruppeCode"))  %>%
      na.omit() %>% left_join(test,by=c("natnr"="natnr","plant_id"="plant_id"))
    
    png(filename=paste0(plotpath,"Karte_Bluehbeginn_Naturraum_Stationen_",var,"_Hoehenstufe_",hoehenstufe,".png"),type="cairo-png",width=3000,height=1800)
    print(
      ggplot(dftempplot) + 
        geom_sf(data=naturraum_rlp_geom , fill = "grey", color = "black")+
        geom_sf(data=rlpgrenze, fill="NA" ,color = "black",size=1.2) +
        geom_sf(data=dffuerflaeche,aes(fill=.data[[var]]),size=1,color="black")+
        #scale_fill_manual(palette = "viridis") +
        geom_sf(data=dftempplot,aes(geometry=geom_station),size=5)+
        geom_sf_text(data=dffuerflaeche,aes(label=MelderanzahlNatnr),size=12,col="grey80",fontface="bold",show.legend = TRUE) +
        scale_fill_stepsn(n.breaks = 29,colours = PairedPalette(29),limits=c(-30,260))+
        theme_bw(base_size = 40) +
        theme(legend.key.size=unit(2, 'cm'),legend.background=element_blank(),
              legend.key.height=unit(3.3, 'cm')) +
        labs(fill="Blühbeginn\nMittlere Julianischer Tag",size = "Höhe in m",x="",y="",
             title=paste0("Mittlerer Blühbeginn in ",hlabels[hoehenstufe]," nach Pflanzenarten und Naturräumen\n","und mittleren Melderanzahl pro Naturraum"),
        )+
        guides(fill = guide_colourbar(barheight = 90),
               size = "none")+
        facet_wrap(vars(plant_id),labeller = labeller(plant_id=plant_names_labeller),nrow=2)
      
    )#print
    dev.off()
  } #for hoehenstufe
} #for var 


### ---- Karte Mittlerer Bluehbeginn nur nach Naturräumen ----


dfnr_group_mean <- dfhoehe %>% filter(refJahr >= 1991 & refJahr <= 2020 ) %>% group_by(plant_id,naturraumgruppeCode) %>%
  summarise(mean_jultag=mean(jultag),max_jultag=max(jultag),min_jultag=min(jultag),median_jultag=median(jultag),mean_Melderanzahl=mean(n_distinct(id))) %>% 
  left_join(stations_rlp %>%  as.data.frame() %>%  select(naturraumgruppe,naturraumgruppeCode) %>%  distinct(),by="naturraumgruppeCode")  %>%  left_join(plant_names) %>% 
  mutate(plant_id=factor(plant_id,levels = c("113","128","104","112","135","203","101")))
#for (var in c("mean_jultag","min_jultag","max_jultag","median_jultag")) {
for (var in c("mean_jultag")) { 
  dftempplot <-   naturraum_rlp_geom %>% select("natnr",geom) %>% mutate_at("natnr", as.numeric) %>% 
    full_join(dfnr_group_mean,by=c("natnr"="naturraumgruppeCode"))  %>%
    na.omit() # %>% mutate(mean_jultag_bin=cut(mean_jultag, breaks=seq(0,240,30)))
  # dftempplot$plant_id  <- factor(dftempplot$plant_id,levels = c("113","128","104","112","135","203","101"))
  
  png(filename=paste0(plotpath,"Karte_Bluehbeginn_Naturraum_",var,".png"),type="cairo-png",width=3000,height=1800)
  print(
    ggplot(dftempplot) + 
      #coord_sf(crs=sf::st_crs(25832)) +  
      #geom_sf(data=dfhoehenr_group_geometry,size=0.8,col="black") +
      #scale_fill_discrete(labels=naturraum_rlp_geom$name)+
      #geom_sf(data=naturraum_rlp_geom,aes(col=natnr)) +
      scale_fill_stepsn(n.breaks = 29,colours = PairedPalette(29),limits=c(-30,260))+
      geom_sf(data=naturraum_rlp_geom , fill = "grey", color = "black")+
      geom_sf(data=dftempplot,aes(fill=.data[[var]]),size=1,color="black")+
      #scale_fill_manual(palette = "viridis") +
      geom_sf(data=rlpgrenze, fill="NA" ,color = "black",size=1.2) +
      geom_sf_text(aes(label=mean_Melderanzahl),size=12,col="grey80",fontface="bold",show.legend = TRUE) +
      #scale_fill_stepsn(n.breaks = 15, colours = viridis(15))+
      #scale_fill_stepsn(n.breaks = length(breaks), colours = viridis(length(breaks)),limits=range(breaks))+
      #scale_fill_binned() +
      #scale_fill_fermenter(palette = "viridis",na.value="grey") +
      theme_bw(base_size = 40) +
      theme(legend.key.size=unit(2, 'cm'),legend.background=element_blank(),
            legend.key.height=unit(3.3, 'cm')) +
      labs(fill=paste0("Blühbeginn\n",Statlabeltitle[[var]]," Julianischer Tag"),size = "Höhe in m",x="",y="",
           title=paste0(Statlabeltitle[[var]]," Blühbeginn nach Pflanzenarten und Naturräumen von 1991-2020\n ","und Summe der Melder pro Naturraum"),
      )+
      #geom_sf_label(aes(label=Melderanzahl),size=12,col="black",fontface="bold") +
      guides(fill = guide_colourbar(barheight = 90))+
      #scale_colour_manual(values=NA) +              
      #   guides(colour=guide_legend("Keine Melder", override.aes=list(colour="grey")))
      facet_wrap(vars(plant_id),labeller = labeller(plant_id=plant_names_labeller),nrow=2)
    #facet_wrap(factor(plant_id, levels = c("113","128","104","112","135","203","101")),labeller = labeller(plant_id=plant_names_labeller),nrow=2)
    
  )#print
  dev.off()
} #for var 





### ---- Karte Nach Naturräumen und Stationen ----

##Stationen
dfhoehenr_group_mean_station <- df %>% filter(refJahr >= 1991 & refJahr <= 2020 ) %>% group_by(id,plant_id,naturraumgruppeCode) %>%
  summarise(mean_jultag=mean(jultag),max_jultag=max(jultag),min_jultag=min(jultag),median_jultag=median(jultag),mean_Melderanzahl=mean(n_distinct(id))) %>% 
  left_join(stations_rlp %>%  as.data.frame() %>%  select(naturraumgruppe,naturraumgruppeCode) %>%  distinct())  %>%  left_join(plant_names) 

dfhoehenr_group_mean_geometry_station <- naturraum_rlp_geom %>% select("natnr",geom) %>% mutate_at("natnr", as.numeric) 


#Flaeche 
dfhoehenr_group_mean_flaeche <- df %>% group_by(plant_id,naturraumgruppeCode) %>%
  summarise(mean_jultag=mean(jultag),max_jultag=max(jultag),min_jultag=min(jultag),median_jultag=median(jultag),mean_Melderanzahl=mean(n_distinct(id))) %>% 
  left_join(stations_rlp %>%  as.data.frame() %>%  select(naturraumgruppe,naturraumgruppeCode) %>%  distinct(),by="naturraumgruppeCode")  %>%  left_join(plant_names)

dfhoehenr_group_mean_flaeche_geometry <- naturraum_rlp_geom %>% select("natnr",geom) %>% mutate_at("natnr", as.numeric)

for (var in c("mean_jultag","median_jultag","min_jultag","max_jultag")) {
  #for (var in c("mean_jultag")) { 
  dftempplot <-  dfhoehenr_group_mean_geometry_station %>% 
    full_join(dfhoehenr_group_mean_station ,by=c("natnr"="naturraumgruppeCode"))  %>%
    na.omit() %>% left_join(st_transform(stations_rlp,st_crs(25832)) %>% as.data.frame() %>%  select(id,geom_station=geom),by="id")
  
  test <- dftempplot %>% group_by(natnr,plant_id) %>% summarise(MelderanzahlNatnr=sum(mean_Melderanzahl)) %>%  as.data.frame() %>%  select(natnr,MelderanzahlNatnr,plant_id)
  
  dftempplot <- dftempplot %>% left_join(test,by=c("natnr"="natnr","plant_id"="plant_id")) %>% left_join(stations_rlp %>%  as.data.frame() %>%  select(id,hoehe)) %>% 
    mutate(plant_id=factor(plant_id,levels = c("113","128","104","112","135","203","101")))
  
  ##Flaeche 
  dffuerflaeche <-  dfhoehenr_group_mean_flaeche_geometry %>% 
    full_join(dfhoehenr_group_mean_flaeche,by=c("natnr"="naturraumgruppeCode"))  %>%
    na.omit() %>% left_join(test,by=c("natnr"="natnr","plant_id"="plant_id")) %>% 
    mutate(plant_id=factor(plant_id,levels = c("113","128","104","112","135","203","101")))
  
  
  png(filename=paste0(plotpath,"Karte_Bluehbeginn_Naturraum_Stationen_",var,".png"),type="cairo-png",width=3000,height=1800)
  print(
    ggplot(dftempplot) + 
      geom_sf(data=naturraum_rlp_geom , fill = "grey", color = "black")+
      geom_sf(data=rlpgrenze, fill="NA" ,color = "black",size=1.2) +
      geom_sf(data=dffuerflaeche,aes(fill=.data[[var]]),size=1,color="black")+
      #scale_fill_manual(palette = "viridis") +
      geom_sf(data=dftempplot,aes(geometry=geom_station),size=3)+
      geom_sf_text(data=dffuerflaeche,aes(label=MelderanzahlNatnr),size=12,col="grey80",fontface="bold",show.legend = TRUE) +
      scale_fill_stepsn(n.breaks = 29,colours = PairedPalette(29),limits=c(-30,260))+
      theme_bw(base_size = 40) +
      theme(legend.key.size=unit(2, 'cm'),legend.background=element_blank(),
            legend.key.height=unit(3.3, 'cm')) +
      labs(fill=paste0("Blühbeginn\n",Statlabeltitle[[var]]," Julianischer Tag"),size = "Höhe in m",x="",y="",
           title=paste0(Statlabeltitle[[var]]," Blühbeginn nach Pflanzenarten und Naturräumen\n","und Summe Melderanzahl pro Naturraum"),
      )+
      guides(fill = guide_colourbar(barheight = 90),
             size = "none")+
      facet_wrap(vars(plant_id),labeller = labeller(plant_id=plant_names_labeller),nrow=2)
    
  )#print
  dev.off()
} #for var 

