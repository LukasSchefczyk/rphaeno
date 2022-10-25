
# ***WIP - Work in Progress -***

# **Readme de**

Skripte zum runter- und einladen von (zunaechst erstmal) Deutscher Wetterdienst DWD Phaenologie Daten und Plotten dieser Daten


# **Readme en**

scripts to download and load phenology data from (for now) German Weather Service DWD and plot it.

## **Geodata**

### vg2500 Verwaltungsgrenzen [© GeoBasis-DE / BKG 2021](http://www.bkg.bund.de)

<https://gdz.bkg.bund.de/index.php/default/digitale-geodaten/verwaltungsgebiete/verwaltungsgebiete-1-2-500-000-stand-31-12-vg2500-12-31.html>

### Naturraum Grenzen
WFS Bundesamt für Naturschutz

<https://geodienste.bfn.de/ogc/wfs/gliederungen?SERVICE=WFS&REQUEST=GetCapabilities>

<https://geoportal.de/Info/325bfe9a-21f0-4fc4-9dae-a8feb4668a08>

### **Layers**

UTM32N EPSG:25832

naturraumgruppe -> Dissolve auf Gruppe_der_Haupteinheiten Layer , Namen und string to int Nummer (ORD2)

naturraum -> Haupteinheiten Layer , Namen angepasst und string to int Nummer (ORD3)  und *10 damit es vierstellig ist wie DWD Nummern...

landkreis -> mit Ländergrenzen (bundesland) vereinigt und auf wesentliche Attributspalten begrenzt

bundesland -> Ländergrenzen , Bodensee dissolved in BY und BW, Küstenpolygone entfernt

staatsgrenze -> vg_land dissolved -> Deutschland Grenze ohne Küstenpolygone

## Needed Libraries  
  ``` R
  require(tidyverse)
  require(rvest)
  require(glue)
  require(lubridate)
  require(RSQLite)
  require(DBI)
  require(dbplyr)
  ```

## **Functions**
#### create_database()
```R
create_database(dbname="temp/temp.sqlite3",temp_dir="temp/",
                             plant=NULL,
                             downloaddata=TRUE,
                             meta_spezifizierung=TRUE,
                             meta_beschreibung=TRUE,
                             keepdldata=TRUE)
```
**dbname**: Name (und relativer Pfad) der Datenbank, die erstellt werden soll. Momentan nur SQLite3 Datenbank. Sollte auf .sqlite3 enden.

**temp_dir**: Temporärer Ordner, standardmäßig temp/, hierhin werden die Daten heruntergeladen

**downloaddata**: logical default TRUE, sollen die Daten heruntergeladen werden oder nicht. Sind die Daten beispielsweise alle heruntergeladen worden um eine DB zu erstellen die alle Daten beinhaltet, man aber nun eine erstellen will die nur bestimmte Pflanzen hat oder man möchte was testen.

**keepdldata**: logical default TRUE. Soll am Ende der temp_dir gelöscht werden?

**meta_spezifizierung**: Sollen die Spezifizierungen von Obst, Mais, Rueben und Weinrebe heruntergeladen werden ? Die Dateien sind unnötig groß beim Download, Obst bspw. ist 1.28 GIGABYTE groß.. da sind dutzende Leerzeichen pro Feld mit drin und Leerzeilen etc. Vernünftig als csv abgespeichert wären es 10mb...

**meta_beschreibung**: logical default TRUE. Sollen die Beschreibungs PDFs mit runtergeladen werden. Ist plant gesetzt werden nur die ausgewählten Pflanzen pdfs geladen.

**plant**: Hier kann ein vektor angegeben werden mit Pflanzen die heruntergeladen werden soll. Bspw. c("Birne","Hasel"). Die Abfrage läuft über str_detect und daher werden bspw. alle Birnen Daten genommen (Birne,Birne_fruehe_Reife,Birne_spaete_Reife).
c("beere") würde bspw. Brombeere Himbeere Erdbeere Johannisbeere Rote Johannisbeere Stachelbeere Schneebeere herunterladen.


#### *(internal)* create_megaframe()


#### create_view_in_db()

#### remove_view_from_db()

#### print_all()

#### station_df_to_sf()

#### nr_df_to_sf()




### **Datenbank**

##### **Haupttabellen**
- **megaframe** (View/Virtuelle Tabelle)
  Join der Tabellen Daten Stationen Pflanze Phasendefinition
  - Daten
  - Stationen
  - Pflanze
  - Phase
  - Phasendefinition : Join aus
    - Phasendefinition_Jahresmelder
    - Phasendefinition_Sofortmelder  
- Notizen
- Spezifizierungen
  - Obst_Spezifizierung
  - Mais_Spezifizierung
  - Ruebe_Spezifizierung
  - Weinrebe_Spezifizierung
  - Spezifizierung_Notizen

- Phasendefinition_Jahresmelder
- Phasendefinition_Sofortmelder
- Phaenologie_Besonderheiten_Zeitreihen
- Phaenologie_Qualitaetsbyte
- Phaenologie_Qualitaetsniveau


#### Anmerkungen zu Tabellen/Spalten
objekt_latein/pflanze_latein  manche "Pflanzen" wie Dauergrünland haben keinen Lateinischen Namen und werden mit ------- eingetragen  (eventuell fixen auf NA ?!?)

Naturraum_code(s) 1382/1551 zu 1380/1550 geändert
Naturraum Namen anhand des Naturraum_codes mit den Namen aus vg2500 Layer ersetzt. Damit die Namen einheitlich sind.
