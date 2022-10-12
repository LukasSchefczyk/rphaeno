
# WIP - Work in Progress - !

# Readme de

Skripte zum runter- und einladen von (zunaechst erstmal) Deutscher Wetterdienst DWD Phaenologie Daten und Plotten dieser Daten 


# Readme en 

scripts to download and load phenology data from (for now) German Weather Service DWD and plot it.




### Geodata 
#### vg2500 Verwaltungsgrenzen [© GeoBasis-DE / BKG 2021](http://www.bkg.bund.de)


https://gdz.bkg.bund.de/index.php/default/digitale-geodaten/verwaltungsgebiete/verwaltungsgebiete-1-2-500-000-stand-31-12-vg2500-12-31.html

#### Naturraum Grenzen 
WFS Bundesamt für Naturschutz

https://geodienste.bfn.de/ogc/wfs/gliederungen?SERVICE=WFS&REQUEST=GetCapabilities

https://geoportal.de/Info/325bfe9a-21f0-4fc4-9dae-a8feb4668a08


#### Layers: 
UTM32N EPSG:25832 

Naturraumgruppe -> Dissolve auf Gruppe_der_Haupteinheiten Layer , Namen und string to int Nummer (ORD2)

Naturraum -> Haupteinheiten Layer , Namen angepasst und string to int Nummer (ORD3)  und *10 damit es vierstellig ist wie DWD Nummern... 

vg_kreis_raw -> Original Kreis Daten 

vg_kreis -> mit Ländergrenzen (GEN_2) vereinigt und auf wesentliche Attributspalten begrenzt

vg_land -> Ländergrenzen , Bodensee dissolved in BY und BW, Küstenpolygone entfernt

vg_staat -> vg_land dissolved -> Deutschland Grenze ohne Küstenpolygone




