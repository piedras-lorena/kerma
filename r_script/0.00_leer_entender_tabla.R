library(dplyr)
library(tidyr)
library(readr)
library(data.table)

#ruta<-'Google Drive/Kerma Partners/datos/raw/'
ruta<-'../data/raw/'

Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
tabla<- fread(paste0(ruta,'170909_tabla_vacia.csv'),fill=T)
