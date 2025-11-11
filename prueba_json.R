library(jsonlite)
library(dplyr)
library(stringr)
library(lubridate)

source("funciones.R")

sitio <- "https://bastianolea.rbind.io/index.json"

datos <- procesar_json(sitio)
  
busqueda <- "waldo"

resultado <- datos |> 
  filter(str_detect(texto, busqueda))

resultado
