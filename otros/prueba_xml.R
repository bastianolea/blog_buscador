
sitio <- "https://bastianolea.rbind.io/index.xml"

library(xml2)
library(dplyr)
library(stringr)
library(lubridate)

source("funciones.R")

datos <- procesar_xml(sitio)

busqueda <- "waldo"

resultado <- datos |> 
  filter(str_detect(texto, busqueda))

resultado
