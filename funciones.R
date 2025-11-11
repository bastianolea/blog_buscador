limpiar_html <- function(texto) {
  # texto <- gsub("<.*?>", "", texto)
  # texto <- gsub("[^[:alnum:] ]", "", texto)
  # texto <- gsub("\n", "", texto)
  return(texto)
}

limpiar_fechas <- function(texto) {
  texto |> 
    str_extract("\\d{2} \\w{3} \\d{4}") |>
    lubridate::dmy()
}

procesar_xml <- function(sitio) {

  # leer sitio
  pg <- read_xml(sitio)
  
  # extraer entradas
  items <- xml_find_all(pg, "//item")
  
  
  # extraer elementos
  titulos <- items |> 
    xml_find_all("title") |> 
    xml_text()
  
  descr <- items |> 
    xml_find_all("description") |> 
    xml_text() |>
    limpiar_html()
  
  fechas <- items |> 
    xml_find_all("pubDate") |> 
    xml_text() |> 
    limpiar_fechas()
  
  links <- items |> 
    xml_find_all("link") |> 
    xml_text() |> 
    str_replace_all("https://bastianoleah.netlify.app", "https://bastianolea.rbind.io")
  
  
  # crear tabla ----
  datos <- tibble(
    titulo = titulos,
    texto = descr,
    fecha = fechas,
    link = links) |> 
    mutate(texto = paste(titulo, descr))
  
  return(datos)
}
