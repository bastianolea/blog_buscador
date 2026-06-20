limpiar_html <- function(texto) {
  texto <- gsub("<.*?>", "", texto)
  texto <- gsub("[^[:alnum:] ]", "", texto)
  texto <- gsub("\n", "", texto)
  return(texto)
}

limpiar_fechas <- function(texto) {
  texto |>
    str_extract("\\d{2} \\w{3} \\d{4}") |>
    lubridate::dmy()
}

extraer_fechas <- function(texto) {
  texto |>
    str_extract("\\d{4}-\\d{2}-\\d{2}") |>
    lubridate::ymd()
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
    str_replace_all(
      "https://bastianoleah.netlify.app",
      "https://bastianolea.rbind.io"
    )

  # crear tabla ----
  datos <- tibble(
    titulo = titulos,
    texto = descr,
    fecha = fechas,
    link = links
  ) |>
    mutate(texto = paste(titulo, descr))

  return(datos)
}


procesar_json <- function(sitio) {
  # sitio <- "https://bastianolea.rbind.io/index.json"
  # sitio <- "~/R/blog-r/public/index.json"

  # obtener <- jsonlite::fromJSON(sitio)

  # tibble(obtener) |>
  #   slice(11) |>
  #   pull(content)

  stopwords <- stopwords::stopwords("es", source = "snowball")

  datos <- sitio |>
    tibble() |>
    rename(
      texto = content,
      resumen = excerpt,
      link = href,
      fecha = date,
      titulo = title
    ) |>
    # minúsculas para búsqueda
    mutate(contenido = paste(titulo, texto)) |>
    # limpiar texto del contenido de posts
    mutate(
      contenido = contenido |>
        str_to_lower() |>
        str_replace_all("[[:punct:]]", " ") |>
        str_remove_all(
          paste(
            paste0("\\b", stopwords, "\\b"),
            collapse = "|"
          )
        ) |>
        str_squish()
    ) |>
    mutate(fecha = extraer_fechas(fecha))

  # datos |> filter(str_detect(texto, "regex")) |> pull()
  return(datos)
}

# texto de etiquetas separado por punto y comas
etiquetas <- function(tag) {
  elementos <- tag |>
    # separar en elementos
    str_split(";") |>
    unlist() |>
    # eliminar espacios
    str_trim()

  # cada elemento convertirlo
  map(
    elementos,
    ~ div(
      class = "etiquetas",
      # enlace
      a(
        div(.x, class = "texto_etiquetas"),
        href = paste0(
          "https://bastianolea.rbind.io/tags/",
          str_replace_all(.x, " ", "-")
        ),
        target = "_blank"
      )
    )
  )
}


reescalar <- function(x, target) {
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  t_min <- min(target, na.rm = TRUE)
  t_max <- max(target, na.rm = TRUE)

  t_min + (x - x_min) / (x_max - x_min) * (t_max - t_min)
}
