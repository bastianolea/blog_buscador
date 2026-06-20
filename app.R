{
  library(shiny)
  library(bslib)
  library(yyjsonr)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(glue)
  library(purrr)
  library(cli)
  library(shinydisconnect)
  library(rbm25)
  library(stopwords)
  library(shinycssloaders)
} |>
  suppressPackageStartupMessages()

source("funciones.R")

# cache local
# shinyOptions(cache = cachem::cache_disk("./cache"))

# interfaz ----
ui <- page_fluid(
  style = "max-width: 700px; padding: 24px;",
  title = "Bastián Olea: Buscador",
  lang = "es",

  # tema
  theme = bs_theme(
    fg = "#553A74",
    bg = "#EAD1FA",
    primary = "#6E3A98",
    font_scale = 1.1,
    # base_font = font_google("Atkinson Hyperlegible"),
    # heading_font = font_google("EB Garamond"),
    base_font = "Atkinson Hyperlegible",
    heading_font = "EB Garamond"
  ),

  # tipografía para html, instalada con gfonts::setup_font()
  # gfonts::setup_font("eb-garamond", "fonts")
  # gfonts::setup_font("atkinson-hyperlegible", "fonts")
  tags$link(rel = "stylesheet", href = "fonts/css/atkinson-hyperlegible.css"),
  tags$link(rel = "stylesheet", href = "fonts/css/eb-garamond.css"),

  # estilos
  includeCSS("styles.css"),

  # iframeResizer: permite que la página padre ajuste el alto del iframe al contenido
  tags$script(src = "iframeResizer.contentWindow.min.js"),

  # mensaje en caso de desconexión
  disconnectMessage(
    refresh = "Volver a cargar",
    background = "#916FC3",
    colour = "#EAD1FA",
    refreshColour = "#CBACE2",
    overlayColour = "#EAD1FA",
    size = 14,
    text = "La aplicación se desconectó. Vuelve a cargar la página."
  ),

  h2("Buscador"),

  div(
    style = "margin-bottom: 32px;",
    HTML(
      "Ingresa cualquier tema, concepto o función de R para buscar entre las publicaciones de mi <a href='https://bastianolea.rbind.io/blog/' target='_blank'>blog de análisis de datos con R.</a>"
    )
  ),

  ## input ----
  textInput(
    "busqueda",
    NULL,
    value = NULL,
    placeholder = "Escribe un término de búsqueda",
    width = "100%"
  ),

  # texto con cantidad de resultados
  div(
    style = "margin-top: 16px; margin-bottom: 32px;",
    textOutput("texto_resultados")
  ),

  # salida en html de resultados
  htmlOutput("resultados") |>
    withSpinner(proxy.height = "128px", color = "#8F6AC0", type = 7)
)


server <- function(input, output, session) {
  # descargar ----
  # observe({
  #   message("descargando sitio...")
  #   download.file("https://bastianolea.rbind.io/index.json", "cache/index.json")
  # })
  #
  # # cargar ----
  # datos_sitio <- reactive({
  #   message("cargando sitio...")
  #
  #   # leer descargado
  #   # yyjsonr::read_json_file("cache/index.json")
  #
  #   # # para probar en local
  #   yyjsonr::read_json_file("~/R/blog-r/public/index.json")
  # })
  #
  # # procesar ----
  # sitio <- reactive({
  #   message("procesando sitio...")
  #
  #   datos_sitio() |> procesar_json()
  # }) |>
  #   # guardar cache por hora
  #   bindCache(
  #     floor_date(
  #       now(),
  #       unit = "6 hours"
  #     )
  #   )

  # obtener datos del sitio y procesarlos sin reactividad,
  # porque es un requisito del buscador que sí o sí debe cumplirse
  message("descargando sitio...")
  download.file("https://bastianolea.rbind.io/index.json", "cache/index.json")

  message("cargando datos...")
  datos_sitio <- yyjsonr::read_json_file("cache/index.json")

  message("procesando datos...")
  sitio <- procesar_json(datos_sitio)
  message("listo para buscar!")

  # buscar ----

  # esperar que se deje de escribir para buscar
  termino_crudo <- reactive(input$busqueda)
  termino_debounce <- debounce(termino_crudo, 800)

  # preprocesar término de búsqueda
  termino <- reactive({
    termino_debounce() |>
      # minúsculas
      tolower() |>
      # eliminar puntuaciones
      str_replace_all("[[:punct:]]", " ") |>
      str_squish()
  })

  # buscar texto
  busqueda <- reactive({
    # va a buscar primero por bm25, y si no sale nada, con str_detect
    req(termino() != "")
    req(nchar(termino()) >= 2)
    message("buscando ", termino())

    # browser()

    # busca textos usando algoritmo bm25
    # también entrega puntaje a publicaciones más recientes
    resultados_bm25 <- sitio |>
      # búsqueda
      mutate(
        puntaje_busqueda = bm25_score(
          contenido,
          termino(),
          lang = "es"
        )
      ) |>
      filter(puntaje_busqueda > 0) |>
      arrange(desc(puntaje_busqueda))

    # resultados_bm25 |>
    #   arrange(desc(puntaje_busqueda)) |>
    #   select(titulo, puntaje_busqueda)

    resultados_bm25 <- resultados_bm25 |>
      relocate(puntaje_busqueda, .after = contenido) |>
      # considerar puntaje para publicaciones recientes
      mutate(puntaje_fecha = dense_rank(fecha)) |>
      mutate(
        puntaje_fecha = reescalar(puntaje_fecha, puntaje_busqueda * 0.4)
      ) |>
      mutate(puntaje = puntaje_busqueda + puntaje_fecha) |>
      # ordenar por mayor puntaje
      arrange(desc(puntaje)) |>
      select(-contenido, -texto)

    # si no se encuentra nada, hacer búsqueda con stringr para mostrar algo
    if (nrow(resultados_bm25) == 0) {
      showNotification(
        "Pocos resultados. Intenta una búsqueda distinta!",
      )

      resultados_stringr <- sitio |>
        # búsqueda
        filter(
          str_detect(
            contenido,
            termino() |>
              str_replace("\\s+", "\\.\\*")
          )
        ) |>
        select(-contenido) |>
        head(n = 15) |> # limitar máximos
        arrange(desc(fecha))

      resultado <- resultados_stringr
    } else {
      resultado <- resultados_bm25
    }

    return(resultado)

    # return(resultados_bm25)
  })

  # outputs ----

  # cantidad de resultados encontrados
  n_resultados <- reactive({
    resultados <- nrow(busqueda())
    message(resultados, " resultados encontrados")
    return(resultados)
  })

  # aviso por muchos resultados
  observe({
    if (n_resultados() == 40) {
      showNotification(
        "Demasiados resultados! Ajusta la búsqueda",
        type = "warning"
      )
    }
  })

  # texto antes de los resultados
  output$texto_resultados <- renderText({
    if (termino() == "") {
      # no mostrar nada si no se ha buscado
      NULL
    } else if (n_resultados() == 0) {
      # texto si no se encuentra nada
      "No hay resultados"
    } else {
      # si se encuentran resultados, texto que se adapta a plurales
      pluralize("Se encontr{?ó/aron} {n_resultados()} publicaci{?ón/ones}:")
    }
  })

  ## resultados ----
  # salida en html de resultados de búsqueda
  output$resultados <- renderUI({
    req(termino() != "")
    req(n_resultados() > 0)

    # separar resultados
    elementos <- busqueda() |>
      mutate(id = row_number()) |>
      split(~id)

    # generar elemento HTML para cada resultado
    ui_resultados <- map(elementos, \(elemento) {
      # elemento <- elementos[[3]]
      div(
        class = "resultado",

        # título con link
        a(
          href = elemento$link,
          target = "_blank",
          h3(markdown(elemento$titulo))
        ),

        # fecha
        div(class = "fecha", elemento$fecha),

        # texto
        div(
          style = "font-size: 90%; line-height: 1.2;",
          markdown(
            str_trunc(elemento$resumen, 320, ellipsis = "…")
          )
        ),

        # etiquetas con links
        div(class = "contenedor_etiquetas", etiquetas(elemento$tags)),

        # separador
        hr()
      )
    })

    # output
    div(
      ui_resultados,
      div(
        HTML(
          "Esta página también es una <a href='https://bastianolea.rbind.io/blog/shiny/' target='_blank'>aplicación Shiny</a> desarrollada en R. Puedes <a href='https://bastianolea.rbind.io/blog/buscador/' target='_blank'>leer más sobre esta app aquí.</a>"
        )
      )
    )
  })
}

shinyApp(ui, server)
