{
  library(shiny)
  library(bslib)
  library(jsonlite)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(glue)
  library(purrr)
  library(cli)
  library(shinydisconnect)
  library(rbm25)
  library(stopwords)
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

  # mensaje en caso de desconexión
  disconnectMessage(
    refresh = "Volver a cargar",
    background = "#EAD1FA",
    colour = "#553A74",
    refreshColour = "#9069C0",
    overlayColour = "#553A74",
    size = 14,
    text = "La aplicación se desconectó. Vuelve a cargar la página."
  ),

  # espaciador
  div(style = "height: 64px;"),

  h2("Buscador"),

  div(
    style = "margin-bottom: 6px;",
    markdown(
      "Ingresa cualquier tema, concepto o función de R para buscar entre [las publicaciones de mi blog de análisis de datos con R](https://bastianolea.rbind.io/blog/)."
    )
  ),

  ## input ----
  # input de texto
  textInput(
    "busqueda",
    NULL,
    value = NULL,
    # value = "rvest",
    placeholder = "Escribe un término de búsqueda",
    width = "100%"
  ),

  # texto con cantidad de resultados
  div(
    style = "margin-top: 16px; margin-bottom: 32px;",
    textOutput("texto_resultados")
  ),

  # salida en html de resultados
  htmlOutput("resultados"),

  # footer
  div(
    style = "margin-bottom: 24px;",
    strong(
      a("Volver al blog", href = "https://bastianolea.rbind.io/blog/")
    )
  )
)


server <- function(input, output, session) {
  # obtener ----
  # obtener datos del sitio
  sitio <- reactive({
    message("obteniendo sitio...")

    procesar_json("https://bastianolea.rbind.io/index.json")
  }) |>
    # guardar cache por hora
    bindCache(
      floor_date(
        now(),
        unit = "6 hours"
      )
    )

  # esperar que se deje de escribir para buscar
  termino_crudo <- reactive(input$busqueda)
  termino_debounce <- debounce(termino_crudo, 600)

  # preprocesar término de búsqueda
  termino <- reactive({
    termino_debounce() |>
      # minúsculas
      tolower() |>
      # eliminar puntuaciones
      str_remove_all("[[:punct:]]") #|>
    # para múltiples palabras
    # str_replace("\\s+", "\\.\\*") # cambia espacios por ".*" (regex para cualquier texto de cualquier largo)
  })

  # buscar ----
  # buscar texto
  busqueda <- reactive({
    # va a buscar primero por bm25, y si no sale nada, con str_detect
    req(termino() != "")
    req(nchar(termino()) >= 2)
    message("buscando ", termino())

    # browser()

    # busca textos usando algoritmo bm25
    # también entrega puntaje a publicaciones más recientes
    resultados_bm25 <- sitio() |>
      # búsqueda
      mutate(
        puntaje_busqueda = bm25_score(
          contenido,
          termino(),
          lang = "es"
        )
      ) |>
      relocate(puntaje_busqueda, .after = contenido) |>
      filter(puntaje_busqueda > 0) |>
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
        "Pocos resultados. Intenta una búsqueda más precisa!",
      )

      resultados_stringr <- sitio() |>
        # búsqueda
        filter(
          str_detect(contenido, termino() |> str_replace("\\s+", "\\.\\*"))
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
        a(href = elemento$link, h3(markdown(elemento$titulo))),

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
        markdown(
          "Esta página también es una [aplicación Shiny](https://bastianolea.rbind.io/tags/shiny/) desarrollada en R. Puedes [revisar el código en este repositorio](https://github.com/bastianolea/blog_buscador)."
        )
      )
    )
  })
}

shinyApp(ui, server)
