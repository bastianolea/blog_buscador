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

source("funciones.R")

# cache local
shinyOptions(cache = cachem::cache_disk("./cache"))

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
    base_font = font_google("Atkinson Hyperlegible"),
    heading_font = font_google("EB Garamond"),
  ),
  
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
    markdown("Ingresa cualquier término, concepto o función de R para buscar entre [las publicaciones del blog](https://bastianolea.rbind.io/blog/).")
  ),
  
  # input de texto
  textInput("busqueda", 
            NULL, 
            value = NULL,
            # value = "rvest",
            placeholder = "Escribe un término de búsqueda",
            width = "100%"),
  
  
  # texto con cantidad de resultados
  div(
    style = "margin-top: 16px; margin-bottom: 32px;",
    textOutput("texto_resultados")
  ),
  # hr(),
  
  # salida en html de resultados
  htmlOutput("resultados"),
  
  # footer
  div(
    style = "margin-bottom: 24px;",
    strong(
      a("Volver al blog",
        href = "https://bastianolea.rbind.io/blog/")
    )
  )
  
)


server <- function(input, output, session) {
  
  # obtener datos del sitio
  sitio <- reactive({
    message("obteniendo sitio...")
    
    # procesar_xml("https://bastianolea.rbind.io/index.xml")
    procesar_json("https://bastianolea.rbind.io/index.json")
    
  }) |> 
    # guardar cache por hora
    bindCache(
      floor_date(
        now(), 
        unit = "hours")
    )
  
  
  # esperar que se deje de escribir para buscar
  termino_crudo <- reactive(input$busqueda)
  termino_debounce <- debounce(termino_crudo, 400)
  
  # procesar término de búsqueda
  termino <- reactive({
    termino_debounce() |> 
      # minúsculas
      tolower() |> 
      # para múltiples palabras
      str_replace("\\s+", "\\.\\*")
  })
  
  # buscar texto
  busqueda <- reactive({
    req(termino() != "")
    req(nchar(termino()) >= 3)
    message("buscando ", termino())
    
    sitio() |> 
      # búsqueda
      filter(str_detect(texto, termino())) |> 
      select(-texto) |>
      head(n = 50) |> # limitar máximos
      arrange(desc(fecha))
  })
  
  # cantidad de resultados encontrados
  n_resultados <- reactive(nrow(busqueda()))
  
  # aviso por muchos resultados
  observe({
    if (n_resultados() == 50) {
      showNotification("Demasiados resultados! Mostrando sólo 50",
                       type = "warning") 
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
  
  # salida en html de resultados de búsqueda
  output$resultados <- renderUI({
    # browser()
    req(termino() != "")
    req(n_resultados() > 0)
    
    # browser()
    
    # separar resultados
    elementos <- busqueda() |> 
      mutate(id = row_number()) |> 
      split(~id)
    
    # generar elemento HTML para cada resultado
    ui_resultados <- map(elementos, \(elemento) {
      
      # elemento <- elementos[[3]]
      div(class = "resultado",
          
          # título con link
          a(href = elemento$link,
            target = "_blank",
            h3(elemento$titulo)),
          
          # fecha
          div(class = "fecha", 
              elemento$fecha),
          
          # # texto
          # p(str_trunc(elemento$texto, 200, ellipsis = "…")),
          
          # etiquetas con links
          div(class = "contenedor_etiquetas",
              etiquetas(elemento$tags)),
          
          # separador
          hr()
      )
    })
    
    # output
    div(
      ui_resultados,
      div(
        # style = "opacity: 60%;",
        markdown("Esta página también es una [aplicación Shiny](https://bastianolea.rbind.io/tags/shiny/) desarrollada en R. Puedes [revisar el código en este repositorio](https://github.com/bastianolea/blog_buscador).")
      )
    )
    
  })
  
  
}

shinyApp(ui, server)
