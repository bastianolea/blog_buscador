library(shiny)
library(bslib)
library(jsonlite)
library(dplyr)
library(stringr)
library(lubridate)
library(glue)
library(purrr)

source("funciones.R")

# cache local
shinyOptions(cache = cachem::cache_disk("./cache"))

ui <- page_fluid(
  
  # estilos
  includeCSS("styles.css"),
  
  # input de texto
  textInput("busqueda", 
            "Buscador", 
            value = "stringr",
            placeholder = "Escribe un término de búsqueda"),
  
  # salida en html de resultados
  htmlOutput("resultados")
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
  termino <- reactive(input$busqueda)
  termino <- debounce(termino, 300)
  
  
  # buscar texto
  busqueda <- reactive({
    message("buscando ", termino())
    
    if (termino() == "") {
      return(tibble())
    } else {
      sitio() |> 
        filter(str_detect(texto, termino())) |> 
        select(-texto) |> 
        arrange(desc(fecha))
    }
  })
  
  
  # salida en html de resultados de búsqueda
  output$resultados <- renderUI({
    # browser()
    
    if (nrow(busqueda()) == 0) {
      p("sin resultados",
        class = "excusas")
    } else {
      
      # separar resultados
      elementos <- busqueda() |> 
        mutate(id = row_number()) |> 
        split(~id)
      
      # generar elemento para cada resultado
      map(elementos, \(elemento) {
        # elemento <- elementos[[6]]
        etiquetas <- etiquetas(elemento$tags) |> tagList()
        
        div(class = "resultado",
            
            h3(
              tags$a(href = elemento$link,
                     elemento$titulo)
            ),
            
            div(class = "fecha", 
                elemento$fecha),
            
            div(class = "contenedor_etiquetas",
                etiquetas),
            
            hr()
        )
      })
    }
  })
}

shinyApp(ui, server)
