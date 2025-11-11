library(shiny)
library(bslib)
library(xml2)
library(dplyr)
library(stringr)
library(lubridate)
library(glue)

shinyOptions(cache = cachem::cache_disk("./cache"))

ui <- page_fluid(
  
  textInput("busqueda", 
            "Buscador", 
            value = "stringr",
            placeholder = "Escribe un término de búsqueda"),
  
  htmlOutput("resultados")
)

server <- function(input, output, session) {
  
  # obtener datos del sitio
  sitio <- reactive({
    message("obteniendo sitio...")
    procesar_xml("https://bastianolea.rbind.io/index.xml")
  }) |> 
    bindCache(floor_date(now(), unit = "hours"))
  
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
        arrange(desc(fecha))
    }
  })
  
  # debounce
  
  output$resultados <- renderUI({
    
    if (nrow(busqueda()) == 0) {
      p("sin resultados",
        class = "excusas")
    } else {
    busqueda() |> 
      mutate(
        enlace = glue("<a href='{link}'>{titulo}</a>"),
        fecha = format(fecha, "%d/%m/%Y"),
        resultado = glue("<h3>{enlace}</h3> <p>({fecha})</p>")
      ) |> 
      pull(resultado) |> 
      paste(collapse = "<hr/>") |> 
      HTML()
    }
  })
}

shinyApp(ui, server)
