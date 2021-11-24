#  _______________________________
# < Hi. This is the server logic. >
#  -------------------------------
#        \   ,__,
#         \  (oo)____
#            (__)    )\
#               ||--|| *

# import packages
library(shiny)
library(reticulate)
library(tidyr)

# local python config and imports
use_virtualenv(virtualenv = here::here("env/"), required = TRUE)

# python imports
px <- import("plotly.express")
py_plotly <- import("plotly")

# read data
PoliceFirearms <- read.csv(here::here("data/processed","compras_armas_final_web.csv"))
ComparedData <- read.csv(here::here("data/processed","merged_data.csv"), stringsAsFactors = FALSE)

# server logic to process data
shinyServer(function(input, output) {
 # warning sign
  showModal(
    modalDialog(title="Advertencia",
                HTML("
    Los datos aquí presentados fueron generados por <i> Stop US Arms to Mexico y la Comisión Mexicana para la Defensa y Promoción de los Derechos Humanos </i> 
    a partir de la revisión de facturas de transferencias de armas de fuego y municiones de la Secretaría de la Defensa Nacional (SEDENA)
    a las autoridades estatales durante el periodo 2006-2018. Se trata de documentación obtenida a través de la solicitud de información <b>#0000700176018</b>.
    La base de datos y la metodología se encuentra disponible para descarga <a href='www.stopusarmstomexico/police-firearms-database'> aquí.</a>
    <br>
    En México la SEDENA es la única autoridad facultada para distribuir armamento de forma legal a empresas de seguridad, particulares,
    dependencias federales y estatales. En este sentido cabe puntulizar dos cosas:
    <br>
    <ol>
      <li>El universo aquí mostrado se refiere únicamente a armas comercializadas por la SEDENA a los gobiernos/autoridades estatales</li>
      <li> Estos datos cuentan con un subregistro. Es decir, no dan cuenta de la totalidad de las transferencias de armas,
      como muestran las discrepancias existentes con solicitudes previas de información. 
    </ol>
    Aunque este ejercicio busca dar una idea de la distribución de armas de fuego, 
    las consideraciones anteriores obligan a tomar con cierta reserva las cifras que se muestran en estas visualizaciones."),
                footer=modalButton("Entendido")))
  
    # ----------- reactive functions -----------
  # text functions
  output$textdata <- renderText({
    paste0("Datos seleccionados a nivel ", input$state, " para el",
           
            ifelse(input$checkbox==TRUE,
                  paste0(" periodo ",paste(input$years[1],input$years[2], sep="-")),
                  paste0(" año ",input$year) ) 
                  ) 
    })
  
  # plot filter functions
    BarOutputFunction <- reactive({
      if(input$state!="Nacional" & input$checkbox==TRUE){
        PoliceFirearms %>%
          filter(ano >= input$years[1],
                 ano <= input$years[2],
                 estado == input$state) %>%
          group_by(marca) %>%
          summarize(cost=round(sum(en_dolares_2019,na.rm = TRUE),2))
        } else if(input$state=="Nacional" & input$checkbox==TRUE){
          PoliceFirearms %>%
            filter(ano >= input$years[1],
                   ano <= input$years[2]) %>%
            group_by(marca) %>%
            summarize(cost=round(sum(en_dolares_2019,na.rm = TRUE),2))
        } else if (input$state=="Nacional" & input$checkbox==FALSE){
          PoliceFirearms %>%
            filter(ano == input$year) %>%
            group_by(marca) %>%
            summarize(cost=round(sum(en_dolares_2019,na.rm = TRUE),2))
        } else {
          PoliceFirearms %>%
            filter(ano == input$year,
                   estado == input$state) %>%
            group_by(marca) %>%
            summarize(cost=round(sum(en_dolares_2019,na.rm = TRUE),2))
        } 
      })
    
    TreemapOutputFunction <- reactive({
      
      if (input$state=="Nacional" & input$checkbox==TRUE) {
        PoliceFirearms %>%
          filter(ano >= input$years[1],
                 ano <= input$years[2]) %>%
          mutate(estado="Nacional") %>%
          group_by(estado, tipo_es, pais_origen_empresa, marca, calibre) %>%
          summarise(costo=round(sum(en_dolares_2019,na.rm = TRUE),2),
                    piezas=sum(no_piezas, na.rm = TRUE))
        
      } else if(input$state!="Nacional" & input$checkbox==TRUE){
        
        PoliceFirearms %>%
          filter(ano >= input$years[1],
                 ano <= input$years[2],
                 estado == input$state) %>% 
          group_by(estado, tipo_es, pais_origen_empresa,marca, calibre) %>%
          summarise(costo=round(sum(en_dolares_2019,na.rm = TRUE),2),
                    piezas=sum(no_piezas, na.rm = TRUE))
      } else if (input$state=="Nacional" & input$checkbox==FALSE){
        PoliceFirearms %>%
          filter(ano == input$year) %>%
          mutate(estado="Nacional") %>%
          group_by(estado, tipo_es, pais_origen_empresa, marca, calibre) %>%
          summarise(costo=round(sum(en_dolares_2019,na.rm = TRUE),2),
                    piezas=sum(no_piezas, na.rm = TRUE)) 
        } else {
          PoliceFirearms %>%
            filter(ano == input$year,
                   estado == input$state) %>%
            group_by(estado, tipo_es, pais_origen_empresa, marca, calibre) %>%
            summarise(costo=round(sum(en_dolares_2019,na.rm = TRUE),2),
                      piezas=sum(no_piezas, na.rm = TRUE)) 
        }
      })
    
    
    SankeyOutputFunction <- reactive({
      
      if (input$state=="Nacional" & input$checkbox==TRUE) {
        PoliceFirearms %>%
          filter(!is.na(marca),
                 ano >= input$years[1],
                 ano <= input$years[2]) %>%
          group_by(marca,estado) %>%
          summarize(piezas=sum(no_piezas, na.rm = TRUE)) 
        
      } else if(input$state!="Nacional" & input$checkbox==TRUE){
        
        PoliceFirearms %>%
          filter(!is.na(marca),
                 ano >= input$years[1],
                 ano <= input$years[2],
                 estado==input$state) %>%
          group_by(marca,vendido_a_cliente) %>%
          summarize(piezas=sum(no_piezas, na.rm = TRUE)) 
      } else if (input$state=="Nacional" & input$checkbox==FALSE){
        PoliceFirearms %>%
          filter(!is.na(marca),
                 ano == input$year) %>%
          group_by(marca,estado) %>%
          summarize(piezas=sum(no_piezas, na.rm = TRUE))  
      } else {
        PoliceFirearms %>%
          filter(!is.na(marca),
                 ano == input$year,
                 estado == input$state) %>%
          group_by(marca,vendido_a_cliente) %>%
          summarize(piezas=sum(no_piezas, na.rm = TRUE)) 
      }
    })
    
    LineOutputFunction <- reactive({
      if(input$state!="Nacional"){
        PoliceFirearms %>%
          filter(ano >= input$years[1],
             ano <= input$years[2],
             estado == input$state) %>%
          group_by(ano) %>%
          summarize(piezas=sum(no_piezas, na.rm = TRUE))
        } else {
          PoliceFirearms %>%
            filter(ano >= input$years[1],
                   ano <= input$years[2]) %>%
            group_by(ano) %>%
            summarize(piezas=sum(no_piezas, na.rm = TRUE))
          }
      })
    

    # ----------- regular functions -----------
    TransformSankeyData <- function(data_frame){
      data_frame[,4:5] <- as.data.frame(lapply(data_frame[,1:2], as.factor))
      data_frame <- as.data.frame(lapply(data_frame, unclass))
      names(data_frame) <- c("lab_cntry","lab_state","values","source","target")
      data_frame[,4:5] <- as.data.frame(lapply(data_frame[,4:5], as.numeric))
      data_frame$target <- data_frame$target + max(data_frame$source)
      return(data_frame)
    }
    
    SetTitles <- function(tema){
      glue::glue(
      "{ paste0(tema,
      ifelse(input$checkbox==TRUE,
      paste(input$state,paste(input$years[1],input$years[2], sep='-'), sep=' '),
         paste(input$state, input$year, sep=' ') )) 
                 }")
    }
   
    
    # ----------- render functions -----------
    
    output$treemap <- renderUI({
      
      data <- TreemapOutputFunction()
      
      HTML(
        py_plotly$offline$plot(
          
          py_plotly$graph_objects$Figure(
            px$treemap(
              data_frame = data,
              path = c("estado","tipo_es","pais_origen_empresa","marca","calibre"),
              values = "piezas",
              color="costo",
              color_continuous_scale="viridis",
              color_continuous_midpoint=weighted.mean(data$costo,data$piezas))
            ),
          output_type = "div")
      ) 
    })
    

    output$barplot <- renderPlotly({
      
      BarOutputFunction() %>%
      plot_ly(x = ~cost, y = ~reorder(marca, cost), type = "bar", orientation = "h",
              marker = list(color = "#F8BF2B",line = list(color = "#F8BF2B", width = 1.5))
      ) %>%
        layout(title = SetTitles("Gasto en armas de fuego: "),
               barmode = "group",
               xaxis = list(title = "Dólares constantes de 2019"),
               yaxis = list(title = ""))
    })
    
    output$lineplot <- renderPlotly({
      
      LineOutputFunction() %>%
        plot_ly(x = ~ano, y = ~piezas, type = "scatter", mode = "lines+markers", color = I("#F8BF2B"),
                marker = list(
                  color = "#F8BF2B",
                  size = 10,
                  line = list(
                    color = "#F8BF2B",
                    width = 1
                  )
                ),
                showlegend = FALSE) %>%
        layout(
          title = glue::glue("Número de armas de fuego distribuidas, {input$state} 2006-2018"),
          xaxis = list(title = "Año"),
          yaxis = list(title = "Total"),
          margin = list(l = 65),
          autosize = TRUE)
    })
    
    output$sankey <- renderPlotly({
      data <- SankeyOutputFunction() 
      data <- TransformSankeyData(data)
        
        
        plot_ly(
          type = "sankey",
          orientation = "h",
          node = list(
            label = c("",unique(data$lab_cntry),unique(data$lab_state)),
            color = viridis::viridis(length(unique(data$lab_cntry)),alpha = 0.8),
            pad = 15,
            thickness = 20,
            line = list(
              color = "black",
              width = 0.5
            )
          ),
          link = list(
            source = data$source,
            target = data$target,
            value = data$values
          )
        ) %>%
          layout(title = SetTitles("Flujo legal de armas de fuego: ") )
    
      })
    
  })