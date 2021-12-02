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

# python venv config needed to deploy in shinyapps.io
VenvDir = Sys.getenv('VENV_')
PythonPath = Sys.getenv('PYTHON_PATH_')
virtualenv_create(envname = VenvDir, python = PythonPath)
virtualenv_install(VenvDir, packages = c("plotly","plotly.express"), ignore_installed=TRUE)
use_virtualenv(VenvDir, required = TRUE)

# read data
PoliceFirearms <- read.csv(here::here("data/processed","compras_armas_final_web.csv"))

# server logic to process data
shinyServer(function(input, output) {
 # warning sign
  showModal(
    modalDialog(title="Importante",
                HTML("
                Los datos aquí presentados fueron generados por <i> Stop US Arms to Mexico y la Comisión Mexicana para la Defensa y Promoción de los Derechos Humanos </i> 
    a partir de la revisión de facturas de transferencias de armas de fuego y municiones de la Secretaría de la Defensa Nacional (SEDENA)
    a las autoridades estatales durante el periodo 2006-2018. Se trata de documentación obtenida a través de la solicitud de información <b>#0000700176018</b>.
    Tanto la base de datos como la metodología se encuentran disponibles para descarga <a href='https://www.stopusarmstomexico.org/police-firearms-database'> aquí.</a>
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
    las consideraciones anteriores obligan a <b> tomar con reserva las cifras que se muestran en estas visualizaciones</b>.
                     "),
                footer=modalButton("Entendido")))
  
    # ----------- reactive functions -----------
  # text functions
  output$textdata <- renderText({
    
    
    if(input$checkbox==FALSE){
      # check for available data
      available  <- SankeyOutputFunction() %>%
        nrow()
  
        if(available == 0) {
          paste0("Sin datos para ", input$state, " en ",input$year)
        }else{
          paste0("Datos a nivel ", input$state, " para el año ",input$year,".")
        }
             
      } else{
        paste0("Datos a nivel ", input$state," para el periodo ",paste(input$years[1],input$years[2], sep="-"),".")
        }
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
          summarize(costo=sum(en_dolares_2019, na.rm = TRUE),
                    piezas=sum(no_piezas, na.rm = TRUE))
        } else {
          PoliceFirearms %>%
            filter(ano >= input$years[1],
                   ano <= input$years[2]) %>%
            group_by(ano) %>%
            summarize(costo=sum(en_dolares_2019, na.rm = TRUE),
                      piezas=sum(no_piezas, na.rm = TRUE))
          }
      })
    

    # ----------- regular functions -----------

    # sankey plot uses numbers to represent nodes,
    # this funtion transforms labels 
      TransformSankeyData <- function(data_frame){
        
        data_frame[,4:5] <- as.data.frame(lapply(data_frame[,1:2], as.factor))
        data_frame <- as.data.frame(lapply(data_frame, unclass))
        names(data_frame) <- c("lab_making","lab_state","values","source","target")
        data_frame[,4:5] <- as.data.frame(lapply(data_frame[,4:5], as.numeric))
        data_frame$target <- data_frame$target + max(data_frame$source)
        
        makings <- data_frame %>%
          dplyr::arrange(source) %>%
          pull(lab_making)
        
        state <- data_frame %>%
          dplyr::arrange(target) %>%
          pull(lab_state)
        
        transformedData <- list(data_frame,makings,state)
        
        return(transformedData)
    }
    
    SetTitles <- function(tema){
      glue::glue("{ paste0(tema,
      ifelse(input$checkbox==TRUE,
      paste(input$state,paste(input$years[1],input$years[2], sep='-'), sep=' '),
         paste(input$state, input$year, sep=' ') )) 
                 }")
    }
   
    # get carto-color palette from python
    pycolors <- px$colors$carto$Tropic_r
    # remove rgb()
    pycolors <- gsub("[r|g|b]|[(|)]","",pycolors)
    pycolors <- gsub(", ","-",pycolors)
    # run R´s rgb function for each color  
    # thanks to :https://stackoverflow.com/questions/31574480/rgb-to-hex-converter?noredirect=1&lq=1
    pycolors <- sapply(strsplit(pycolors, "-"),
                       function(pycolors){
                         rgb(pycolors[1], pycolors[2], pycolors[3], maxColorValue=255)
                       }
    )
    # genereta palette
    pypalette <- grDevices::colorRampPalette(c(pycolors[1], pycolors[7]))
    
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
              color_continuous_scale=pycolors,
              color_continuous_midpoint=weighted.mean(data$costo,data$piezas)
            )
          )$update_layout(title_text = SetTitles("Detalle del flujo: ")),
          output_type = "div"
          )
      ) 
    })
    

    output$barplot <- renderPlotly({
      
      BarOutputFunction() %>%
      plot_ly(x = ~cost, y = ~reorder(marca, cost), type = "bar", orientation = "h",
              marker = list(color = pycolors[3],line = list(color = pycolors[3], width = 1.5))
      ) %>%
        layout(title = SetTitles("Gasto en armas de fuego: "),
               barmode = "group",
               xaxis = list(title = "Dólares constantes de 2019"),
               yaxis = list(title = ""))
    })
    
    output$lineplot <- renderPlotly({
      
     costos <- LineOutputFunction() %>%
        plot_ly(x = ~ano, y = ~costo, name = "Costo", type = "scatter", mode = "lines+markers", color = I(pycolors[1]),
                marker = list(
                  color = pycolors[1],
                  size = 10,
                  line = list(
                    color = pycolors[1],
                    width = 1
                  )
                ),
                showlegend = TRUE) %>%
        layout(
          title = glue::glue("Gasto y total de armas de fuego distribuidas: {input$state} 2006-2018"),
          yaxis = list(title = "Dólares constantes de 2019"),
          margin = list(l = 65),
          autosize = TRUE)
      
      total <- LineOutputFunction() %>%
        plot_ly(x = ~ano, y = ~piezas, name = "Piezas", type = "scatter",mode = "lines+markers",color = I(pycolors[7]),
                marker = list(
                  color = pycolors[7],
                  size = 10,
                  line = list(
                    color = pycolors[7],
                    width = 1
                  )
                ),
                showlegend = TRUE) %>%
        layout(
          yaxis = list(title = "Total de armas"),
          margin = list(l = 65),
          autosize = TRUE)
      
      subplot(costos,total,nrows = 2, shareX = FALSE, shareY = FALSE, titleY=TRUE)
      
    })
    
    output$sankey <- renderPlotly({
      
      data <- SankeyOutputFunction() 
      
      data <- TransformSankeyData(data)
        
        
        plot_ly(
          type = "sankey",
          orientation = "h",
          node = list(
            label = c("",
                      unique(data[[2]]),
                      unique(data[[3]])),
            color = pypalette(70),
            pad = 15,
            thickness = 20,
            line = list(
              color = "black",
              width = 0.5
            )
          ),
          link = list(
            source = data[[1]][["source"]],
            target = data[[1]][["target"]],
            value = data[[1]][["values"]]
          )
        ) %>%
          layout(title = SetTitles("Flujo legal de armas de fuego: ") )
    
      })
    
  })