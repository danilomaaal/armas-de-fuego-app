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

# python config and imports
use_virtualenv(virtualenv = here::here("env/"), required = TRUE)
px <- import("plotly.express")
py_plotly <- import("plotly")



# read data
PoliceFirearms <- read.csv(here::here("data/processed","compras_armas_final_web.csv"))


# server logic to process data
shinyServer(function(input, output) {

    # ----------- reactive functions -----------
   
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
          filter(!is.na(pais_origen_empresa),
                 ano >= input$years[1],
                 ano <= input$years[2]) %>%
          group_by(pais_origen_empresa,estado) %>%
          summarize(piezas=sum(no_piezas, na.rm = TRUE)) 
        
      } else if(input$state!="Nacional" & input$checkbox==TRUE){
        
        PoliceFirearms %>%
          filter(!is.na(pais_origen_empresa),
                 ano >= input$years[1],
                 ano <= input$years[2],
                 estado==input$state) %>%
          group_by(pais_origen_empresa,estado) %>%
          summarize(piezas=sum(no_piezas, na.rm = TRUE)) 
      } else if (input$state=="Nacional" & input$checkbox==FALSE){
        PoliceFirearms %>%
          filter(!is.na(pais_origen_empresa),
                 ano == input$year) %>%
          group_by(pais_origen_empresa,estado) %>%
          summarize(piezas=sum(no_piezas, na.rm = TRUE))  
      } else {
        PoliceFirearms %>%
          filter(!is.na(pais_origen_empresa),
                 ano == input$year,
                 estado == input$state) %>%
          group_by(pais_origen_empresa,estado) %>%
          summarize(piezas=sum(no_piezas, na.rm = TRUE)) 
      }
    })
    
    TransformSankeyData <- function(data_frame){
        data_frame[,4:5] <- as.data.frame(lapply(data_frame[,1:2], as.factor))
        data_frame <- as.data.frame(sapply(data_frame, unclass))
        names(data_frame) <- c("lab_cntry","lab_state","values","source","target")
        data_frame[,4:5] <- as.data.frame(lapply(data_frame[,4:5], as.numeric))
        data_frame$target <- data_frame$target + max(data_frame$source)
        return(data_frame)
      }
    
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
              color_continuous_scale="tropic",
              color_continuous_midpoint=weighted.mean(data$costo,data$piezas)),
      
            py_plotly$graph_objects$Layout(title= "Composición de las armas distribuidas",showlegend=TRUE)
          ), output_type = "div")
      )
    })
    

    output$barplot <- renderPlotly({
      
      BarOutputFunction() %>%
      plot_ly(x = ~cost, y = ~reorder(marca, cost), type = 'bar', orientation = 'h',
              marker = list(color = '#16A4A7',line = list(color = '#16A4A7', width = 1.5))
      ) %>%
        layout(title = glue::glue("Gasto en armas de fuego, { ifelse(input$checkbox==TRUE,
         paste(input$state,paste(input$years[1],input$years[2], sep='-'), sep=' '),
         paste(input$state, input$year, sep=' ') ) }"),
               barmode = 'group',
               xaxis = list(title = "Dólares constantes de 2019"),
               yaxis = list(title = ""))
    })
    
    output$lineplot <- renderPlotly({
      
      LineOutputFunction() %>%
        plot_ly(x = ~ano, y = ~piezas, type = 'scatter', mode = 'lines+markers', color = I("#16A4A7"),
                marker = list(
                  color = "#16A4A7",
                  size = 10,
                  line = list(
                    color = "#16A4A7",
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
            color = viridis::inferno(length(unique(data$lab_cntry))),
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
        )
        })
  })