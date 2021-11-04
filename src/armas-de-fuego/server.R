#  _______________________________
# < Hi. This is the server logic. >
#  -------------------------------
#        \   ,__,
#         \  (oo)____
#            (__)    )\
#               ||--|| *

# import packages
library(shiny)


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
    output$barplot <- renderPlotly({
      
      BarOutputFunction() %>%
      plot_ly(x = ~cost, y = ~reorder(marca, cost), type = 'bar', orientation = 'h',
              marker = list(color = '#3d405b',line = list(color = '#3d405b', width = 1.5))
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
        plot_ly(x = ~ano, y = ~piezas, type = 'scatter', mode = 'lines+markers', color = I("#3d405b"),
                marker = list(
                  color = "#3d405b",
                  size = 10,
                  line = list(
                    color = "#3d405b",
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
    
  })