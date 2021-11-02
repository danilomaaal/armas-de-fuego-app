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
    
    # filter year and state
    FilterYearState <- reactive({
      PoliceFirearms %>%
        filter(ano >= input$year[1],
               ano <= input$year[2],
               estado == input$state) %>%
        group_by(marca) %>%
        summarize(cost=round(sum(en_dolares_2019,na.rm = TRUE),2),
                  piezas=sum(no_piezas, na.rm = TRUE))
    })
    
    # ----------- render functions -----------
    output$barplot <- renderPlotly({
      FilterYearState() %>%
      plot_ly(x = ~cost, y = ~reorder(marca, cost), type = 'bar', orientation = 'h',
              marker = list(color = '#3d405b',line = list(color = '#3d405b', width = 1.5))
      ) %>%
        layout(title = glue::glue("Gasto en armas de fuego, { ifelse(input$checkbox==TRUE,
         paste(input$state,paste(input$year[1],input$year[2], sep='-'), sep=' '),
         paste(input$state, input$yearly, sep=' ') ) }"),
               barmode = 'group',
               xaxis = list(title = "Dólares constantes de 2019"),
               yaxis = list(title = ""))
    })
    
  })