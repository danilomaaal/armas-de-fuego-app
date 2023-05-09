# vim set fileencoding=utf-8:

#---
# file name: server.R
# purpose: runs shiny app server
# author: Daniel Mata <daniel.mata@flacso.edu.mx>
# date created: 02-11-2021
# license: (c) DM, 2021, GPL v2
#---

# packages
library(shiny)
library(reticulate)
library(tidyr)

# local python config and imports
use_virtualenv(virtualenv = here::here("env/"), required = TRUE)

# python imports
px <- import("plotly.express")
py_plotly <- import("plotly")

# read data
PoliceFirearms <- read.csv(here::here("armas-de-fuego","compras_armas_final_web.csv"))

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
      available  <- SankeyOutputFunction() |>
        nrow()
  
        if(available == 0) {
          paste0("Sin datos para ", input$state, " en ",input$year)
        }else{
          paste0("Datos a nivel ", input$state, " para el año ",input$year,".")
        }
             
      } else{
        paste0("Datos a nivel ", input$state," para el periodo ", paste(input$years[1], input$years[2], sep="-"),".")
        }
    })
  
  # plot filter functions
    BarOutputFunction <- reactive({
	    if(input$state=="Nacional" & input$checkbox==TRUE){
		PoliceFirearms |>
		    filter_pnational(ano, input$years[1], input$years[2], marca, en_pesos_2019)
      } else if(input$state!="Nacional" & input$checkbox==TRUE){
		PoliceFirearms |>
		  filter_pstate(ano, input$years[1], input$years[2], estado, input$state, marca, en_pesos_2019)
      } else if(input$state=="Nacional" & input$checkbox==FALSE){
		PoliceFirearms |>
		  filter_national(ano, input$year, marca, en_pesos_2019)
      } else {
          PoliceFirearms |>
            filter_state(ano, input$year, estado, input$state, marca,
                         en_pesos_2019)
	    }
      })
    
    TreemapOutputFunction <- reactive({
      
      if (input$state=="Nacional" & input$checkbox==TRUE) {
        PoliceFirearms |>
	mutate(estado="Nacional") |>
	filter_pnational(ano, input$years[1], input$years[2],
			 vars = c(estado, tipo_es, pais_origen_empresa, marca, calibre),
				to_aggregate = c(en_pesos_2019, no_piezas))
      } else if(input$state!="Nacional" & input$checkbox==TRUE){
	      PoliceFirearms |>
	      filter_pstate(ano, input$years[1], input$years[2], estado, input$state, 
			    vars=c(estado, tipo_es, pais_origen_empresa, marca, calibre),
		            to_aggregate=c(en_pesos_2019, no_piezas)) 

      } else if (input$state=="Nacional" & input$checkbox==FALSE){
        PoliceFirearms |>
        mutate(estado="Nacional") |>
        filter_national(ano, input$year,
		      vars=c(estado, tipo_es, pais_origen_empresa, marca, calibre),
		      to_aggregate=c(en_pesos_2019, no_piezas))

        } else {
          PoliceFirearms |>
            filter_state(ano, input$year, estado, input$state,
			 vars=c(estado, tipo_es, pais_origen_empresa, marca, calibre),
			 to_aggregate=c(en_pesos_2019,no_piezas))

        }
      })
    
    
    SankeyOutputFunction <- reactive({
      
      if(input$state=="Nacional" & input$checkbox==TRUE) {
	      PoliceFirearms |>
	      filter_pnational(ano, input$years[1], input$years[2], vars=c(marca,estado), to_aggregate=c(no_piezas))
      } else if(input$state!="Nacional" & input$checkbox==TRUE){
	      PoliceFirearms |>
	      filter_pstate(ano, input$years[1], input$years[2], estado, input$state, vars=c(marca, vendido_a_cliente), to_aggregate=c(no_piezas))
      } else if (input$state=="Nacional" & input$checkbox==FALSE){
	      PoliceFirearms |>
	      filter_national(ano, input$year, vars=c(marca, estado),to_aggregate=c(no_piezas))
      } else {
	      PoliceFirearms |>
	      filter_state(ano, input$year, estado, input$state, vars=c(marca,vendido_a_cliente), to_aggregate=c(no_piezas))
      }
    })
    
    LineOutputFunction <- reactive({
      if(input$state!="Nacional"){
        PoliceFirearms %>%
          filter(ano >= input$years[1],
             ano <= input$years[2],
             estado == input$state) %>%
          group_by(ano) %>%
          summarize(costo=sum(en_pesos_2019, na.rm = TRUE),
                    piezas=sum(no_piezas, na.rm = TRUE))
        } else {
          PoliceFirearms %>%
            filter(ano >= input$years[1],
                   ano <= input$years[2]) %>%
            group_by(ano) %>%
            summarize(costo=sum(en_pesos_2019, na.rm = TRUE),
                      piezas=sum(no_piezas, na.rm = TRUE))
          }
      })
    

    # ----------- regular functions -----------

# filtering functions
    # state by period
    filter_pstate <- function(data, year, year_beggin, year_end, state, selected_st, vars, to_aggregate) {
      data |>
        filter(if_any({{ vars }}, ~ !is.na(.x) ),
                {{ year }} >= {{ year_beggin }},
                {{ year }} <= {{ year_end }},
                {{ state }} == {{ selected_st }} ) |>
        group_by(across({{ vars }})) |>
        summarize(across({{ to_aggregate }}, ~ round(sum(.x, na.rm = TRUE), 2), .names = "total_{.col}" ) ) -> filtered_data

      return(filtered_data)
    }
    # national by period
    filter_pnational <- function(data, year, year_beggin, year_end, vars, to_aggregate) {
      data |>
        filter(if_any({{ vars }}, ~ !is.na(.x) ),
               {{ year }} >= {{ year_beggin }},
               {{ year }} <= {{ year_end }}) |>
        group_by(across({{ vars }})) |>
        summarize(across( {{ to_aggregate }}, ~ round(sum(.x, na.rm = TRUE), 2), .names = "total_{.col}" )) -> filtered_data

      return(filtered_data)
    }
    # state by year
    filter_state <- function(data, year, selected_yr, state, selected_st, vars, to_aggregate) {
      data |>
        filter(if_any({{ vars }}, ~ !is.na(.x) ),
               {{ year }} == {{ selected_yr }},
               {{ state }} == {{ selected_st }} ) |>
        group_by(across( {{ vars }} )) |>
        summarize(across( {{ to_aggregate }}, ~ round(sum(.x, na.rm = TRUE), 2), .names = "total_{.col}" )) -> filtered_data

      return(filtered_data)
    }
    # national by year
    filter_national <- function(data, year, selected_yr, vars, to_aggregate) {
      data |>
        filter( {{ year }} == {{ selected_yr }}) |>
        group_by(across({{ vars }})) |>
        summarize(across( {{ to_aggregate }}, ~ round(sum(.x, na.rm = TRUE), 2), .names = "total_{.col}" )) -> filtered_data

      return(filtered_data)
    }

#   __________________________________________________________
#  / Note to self:                                            \
# | Ploty uses numbers to represent nodes in sankey plots,     |
# | the next function is important bc dynamically generates    |
#  \  labels for each node.                                   /
#   ----------------------------------------------------------
#        \   ,__,
#         \  (oo)____
#            (__)    )\
#              ||--|| *

TransformSankeyData <- function(data){
# format columns one and two as factors,
# generate copies of the formatted columns
# and place them in places four and five of the data frame:

  data[,4:5] <- as.data.frame(lapply(data[,1:2], as.factor))

# by removing the class from a factor variable,
# R automatically converts it to a numerical one, i.e.
# each category is assigned a unique number consecutively:

  data <- as.data.frame(lapply(data, unclass))

# now let´s change the names of the columns according to the
# functions they´ll fulfill for the Sankey:

  names(data) <- c("lab_making","lab_state","values","source","target")

# because we used the lapply function, columns 4 and 5 were transformed
# to string format, which is not readable for plotly. So let's convert them back to numeric format:

  data[,4:5] <- as.data.frame(lapply(data[,4:5], as.numeric))

# The following is to make a small transformation, bc plotly sankeys
# use consecutive numbers to assign the nodes, we need to avoid that the numbers
# of the two categories overlap. So we take the maximum value of the source
# and add it to each of the values of the target,
# e.g. if the maximum in the source is 29 then
# target: 10 + 29 --->> 39
# target: 2 + 29 --->> 31

  data$target <- data$target + max(data$source, na.rm = TRUE)

# now let's extract the labels, we use the function arrange so that
# all of them are arranged consecutively:

  data |>
      dplyr::arrange(source) |>
      pull(lab_making) -> makings

# the same for state labels:

  data |>
      dplyr::arrange(target) |>
      pull(lab_state) -> state

#TODO:fix mislabel bug in this function
# return list with generated tags and data frame
  transformedData <- list(data,makings,state)
  return(transformedData)
}

    #TODO: replace this
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
    # gen palette
    pypalette <- grDevices::colorRampPalette(c(pycolors[7],pycolors[1]))
    
    
    # ----------- render functions -----------
    
    
     output$treemap <- renderUI({
      
      data <- TreemapOutputFunction()

      HTML(
        py_plotly$offline$plot(
          py_plotly$graph_objects$Figure(
            px$treemap(
              data_frame = data,
              path = c("estado","tipo_es","pais_origen_empresa","marca","calibre"),
              values = "total_no_piezas",
              color = "total_en_pesos_2019",
              color_continuous_scale=pycolors,
              color_continuous_midpoint=weighted.mean(data$total_en_pesos_2019,data$total_no_piezas)
            )
          )$update_layout(title_text = SetTitles("Detalle del flujo: ")),
          output_type = "div"
          )
      ) 
    })
    

    output$barplot <- renderPlotly({
      
      BarOutputFunction() %>%
      plot_ly(x = ~total_en_pesos_2019, y = ~reorder(marca, total_en_pesos_2019), type = "bar", orientation = "h",
              marker = list(color = pycolors[3],line = list(color = pycolors[3], width = 1.5))
      ) %>%
        layout(title = SetTitles("Gasto en armas de fuego: "),
               barmode = "group",
               xaxis = list(title = "Pesos constantes de 2019"),
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
          title = glue::glue("Gasto y total de armas de fuego distribuidas: {paste0(input$state,' ',input$years[1],'-',input$years[2]) }"),
          yaxis = list(title = "Pesos constantes de 2019"),
          hovermode = "x unified",
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
          xaxis = list(title = "Año"),
          yaxis = list(title = "Total de armas"),
          margin = list(l = 65),
          hovermode = "x unified",
          autosize = TRUE)
      
      subplot(costos,total,nrows = 2, shareX = TRUE, shareY = FALSE, titleY=TRUE)
      
    })
    
    output$sankey <- renderPlotly({
      
      data <- SankeyOutputFunction() 
      
      data <- TransformSankeyData(data)
      
    # Note: the initial empty string "" concatenated to unique 
    #  labels from makes and states is for compensation, 
    # remove it and the tags will appear one space
    # out of place, not sure why. Best guess: since plotly is
    # based on d3.js, it starts enumerating from 0 while R starts from 1 (?)
      nodelabels <- c("", unique(data[[2]]), unique(data[[3]]))
        
        plot_ly(
          type = "sankey",
          orientation = "h",
          node = list(
            label = nodelabels,
            color = ifelse(nodelabels == "No especificado",
                           pycolors[1], pypalette(70)),
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
        ) |>
          layout(title = SetTitles("Flujo legal de armas de fuego: ") )
    
      })
    
  })
