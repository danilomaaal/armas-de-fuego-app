#  ________________________________________
# / Hello. This is my first Shiny web app. \
# |    LOL. Here is the user interface     |
# \ definition.                            /
#  ----------------------------------------
#        \   ,__,
#         \  (oo)____
#            (__)    )\
#               ||--|| *

# import packages
library(shiny)
library(dplyr)
library(plotly)

# read data
PoliceFirearms <- read.csv(here::here("data/processed","compras_armas_final_web.csv"))

# theme
shinythemes::shinytheme("sandstone")

# user interface scheme
shinyUI(
  fluidPage(
    titlePanel(title = "Armas de fuego distribuidas por la SEDENA a autoridades estatales en México"),
    sidebarLayout(
      sidebarPanel(
        selectInput("state",
                    "Selecciona un Estado:",
                    choices = c("Nacional",unique(PoliceFirearms$estado))),
        conditionalPanel(
          condition = "input.checkbox==true",
          sliderInput("years",
                      "Selecciona un periodo:",
                      min = min(PoliceFirearms$ano),
                      max = max(PoliceFirearms$ano),
                      sep = "",
                      value = c(2006,2018),
                      step = 1)
        ),
        conditionalPanel(
          condition = "input.checkbox==false",
          sliderInput("year",
                      "Selecciona un año:",
                      min = min(PoliceFirearms$ano),
                      max = max(PoliceFirearms$ano),
                      sep = "",
                      value = 2006,
                      step = 1)
          ),
        checkboxInput("checkbox",label="Periodo",value=TRUE)
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("General",
                     uiOutput("treemap")),
            tabPanel("Costo",
                     plotlyOutput("barplot")),
            tabPanel("Tendencia",
                     plotlyOutput("lineplot")
            )
          )
        )
      )
    )
  )

