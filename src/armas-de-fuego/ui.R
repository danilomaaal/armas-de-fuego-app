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
library(shinyWidgets)
library(shinythemes)
library(dplyr)
library(plotly)

# read data
PoliceFirearms <- read.csv(here::here("data/processed","compras_armas_final_web.csv"))

# theme
shinytheme("sandstone")

# user interface scheme
shinyUI(
  fluidPage(
    # this tag will override default shiny blue color in select inputs and tabs to a nicer purple
    tags$style(HTML("
    .js-irs-0 .irs-bar,
    .irs--shiny .irs-from,
    .irs--shiny .irs-to,
    .irs--shiny .irs-single,
    .irs--shiny .irs-bar--single {
    background-color:#280B54;
    border-top:none;
    border-bottom:none;
}
.nav-tabs > li > a {
  color: #280B54;
}")), titlePanel(title = "Armas de fuego distribuidas por la SEDENA a autoridades estatales en México"),
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
        prettyCheckbox("checkbox",
                       label="Periodo",
                       value=FALSE,
                       shape = "curve",
                       animation = "pulse"),
        helpText("Elige los filtros para mostrar datos a nivel nacional o estatal durante un periodo determinado o para un año en particular.
                 Selección actual:"),
        textOutput("textdata"),
        br(),
        helpText("TODO: ADD DESCRIPTION")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Flujo",
                     br(),
                     plotlyOutput("sankey",height = "auto")),
            tabPanel("Desglose",
                     uiOutput("treemap")),
            tabPanel("Costo",
                     br(),
                     plotlyOutput("barplot")),
            tabPanel("Tendencia",
                     br(),
                     plotlyOutput("lineplot"))
            
          )
        )
      )
    )
  )

