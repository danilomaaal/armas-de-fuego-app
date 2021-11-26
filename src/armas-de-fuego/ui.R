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
        helpText(
        HTML("
        En febrero de 2019 un tribunal local en Stuttgart, Alemania, condenó a dos empleados de la empresa de armas
        Heckler & Koch (H&K) por haber exportado rifles de asalto G36 a México entre 2006 y 2009, usando documentación falsa.
        Esta acción buscaba eludir la prohibición del gobierno alemán de exportar armas de fuego a zonas donde
        las autoridades locales cometieran violaciones de derechos humanos. En el caso de México,
        esta prohibición se encontraba vigente para los estados de Chiapas, Chihuahua, Guerrero y Jalisco.<br>
        <br>
        Esta omisión ocurrió exclusivamente del lado alemán, de acuerdo con las declaraciones de testigos,
        el personal de H&K sugirió a la SEDENA que en la documentación evitara mencionar a estos estados
        o que los substituyera con los nombres de otros. Estas omisiones salieron a la luz debido a que
        algunos de estos rifles terminaron en manos de los policías municipales de Iguala, Guerrero, que participaron
        en el ataque a los normalistas de Ayotzinapa el 26 de septiembre de 2014.<br>
        <br>
        Si bien el tráfico ilegal de armas de fuego es un grave problema en México, es importante también mejorar/transparentar
        los mecanismos legales de distribución de armas que concentra la SEDENA.
                 "))
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

