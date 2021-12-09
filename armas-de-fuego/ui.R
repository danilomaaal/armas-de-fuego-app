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
PoliceFirearms <- read.csv(here::here("compras_armas_final_web.csv"))

# theme
shinytheme("sandstone")

# user interface scheme
shinyUI(
  fluidPage(
    # this tag will override default shiny blue color in select inputs and tabs to a nicer green
    tags$style(HTML("
    .js-irs-0 .irs-bar,
    .irs--shiny .irs-from,
    .irs--shiny .irs-to,
    .irs--shiny .irs-single,
    .irs--shiny .irs-bar--single {
    background-color:#029B9E;
    border-top:none;
    border-bottom:none;
}
.nav-tabs > li > a {
  color: #029B9E;
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
        helpText(HTML("Elige los filtros para mostrar datos a nivel nacional o estatal durante un periodo determinado o para un año en particular.<br>
                 Selección actual:")),
        textOutput("textdata")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Flujo",
                     br(),
                     plotlyOutput("sankey",height = "auto")),
            tabPanel("Desglose",
                     uiOutput("treemap"),
                     helpText(
                       HTML("Haz click sobre un cuadro para ver en detalle.<br>
                            Nota: Costo en pesos de 2019.<br>")
                       )
                     ),
            tabPanel("Costo",
                     br(),
                     plotlyOutput("barplot")),
            tabPanel("Tendencia",
                     br(),
                     plotlyOutput("lineplot"))
            
          )
        )
      ),
    helpText(
      HTML("
            <h4>¿Qué es esto?</h4>
             En febrero de 2019 un tribunal local en Stuttgart, Alemania, condenó a dos empleados de la empresa de armas
        Heckler & Koch (H&K) por haber exportado -con documentación falsa- unos
        <a href='https://www.dw.com/es/supremo-de-alemania-ratifica-condenas-a-heckler-koch-por-venta-ilegal-de-armas-a-m%C3%A9xico/a-57062498'>
        4,219 rifles de asalto G36 a México entre 2006 y 2009</a>.
        
        Esta acción buscaba eludir una prohibición del gobierno alemán para exportar armas de fuego a zonas donde
        las autoridades locales cometieran violaciones de derechos humanos. En el caso de México,
        esta prohibición se encontraba vigente para los estados de Chiapas, Chihuahua, Guerrero y Jalisco.<br>
        <br>
        Este acto no ocurrió exclusivamente por responsabilidad de la empresa alemana, en México la SEDENA es la única autoridad facultada para importar armas de fuego.
        De acuerdo con declaraciones de testigos, <a href='https://www.rosalux.org.mx/juicio-heckler'> el personal de H&K sugirió a la SEDENA que
        en la documentación evitara mencionar a estos estados o que los substituyera con los nombres de otros</a>.
        Las omisiones antes señaladas salieron a la luz debido a que algunos de estos rifles terminaron
        en manos de los policías municipales de Iguala, Guerrero, que participaron en la desaparición de los normalistas de Ayotzinapa el 26 de septiembre de 2014.<br>
        <br>
        Esta app busca mostrar mediante visualizaciones los datos de algunas de esas transferencias. Se pueden consultar los datos de esos
        estados particulares. Si bien el tráfico ilegal de armas de fuego es un grave problema en México,
        es importante también mejorar/transparentar los mecanismos nacionales de distribución de armas para asegurar que se cumpla con los criterios de 
        las licencias de usuario final. En la pestaña marcada como <b>flujo</> podrás observar que en algunas transancciones en las que no se especificó 
        la autoridad final que recibió el arma.
                 "))
    )
  )

