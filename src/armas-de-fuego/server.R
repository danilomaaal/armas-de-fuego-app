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
shinyServer(
  function(input, output) {
    # code goes here
  }
)