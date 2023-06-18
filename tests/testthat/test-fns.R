# vim set fileencoding=utf-8:

#---
# script name: test-fns.R
# purpose: test functions
# author: Daniel Mata <daniel.mata@flacso.edu.mx>
# date created:
# license: (c) DM, 2022, GPL v3
#---

# imports
library(testthat)
source(here::here("tests/functions.R"))

# data
PoliceFirearms <- read.csv(here::here("armas-de-fuego","compras_armas_final_web.csv"))


## testing filter_pstate
filter_pstate(PoliceFirearms,
              ano,
              2006, 
              2018,
              estado,
              "Guerrero",
              vars=c(marca, vendido_a_cliente),
              to_aggregate=c(no_piezas)) -> fn_output

PoliceFirearms %>%
  filter(!is.na(marca),
         ano >= 2006,
         ano <= 2018,
         estado=="Guerrero") %>%
  group_by(marca,vendido_a_cliente) %>%
  summarize(total_no_piezas=sum(no_piezas, na.rm = TRUE)) -> orig_output


test_that("filter_pstate function match original code",
          {
            expect_equal(object = fn_output, expected = orig_output)
            }
          )
          
## testing filter_pnational  
filter_pnational(PoliceFirearms,
                 ano,
                 2006,
                 2018,
                 vars=c(marca, vendido_a_cliente),
                 "no_piezas") -> fn_output

PoliceFirearms |>
  filter(!is.na(marca),
         ano >= 2006,
         ano <= 2018) |>
  group_by(marca,vendido_a_cliente) |>
  summarize(total_no_piezas=sum(no_piezas, na.rm = TRUE)) -> orig_output

test_that("filter_pnational function match original code",
          {
          expect_equal(object = fn_output, expected = orig_output)
          }
)


## testing filter_national
filter_national(PoliceFirearms,
                ano,
                2006,
                vars=c(marca, estado),
                to_aggregate=c(no_piezas)) -> fn_output


PoliceFirearms %>%
  filter(!is.na(marca),
         ano == 2006) %>%
  group_by(marca,estado) %>%
  summarize(total_no_piezas=sum(no_piezas, na.rm = TRUE)) -> orig_output


test_that("filter_national function match original code",
          {
            expect_equal(object = fn_output, expected = orig_output)
          }
)


## testing filter_state
filter_state(PoliceFirearms,
             ano,
             2010,
             estado,
             "CDMX",
             vars=c(marca,vendido_a_cliente),
             to_aggregate=c(no_piezas)) -> fn_output


PoliceFirearms %>%
    filter(!is.na(marca),
           ano == 2010,
           estado == "CDMX") %>%
    group_by(marca,vendido_a_cliente) %>%
    summarize(total_no_piezas=sum(no_piezas, na.rm = TRUE)) -> orig_output



test_that("filter_national function match original code",
          {
            expect_equal(object = fn_output, expected = orig_output)
          }
)










