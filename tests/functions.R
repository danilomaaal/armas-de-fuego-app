library(tidyr)
library(dplyr)

filter_pstate <- function(data, year, year_beggin, year_end, state, selected_st, vars, to_aggregate) {
  data |>
    filter(if_all({{ vars }}, ~ !is.na(.x) ),
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
    filter(if_all({{ vars }}, ~ !is.na(.x) ),
           {{ year }} >= {{ year_beggin }},
           {{ year }} <= {{ year_end }}) |>
    group_by(across({{ vars }})) |>
    summarize(across( {{ to_aggregate }}, ~ round(sum(.x, na.rm = TRUE), 2), .names = "total_{.col}" )) -> filtered_data
  
  return(filtered_data)
}

# state by year
filter_state <- function(data, year, selected_yr, state, selected_st, vars, to_aggregate) {
  data |>
    filter(if_all({{ vars }}, ~ !is.na(.x) ),
           {{ year }} == {{ selected_yr }},
           {{ state }} == {{ selected_st }} ) |>
    group_by(across( {{ vars }} )) |>
    summarize(across( {{ to_aggregate }}, ~ round(sum(.x, na.rm = TRUE), 2), .names = "total_{.col}" )) -> filtered_data
  
  return(filtered_data)
}
# national by year
filter_national <- function(data, year, selected_yr, vars, to_aggregate) {
  data |>
    filter(if_all({{ vars }}, ~ !is.na(.x) ),
           {{ year }} == {{ selected_yr }}) |>
    group_by(across({{ vars }})) |>
    summarize(across( {{ to_aggregate }}, ~ round(sum(.x, na.rm = TRUE), 2), .names = "total_{.col}" )) -> filtered_data
  
  return(filtered_data)
}



