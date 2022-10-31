# Request-spec.: Format -------------------------------------------------------

requester <- "eckert"
focus <- c("parents", "children")
on_children <- FALSE
children_as_pid <- FALSE
file_type <- "csv"

# Request-spec.: Source-times -------------------------------------------------------

## Where all requested variables come from
data_type <- c("survey", "asa")

requested_asa_files <- "totals"
requested_times <- tibble(
  phase = c(rep("1", 3),
            rep("2", 3),
            rep("3", 4)),
  time_point = c(glue("t{4:6}"),
                 glue("t{3:5}"),
                 glue("t{1:4}"))
)

#  Request-spec.: Variables ---------------------------------------------------------------

## Functionalize specific_vars and scales
## (or only specific_vars once item labels are incorporated)
## Replace them with an imported file outputted by Shiny

specific_vars <- c("d_total",
                   "g_whole",
                   "pf_mps_total",
                   "prot",
                   "pf_soy",
                   "pf_nutsds",
                   "pf_legumes",
                   "g_total",
                   "f_total",
                   "v_total",
                   "f_juice")

scales <- c("family_functioning")


# Request-spec.: Calculations -----------------------------------------------------------

is_after <- function(x, y){
  case_when(
    {{ x }} > y ~ "Yes",
    {{ x }} <= y ~ "No",
    TRUE ~ NA_character_
  )
}

years_after <- function(x, y){
  time_length(
    interval(y, {{ x }}),
    "years"
  )
}

apply_calculations <- function(dat){
  dat <- dat %>%
    mutate(
      date_covid_19 = ymd("2020-03-15"),
      date_cfg_19 = ymd("2019-01-22"),
      dplyover::across2x(
        c(date_asa, date_survey),
        c(date_covid_19, date_cfg_19),
        .fns = list(during = ~.x %>% is_after(.y),
                    duration_in_yrs = ~ .x %>% years_after(.y)),
        .names = "{ycol}_{fn}_{xcol}",
        .names_fn = function(x) x %>% str_remove_all("date_")
      ),
      plant_based_protein = rowSums(across(c(pf_soy, pf_nutsds, pf_legumes))),
      relative_whole_grains = rowSums(across(c(g_whole, g_total))),
      relative_plant_based_protein = rowSums(across(c(plant_based_protein, prot))),
      fruit_and_vegetables = rowSums(across(c(f_total, v_total, f_juice)))
    )

}

