## NB Manually downloaded data was used due to API error
## Also checked that no exceptions
## (i.e. data from other surveys but t1) were needed

# Instructions ------------------------------------------------------------

## In 02_pull.R, run 1 up to survey_data
## Run 2 instead of survey_data <- .. in 1

# 1 -----------------------------------------------------------------------

requester <- "bosse"
data_type <- c("survey", "asa")
roles <- c("child")

on_child <- TRUE
pid_is_child <- TRUE

requested_asa_files <- "totals"
## Can make this depend on requested_asa_files
## if you only want rds when including asa files
file_type <- "rds"


# non_qualtrics_regex <- "grocery_shopping|family_meals"
requested_times <- tibble(
  phase = c(
    "3"
  ),
  time_point = c(
    "t1"
  )
)

scales <- NULL
specific_vars <- c("childcare",
                   "type_of_care",
                   "hrs_at_childcare",
                   "hrs_at_childcare_iamnotcomf",
                   "childcare_food_provider")

apply_calculations <- function(df){df}

# 2 -----------------------------------------------------------------------

ui <- here("data", "processed", "surveys",
           "qualtrics_ui_gfhs_survey.rds") %>% read_rds

raw_surveys <- here("data", "temp") %>%
  read_all_qualtrics_csv

raw_surveys <- raw_surveys %>%
  mutate(
    duplicate_names = map(
      data,
      ~ .x %>% names %>% .[duplicated(.)] %>% unique
    )
  )

raw_surveys <- raw_surveys %>%
  mutate(
    data = map2(
      data,
      duplicate_names,
      ~ coalesce_duplicate_names(.x, .y)
    )
  )

survey_data <- raw_surveys %>% deidentify_and_add(ui)


# Appendix ----------------------------------------------------------------

# survey_data %>%
#   select(matches("x1")) %>%
#   filter(if_any(everything(), ~.x %>% str_detect("comf"))) %>%
#   names %>%
#   str_remove("x1_") %>%
#   clipr::write_clip()

# ## unlabelled:
# survey_data %>%
#   names %>%
#   str_subset("_", negate = TRUE) %>%
#   str_subset("^[[:upper:]]")
