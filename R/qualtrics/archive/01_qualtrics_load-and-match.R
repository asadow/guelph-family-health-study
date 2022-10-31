source(here("R", "ids.R"))
source(here("R", "qualtrics", "functions_qualtrics.R"))

# Import ------------------------------------------------------------------

df_qualtrics <- all_surveys() %>%
  separate_survey_name %>%
  filter(
    study_sv == "gfhs" & survey_type == "survey"
    # time_point_sv %in% c("t1", "t2") & phase_sv %in% c("1", "2", "3")
    )

nested_surveys <- df_qualtrics %>% import_embedded_data



#
# reprex::reprex({
# library(qualtRics)
# df_qualtrics <- all_surveys()
# sq <- survey_questions(df_qualtrics$id[2])
# sq
# attributes(sq)
# }
# )

# Unnest and name clean ---------------------------------------------------

nested_surveys <- nested_surveys %>%
  add_child_fname_cols %>% # Creates first_name_cols variable
  check_child_fname_cols_exist # Check we grabbed cols from surveys that have them

surveys <- nested_surveys %>%
  clean_embedded_names %>%
  remove_duplicate_surveys %>%
  unnest_surveys %>%
  clean_person_names %>%
  remove_drop_outs_and_tests

# Match IDs ---------------------------------------------------------------

surveys <- surveys %>%
  match_ID(fid) %>%
  match_ID(pid)

surveys %>% see_missing(fid)

surveys %>%
  partial_matches(suffix = "_sv") %>%
  arrange(name_sv)

surveys %>% see_missing(pid)

# One can use below to search for
# variant names in master list
master %>%
  filter(
    if_any(
      contains("name"),
      ~ str_detect(.x, "Bren")
    )
  ) %>% select(name, pid)

## Remove unmatched

surveys <- surveys %>% filter(!mi(pid))


# Time cleaning -------------------------------------------------------------

surveys <- surveys %>%
  arrange(pid, time_point_sv) %>%
  fix_time_points %>% ## See exceptions.csv
  remove_extra_cases %>% ## See exceptions.csv, # why again?
  remove_rephased

## pid P/S not corresponding to parent_sv
pid_neq_pn(surveys)

## fid not corresponding to phase_sv
fid_neq_ph(surveys)

## The inequalities do not need correction

final_dups <- get_dups(surveys)
stopifnot(nrow(final_dups) == 0)

surveys %>% misordered_dates


# Pivot -------------------------------------------------------------------


surveys <- surveys %>%
  pivot_to_child_per_row(child_name_regex) %>%
  check_extra_child_names %>%
  add_child_name(child_name_regex) %>%
  replace_child_fnames(ls = fid_sv_master) %>%
  match_child

surveys %>% show_unmatched
surveys %>% get_dups_c

surveys <- surveys %>% remove_duplicate_child_pid

dups_c <- get_dups_c(surveys)

stopifnot(nrow(dups_c) == 0)

surveysl <- surveys %>%
  mutate(
    progress_sv = progress,
    child = child %>% str_extract("\\d")
  ) %>%
  de_identify

surveysl <- surveysl %>%
  clean_names %>%
  clean_not_on_child %>%  # removes unneeded rows resulting from child per row pivot
  add_phase

surveysl %>%
  write_rds(
  here("data", "processed", "qualtrics_ui.rds")
)


### Note ----------------------------------------------------------------------
## Children not in the study
## Regarding data on children not in the study,
## whether they can join later
## and whether to use earlier data
## from when they were not in the study:
### Thu Apr 14 13:34:33 2022
## Email from Maddy:
## In terms of using it [their data] later, that is a good question.
## Periodically parents will tell me that they have another
## child that is now in our age range and at this point
## I continue to tell them that we cant include them
## because of the intervention.
## This is something that Jess would have to decide
## and then we could enroll a bunch more children!
