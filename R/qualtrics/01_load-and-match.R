source(here("R", "study-info", "ids.R"))

study_sv <- "gfhs"
survey_type <- "ahha"

# Functionalize from here -----------------------------------------------------------

# Import ------------------------------------------------------------------

df_qualtrics <- all_surveys() %>%
  rename(survey_name = name) %>%
  separate_survey_name("survey_name") %>%
  filter(
    study_sv == !!study_sv & survey_type == !!survey_type
     # &
     #  (
     #    (time_point_sv %in% c("t1", "t2") & phase_sv %in% "3") |
     #   (time_point_sv %in% c("t5") & phase_sv %in% "1")
     #  )
    )

nested_surveys <- df_qualtrics %>% import_all_data
nested_surveys <- nested_surveys %>% select_identifiers

mcfn <- nested_surveys %>% missing_child_first_name
assert_that(nrow(mcfn) == 0)

# Unnest and name clean ---------------------------------------------------

cleaned_surveys <- nested_surveys %>%
  clean_embedded_names %>%
  remove_duplicate_surveys %>%
  unnest_surveys %>%
  clean_person_names %>%
  remove_drop_outs_and_tests

# Match IDs ---------------------------------------------------------------

matched_surveys <- cleaned_surveys %>%
  match_ID(fid) %>%
  match_ID(pid)

matched_surveys %>% see_missing(fid)

## Partial matches to those with IDs
matched_surveys %>%
  partial_matches(suffix = "_sv") %>%
  arrange(name_sv)

matched_surveys %>% see_missing(pid)

## Remove unmatched

matched_surveys <- matched_surveys %>%
  filter(!mi(pid)) %>%
  arrange(pid, time_point_sv)

# Time cleaning -------------------------------------------------------------

timely_surveys <- matched_surveys %>%
  ## See exceptions.csv
  fix_time_points %>%
  remove_extra_cases %>%
  remove_rephased

## pid P/S not corresponding to parent_sv
pid_neq_pn(timely_surveys)

## fid not corresponding to phase_sv
fid_neq_ph(timely_surveys %>% add_phase)

timely_surveys %>% filter(pid == "P0091")

## The inequalities do not need correction

## Assertions
final_dups <- get_dups(timely_surveys)
assert_that(nrow(final_dups) == 0)

## Continue
timely_surveys %>% misordered_dates

# Pivot -------------------------------------------------------------------

row_per_child <- timely_surveys %>%
  rename_with(~ str_replace(.x,
                            "(.*)_(\\d)_(.*)",
                            "child_\\2_child_\\1_\\3")) %>%
  pivot_longer(starts_with("child"),
               names_pattern = "child_(.)_(.*)",
               names_to = c("child", ".value"))


row_per_child_matched <- row_per_child %>%
  clean_and_match_child %>%
  remove_duplicate_child_pid

checked_children_path <- here(
  "data",
  "processed",
  "surveys",
  "checked-names-for-missing-child-pid.rds"
  )

## Amelia is separate from Ellie, Ella from Olivia
# row_per_child_matched %>%
#   show_unmatched %>%
#   select(
#     study_sv,
#     survey_type,
#     pid,
#     time_point_sv,
#     parent_sv,
#     child,
#     fid
#   ) %>%
#   write_rds(checked_children_path)

checked_children <- read_rds(checked_children_path) %>%
  mutate(checked = 1)

unchecked_and_unmatched <- row_per_child_matched %>%
  full_join(
    checked_children,
    by = names(checked_children) %>%
      str_subset("checked", negate = TRUE)
  ) %>%
  filter(checked != 1) %>%
  show_unmatched

assert_that(unchecked_and_unmatched %>% nrow(.) == 0)
# unchecked_and_unmatched %>% View

## Assertions
dups_cpid <- row_per_child_matched %>% get_dups_c(child_pid)
assert_that(dups_cpid %>% nrow(.) == 0)
dups_cname <- row_per_child_matched %>% get_dups_c(child_fname)
assert_that(dups_cname %>% nrow(.) == 0)

## Continue
row_per_child_matched <- row_per_child_matched %>%
  remove_extra_rows # rows resulting from child per row pivot

warn_before_pivot_wider <- function(df, names_from, values_from, ...){
  id_cols <- df %>%
    select(
      everything(),
      - {{ names_from }}, ## values_from
      - {{ values_from }} ## names_from
    ) %>%
    names

  group_cols <- df %>% group_by(...) %>% group_vars
  non_group_cols <- df %>% names %>% .[!. %in% group_cols]

  extra_rows <- df %>%
    group_by(...) %>%
    mutate(
      distinct_rows = n_distinct(
        !!!non_group_cols
        )
    ) %>%
    filter(distinct_rows > 1)

  extra_rows

  if(nrow(extra_rows) > 0){
    warning(
      glue("Pivot wider will produce extra rows ",
      "because, in reference to pivot_wider documentation, ",
      "you unintentionally have id_cols ",
      "that, within group, are not unique per row."
      )
    )
  }

}

## Pre pivot check
row_per_child_matched %>%
  warn_before_pivot_wider(
    names_from = starts_with("child_"),
    values_from = child,
    pid,
    on_who,
    time_point
    )

matched <- row_per_child_matched %>%
  rename(child_number = child) %>%
  pivot_wider(
    names_from = child_number,
    values_from = starts_with("child_"),
    names_glue = "{.value %>%
    str_replace('child', paste0('child_', child_number))}",
    ) %>%
  de_identify %>%
  clean_names

# Export ------------------------------------------------------------------

matched %>%
  write_rds(
  here("data",
       "processed",
       "surveys",
       glue("qualtrics_ui_{study_sv}_{survey_type}.rds")
       )
)

# Strange survey case ------------------------------------------------------------
#
# here("data",
#      "processed",
#      "surveys",
#      glue("qualtrics_ui_gfhs_survey.rds")
# ) %>% read_rds %>%
#   filter(pid == "P0091") %>%
#   select(pid, time_point, phase_sv,
#          date_sv, progress, response_id)

### Note ----------------------------------------------------------------------
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
