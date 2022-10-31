df_qualtrics <- all_surveys() %>%
  mutate(survey_name = name) %>%
  separate_survey_name %>%
  filter(
    ## objects are from 01_load-and-match.R
    study_sv == !!study_sv & survey_type == !!survey_type
  )

nested_surveys <- df_qualtrics %>% import_all_data

ui <- here("data",
           "processed",
           "surveys",
           "qualtrics_ui_gfhs_survey.rds") %>% read_rds

# ui %>%
#   filter(fid == "104" & time_point == "t4") %>%
#   select(pid, time_point, starts_with("child"))

# Fix names ---------------------------------------------------------------

## Duplicate names are caused by duplicate labels for questions
## whose response options were split into two questions
nested_surveys <- nested_surveys %>%
  mutate(
    duplicate_names = map(
      data,
      ~ .x %>% names %>% .[duplicated(.)] %>% unique
    )
  )

nested_surveys_corrected <- nested_surveys %>%
  mutate(
    data = map2(
      data,
      duplicate_names,
      ~ coalesce_duplicate_names(.x, .y)
      )
  )

# nested_surveys_corrected %>% select(phase_sv:parent_sv) %>% arrange(phase_sv, time_point_sv) %>% View

nested_surveys_deidentfied <- nested_surveys_corrected %>% deidentify_and_add(ui)

nested_surveys_deidentfied %>%
  write_rds(
    here("data",
         "processed",
         "surveys",
         glue("{study_sv}_{survey_type}_nested.rds")
    )
  )


# Archive -----------------------------------------------------------------



# # Unnest, match, export -------------------------------------------------
#
# surveys <- nested_surveys_corrected %>% unnest(data)
#
# which_survey <- intersect(names(ui), names(surveys))
#
# surveys <- ui %>%
#   inner_join(
#     surveys,
#     by = c("response_id" = "Response ID",
#            all_of(which_survey))
#   ) %>%
#   de_identify
#
# surveys %>%
#   write_rds(
#     here("data",
#          "Clean Data",
#          glue("{study_sv}_{survey_type}.rds")
#          )
#     )
