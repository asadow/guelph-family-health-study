
study_sv <- "gfhs"
survey_type <- "ahha"

df_qualtrics <- all_surveys() %>%
  mutate(survey_name = name) %>%
  separate_survey_name %>%
  filter(
    study_sv == !!study_sv & survey_type == !!survey_type
  )

nested_surveys <- df_qualtrics %>% import

file <- glue("qualtrics_ui_{study_sv}_{survey_type}.rds")
ui <- here("data", "processed", "surveys", file) %>% read_rds

nested_surveys <- nested_surveys %>%
  rename_relabel_each_survey %>%
  deidentify_raw_data_and_add(ui)

nested_surveys %>%
  write_rds(
    here("data",
         "processed",
         "surveys",
         glue("{study_sv}_{survey_type}_nested.rds")
    )
  )


#
# # Exploring ---------------------------------------------------------------
#
#
# t <- nested_surveys %>%
#   deidentify_raw_and_add(ui)
#
# nested_surveys %>%
#   deidentify_raw_and_add(ui) %>%
#   filter(survey_name %>% str_detect("gfhs-3_survey_t1_parent-1")) %>%
#   select(data) %>%
#   unnest(data) %>% View
#
#
# select(data) %>%
#   unnest(data, names_repair = make.unique) %>%
#   select(matches("sdq"))
#
#
