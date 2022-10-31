study_sv <- "gfhs"
survey_type <- "survey"

## Frame
df_qualtrics <- all_surveys() %>%
  select(name, id) %>%
  separate_survey_name %>%
  filter(study_sv == !!study_sv & survey_type == !!survey_type)

# Start ------------------------------------------------------------------
nested_surveys <- df_qualtrics %>% import_all_data

df_deidentified <- nested_surveys %>% match_to_ui %>% de_identify

# Export ------------------------------------------------------------------


df_deidentified %>%
  select(path, file, data) %>%
  pwalk(
    ~ write.csv(
      ..3,
      file = paste0(..1, "qualtrics_deidentified/", ..2),
      row.names = FALSE
    )
  )
