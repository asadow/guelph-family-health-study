source(here("R", "qualtrics", "functions_qualtrics.R"))

df_qualtrics <- all_surveys() %>%
  separate_survey_name %>%
  filter(time_point_sv %in% c("t1", "t7") & phase_sv %in% c("1", "2")) %>%
  import_all_data

nested_surveys <- df_qualtrics %>% deidentify_each

qp <- here("data", "raw", "qualtrics")

dir_create(qp)

file_path <- here(qp, "df_qualtrics.rds")

## Exports

nested_surveys %>%
  select(data, id) %>%
  pwalk(
    ~ write_rds(
      ..1,
      glue(
        here(qp, "{..2}.rds")
        )
    )
  )


df_qualtrics %>%
  select(- data) %>%
  write_rds(file_path)


