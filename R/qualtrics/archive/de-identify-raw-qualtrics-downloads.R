sv_ui <- vroom(
  paste0(cle_p, "svw_matched.csv"),
  col_types = c(.default = "c")
)

## revamp this function
# df_files <- files_as_tibble(sv_p)

## should be the path to raw Qualtrics downloads
directory_path <- here("data", "Raw Data", "SV")

df_data <- directory_path %>% read_all_qualtrics_csv

df_deidentified <- df_data %>%
  mutate(
    data = map(
      data,
      ~ sv_ui %>%
        clean_names %>%
        right_join(
          .x,
          by = c("response_id" = "Response ID")
        ) %>%
        select(
          - c(starts_with("Recipient"), starts_with("Location"), "IP Address")
        )
    )
  )

df_deidentified %>%
  select(path, file, data) %>%
  pwalk(
    ~ write.csv(
      ..3,
      file = paste0(..1, "qualtrics_deidentified/", ..2),
      row.names = FALSE
      )
    )
