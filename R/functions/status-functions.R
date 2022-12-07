# Status of Data -------------------------------------------------------
## survey: isActive - whether survey is open or closed

qualtrics_api_credentials(
  api_key = key_get("QUALTRICS_API_KEY"),
  base_url = key_get("QUALTRICS_BASE_URL")
)

survey_info <- all_surveys() %>%
  select(name, isActive) %>%
  separate_survey_name

survey_export_info <- here("data",
                           "processed",
                           "surveys",
                           "gfhs_survey_nested.rds") %>%
  file_info %>%
  mutate(
    date_downloaded = as_date(birth_time),
    data_type = "survey"
  ) %>%
  select(date_downloaded, data_type)


## asa: birth_time - file creation date
asa_file_info <- here("data", "raw", "diet-asa") %>%
  dir_ls %>%
  file_info

asa_path_to_names <- function(df){
  df %>%
    mutate(
      file = path %>% path_file,
      time_point = sub(".*(t\\d{1}).*", "\\1", file),
      phase = sub(".*phase_(\\d{1}).*", "\\1", file),
      role = sub(".*phase_\\d{1}_(parents|children).*", "\\1", file),
      asa_year = sub(".*(\\d{4})_.*", "\\1", file),
      asa_type = sub(".*\\d{4}_(.*)$", "\\1", file)
    ) %>%
    select(-file)
}

asa_file_info <- asa_file_info %>%
  asa_path_to_names %>%
  mutate(data_type = "asa",
         birth_time = as_date(birth_time)) %>%
  select(data_type, birth_time,
         time_point, phase,
         role, asa_year, asa_type) %>%
  rename(date_downloaded = birth_time)

asa_cleaned <- here("data", "processed", "diet-asa",
                    "asa_cleaned_summary.csv") %>%
  read_csv(col_types = c(.default = "c")) %>%
  mutate(review_status = "reviewed", data_type = "asa")

survey_file_info <- here("data",
                         "raw",
                         "surveys",
                         "archive",
                         "gfhs-123_survey_deidentified_qualtrics") %>%
  dir_ls %>%
  file_info

survey_file_info <- survey_file_info %>%
  mutate(survey_name = path %>%
           path_file %>%
           str_remove("\\.csv"))

print_and_capture <- function(x)
{
  paste(
    capture.output(print(x)),
    collapse = "\n"
  )
}

status_update <- function(requested_data){

  ## Data availability
  da <- here("data", "manually-entered", "data_status.csv") %>%
    read_csv(col_types = cols(.default = "c")) %>%
    rename(collection_status = status) %>%
    rename_with(~ .x %>% str_replace("sv", "survey"))

  ## Rename and pivot
  da_long <- da %>%
    rename_with(~ glue({"{.x}_source"}), 5:last_col()) %>%
    pivot_longer(
      cols = ends_with("_source"),
      names_pattern = "(.*)_source",
      names_to = "data_type",
      values_to = "collected"
    )

  matching_vars <- c("phase", "time_point", "data_type")

  ## Join with requested_data
  request_status <- da_long %>%
    right_join(
      requested_data,
      by = matching_vars
    ) %>%
    select(- collected)

  ha_cleaned <- da %>%
    rename(review_status = ha) %>%
    filter(review_status == "reviewed") %>%
    mutate(data_type = "ha") %>%
    select(phase, time_point, review_status, data_type)

  unreviewed <- request_status %>%
    ## Surveys are never reviewed
    filter(data_type != "survey") %>%
    left_join(asa_cleaned %>% bind_rows(ha_cleaned), by = matching_vars) %>%
    mutate(review_status = if_else(is.na(review_status),
                                   "unreviewed",
                                   review_status)
    ) %>%
    filter(review_status == "unreviewed") %>%
    select(- review_status)

  ## Create tibbles of incomplete and unreviewed statuses
  incomplete <- request_status %>%
    filter(!collection_status %in% c("complete", "N/A")) %>%
    left_join(asa_file_info, by = matching_vars) %>%
    mutate(
      date_downloaded = case_when(
        data_type == "survey" ~ survey_export_info$date_downloaded,
        TRUE ~ date_downloaded)
    )

  open_surveys <- survey_info %>%
    right_join(
      request_status,
      by = c("phase_sv" = "phase",
             "time_point_sv" = "time_point",
             "survey_type" = "data_type"
      )
    ) %>%
    filter(isActive == TRUE) %>%
    select(
      phase_sv, time_point_sv, collection_status, everything(),
      ## Remove columns like on_who if they are empty and unneeded
      where(
        ~ !all(is.na(.x))),
      - isActive
    ) %>%
    rename_with(~ .x %>% str_replace("_sv", "")) %>%
    mutate(date_downloaded = survey_export_info$date_downloaded) %>%
    arrange(phase, time_point)

  ## Warning
  if(nrow(incomplete) > 0){
    warning("\nSome data is incomplete:\n", print_and_capture(incomplete))
  }
  if(nrow(unreviewed) > 0){
    warning("\n\nSome data is unreviewed:\n", print_and_capture(unreviewed))
  }
  if(nrow(open_surveys) > 0){
    warning("\n\nSome surveys are still open to participants:\n", print_and_capture(open_surveys))
  }

  List(incomplete, unreviewed, open_surveys)

  #
  #   ## Surveys that need to be downloaded/dated
  #   surveys <- here("data",
  #                   "processed",
  #                   "surveys",
  #                   "gfhs_survey_nested.rds") %>%
  #     read_rds
  #
  #   api_and_files <- surveys %>%
  #     left_join(survey_file_info, by = "survey_name")
  #
  #   errored_surveys <- api_and_files %>%
  #     filter(data == "Error") %>%
  #     select(- data)
  #
  #   errored_surveys <- errored_surveys %>%
  #     left_join(
  #       request_status,
  #       by = c("phase_sv" = "phase",
  #              "time_point_sv" = "time_point",
  #              "survey_type" = "data_type"
  #       )
  #     ) %>%
  #     select(date_completed, survey_name)
  #
  #   warning(
  #     "\nSome surveys need to be downloaded/updated:\n",
  #     print_and_capture(errored_surveys)
  #   )

}


write_status_for <- function(status, requester){
  names(status) <- names(status) %>% str_replace("_", "-")
  status %>%
    iwalk(
      ~ if(nrow(.x) > 0) {
        write_csv(
          .x,
          here(
            "data",
            "processed",
            "requested",
            glue("{requester}_status_{.y}_{Sys.Date()}.csv")
          )
        )
      }
    )
}


## Qualtrics API-accessed surveys filtered by needed surveys
api_surveys <- function(needed){
  all_surveys() %>%
    select(name, id) %>%
    separate_survey_name %>%
    right_join(
      needed,
      by = c("phase_sv" = "phase",
             "time_point_sv" = "time_point")
    )
}
