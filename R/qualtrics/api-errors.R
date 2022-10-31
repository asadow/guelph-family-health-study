surveys_read <- here("data",
                "processed",
                "surveys",
                "gfhs_survey_nested.rds") %>%
  read_rds

#
# qualtrics_api_credentials(api_key = key_get("QUALTRICS_API_KEY"),
#                           base_url = key_get("QUALTRICS_BASE_URL"),
#                           install = TRUE,
#                           overwrite = TRUE
#                           )
# readRenviron("~/.Renviron")

surveys <- all_surveys() %>% select(name, id)

id_subset <- surveys %>%
  separate_survey_name %>%
  filter(phase_sv %in% "3"
         & time_point_sv == "t1"
         & parent_sv == "1") %>%
  pull(id)

survey_questions(id_subset)
fetch_survey(id_subset)
# remotes::install_github("ropensci/qualtRics")

reprex::reprex(
  {
  id_subset = "SV_2rF0CcmMF1MmTiJ"

  # If you are calling fetch_survey(), you first make a POST request to:

  qualtRics:::generate_url("exportresponses", surveyID = id_subset)

  # This gives you a requestID. After that you make a GET request (for the progress) to:

  qualtRics:::generate_url("exportresponses_progress", surveyID = id_subset, requestID = "[request_id]")

  # This gives you a file ID and last you make a GET request (for the export file) to:

  qualtRics:::generate_url("exportresponses_file", surveyID = id_subset, fileID = "[file_id]")

  ####
  ####

  ## gives you the requestID:
  (requestID <- qualtRics:::export_responses_init(
    surveyID = id_subset,
    body = '{"format":"csv","useLabels":true,"includeDisplayOrder":true,"breakoutSets":true} '
  ))

  ## gives you the fileID:
  (fileID <- qualtRics:::export_responses_progress(
    surveyID = id_subset,
    requestID = requestID
  ))

  ## downloads the file and returns the path:
  qualtRics:::export_responses_filedownload(
    surveyID = id_subset,
    fileID = fileID
  )
  }
)

qualtRics:::generate_url("exportresponses", surveyID = id_subset)


s <- fetch_survey(id_subset)
