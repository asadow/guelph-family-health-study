#
# get_surveys <- function(needed_data, requested_survey_vars){
#   ## Combine labels direct from Qualtrics with data already downloaded
#   ## Unnecessary elements:
#   ## 1. needed_data can be requested_times once qualtrics_ui.rds is incorporated
#   ## 2. api is not needed once final downloads are made
#   ## 3. bind_child_centric_data is confusing as it is hard to distinguish between
#   ## columns that are self-reported and columns that are not.
#   ## For the latter, hence there is the function questions_on_child() to provide to requesters
#
#   api_labels <- api_surveys(needed_data) %>%
#     import_headers %>%
#     extract_labels
#
#   api_join_dl <- api_labels %>% join_to_downloads
#
#   survey_downloads_missing <- api_join_dl %>%
#     filter(is.na(file_path))
#
#   survey_data <- api_join_dl %>%
#     read_downloads %>%
#     extract_names %>%
#     label_downloads
#
#   surveys_long <- survey_data %>%
#     unnest_and_select(requested_survey_vars) %>%
#     relabel_on_child_cols %>%
#     pivot_child_per_row
#
#   ## Clean
#   surveys_long <- surveys_long %>%
#     ## In names, remove first of two consecutive identical words
#     rename_with(~ str_replace_all(.x, "([^_]+)(?:_\\1(?=_|$))*", "\\1")) %>%
#     ## Remove duplicate rows created by pivot_longer and child 1, 2, etc.
#     filter(
#       !(mi(child_pid) & !child == "1")
#     )
#
#   surveys <- surveys_long %>%
#     bind_child_centric_data
# }
