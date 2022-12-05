# These are "filled"

pid_static_vars <- c("fid",
                    "experimental_group",
                    "phase",
                    "role",
                    "dob",
                    "sex",
                    "ethnicity",
                    "ethnicity_selected_choice",
                    "ethnicity_other_please_specify_text",
                    "country_born_selected_choice",
                    "country_born_other_please_specify_text")

file_vars_asa <- c("folder_asa",
                   "data_path_asa",
                   "path_name_asa",
                   "time_last_modified_asa",
                   "status_asa",
                   "responses_asa",
                   "i_note_asa")

## Demographics ------------------------------------------------------------
survey_demographics <- read_and_glue_labels() %>%
  filter(category_or_block %>% str_detect("demographics")) %>%
  pull(label)

survey_demographics <- survey_demographics %>%
  str_subset("^age$", negate = TRUE) %>%
  ## To avoid children_not_eating_enough
  str_replace("children$", "children$")

vars <- c(survey_demographics, labels)
#
# ## Scales ------------------------------------------------------------------
#
# if(!is.null(scales)){
#   scales <- scales %>% glue_collapse("|")
#   scales_regex <- glue("^(self_report_|child_\\d_|parent_child_|){scales}")
#   requested_survey_vars <- read_and_glue_labels() %>%
#     filter(label %>% str_detect(scales_regex)) %>%
#     pull(label)
#   requested_survey_vars <- c(requested_survey_vars,
#                              survey_demographics)
# }else{
#   scales_regex <- "\\b\\B"
#   requested_survey_vars <- survey_demographics
# }

## Requested Objects --------------------------------------------------------
requesting_phase_1 <- any(requested_times$phase %in% 1)

qualtrics_requested_times_and_t1 <- requested_times %>%
  bind_rows(
    requested_times %>%
      ## Phase 1 demographics are from paper surveys
      filter(!phase == "1") %>%
      mutate(time_point = "t1")
  ) %>%
  distinct

requested_times_and_t1 <-requested_times  %>%
  bind_rows(
    requested_times %>% mutate(time_point = "t1")
  ) %>%
  distinct
s
qualtrics_requested_data <- qualtrics_requested_times_and_t1 %>%
  expand_grid(data_type)

requested_data <- requested_times_and_t1 %>%
  expand_grid(data_type)

times_and_ids <- requested_times_and_t1 %>%
  join_id_pairs %>%
  select(- fid)

# times_and_ids <- if(is_pid_child == TRUE){
#   times_and_ids %>%
#     distinct(time_point, pid) %>%
#     select(time_point, pid)
#   }else{times_and_ids}

## Can delete if using t1 pilot 1 survey
old_vars <- c('income_household', 'marital_status') %>%
  cross2(c('x', 'y')) %>%
  map_chr(paste, sep = "_", collapse = "_") %>%
  c("education",
    "country_born_in_other",
    "country_born_in_selected",
    "e3")

