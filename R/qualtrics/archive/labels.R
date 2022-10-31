source(here("R", "functions", "qualtrics-functions.R"))

phase <- c("1")

## See example in ?recover
# options(error = recover)

df_qualtrics <- all_surveys() %>%
  select(name, id) %>%
  separate_survey_name %>%
  filter(
    phase_sv %in% phase
    )

df_questions <- df_qualtrics %>% import_questions
df_errored <- df_questions %>% filter(survey_questions == "Error")
df_questions <- df_questions %>% filter(survey_questions != "Error")

# qualtRics:::generate_url("fetchdescription", surveyID = df_errored$id)
#
# browser(fetch_survey(df_errored$id[1]))

# df_questions <- df_questions %>%
#   mutate(
#     qid_first_name = map(
#       survey_questions,
#       ~ .x %>%
#       filter(
#         question %>% str_detect("first name of your.*child")
#       ) %>%
#       pull(qid)
#     )
#   )

df_headers_nested <- df_questions %>% import_headers

## Commented code is to give up
## on using question column from
## survey_questions()
## as it is being deprecated
## and is giving incorrect output
## in terms of inconsistent or unlabelled

# df_headers_nested <- df_headers_nested %>%
#   mutate(
#     survey_labels = map(
#       data,
#       ~ tibble(
#         label = .x %>% label_to_colnames %>% names,
#         qname_derived = .x %>% names %>% str_extract("Q\\d+")
#         )
#     )
    ## Derive qname from label which is in form Q\\d+
    # ,
    # survey_questions_merged = map2(
    #   survey_questions,
    #   survey_labels,
    #   ~ left_join(.x, .y, by = c("qname" = "qname_derived"))
    # )
  # )

df_headers_nested <- df_headers_nested %>%
  mutate(
    labels = map(data, ~ .x %>% label_to_colnames %>% names)
  )

df_headers <- df_headers_nested %>%
  select(- c(
    # survey_labels,
    data, id, survey_questions))
# %>%
#   unnest(survey_questions_merged)


# Clean -------------------------------------------------------------------

## Remove all but qualtrics prefix label in label
df_headers <- df_headers %>%
  mutate(
    # label = label %>% str_remove(" .*")
  ) %>%
  rename_with(~ .x %>% str_remove("_sv"))









# Unlabelled --------------------------------------------------------------
clean_questions <- df_headers %>%
  group_by(question) %>%
  mutate(
    question = question %>%
         str_replace_all("\\$.*\\}", "[the child]") %>%
         str_replace_all("\\\n", " ") %>%
         str_replace_all("  ", " ") %>%
         str_remove_all("<.*>|&nbsp")
  ) %>%
  filter(!question %>% str_detect("^\\s*$")) %>%
  select(- c(force_resp, qid, qname)) %>%
  relocate(question) %>%
  relocate(survey_type, study, .after = last_col()) %>%
  relocate(role, .after = parent)

unlabelled <- clean_questions %>%
  ## filter for unlabelled
  filter(
    !label %>% str_detect("^([[:lower:]]|_|\\d{2,100})+$")
    ) %>%
  filter(!duplicated(question)) %>%
  select(- label)

unlabelled %>% write_csv_for(glue("unlabelled_phase_{phase}"))

# Check inconsistent labels -----------------------------------------------

inconsistent <- clean_questions %>%
  ## filter for labelled
  filter(
    label %>% str_detect("^([[:lower:]]|_|\\d{2,100})+$")
    ) %>%
  mutate(n_distinct_labels = n_distinct(label)) %>%
  filter(n_distinct_labels != 1) %>%
  # question only contains the intro, hence there are duplicates to remove
  filter(!duplicated(label, question)) %>%
  select(- n_distinct_labels) %>%
  relocate(label, .after = question) %>%
  arrange(question)

inconsistent %>% write_csv_for(glue("inconsistent_phase_{phase}"))















# Check which times have same labels --------------------------------------------

times <- df_headers %>%
  select(phase, time_point, parent, on_who, label) %>%
  group_by(label) %>%
  nest()

crush_same_rows_for_different <- function(df, x){
  df %>%
    group_by(across(- {{ x }})) %>%
    mutate(
      "{{ x }}" := {{ x }} %>%
        ifelse(
          length(.) >= 2,
          paste(., collapse = ", "),
          .
        )
    ) %>%
    distinct
}


times %>%
  mutate(
    data = map(
      data,
      ~ .x %>%
        arrange(parent) %>%
        distinct %>%
        crush_same_rows_for_different(parent) %>%
        crush_same_rows_for_different(phase) %>%
        crush_same_rows_for_different(time_point)
      )
  ) %>%
  unnest(data)

m <- metadata(nh$id[[1]])
m %>% View


m[["questions"]][["QID98"]]$choices %>% View
