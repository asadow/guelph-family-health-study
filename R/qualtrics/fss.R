study_sv <- "fss"
survey_type <- "survey"

df_qualtrics <- all_surveys() %>%
  rename(survey_name = name) %>%
  separate_survey_name("survey_name") %>%
  filter(study_sv == !!study_sv & survey_type == !!survey_type)

pare <- df_qualtrics %>%
  filter(time_point_sv == "t1")

ns <- pare %>% import

ns <- ns %>%
  rename_relabel_each_survey %>%
  unnest(data, names_repair = make_clean_names)

ns %>% names %>% str_subset("social_support|pss|chaos")

ns <- ns %>% filter(finished != "False")

# ns %>% select(starts_with("chaos")) %>% unlist %>% unique %>% dput

n <- ns %>%
  mutate(
    across(starts_with("social_support"),
           ~ likert_score(.x, c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))
           ),
    across(starts_with("pss"),
           ~ likert_score(.x, c("Never", "Almost Never", "Sometimes", "Fairly Often", "Very Often"))
    ),
    across(starts_with("chaos"),
           ~ likert_score(.x,
                          c("Very much like your own home environment",
                            "Somewhat like your own home environment",
                            "A little bit like your own home environment",
                            "Not at all like your own home environment"))),
    # across(starts_with(c("social_support", "pss", "chaos")), ~ as.numeric(.x)),
    # across(c(pss_confident_in_handling_problems, pss_things_were_going_your_way),
    #        ~ 5 + 1 - .x),
    # across(starts_with(c("social_support", "pss", "chaos")), ~ as.numeric(.x)),
    across(c(pss_confident_in_handling_problems, pss_things_were_going_your_way),
           ~ if_else(str_detect(.x, "\\d{1}"),
                     as.character(5 + 1 - as.numeric(.x)),
                     .x)),
  ) %>%
  suppressWarnings()

n <- n %>%
  mutate(
    chaos_score = rowSums(select(., starts_with("chaos")) %>%
                            mutate(across(.cols = everything(), ~as.numeric(.x)))),
    chaos_unscored_answered = case_when(
      if_any(starts_with("chaos"), ~ str_detect(.x, "\\D")) ~ TRUE,
      TRUE ~ FALSE)
  )

library(gtsummary)

tbl1 <- n %>%
  mutate(
    across(starts_with("pss"),
    ~ case_when(.x %in% c(3:5) ~ "3, 4, or 5",
                TRUE ~ .x)),
    across(starts_with("social_support"),
           ~ case_when(.x %in% c(1:2) ~ "1 or 2",
                       TRUE ~ .x)),
  ) %>%
  select(starts_with(c("pss", "social_support"))) %>%
  tbl_summary

tbl2 <- n %>%
  mutate(
    chaos_score_cat = case_when(chaos_score > 37.5 ~ "> 37.5",
                                chaos_score <= 37.5 ~ "<= 37.5",
                                TRUE ~ NA_character_)
  ) %>%
  filter(parent_sv == 1) %>%
  select(chaos_score_cat) %>%
  tbl_summary



# ns <- nested_surveys %>%
#   mutate(
#     data_names = map(data,
#                      ~ names(.x)),
#     labels = map(data, ~ .x %>% label_to_colnames %>% names)
#   )
#
#
# s <- nested_surveys %>%
#   unnest(data, names_repair = make.unique) %>%
#   ## UTC is default of fetch_survey() used in qualtRics
#   # mutate(date_sv = as_datetime(date_sv, tz = "UTC")) %>%
#   rename_child_questions %>%
#   clean_names
#
#
#
# s %>% names %>% str_subset("ethnicity")
#
# nested_surveys <- nested_surveys %>%
#   mutate(
#     data = map(
#       data,
#       ~ .x %>%
#         as_tibble(.name_repair = make_clean_names) %>%
#         clean_names %>%
#         rename(date_sv = recorded_date)
#     )
#   )
#
# nested_surveys <- nested_surveys %>%
#   mutate(
#     labels = map(data,
#                  . %>%
#                    label_to_colnames %>%
#                    names %>%
#                    make_clean_names %>%
#                    str_subset("^(recipient|location|ip_address)",
#                               negate = TRUE)
#     )
#   )
#
# surveys <- nested_surveys %>% unnest(data)
# surveys %>% names
