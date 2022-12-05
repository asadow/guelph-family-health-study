# ---
#   title: "Social Support, Caregiver Stress, and Home Chaos in the Family Stress Study"
# author: "Adam Sadowski"
# date: today
# toc: true
# number-sections: true
# highlight-style: pygments
# code-fold: true
# format:
#   html:
#   df-print: kable
# execute:
#   echo: true
# warning: false
# output: false
# freeze: true
# cache: true
# fig-width: 6
# fig-asp: 0.618
# fig-align: "center"
# comment: "#>"
# editor: visual
# ---

library(here)
library(magrittr)
library(fs)

## Loads/installs packages and functions
here("R", "packages.R") %>% source
here("R", "functions") %>% dir_ls %>% as.character() %>% map(source)


# Surveys -----------------------------------------------------------------


#| label: load-surveys
#| code-summary: "Import and Process Survey Data"
study_sv <- "fss"
survey_type <- "survey"

df_qualtrics <- all_surveys() %>%
  rename(survey_name = name) %>%
  separate_survey_name("survey_name") %>%
  filter(study_sv == !!study_sv & survey_type == !!survey_type)

df <- df_qualtrics %>%
  filter(time_point_sv == "t1")

ns <- df %>% import

ns <- ns %>%
  rename_relabel_each_survey %>%
  unnest(data, names_repair = make_clean_names)

ns %>% names %>% str_subset("cebq")

ns <- ns %>% filter(finished != "False")

ns %>% select(starts_with("cebq")) %>% unlist %>% unique %>% dput

sr <- c("full_easily",
        "leaves_food_on_plate",
        "full_before_meal_finished",
        "snack",
        "big_appetite")

cebq_sr <- glue("cebq_{sr}")

s <- ns %>%
  mutate(
    across(
      all_of(cebq_sr),
      ~ likert_score(.x,
                     c("Never",
                       "Rarely",
                       "Sometimes",
                       "Often",
                       "Always")
                      )
    ))


s <- s %>%
  mutate(
    satiety_responsiveness = rowMeans(select(., all_of(cebq_sr)) %>%
                                        mutate(across(.fns = ~as.numeric(.x)))), na.rm = TRUE
    )

s <- s %>%
  mutate(
    satiety_responsiveness_cat = cut(satiety_responsiveness,
                                     breaks = c(1, 3, 5),
                                     right = FALSE)
  )





# AHHA --------------------------------------------------------------------



df_ha <- all_surveys() %>%
  rename(survey_name = name) %>%
  separate_survey_name("survey_name") %>%
  filter(study_sv == "fss" & survey_type == "ahha")

df <- df_ha %>%
  filter(time_point_sv == "t1")

ns <- df %>% import

ns <- ns %>%
  rename_relabel_each_survey %>%
  unnest(data, names_repair = make_clean_names)

ns %>% names %>% str_subset("wt")



library(gtsummary)

tbl1 <- s %>%
  mutate(
    across(starts_with("pss"),
           ~ case_when(.x %in% c(3:5) ~ "3, 4, or 5",
                       TRUE ~ .x)),
    across(starts_with("social_support"),
           ~ case_when(.x %in% c(1:2) ~ "1 or 2",
                       TRUE ~ .x)),
  ) %>%
  select(starts_with(c("pss", "social_support"))) %>%
  tbl_summary %>%
  bold_labels

tbl2 <- s %>%
  mutate(
    chaos_score_cat = case_when(chaos_score > 37.5 ~ "> 37.5",
                                chaos_score <= 37.5 ~ "<= 37.5",
                                TRUE ~ NA_character_)
  ) %>%
  filter(parent_sv == 1) %>%
  select(chaos_score_cat) %>%
  tbl_summary %>%
  bold_labels


tbl1
tbl2


