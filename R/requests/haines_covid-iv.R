file_date_needed <- FALSE
pivot_parents_wide <- FALSE
requesting_calculations <- FALSE
needs_unnest_asa <- FALSE
remove_child_prefix <- TRUE
child_self_report = FALSE

requester <- "haines"
data_type <- c("survey", "ha", "ahha", "asa", "ll")
data_type_children <- c("survey", "ha", "ahha", "asa")

requested_asa_files <- "totals"
roles <- c("child", "parent")
file_type <- "rds"

labels <- c("sdq",
            "active_play",
            "play_outside",
            "sugar_bev",
            "bmi_z",
            "bm_kg",
            "ht_cm",
            "i_note_ht",
            "i_note_bm",
            "asa_totals",
            "nutristep_toddler_fruit_veg",
            "nutristep_preschool_fruit",
            "nutristep_preschool_veg",
            "nutristep_toddler_screens",
            "nutristep_preschool_screen_hrs",
            "screen_time_weekday",
            "screen_time_weekend_day",
            "evening_meal_prep_time",
            "meal_prep_person",
            "help_with_meal_prep",
            "meal_prep_v",
            "food_served",
            "meal_time_screens",
            "meal_time_device_use",
            "media_parenting")

vars_child_self_report <- "\\b\\B"
vars_child_not_by_parent_survey <- c("bmi_z",
                                     "bm_kg",
                                     "ht_cm",
                                     "i_note_ht",
                                     "i_note_bm",
                                     "asa_totals")

vars_exclude_parents <- "\\b\\B"


# Requested times ---------------------------------------------------------

allsv <- all_surveys()

sdq_related <- allsv %>%
  separate_survey_name %>%
  filter(study_sv == "gfhs"
         & survey_type == "survey"
         & role != "child")

requested_times <- sdq_related %>%
  select(phase_sv, time_point_sv) %>%
  rename_with(~ .x %>% str_remove("_sv")) %>%
  distinct %>%
  ## for bmi, add rows
  add_row(phase = "1", time_point = "t1") %>%
  add_row(phase = "1", time_point = "t2") %>%
  arrange(phase, time_point)








# Archive -----------------------------------------------------------------



# vars_exclude_parents <- c("bmi_z",
#                          "bm_kg",
#                          "ht_cm",
#                          "i_note_ht",
#                          "i_note_bm",
# "asa_totals",
# scales,
# "screen_time_weekday",
# "screen_time_weekend_day"
# )


# scales <- c("sdq", "active_play", "play_outside", "sugar_bev")

# vars_specific <-  c("bmi_z",
#                     "bm_kg",
#                     "ht_cm",
#                     "i_note_ht",
#                     "i_note_bm",
#                     "asa_totals",
#                     "nutristep_toddler_fruit_veg",
#                     "nutristep_preschool_fruit",
#                     "nutristep_preschool_veg",
#                     "nutristep_toddler_screens",
#                     "nutristep_preschool_screen_hrs",
#                     "screen_time_weekday",
#                     "screen_time_weekend_day")
