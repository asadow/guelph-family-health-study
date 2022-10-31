file_date_needed <- FALSE
pivot_parents_wide <- TRUE
requesting_calculations <- FALSE
needs_unnest_asa <- FALSE
remove_child_prefix <- TRUE
child_self_report <- FALSE
requested_asa_files <- "totals"

requested_times <- tibble(
  phase = c(
    c("2", "3")
  ),
  time_point = c(
    c("t1")
  )
)

requester <- "haines_cebq"
data_type <- c("survey", "ha", "asa")
data_type_children <- data_type
roles <- c("parent", "child")
file_type <- "rds"

labels <- c("cebq_35",
            "bmi_z",
            "bm_kg",
            "ht_cm",
            "i_note_ht",
            "i_note_bm",
            "asa_totals")

# scales <- c("cebq_35")
# vars_specific <-  "\\b\\B"
vars_child_self_report <- "\\b\\B"
vars_child_not_by_parent_survey <- labels %>% str_subset("cebq", negate = TRUE)
vars_exclude_parents <- "\\b\\B"

# Requested times ---------------------------------------------------------

cspec <- c("refuses_new_foods",
            "dislike_without_tasting" ,
            "hard_to_please",
            "enjoys_new_foods",
            "variety_of_foods",
            "tasting_new_foods",
            "full_easily",
            "leaves_food_on_plate",
            "full_before_meal_finished",
            "snack",
            "big_appetite",
            "always_asking_for_food",
            "eat_too_much",
            "eat_most_of_the_time",
            "favourite_foods",
            "food_in_mouth")
