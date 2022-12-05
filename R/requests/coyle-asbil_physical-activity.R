file_date_needed <- FALSE
pivot_parents_wide <- TRUE
requesting_calculations <- TRUE
needs_unnest_asa <- FALSE
remove_child_prefix <- TRUE
child_self_report = FALSE

requested_times <- tibble(
  phase = c(
    "3"
  ),
  time_point = c(
    c("t1")
  )
)

requester <- "coyle-asbil"
data_type <- c("survey", "ha")
data_type_children <- c("survey", "ha", "ahha", "asa")

roles <- c("child")
file_type <- "csv"

labels <- c("bmi_z",
            "bm_kg",
            "ht_cm",
            "wc_cm",
            "i_note_wc",
            "i_note_ht",
            "i_note_bm",
            "nutristep_toddler_screens",
            "nutristep_preschool_screen_hrs",
            "screen_time_weekday",
            "screen_time_weekend_day"
            # "meal_time_screens",
            # "meal_time_device_use",
            # "media_parenting"
            )

vars_child_self_report <- "\\b\\B"
vars_child_not_by_parent_survey <- c("bmi_z",
                                     "bm_kg",
                                     "ht_cm",
                                     "wc_cm",
                                     "i_note_wc",
                                     "i_note_ht",
                                     "i_note_bm",
                                     "asa_totals")

vars_exclude_parents <- "\\b\\B"

calculation <- function(dat) {
  dat %>%
    add_bmi_z %>%
    mutate(wc_ht_ratio = as.numeric(wc_cm)/as.numeric(ht_cm))
}




