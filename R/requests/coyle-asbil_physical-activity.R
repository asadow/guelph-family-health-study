file_date_needed <- TRUE
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

requester <- "coyle-asbil_sleep"
data_type <- c("survey", "ha")
data_type_children <- c("survey", "ha")

roles <- c("child")
file_type <- "csv"

labels <- c("bmi_z",
            "bm_kg",
            "ht_cm",
            "wc_cm",
            "fat_percent_bia",
            "bia_res_ohm",
            "i_note_bm",
            flags_wc,
            flags_ht,
            flags_bia,
            raw_measures_ha_snake_case %>% str_subset("ht|wc|bia"),
            "nutristep_toddler_screens",
            "nutristep_preschool_screen_hrs",
            "screen_time_weekday",
            "screen_time_weekend_day"
            # "meal_time_screens",
            # "meal_time_device_use",
            # "media_parenting"
            )

vars_child_self_report <- "\\b\\B"
vars_child_not_by_parent_survey <- labels %>% str_subset("screen|nutristep",
                                                         negate = TRUE)
vars_exclude_parents <- vars_child_not_by_parent_survey

calculation <- function(dat) {
  dat %>%
    add_bmi_z(remove_child_prefix) %>%
    mutate(wc_ht_ratio = as.numeric(wc_cm)/as.numeric(ht_cm)) %>%
    suppressWarnings()
}

source(here("R", "requests", "02_pull.R"))


