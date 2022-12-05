
# NB Request code does not include t2 phase 1 survey,  and ESHA -----------

requester <- "leung"
roles <- c("child")
data_type <- c("survey", "asa", "ha")
data_type_children <- c("asa", "ha")
file_type <- "csv"
needs_unnest_asa <- TRUE

requested_asa_files <- c("totals")
file_date_needed <- TRUE
remove_child_prefix <- TRUE
requested_times <- tibble(phase = "3", time_point = "t1")
scales <- NULL

labels <- c(
  "born_before_due_date",
  "birth_weeks_early",
  "asa_totals",
  "wc_ht_ratio",
  "i_note_ht",
  "i_note_bm",
  measures_ha,
  bia_calc,
  raw_measures_ha_snake_case,
  "cleaned_ha",
  flags_ha[!flags_ha %in% c(flags_bod_pod, flags_parent_ha)]
)  %>%
  str_subset("bp|bodpod", negate = TRUE)

child_self_report <- FALSE
vars_child_self_report <- "\\b\\B"
vars_child_not_by_parent_survey <- labels %>%
  str_subset("born_before|birth_weeks", negate = TRUE)
vars_exclude_parents <- labels

# child_primary_vars <- specific_vars
# child_only_vars <- specific_vars

vars_parent_answered_on_child <- c("relationship")

requesting_calculations <- TRUE
pivot_parents_wide <- TRUE

calculation <- function(dat) {
  dat %>%
    add_bmi_z %>%
    mutate(wc_ht_ratio = as.numeric(wc_cm)/as.numeric(ht_cm))
}
