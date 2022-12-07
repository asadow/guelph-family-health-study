file_date_needed <- TRUE
pivot_parents_wide <- FALSE
requesting_calculations <- TRUE
needs_unnest_asa <- FALSE
remove_child_prefix <- FALSE
child_self_report = FALSE

requested_times <- tibble(
  phase = c(
    rep(3, 3), rep(2, 3)
  ) %>% as.character,
  time_point = c(
    glue("t{1:3}") %>% rep(2)
  )
)

requester <- "jacob_eating-behaviour"
data_type <- c("survey", "ha")
data_type_children <- data_type

roles <- c("child", "parent")
file_type <- "csv"

labels <- c("cebq_35", "cfpq_v2",
            "bmi_z",
            "bm_kg",
            "ht_cm",
            "wc_cm",
            "i_note_wc",
            "i_note_ht",
            "i_note_bm")

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
    add_bmi_z(remove_child_prefix)
  }

# source(here("R", "requests", "02_pull.R"))

# status <- status_update(requested_data)
# status %>% write_status_for(requester)


create_package(here())
