file_date_needed <- TRUE
pivot_parents_wide <- FALSE
requesting_calculations <- FALSE
needs_unnest_asa <- FALSE
vars_child_self_report <- "\\b\\B"
vars_child_not_by_parent_survey <- "\\b\\B"
vars_exclude_parents <- "\\b\\B"


child_self_report <- if(vars_child_self_report == "\\b\\B") FALSE else TRUE


requester <- "barsse"
data_type <- c("survey")
data_type_children <- c("survey")

roles <- c("child", "parent")
file_type <- "csv"

requested_times <- tibble(
  phase = c(
    "3"
  ),
  time_point = c(
    c("t1")
  )
)

scales <- c()

vars_specific <- c("birth_weeks_early")
