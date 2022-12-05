file_date_needed <- FALSE
pivot_parents_wide <- FALSE
requesting_calculations <- FALSE
needs_unnest_asa <- FALSE
remove_child_prefix <- FALSE
child_self_report = FALSE

requested_times <- tibble(
  phase = c(
    3, 1, 2
  ) %>% as.character,
  time_point = c(
    "t1", "t4", "t3"
  )
)

requester <- "douglas"
data_type <- c("survey")
data_type_children <- c("survey")

roles <- c("child", "parent")
file_type <- "csv"

labels <- c("co_parenting_quality", "co_parenting_behaviour")

vars_child_self_report <- "\\b\\B"
vars_child_not_by_parent_survey <- "\\b\\B"
vars_exclude_parents <- "\\b\\B"


families <- c(66, 214, 301, 308, 319, 326, 327, 325, 413,
              420, 421, 455, 467, 470, 491, 502, 536, 540, 734)
