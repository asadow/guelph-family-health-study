file_date_needed <- TRUE
pivot_parents_wide <- FALSE
requesting_calculations <- FALSE
needs_unnest_asa <- FALSE
vars_child_self_report <- "\\b\\B"
vars_child_not_by_parent_survey <- "\\b\\B"
vars_exclude_parents <- "\\b\\B"


child_self_report <- if(vars_child_self_report == "\\b\\B") FALSE else TRUE


requester <- "saher"
data_type <- c("survey")
data_type_children <- c("survey")


roles <- c("child", "parent")
file_type <- "csv"

requested_times <- tibble(
  phase = c(
    "3"
  ),
  time_point = c(
    c("t3")
  )
)

sm_use_info_items <- c("family_meal_ideas",
                       "feeding_child_advice")

sm_use_parenting_items <- c("compare_food_choices",
                            "share_family_meals",
                            "food_choices_posts",
                            "post_interaction")

sm_use_items <- c(sm_use_info_items, sm_use_parenting_items)
sm_use_items <- glue("sm_use_{sm_use_items}")
scales <- c("sm_networks", "cfpq_v2")

vars_specific <- c("sm_hrs",
                   "sm_times_per_day",
                   sm_use_items)
