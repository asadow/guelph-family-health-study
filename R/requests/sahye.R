requester <- "sahye"
data_type <- c("survey")
roles <- c("parent", "child")
data_type_children <- data_type
non_qualtrics_regex <- "grocery_shopping|family_meals"
vars_child_not_by_parent_survey <- "\\b\\B"
vars_exclude_parents <- "\\b\\B"

needs_unnest_asa <- FALSE
file_date_needed <- TRUE
pivot_parents_wide <- FALSE
requesting_calculations <- FALSE

requested_times <- tibble(
  phase = c(
    rep(c("1", "2"), 2)
            ),
  time_point = c(
    rep("t1", 2),
    rep("t7", 2)
   )
)


file_type <- "csv"

vars_parent_answered_on_child <- c("relationship")
scales <- c("food_skills",
            "participation_in_meals")

cfpq_items <- c("planning_meals",
                "help_with_meal_prep",
                "grocery_shopping")

cfpq_versions <- glue("cfpq_v{1:4}")
cfpq <- cross(
  list(cfpq_versions,
       cfpq_items)
  ) %>%
  map_chr(paste,
          sep = "_",
          collapse = "_")

vars_specific <- c(cfpq,
                   "involvement_involving",
                   "involvement_allowing",
                   "involvement_encouraging",
                   "i_we_allow_my_child_to_help_prepare_family_meals",
                   "i_we_encourage_my_child_to_participate_in_grocery_shopping",
                   "child_grocery_shopping",
                   "child_involvement_meal_prep")

vars_child_self_report <- c("food_skills", "participation_in_meals")

child_self_report <- if("\\b\\B" %in% vars_child_self_report) FALSE else TRUE
