scales <- vars$scale[[1]]
scales_include <- function(x) any(scales %in% x)

## Question instructions to remove
## Start the pattern with _ if instructions
## come after scale title
qi_to_remove <- c(
"please_indicate_how_much_you_agree_or_disagree_with_each_of_these_statements_using_the_following_options_strongly_disagree_disagree_agree_strongly_agree",
"for_each_item_select_the_response_that_best_describes_the_way_you_and_your_partner_work_together_as_parents",
"child_eating_behaviour_please_indicate_how_much_you_agree_or_disagree_with_each_of_these_statements",
"food_parenting_please_let_us_know_how_much_you_agree_or_disagree_with_the_following_statements",
"family_feeding_practices_and_parental_modelling_please_indicate_how_much_you_agree_or_disagree_with_each_of_these_statements",
"these_questions_ask_you_to_describe_things_you_do_when_both_you_and_your_partner_are_physically_present_together_with_your_child_ren_i_e_in_the_same_room_in_the_car_on_outings_count_only_times_when_you_your_partner_and_your_child_ren_are_within_the_company_of_one_another_even_if_this_is_just_a_few_hours_per_week",
"rate_yourself_on_the_following_items_on_a_scale_from_1_to_7"
)

qi_to_remove <- glue_collapse(qi_to_remove, "|")

df <- df %>%
  rename_with(~ str_replace(.x, qi_to_remove, ""))
#
# if(scales_include("coparenting_relationship")){
#   ## Coparenting ####
#   y <- "for_each_item_select_the_response_that_best_describes_the_way_you_and_your_partner_work_together_as_parents"
#   df <- df %>%
#     rename_with(
#       ~ str_replace(.x, y, ""),
#       matches("^coparenting_relationship")
#     )
# }
#
# if(scales_include("child_eating_behaviour")){
#   ## Coparenting ####
#   y <- "please_indicate_how_much_you_agree_or_disagree_with_each_of_these_statements"
#   df <- df %>%
#     rename_with(
#       ~ str_replace(.x, y, ""),
#       matches("^child_eating_behaviour")
#     )
# }
#
# if(scales_include("food_parenting")){
#   ## Coparenting ####
#   y <- "please_let_us_know_how_much_you_agree_or_disagree_with_the_following_statements"
#   df <- df %>%
#     rename_with(
#       ~ str_replace(.x, y, ""),
#       matches("^food_parenting")
#     )
# }


if(scales_include("depression_")){
  ## Parenting Depression ####
  ## Looking for depression items

  df <- df %>%
    rename_with(
      ~ str_replace(.x, "Depressive.*box_", ""),
      matches("Depressive")
    )

  str_subset(names(df), "during_the_past_week")
  dur <- str_subset(dit, "^(?!during_the_past_week)")
}

if(scales_include("child_antibiotics_")){
  ## Child Antibiotics ####

  df %>%
    rename_with(
      ~ str_replace(.x, "below.*received", ""),
      starts_with("antibiotics")
    ) %>%
    rename_with(
      ~ str_replace(.x, "since_was.*number_of_episodes", "number_of_episodes"),
      starts_with("antibiotics_since_was_born")
    ) %>%
    rename_with(
      ~ str_replace(.x, "__", "_")
    )
}

if(scales_include("sdq")){
  ## Strengths and Difficulties ####
  ## The same sdq questions are repeated in data twice with slight differences;

  df <- df %>%
    dup_coalesce("(2_4y|5_17y).*school_year_", "")


  ## The following
  # str_subset(names(df), "sdq_helpful.*ill")
  ## ...shows an item is mistyped and a duplicate

  df <- df %>% dup_coalesce("sdq_helpful.*ill", "sdq_helpful_if_someone_is_hurt_upset_or_feeling_ill")

  # SDQ's for 2-4y and 5-17 only differ in the wording of 3 items
  # Softer language is used for 2-4y
  # We change the harder language to softer for 5-17y

  softer <- c(
    "often_argumentative_with_adults",
    "can_be_spiteful_to_others",
    "can_stop_and_think_things_out_before_acting"
  )

  harder <- c(
    "often_lies_or_cheats",
    "steals_from_home_school_or_elsewhere",
    "thinks_things_out_before_acting"
  )

  for (i in seq(softer)){
    pattern <- glue("sdq_({softer[i]}|{harder[i]})")
    replacement <- glue("sdq_{softer[i]}")
    df <- df %>% dup_coalesce(pattern, replacement)
  }

}
