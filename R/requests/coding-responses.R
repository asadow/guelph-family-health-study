
# Coded Response ####
#### PA ####


#names(df) <- str_replace(names(df), "units$", "_units")

## EDIT BELOW IF USING TO CONSIDER ABOVE NOTES
cpa_quantity <- c(
  "PA_play_time_avg_week_day",
  "PA_outside_time_avg_week_day",
  "PA_play_time_avg_weekend_day",
  "PA_outside_time_avg_weekend_day"
)

cpa_units <- paste0(cpa_quantity, "_units")
cpa_hours <- paste0(cpa_quantity, "_hours")
df <- as.data.frame(df)

for (j in seq(cpa_quantity)){

  q <- cpa_quantity[j]
  u <- cpa_units[j]
  h <- cpa_hours[j]

  df[, h] <- ifelse(df[, u] == "Minutes",
                    an(df[, q])/60,
                    ifelse(df[, u] == "Hours", an(df[, q]), NA)
  )
}

df <- df %>%
  mutate(
    avg_day_PA_play_hours = 1/7 * (
      5 * PA_play_time_avg_week_day_hours +
        2 * PA_play_time_avg_weekend_day_hours
    ),
    avg_day_PA_outside_hours = 1/7 * (
      5 * PA_outside_time_avg_week_day_hours +
        2 * PA_outside_time_avg_weekend_day_hours
    )
  )

#### Meals ####

## family_meal_frequency binary

df <- df %>%
  mutate(
    fam_meal_freq_per_week_7_or_more = recode(
      family_meal_frequency,
      "Never" = 0,
      "1 to 2 times" = 0,
      "3 to 4 times" = 0,
      "5 to 6 times" = 0,
      "7 or more times" = 1
    )
  )


#### Screen Time ####

screen_vars <- c("avg_week_day_screen_time", "avg_weekend_day_screen_time")

df <- df %>%
  mutate(
    across(
      all_of(screen_vars),
      list(hours = st_quantify),
      .names = "{col}_{fn}"
    ),
    avg_day_screen_hours = 1/7 * (
      5 * avg_week_day_screen_time_hours +
        2 * avg_weekend_day_screen_time_hours
    )
  )

## Can code a variable as 2 or more (against recommendation) vs. < 2 (recommended)


#### Beverage Frequency ####

## bev_freq quantified to times per week

bev_vars <- c(
  "bev_freq_flav_milk",
  "bev_freq_100_juice",
  "bev_freq_fruit_drinks",
  "bev_freq_soda",
  "bev_freq_sug_free_soda",
  "bev_freq_energy_drinks",
  "bev_freq_sports_drinks"
)

df <- df %>%
  mutate(
    across(
      all_of(bev_vars),
      list(per_week = bev_quantify),
      .names = "{col}_{fn}"
    )
  )

bv_pw <- glue("{bev_vars}_per_week")

df <- df %>%
  mutate(
    SSB_freq_per_week = rowSums(
      across(
        all_of(bv_pw),
        ~ as.numeric(.x)
      )
    )
  )

#### Sleep Time ####
df <- df %>%
  add_avg_sleep("week_day") %>%
  add_avg_sleep("weekend_day")

df <- df %>%
  mutate(
    across(
      c(avg_sleep_hours_week_day, avg_sleep_hours_weekend_day),
      ~ case_when(
        . <= 0 ~ . + 24,
        . < 7 ~ . + 12,
        . > 16 ~ . - 12,
        TRUE ~ .
      )
    )
  )
## NB values are still wonky;
## better to use directly asked sleep hours
# unique(df$avg_sleep_hours_week_day)

df$avg_day_sleep_hours_indirect <- 1/7 * (
  5 * df$avg_sleep_hours_week_day +
    2 * df$avg_sleep_hours_weekend_day
)

df$avg_day_sleep_hours_direct <- 1/7 * (
  5 * an(df$sleep_time_week_day) +
    2 * an(df$sleep_time_weekend_day)
)


#### SDQ

#names(df) <- janitor::make_clean_names(names(df))
# # Checked there are no "" missing cases;
# # as these would need to be turned to NA for coalesce
# ulist <- map(
#   df %>%
#     select(
#       matches("sdq")
#       ),
#   unique
#   )
#
# unique(unlist(ulist))



## Rename SDQ; remove fluff ####
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

## Reverse scoring
sdq <- str_subset(names(df), "^sdq.*")

sdq_r <- c(
  "sdq_generally_well_behaved_usually_does_what_adults_request",
  "sdq_has_at_least_one_good_friend",
  "sdq_generally_liked_by_other_children",
  "sdq_can_stop_and_think_things_out_before_acting",
  "sdq_good_attention_span_sees_work_through_to_the_end"
)

df <- df %>%
  mutate(
    across(
      all_of(sdq),
      ~ to_snake_case(.)
    ),
    across(
      all_of(sdq),
      ~ case_when(
        . == "certainly_true" ~ 0,
        . == "somewhat_true" ~ 1,
        . == "not_true" ~ 2,
        TRUE ~ NA_real_
      )
    ),
    across(
      all_of(sdq_r),
      ~ case_when(
        . == 0 ~ 2,
        . == 1 ~ 1,
        . == 2 ~ 0,
        TRUE ~ NA_real_
      )
    )
  )


## Subscales and amalgamated scales

sdq_subscales <- list(
  prosocial = str_subset(sdq, "considerate|shares|helpful|kind|offers_to_help"),
  emotional_problems = str_subset(sdq, "headaches|worries|unhappy|clingy|fears"),
  conduct_problems = str_subset(sdq, "temper|well_behaved|fights|argumentative|spiteful"),
  hyperactivity = str_subset(sdq, "overactive|fidgeting|distracted|think|sees_work_through"),
  peer_problems = str_subset(sdq, "solitary|one_good_friend|liked|bullied|better_with_adults")
)

## From pdf "Scoring the Strengths & Difficulties Questionnaire for age 4-17"
sdq_superscales <- list(
  difficulty = setdiff(sdq, sdq_subscales$prosocial),
  externalizing = combine(sdq_subscales, "conduct_problems","hyperactivity"),
  internalizing = combine(sdq_subscales, "emotional_problems", "peer_problems")
)

## Scores

sdq_scales <- c(sdq_superscales, sdq_subscales)

for (i in seq(sdq_scales)){

  scale <- names(sdq_scales)[i]
  items <- sdq_scales[[i]]

  df[, paste0(scale, "_score")] <- rowSums(df[, items])

}


## Other ####
# Do not know why the below does not work
# code_PA <- function(df, x){
#   df %>%
#     mutate(
#       # !!sym(x) := str_split({{ x }}, "-| to ") %>% map_dbl(., ~ mean(an(.x)))
#       #   )
#       # ,
#       "{{ x }}_hours" := case_when(
#         !!sym(paste0(x, "_units")) == "Minutes" ~ an(!!sym(x))/60,
#         !!sym(paste0(x, "_units")) == "Hours" ~ an(!!sym(x)),
#         TRUE ~ NA_real_
#       )
#     )
# }
#
# df %>% reduce(cpa_quantity, code_PA)
#
# unique(df$PA_play_time_avg_weekend_day_hours)
# unique(an(df$PA_play_time_avg_weekend_day))
#
# map(cpa_quantity, ~unique(df %>% pull(.x)))
#
# ud <- unique(df$PA_play_time_avg_weekend_day)
#
# str_split(ud, "-| to ") %>% map_dbl(., ~ mean(an(.x)))
#
# # replace , with .
# ud
