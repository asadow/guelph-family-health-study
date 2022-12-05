p <- here("data", "processed", "requested", "haines_cebq.rds")
dat <- read_rds(p)

cdat <- dat %>%
  select(- starts_with("parent_2")) %>%
  rename_with(~ .x %>% str_remove("parent_1_child_"))
#
#
# cdat %>%
#   group_by(phase) %>%
#   summarize(
#     across(starts_with("cebq"),
#            ~ sum(!is.na(.x)))
#   )

cdat <- cdat %>%
  ## Why is there a duplicate named column with some data missing?
  select(- matches("eat_too_much_1")) %>%
  add_phase %>%
  # select(phase, dob, date_ha, starts_with("cebq"), matches("bm_kg|ht_cm|asa_totals")) %>%
  rename_with(~ .x %>% str_remove("cebq_35_"))

cdat <- cdat %>%
  unnest(asa_totals)

cdat <- cdat %>% hei %>% add_bmi_z()

ff <- c("refuses_new_foods",
        "dislike_without_tasting" ,
        "hard_to_please",
        "enjoys_new_foods",
        "variety_of_foods",
        "tasting_new_foods")
sr <- c("full_easily",
        "leaves_food_on_plate",
        "full_before_meal_finished",
        "snack",
        "big_appetite")
fr <- c("always_asking_for_food",
         "eat_too_much",
         "eat_most_of_the_time",
         "favourite_foods",
         "food_in_mouth")

level_key <- c("Never" = 1,
              "Rarely" = 2,
              "Sometimes" = 3,
              "Often" = 4,
              "Always" = 5,
              "I am not comfortable answering this question" = NA_real_)

cc <- cdat %>%
  mutate(
    across(
      all_of(c(ff, sr, fr)),
      ~ recode(.x, !!!level_key)
    ),
    ## Reverse
    across(
      c(enjoys_new_foods,
        variety_of_foods,
        tasting_new_foods,
        big_appetite),
      ~ 6 - .x
      )
  )

cb <- function(x) cut(x, breaks = c(0.99, 3, 5))

# cut(cc$satiety_responsiveness,
#     breaks = c(0, 3.3, 5),
#     right = TRUE)

cc <- cc %>%
  mutate(
    food_fussiness = rowMeans(across(all_of(ff)), na.rm = TRUE),
    satiety_responsiveness = rowMeans(across(all_of(sr)), na.rm = TRUE),
    food_responsiveness = rowMeans(across(all_of(fr)), na.rm = TRUE),
    across(c(food_fussiness,
             food_responsiveness),
           ~ cb(.x),
           .names = "{.col}_cat"
           ),
    satiety_responsiveness_cat = cut(satiety_responsiveness,
                                      breaks = c(1, 3, 5),
                                      right = FALSE),
    food_fussiness_2nd_cat = cut(food_fussiness, breaks = c(0.99, 3.3, 5)),
  )

cats <- names(cc) %>% str_subset("_cat$")

map(cc[cats],
     ~ cc %>%
          group_by(!!.x) %>%
          summarise(
            n = n()
          ) %>%
         clean_names %>%
         rename(level = fct)
    )
cc <- cc %>%
  distinct(fid, .keep_all = TRUE)

library(gtsummary)

sum_by <- function(x){
  cc %>%
    select(bmi_z, HEI2015_TOTAL_SCORE, {{ x }}) %>%
    tbl_summary(by = {{ x }}) %>%
    add_n() %>% # add column with total number of non-missing observations
    add_p() %>% # test for a difference between groups
    modify_header(label = "**Variable**") %>% # update the column header
    bold_labels()
}

map(cats, sum_by)

