
# Random sample -----------------------------------------------------------

parent_child_ids <- here("data",
                         "processed",
                         "parent-child-ids.csv") %>%
  read_csv %>%
  add_phase %>%
  distinct(pid, phase)

get_s <- function(character, phase){
  parent_child_ids %>%
    filter(pid %>% str_detect(!!character) &
           phase == !!phase
           ) %>%
    sample_n(1) %>%
    select(pid)

}

set.seed(1)
pid_letters <- c("P", "S", "A", "B") %>% list %>% rep(3)
phases <- 1:3 %>% map(~ rep(.x, 4))
times <- sample(1:8, 12, replace = TRUE) %>% {glue("t{.}")}
samples <- tibble(pid_letters, phases) %>% unnest

random_data <- map2(samples$pid_letters,
     samples$phases,
     ~ get_s(.x, .y)) %>%
  bind_rows %>%
  mutate(time_point = times)

survey_demographics <- read_and_glue_labels() %>%
  filter(category_or_block %>% str_detect("demographics")) %>%
  pull(label)

random_cols <- list(survey_demographics,
     nasa,
     raw_measures_ha_snake_case %>%
       str_subset("3|4", negate = TRUE)) %>%
  map_chr(~ sample(.x, 1)) %>%
  c("ldl",
    "date_ahha",
    "screen_time_weekday") %>%
  to_snake_case()

random_data[random_cols] <- NA

# random_data <- random_data %>% sample_n(5)

random_data %>% write_csv(here("data",
                               "manually-entered",
                               "raw-sample.csv"))


# Request Info ------------------------------------------------------------


random_data <- read_csv(here("data",
                               "manually-entered",
                               "raw-sample.csv"))


requested_times <- random_data %>%
  add_fid %>%
  add_phase %>%
  distinct(time_point, phase)

file_date_needed <- FALSE
pivot_parents_wide <- TRUE
requesting_calculations <- FALSE
needs_unnest_asa <- FALSE
child_self_report <- TRUE
remove_child_prefix <- TRUE
requester <- "random"
data_type <- c("survey", "ha", "ahha", "asa", "ll")
data_type_children <- data_type

requested_asa_files <- "totals"
roles <- c("parent", "child")
file_type <- "rds"

labels <- random_cols %>% c("asa_totals", "food_skills")

vars_child_self_report <- "food_skills"
vars_exclude_parents <- "\\b\\B"
vars_child_not_by_parent_survey <- c("hr_bpm_2",
                                     "ldl",
                                     "asa_totals")

source(here("R", "requests", "02_pull.R"))
 # vars_exclude_parents <- c("bmi_z",
#                           "bm_kg",
#                           "ht_cm",
#                           "i_note_ht",
#                           "i_note_bm",
#                           # "asa_totals",
#                           scales,
#                           "screen_time_weekday",
#                           "screen_time_weekend_day")
# "owns_screen_device",

# Check data --------------------------------------------------------------
sample_path <- here("data", "processed",
                    "requested", "random.rds")

pulled_data <- sample_path %>% read_rds

children <- pulled_data %>%
  mutate(ethnicity_other = parent_1_child_ethnicity_other_please_specify_text,
         ethnicity_selected_choice = parent_1_child_ethnicity_selected_choice,
         ethnicity = parent_1_child_ethnicity,
         screen_time_weekday = parent_1_child_screen_time_weekday)

parents <- pulled_data %>%
  select(time_point, matches("^parent", perl = TRUE)) %>%
  pivot_longer(matches("parent_\\d"),
               names_to = c("parent", ".value"),
               names_pattern = "parent_(.)_(.*)")

dat <- children %>% bind_rows(parents)
dat <- dat %>% distinct(pid, time_point, .keep_all = TRUE)
dat <- dat %>%
  unnest(asa_totals, keep_empty = TRUE) %>%
  clean_names %>%
  select(pid, time_point,
         ethnicity,
         ethnicity_selected_choice,
         ethnicity_other_please_specify_text,
         any_of(names(random_data)),
         matches("use_knife"))

result <- random_data %>%
  select(pid, time_point) %>%
  left_join(dat,
             by = c("pid", "time_point"))

result %>%
  write_csv(here("data", "processed", "raw-sample_filled-by-r.csv"))


#
#
#
# ## To pivot data as is
# pulled_data %>%
#   rename_with(~ glue("child_{.x}"),
#               matches("^(?!.*parent).*", perl =TRUE)) %>%
#   pivot_longer(matches("parent_\\d"),
#                names_to = c("parent", ".value"),
#                names_pattern = "parent_(.)_(.*)")
#
# map(pulled_data, is.list) %>% unlist %>% any
#
