
ui_todo("Querying data for {requester}...")

source(here("R", "requests", "01_prepull.R"))

# Get and clean --------------------------------------------------------------------
all_data <- get_data(qualtrics_requested_data,
                     vars,
                     requesting_phase_1,
                     data_type,
                     requested_asa_files)

all_data <- all_data %>%
  ### To avoid loss of pilot 1 ethnicity if match is by fid...why this happens?
  map(~ .x %>% select(-fid)) %>%
  reduce(
    full_join,
    by = c("pid", "time_point")
  ) %>%
  clean_names %>%
  rename(progress_sv = progress,
         sex = gender) %>%
  ## Remove technical vars
  select(- any_of(file_vars_asa),
         - any_of("age"),
         - study_survey,
         - trigger_survey_id)

## Store names for later use
ns <- names(all_data) %>%
  str_replace("child_\\d", "child") %>%
  remove_1st_of_2_consec %>%
  unique

# Format data -------------------------------------------------------------

dat <- all_data %>%
  select_given(vars) %>%
  squish_on_child_and_self_surveys %>%
  pivot %>%
  add_missing_pairs(times_and_ids) %>%
  add_static_vars(requesting_phase_1) %>%
  fix_dob_and_sex

dat <- dat %>%
  fill_vars(pid_static_vars) %>%
  ## rename squish_child_rows fnc
  squish_child_rows(roles,
                    data_type_children,
                    vars_child_self_report,
                    vars_child_not_by_parent_survey,
                    vars_exclude_parents) %>%
  add_age(data_type) %>%
  arrange_and_relocate

dat_final <- dat %>%
  prefix_parent_and_remove_child_prefix(remove_child_prefix) %>%
  calculate_if(requesting_calculations) %>%
  select_given(vars) %>%
  pivot_wide(pivot_parents_wide) %>%
  remove_self_report_if_no(child_self_report) %>%
  select(- any_of(c("parent_1_in_study", "parent_2_in_study")))

# Write ----
pre_path <- here("data",
                 "processed",
                 "requested")

file_name <- if(file_date_needed){
  glue("{requester}_{Sys.Date()}.{file_type}")
} else {glue("{requester}.{file_type}")}

path <- here(pre_path, file_name)

write_for(dat_final,
          path,
          data_type,
          file_type = file_type,
          file_date_needed = file_date_needed,
          needs_unnest_asa = needs_unnest_asa)

ui_done("Processed data saved to {path}")

# Develop -----------------------------------------------------------------

## Specify questions asked on child
## in squish_parent_rows(), pivots to row per child,

## Need to let requesters know that some parent_two's are giving data as
## if they are parent_one

## children_18mo_to_5yr and children_18mo_to_5yr_1
## due to being duplicated in a survey

## Create dependencies like ------------------------------------------------

## If requesting HA but not parents, do not include preg or breastfeeding
## If requesting BIA, include set of variables like bia_calc vector of names and
## bia, vector of names of raw measures


## Warn when Master is updated?

## --If bmi
# ## Note warnings are from "N/A (pregnant)" in bm_kg
# add_bmi_z %>%
