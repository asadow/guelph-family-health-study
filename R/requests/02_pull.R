
prepare_vars()

# Get and clean --------------------------------------------------------------------
all_data <- get_data(requested_data,
                     vars,
                     requesting_phase_1,
                     data_type,
                     requested_asa_files)

all_data <- all_data %>%
  ### To avoid loss of pilot 1 ethnicity if match is by fid...
  ### No clue why this happens
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
  add_missing_dob_and_sex

dat <- dat %>%
  fill_vars(pid_static_vars) %>%
  ## RENAME THIS
  squish_child_rows(roles,
                    data_type_children,
                    vars_child_self_report,
                    vars_child_not_by_parent_survey,
                    vars_exclude_parents) %>%
  add_age(data_type) %>%
  calculate_if(requesting_calculations) %>%
  arrange_and_relocate

# ns %>% str_subset("ethnicity")
# dat %>% names %>% str_subset("ethnicity")

dat_final <- dat %>%
  prefix_parent_and_remove_child_prefix(remove_child_prefix) %>%
  select_given(vars) %>%
  pivot_wide(pivot_parents_wide) %>%
  remove_self_report_if_no(child_self_report) %>%
  select(- any_of(c("parent_1_in_study", "parent_2_in_study")))


# Write ----
dat_final %>% write_for(requester,
                        data_type,
                        file_type = file_type,
                        file_date_needed = file_date_needed,
                        needs_unnest_asa = needs_unnest_asa)

# s1 <- here("data", "processed", "requested", "saher_2022-09-14.csv") %>% read_csv
# dat %>% nrow
# s1 %>% add_count(pid, time_point, child_pid) %>% filter(n > 1)
# s1 %>% anti_join(dat, by = c("pid", "time_point", "child_pid"))
# status <- status_update(requested_data)
# status %>% write_status_for(requester)

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
