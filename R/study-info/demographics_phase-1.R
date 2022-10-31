## Pilot demographics ------------------------------------------------------

demo_path <-   here("data", "processed", "other")

demo_file <- "t1_phase_1_2_parent_demographics.csv"

pdemo <- read_csv(
  here(demo_path, demo_file),
  col_types = c(.default = "c")
)

cdemo <- read_csv(
  here(demo_path, demo_file %>% str_replace("parent", "child")),
  col_types = c(.default = "c")
)

pcdemo <- bind_rows(pdemo, cdemo)

names(pcdemo)[10:12] <- c(
  "ethnicity_selected_first",
  "ethnicity_selected_second",
  "ethnicity_written_other"
)

names(pcdemo)[8:9] <- c(
  "country_born_in_selected",
  "country_born_in_other"
)

demo_phase_1 <- pcdemo %>%
  clean_names %>%
  rename(
    school_completed = what_is_the_highest_grade_or_degree_you_completed_in_school,
    income_household = what_is_the_total_annual_income_of_your_household_before_taxes,
    children = number_of_children,
    country_born_selected_choice = country_born_in_selected,
    country_born_other_please_specify_text = country_born_in_other
  ) %>%
  filter(
    !mi(pid)
    & !mi(fid)
    & as.numeric(fid) < 300
  ) %>%
  select(- c(fid, gender, dob, timepoint))

demo_phase_1 %>%
  write_rds(
    here("data", "processed", "other", "demographics_phase-1.rds")
  )
