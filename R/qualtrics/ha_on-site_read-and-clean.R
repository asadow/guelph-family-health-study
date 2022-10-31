# surveys <- all_surveys()
onsite <- surveys %>%
  filter(name %>% str_detect("In Person")) %>%
  import_all_data

onsite <- onsite %>%
  unnest(data, names_repair = make.unique) %>%
  clean_names

redundant <- c("yes_please_describe",
              "_selected_choice",
              "participant_info_",
              "_ri",
              "_ha$",
              "ha_",
              "ohms",
              "stolic",
              "reading",
              "height",
              "weight_reading_3_decimals",
              "researcher",
              "to_nearest_0_1_cm",
              "waist_circumference",
              "weight",
              "my_weight_kg_with_1_decimal")

redundant <- redundant %>% glue_collapse("|")

pattern_replace <- list(
  c("wt", "bm"),
  c(redundant, ""),
  c("__", "_"),
  c("_$|^_", ""),
  c("^(?!.*(oral_assent|initial))bp_", ""),
  c("(\\d)(nd|st|rd|th)", "\\1"),
  c("^(\\d)_(.*)", "\\2_\\1"),
  c("(bp|bia)_(\\d)_(.*)", "\\1_\\3_\\2"),
  c("bp_sy_", "bp_sys_"),
  c("blood_pressure", "bp"),
  c("heart_rate", "hr"),
  c("reactance", "rea"),
  c("phase_angle", "pa"),
  c("resistance", "res"),
  c("impedance", "imp"),
  c("bod_pod_food_or_drink_past", "i_eat_drink_past"),
  c("bod_pod_exercise", "i_exercise"),
  c("bia_bia", "bia"),
  c("mm_hg", "mmhg"))

for (i in seq(pattern_replace)){
  pr <- pattern_replace[[i]]
  onsite <- onsite %>%
    rename_with(~ str_replace_all(.x, pr[1], pr[2]))
}

#
# onsite <- onsite %>%
#   rename_with(~ str_replace(.x, "wt", "bm")) %>%
#   rename_with(~ str_remove_all(.x, redundant)) %>%
#   rename_with(~ str_replace_all(.x, "__", "_")) %>%
#   rename_with(~ str_remove_all(.x, "_$|^_")) %>%
#   rename_with(~ str_remove(.x, "^(?!.*(oral_assent|initial))bp_")) %>%
#   rename_with(~ str_replace_all(.x, "(\\d)(nd|st|rd|th)", "\\1")) %>%
#   rename_with(~ str_replace_all(.x, "^(\\d)_(.*)", "\\2_\\1")) %>%
#   rename_with(~ str_replace_all(.x, "(bp|bia)_(\\d)_(.*)", "\\1_\\3_\\2")) %>%
#   # rename_with(~ str_replace_all(.x, "ht", "height")) %>%
#   rename_with(~ str_replace_all(.x, "bp_sy_", "bp_sys_")) %>%
#   rename_with(~ str_replace_all(.x, "blood_pressure", "bp")) %>%
#   rename_with(~ str_replace_all(.x, "heart_rate", "hr")) %>%
#   rename_with(~ str_replace_all(.x, "reactance", "rea")) %>%
#   rename_with(~ str_replace_all(.x, "phase_angle", "pa")) %>%
#   rename_with(~ str_replace_all(.x, "resistance", "res")) %>%
#   rename_with(~ str_replace_all(.x, "impedance", "imp")) %>%
#   rename_with(~ str_replace_all(.x, "bod_pod_food_or_drink_past", "i_eat_drink_past")) %>%
#   rename_with(~ str_replace_all(.x, "bod_pod_exercise", "i_exercise")) %>%
#   rename_with(~ str_replace_all(.x, "bia_bia", "bia"))

onsite <- onsite %>%
  rename(room_temp_celsius = bod_pod_info_room_temp_c,
         room_humidity_percent = bod_pod_info_room_humidity_percent,
         ## HA name = name in this data
         # Switch order? Change HA name?
         researcher_initials_for_bod_pod_and_child_weight = bod_pod_info_initials,
         pacemaker = bia_pacemaker,
         i_pregnancy_due_date_ha = pregnant_yes_enter_due_date_mm_yyyy_text,
         i_pregnant_ha = pregnant_selected_choice,
         ht_initials = ht_cm_initials,
         bia_initials = bia_s_initials,
         bm_kg = bm_kg_kg,
         bm_notes = bm_kg_notes,
         ht_notes = ht_cm_notes,
         wc_cm_3 = wc_cm_3__3,
         ht_cm_3 = ht_cm_3__3,
         date_ha = date,
         i_breastfeeding_notes_ha = bf_notes)

onsite <- onsite %>%
  mutate(bm_initials = coalesce(!!!select(., matches("bm.*initials"))),
         bp_initials = coalesce(!!!select(., matches("bp.*initials"))),
         wc_initials = coalesce(!!!select(., matches("wc.*initials"))),
         wc_notes = coalesce(!!!select(., matches("wc.*notes"))),
         wc_cm_1 = coalesce(wc_1, wc_cm_1),
         wc_cm_2 = coalesce(wc_2, wc_cm_2),
         pid = coalesce(participant_id_eg_p0073_or_s0073,
                        participant_id_eg_a0073_b0073_or_c0073)) %>%
  select( - c(bm_kg_initials,
              bm_kg__initials,
              bp_s_initials,
              wc_cm_initials,
              wc_cm_notes,
              wc_1,
              wc_2))

stopifnot(onsite %>% filter(pid != pid_participant_id_repeat) %>% nrow == 0)


# Means -------------------------------------------------------------------
## Check for non numeric characters
regex_numeric_cols <- "ht_cm|wc_cm|bp_(dia|sys)_mmhg|bm_kg|hr_bpm|bia_(imp|pa|res|rea)"

onsite %>%
  filter(
    if_any(matches(regex_numeric_cols),
           ~ .x %>% str_detect("[^0-9.]")
      )
    )

onsite <- onsite %>%
  mutate(
    across(
      matches(regex_numeric_cols),
      as.numeric
    )
  )

## bia, hr

onsite <- onsite %>%
  mutate(
    ht_cm = rowMeans(
      select(., matches("ht_cm_\\d")),
      na.rm = TRUE
      ),
    wc_cm = rowMeans(
      select(., matches("wc_cm_\\d")),
      na.rm = TRUE
    ),
    bp_dia_mmhg = rowMeans(
      select(., matches("bp_dia_mmhg_\\d")),
      na.rm = TRUE
    ),
    bp_sys_mmhg = rowMeans(
      select(., matches("bp_sys_mmhg_\\d")),
      na.rm = TRUE
    ),
    # date_ha = mdy(date_ha),
    ## recorded_date is more accurate than original date_ha
    date_ha = as_datetime(recorded_date, tz = "UTC"),
    i_pregnancy_due_date_ha = mdy(i_pregnancy_due_date_ha)
  )

add_2 <- function(x){as.numeric(x) + 2}

onsite <- onsite %>%
  mutate(
    across(
      matches(regex_numeric_cols),
      as.character
    ),
    ## gender in all_data (see 02_pull.R) will be renamed sex
    gender = case_when(
      sex == "Male" ~ "M",
      sex == "Female" ~ "F",
      TRUE ~ sex
    ),
    ## sex here is just for calculations
    sex = case_when(
      sex == "Male" ~ 1,
      sex == "Female" ~ 2,
      TRUE ~ NA_real_
    ),
    time_point = time_point %>%
      str_extract("\\d") %>%
      add_2 %>%
      {glue("t{.}")}
  )

ha_qualtrics <- onsite %>%
  rename_with(
    ~ glue("{.x}_ha"),
    !c(pid,
      time_point,
      date_ha,
      gender,
      any_of(flags_ha),
      matches(regex_numeric_cols))
  )

dups <- ha_qualtrics %>%
  add_count(pid, time_point) %>%
  filter(n > 1) %>%
  select(pid, time_point, date_ha, progress_ha, finished_ha)

ha_qualtrics <- ha_qualtrics %>%
  add_count(pid, time_point) %>%
  filter(
    ! (n > 1 & finished_ha == "False")
    ) %>%
  select(- n)

dups <- ha_qualtrics %>%
  add_count(pid, time_point) %>%
  filter(n > 1)

assert_that(nrow(dups) == 0)

ha_qualtrics %>% write_rds(
  here("data", "processed", "incomplete", "ha_on-site_post-qualtrics.csv")
  )


# Questions ---------------------------------------------------------------

# Why not use recorded date?
# Should I assume gender from sex?

