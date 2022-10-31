# Data ####

## Read ####
### Lifelabs [LL] ####
# Blood measures

ll <- vroom(
  paste0(cle_p, "lifelabs.csv"),
  col_types = c(.default = "c")
  )

ll <- ll %>%
  rename(
    time_point = time.point
  ) %>%
  mutate(
    date_ll = ymd(CDATE),
    time_point = str_replace_all(time_point, ntp),
    HBA1C = as.numeric(HBA1C),
    HBA1C = case_when(
      # turn remaining decimals into percentages
      HBA1C < 0.1 ~ 100*HBA1C,
      TRUE ~ HBA1C
    )
  )

###  ---------ASA-24 [ASA] ####
# 24-hour diet intakes and HEI calculation

asa <- vroom(
  paste0(cle_p, "asa_totals.csv"),
  col_types = c(.default = "c")

  )

asa <- hei(asa)

## --- Merge HA's #####

### --------- Health Assessment [HA]  ####
# Health measures, date of measures

ha <- vroom(
  paste0(cle_p, "ha.csv"),
  col_types = c(.default = "c")
  ) %>%
  mutate( # needed due to rows created for missed HA
    time_point = case_when(
      mi(time_point) ~ "t1",
      TRUE ~ time_point
    ),
    FID = as.numeric(FID)
  )

###  ---------At-Home Health Assessment [AHHA]----

## code that height is to be replaced
## by the measured height in the lag time point
## from data source ha
ahha <- read.csv(
  paste0(cle_p, "ahha.csv"),
  header = TRUE,
  check.names = FALSE
)

hh <- bind_rows(ahha, ha)

names(hh) <- str_replace_all(
  names(hh),
  "flag",
  "i"
)

hh <- hh  %>%
  mutate(
    across(
      c(bm_kg_ahha, ht_cm_ahha, wc_cm_ahha),
      ~ as.character(.x)
    ),
    location = case_when(
      !mi(date_ha) ~ "on-site",
      !mi(date_ahha) ~ "at-home",
      TRUE ~ NA_character_
    ),
    i_breastfeeding_ha = coalesce(i_breastfeeding_ahha, i_breastfeeding_ha),
    i_pregnant_ha = coalesce(i_pregnant_ahha, i_pregnant_ha),
    i_note_ha = i_comments_ahha,
    date_ha = coalesce(date_ahha, date_ha),
    bm_kg = coalesce(body_mass_kg, bm_kg_ahha),
    ht_cm = coalesce(height_mean_cm, ht_cm_ahha),
    wc_cm = coalesce(wc_mean_cm, wc_cm_ahha),
    i_note_bm = coalesce(i_note_body_mass, bm_notes),
    i_note_wc = coalesce(i_note_wc, wc_notes),
    i_note_ht = coalesce(i_note_height, ht_notes)
  ) %>%
  rename(
    i_measures_wc = i_count_measures_wc,
    i_measures_ht = i_count_measures_height,
    i_measures_bia = i_count_measures_bia,
    i_measures_bp = i_count_measures_bp,
    i_broke_sop_ht = i_broke_sop_height,
    bia_res_ohm = bia_res_mean_ohm,
    bp_dia_mmhg = bp_dia_mean_mmhg,
    bp_sys_mmhg = bp_sys_mean_mmhg
  )

##  ---------Surveys [SV]---------------
sv <- vroom(
  paste0(cle_p, "sv_t1_t2_t3_t4_phase_1_2_3.csv"),
  col_types = c(.default = "c")
)

# sv12 <- vroom(
#   paste0(cle_p, "sv_t1_t2_phase_3.csv"),
#   col_types = c(.default = "c")
# )
#
# sv34 <- vroom(
#   paste0(cle_p, "sv_t3_t4_phase_3.csv"),
#   col_types = c(.default = "c")
# )
#
# sv <- bind_rows(sv12, sv34)

###  ---------Pilot Demographics---------------

dem_p <- paste0(
  ref_p,
  "Demographics - Pilot 1 and Pilot 2",
  sl
  )

pdemo <- vroom(
  paste0(dem_p, "t1_phase_1_2_parent_demographics.csv"),
  col_types = c(.default = "c")
)
cdemo <- vroom(
  paste0(dem_p, "t1_phase_1_2_child_demographics.csv"),
  col_types = c(.default = "c")
)
ethn <- plyr::rbind.fill(pdemo, cdemo)

names(ethn)[10:12] <- c(
  "ethnicity_selected_first",
  "ethnicity_selected_second",
  "ethnicity_written_other"
  )
names(ethn)[8:9] <- c(
  "country_born_in_selected",
  "country_born_in_other"
  )

ethn$time_point <- "t1"
ethn <- ethn %>%
  mutate(
    time_point = "t1",
    household_income = select(
      .,
      matches("income of your household")
      ) %>% pull,
    education = select(
      .,
      matches("highest grade or degree")
      ) %>% pull,
    parent_sv = case_when(
      grepl("P", PID) ~ "1",
      grepl("S", PID) ~ "2",
      TRUE ~ NA_character_
    )
  )
ethn <- add_phase(ethn)
ethn <- ethn %>%
  filter(
    !mi(PID) & !mi(FID)
  )


## Clean and Merge ####

datalist <- list(hh, asa, ll, ethn, sv)

# Standardize FID

datalist <- datalist %>%
  map(
    ~ .x %>%
      mutate(
        FID = as.numeric(
          str_sub(PID, start = -3)
        )
      )
  )

# Check for missing IDs after fix
datalist %>%
  milist(PID)

datalist %>%
  milist(FID)

datalist %>%
  milist(time_point)


idt <- c("PID", "FID", "time_point")

df <- datalist %>%
  reduce(
    full_join,
    by = idt
  ) %>%
  add_phase %>%
  mutate(
    education = coalesce(education.x,
                         education.y),
    parent_sv = coalesce(parent_sv.x,
                         as.character(parent_sv.y)
                         ),
    household_income = coalesce(household_income.x,
                                household_income.y)
  )  %>%
  # Remove drop-out families
  filter(
    !FID %in% drop_outs$FID_decline
    )
names(df) <- str_replace_all(names(df), "flag", "i")

# Check whether there are still any in data
# that are not on master
dont_belong <- df %>%
  filter(
    !FID %in% master$FID
    | !PID %in% master$PID
    #| !child_pid %in% master$PID
  ) %>%
  select(PID, time_point)

stopifnot(nrow(dont_belong) == 0)


# Master Merge ####

# Join with missing PIDs per time_point
# Nest data by time_point

dfn <- df %>%
  group_by(time_point) %>%
  nest()

dfn <- dfn %>%
  mutate(
    data = map(
      data,
      ~ full_join(
        .x,
        master %>%
          filter(
            !mi(PID) & !mi(FID)
            ) %>%
          select(PID, FID),
        by = c("PID", "FID")
        )
    )
  )

df <- dfn %>%
  group_by(time_point) %>%
  unnest(
    cols = c(data)
  )


## Filling Demographics ####
PID_static_vars <- c(# These are "filled"
  "dob",
  "gender",
  "ethnicity",
  "ethnicity_selected_first",
  "ethnicity_selected_second",
  "ethnicity_written_other",
  "country_born_in_selected",
  "country_born_in_other"
)

# FID_static_vars <- c(
#   "ethnicity",
#   "ethnicity_selected_first",
#   "ethnicity_selected_second",
#   "ethnicity_written_other",
#   "country_born_in_selected",
#   "country_born_in_other"
# )

vars_to_append <- c(
  PID_static_vars,
  "education",
  "marital_status",
  "lives_with_partner",
  "number_children",
  "number_children_18mo_to_5yr",
  "household_income",
  "household_number_adults"
)

# Per PID

df <- df %>%
  add_ethnicity %>%
  add_phase %>%
  add_role %>%
  group_by(PID) %>%
  mutate(
    across(
      all_of(PID_static_vars),
      ~na_if(., "")
    )
  ) %>%
  fill(
    all_of(PID_static_vars),
    .direction = c("downup")
  )

### Calculate Age ####

origin <- c("ha", "ll", "asa", "sv")
ages <- paste0("age_", origin)
dates <- paste0("date_", origin)

df <- df %>%
  mutate(
    date_asa = mdy(date_asa),
    date_sv = as.Date(date_sv)
  )

for (i in seq(origin)) {
  #df[, dates[i]] <- anydate(df %>% pull(dates[i]))
  df[, ages[i]] <- time_to_from(
    df %>% pull(dates[i]),
    df %>% pull(dob),
    "years"
  )
}

# # Parent vars for each child
#
# df_t1 <- df %>%
#   filter(time_point == "t1") %>%
#   distinct( # Get one row from multiple rows per PID (1 per child_pid)
#     PID,
#     .keep_all = TRUE
#     ) %>%
#   select(
#     FID,
#     PID,
#     all_of(FID_static_vars)
#   )
#
# appended_t1 <- df_t1 %>%
#   append_parent("P", "one", FID_static_vars) %>%
#   full_join(
#     df_t1 %>%
#       append_parent("S", "two", FID_static_vars),
#     by = "FID"
#     )

# Family time-point level

# Parent one and two vars for each child

tv <- df %>%
  distinct( # needed due to multiple rows per PID (1 per child)
    PID,
    time_point,
    .keep_all = TRUE
  ) %>%
  select(
    FID,
    PID,
    time_point,
    any_of(vars_to_append)
  )

appended <- append_parent(tv, "P", "one", vars_to_append) %>%
  full_join(
    append_parent(tv, "S", "two", vars_to_append),
    by = c("FID", "time_point")
    )

df <- df %>%
  # full_join(appended_t1, by = "FID") %>%
  left_join(
    appended,
    by = c(
      "FID",
      "time_point"
    )
  )

## --BMI Z-Score----------------------------------------------------------------------

df <- df %>%
  mutate(
    age_ha_days = time_to_from(
      date_ha,
      dob,
      "days"
    ),
    sex = case_when(
      gender == "M" ~ 1,
      gender == "F" ~ 2,
      TRUE ~ NA_real_
    ),
    bm_kg_numeric = as.numeric(bm_kg),
    ht_cm_numeric = as.numeric(ht_cm)
  )

df <- addWGSR(
  data = df,
  sex = "sex",
  firstPart = "bm_kg_numeric",
  secondPart = "ht_cm_numeric",
  thirdPart = "age_ha_days",
  index = "bfa"
) %>%
  rename(bmi_z = bfaz)

df <- df %>%
  mutate(
    FID_order = case_when(
      grepl("P", PID) ~ 1,
      grepl("S", PID) ~ 2,
      grepl("A", PID) ~ 3,
      grepl("B", PID) ~ 4,
      grepl("C", PID) ~ 5,
      grepl("D", PID) ~ 6,
      TRUE ~ as.numeric(NA)
    )
  ) %>%
  arrange(
    FID,
    time_point,
    FID_order
  )

# Export ####

vroom_write(
  df,
  paste0(
    cle_p,
    # paste(origin, collapse = "_"),
    "df",
    ".csv"
  )
)
