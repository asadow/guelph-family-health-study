### [Intervention Status]

wb <- loadWorkbook(
  paste0(fpath, "GFHS Study Visit Tracker.xlsx"),
  password = key_get("ASA_LOGIN_KEY")
)

ws <- readWorksheet(
  wb,
  sheet = c("Pilot Study", "Pilot Phase 2", "Full Study"),
  colTypes = c(XLC$DATA_TYPE.STRING)
)

tracker <- bind_rows(ws) %>% clean_names

tracker <- tracker %>%
  rename(fid = x_family_id) %>%
  mutate(group = intervention)

## On considering 2 home visit as being in the same group
# as 4 home visit intervention
## Jess (Jan. 14, 2022)
## Yes, please consider these individuals/families as
## intervention together with those that received 4 home visits.

tracker <- tracker %>%
  select(fid, group, intervention) %>%
  mutate(
    group = str_replace_all(group, " ", ""),
    group = case_when(
      group %in% c("2HV", "4HV") ~ "I",
      group %in% c("Control", "C") ~ "C",
      TRUE ~ group
    ),
    fid = as.numeric(fid)
  ) %>%
  filter(!is.na(fid))

tracker <- tracker %>% filter(!fid == 44)

write.csv(tracker, here("data", "iv_status.csv"))
