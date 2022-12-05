options(java.parameters = "- Xmx1024m")
library(xlsx)
library(stringi)
library(stringr)
library(anytime)
library(lubridate)
library(zscorer)
library(readr)
library(tidyverse)
library(here)
library(vroom)
library(glue)
library(janitor)
library(keyring)
master_path <- "/Users/Adam/Library/CloudStorage/OneDrive-UniversityofGuelph/Project Management/GFHS Master List.xlsx"

master <- xlsx::read.xlsx(
  master_path,
  sheetName = "GFHS Master List",
  password = key_get("MASTER_KEY"),
  header = TRUE,
  stringsAsFactors = F
  ) %>%
  clean_names %>%
  rename(fid = new_family_id, pid = participant_id) %>%
  mutate(
    address_updated = as.Date(
      as.numeric(as.character(address_update)),
      origin = "1899-12-30"
    ),
    across(
      c(first_name, last_name),
      ~ trimws(.x, which = c("both"), whitespace = "[ \t\r\n]")
    ),
    name = paste(first_name, last_name) %>% stri_trans_totitle
  )

ha <- "/Users/Adam/Library/CloudStorage/OneDrive-UniversityofGuelph/guelph-family-health-study/data/processed/incomplete/ha_all.csv" %>%
  read_csv %>%
  clean_names %>%
  arrange(pid, time_point) %>%
  select(pid, dob, gender) %>%
  filter(!duplicated(pid) & !str_detect(pid, "^(P|S)")) %>%
  mutate(
    dob = ymd(dob),
    dob = replace(
      dob,
      pid == "A0720",
      as.Date("2016-07-13")
    ),
    dob = replace(
      dob,
      pid == "B0720",
      as.Date("2018-01-16")
    ),
    bday_month_ha = format(dob, "%B") %>% factor(levels = month.name)
  )

child_names <- master %>%
  select(fid, pid, first_name, last_name, name) %>%
  filter(!is.na(pid) & !str_detect(pid, "^(P|S|T)"))
child_dob_list <- merge(child_names, ha, by = "pid", all = TRUE)

## Contact info for kids #####
## Get data set of just first rows of every family
## This is the parent 1 row with contact info

parent_one <- master %>%
  group_by(fid) %>%
  fill(
    c(fid_some_discontinued, street_address),
    .direction = c("downup")
    ) %>%
  ungroup() %>%
  filter(!is.na(pid) & !duplicated(fid))

address_info <- parent_one %>%
  select(fid, mailing_name, street_address,
         city, province_state, country,
         postal_code, address_updated,
         fid_some_discontinued, pid_continued)

bday_list <- merge(child_dob_list, address_info, by = "fid", all = T)

bday_list <- bday_list %>%
  mutate(
    birthday_month = bday_month_ha,
    dob = ymd(dob),
    age = time_length(
      interval(dob, today()),
      unit = "year"
      ) %>% round(2),
    parent_mailing_names = mailing_name,
    across(# remove discontinues
      c(fid_some_discontinued, pid_continued),
      ~ if_else(is.na(.x), "0", .x)
    )
  ) %>%
  arrange(birthday_month, dob, age) %>%
  filter(
    !is.na(first_name) &
    !stri_trans_totitle(trimws(fid_some_discontinued)) %in% "Y"
    | pid_continued %in% "Y"
    ) %>%
  select(birthday_month, dob, gender, age, first_name, last_name,
    parent_mailing_names, street_address, city,
     postal_code, address_updated)

write.csv(
  bday_list,
  file = glue(
    "/Users/Adam/Library/CloudStorage/OneDrive-UniversityofGuelph/Project Management/Birthdays/",
     "birthday-card-list_{Sys.Date()}.csv"
    ),
  row.names = FALSE
  )


