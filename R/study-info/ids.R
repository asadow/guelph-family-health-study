# GFHS Membership (each contains 'family identifiers': fid's)----
### [Master]----
#### Personal-names, emails, 'participant identifiers': pid's
##### "GFHS Master List"
fpath <- "/Users/Adam/Library/CloudStorage/OneDrive-UniversityofGuelph/Project Management/GFHS Master List.xlsx"

wb <- loadWorkbook(
  paste0(fpath, "GFHS Master List.xlsx"),
  ## To set-up for key_get, use key_set("MASTER_KEY")
  password = key_get("MASTER_KEY")

)

master <- readWorksheet(
  wb,
  sheet = getSheets(wb),
  colTypes = c(XLC$DATA_TYPE.STRING)
)

Encoding(master$First.Name) <- "UTF-8"

master <- tibble(master) %>%
  mutate(
    first_name = title_trim(First.Name),
    last_name = title_trim(Last.Name),
    email = title_trim(Email.Adress),
    pid = Participant.ID,
    fid = as.numeric(New.Family.ID)
  ) %>%
  filter(
    !pid %in% c("Number of Males",
                "Number of Females",
                "Number of Children")
  ) %>%
  unite_names

master <- add_role(master)
master <- add_phase(master)

### [Declines/Drop-Outs]-----
#### Parent names
##### "GFHS Study Visit Tracker_Current": "Full Study Decline" sheet

wb <- XLConnect::loadWorkbook(
  paste0(fpath, "GFHS Study Visit Tracker.xlsx"),
  password = key_get("ASA_LOGIN_KEY")
)

declines <- XLConnect::readWorksheet(
  wb,
  sheet = c("Pilot Decline", "Full Study Decline"),
  colTypes = c(XLC$DATA_TYPE.STRING)
)

declines <- bind_rows(declines)

declines <- declines %>%
  rename(
    P1.Surname = P1..Surname
  ) %>%
  pivot_longer(
    cols = matches("^p\\d"),
    names_to = c("parent", ".value"),
    names_pattern = "P(.)\\.?(.*)",
  )

#p1 represents parent 1
declines <- tibble(declines) %>%
  mutate(
    first_name = title_trim(First.Name),
    last_name = title_trim(Surname),
    fid_decline = as.numeric(X.Family.ID),
    last_name = replace(
      last_name,
      grepl("O.*Neill", last_name),
      "O'neill"
    )
  ) %>%
  unite_names

## Checks ####
# families on both declines and master
both <- declines %>%
  inner_join(master,
             by = "name",
             na_matches = "never")

# # 3 families restarted the study - [Master] contains new fid's
# both %>%
#   select(fid, fid_decline, name, Restarted) %>%
#   filter(!is.na(fid))

drop_outs <- declines %>%
  filter(!Restarted %in% "Y") %>%
  mutate(i_drop_out = 1) %>%
  filter(!mi(name))
# drop out status

misc_status <- master %>%
  filter(
    name %in% drop_outs$name
    & !name == ""
  ) %>%
  select(name, email, pid)

assert_that(nrow(misc_status) == 0)

study <- bind_rows(master, drop_outs)

study <- study %>%
  filter(
    grepl("^P|^S", pid)
    | is.na(pid)
  ) %>%
  select(fid, pid, email, ends_with("name"), i_drop_out)

a_names <- function(x) {
  study %>%
    pull( {{x}} ) %>%
    .[order(.)]
}


# Master Kids ####
master_kids <- master %>%
  filter(
    str_detect(pid, "A|B|C|D")
  ) %>%
  mutate(child_fname = first_name,
         child_pid = pid) %>%
  select(fid,
         starts_with("child"),
         last_name)
