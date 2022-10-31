
df <- df %>%
  left_join(
    ethn,
    by = c("PID", "time_point"),
    suffix =c("", "_pilot_demo")
  )

# May want to adjust this code later if not using _pilot_demo

df <- df %>%
  mutate(
    across(
      all_of(
        c("household.income", "education")
        ),
      ~ na_if(.x, "")
      ),
    household.income = coalesce(household.income, 
                                household.income_pilot_demo),
    education = coalesce(education, 
                         education_pilot_demo),
    parent_sv = coalesce(parent_sv, 
                         parent_sv_pilot_demo)
    )


## --Fill by PID; static variables----------------------------------------------------------------------

pid_fill <- c(
  "dob",
  "gender",
  "ethnicity.selected",
  "ethnicity.other",
  "ethnicity.selected.first", 
  "ethnicity.selected.second", 
  "ethnicity.written.other",
  "country.born.in.selected",
  "country.born.in.other"
  )

# add phase

dfv <- df %>%
  mutate(
    phase = case_when(
      FID > 400 ~ "3",
      FID > 299 ~ "2",
      FID <= 299 ~ "1"
    )
  ) %>%
  group_by(PID) %>%
  mutate(
    across(
      all_of(pid_fill),
      ~na_if(., "")
    )
  ) %>% 
  fill(
    all_of(pid_fill),
    .direction = c("downup")
  )

## --Code Final Ethnicity----------------------------------------------------------------------


dfv <- dfv %>% 
  mutate(
    ethnicity = case_when(
      
      #---
      
      ethnicity.selected %in% c(
        "South Asian (for example: East Indian, Pakistani, Sri Lanken, etc.)",
        "South Asian (for example: East Indian, Pakistani, Sri Lankan, etc.)"
        )
      ~ "South Asian (e.g. East Indian, Pakistani, Sri Lankan, etc.)",
      
      #---
      
      ethnicity.selected %in% c(
        "Chinese",
        "Korean or Japanese",
        "Southeast Asian (for example: Vietnamese, Cambodian, Filipino, Malaysian, Laotian, etc.)"
        ) 
      ~ "Chinese, Korean, Japanese, or Southeast Asian (e.g. Vietnamese, Cambodian, Filipino, Malaysian, Laotian, etc.)",
      
      #---
      
      grepl(
        "mix|Mix|Asian/Caucasian|white, phillipino scottish|Caucasian.*African", 
        ethnicity.other
        )
      | grepl(
        "White,(South|East|West|Latin|Korean|Chinese|Black|Aboriginal).*", 
        ethnicity.selected, 
        perl = T
        )
      ~ "Mixed ethnicity",
      
      #---
      
      grepl(
        "I am not comfortable answering", 
        ethnicity.selected
        )
      | grepl(
        "question infuriates me|human|Coloured|unknown", 
        ethnicity.other
        ) 
      ~ "Explicitly did not disclose",
      
      #---
      
      mi(ethnicity.selected)
      ~ as.character(NA),
      
      #---
      
      grepl(
        "White", 
        ethnicity.selected
        )
      ~ "White",
      
      #---
      
      TRUE 
      ~ "Other (e.g. Black, West Asian, Latin American)")
  ) 

# hm <- dfv %>% 
#   group_by(gender.reg, ethnicity, grepl("P|S", PID)) %>% 
#   count()
# View(hm)
# ue <- dfv %>% 
#   group_by(ethnicity.selected, ethnicity.other, ethnicity) %>% 
#   summarise(count = n()) %>%
#   select(count, ethnicity.selected, ethnicity.other, ethnicity)


#View(ue)

## --Code Age----------------------------------------------------------------------

# Dates relating to data-type collection 

origin <- c("ha", "ll", "asa", "sv")
ages <- paste0("age_", origin)
dates <- paste0("date_", origin)

for (i in seq(origin)) {
  dfv[, dates[i]] <- anydate(dfv %>% pull(dates[i]))
  dfv[, ages[i]] <- time_to_from(
    dfv %>% pull(dates[i]),
    dfv %>% pull(dob),
    "years"
  )
}

df <- dfv


## --Fill by FID and time_point: Parent one and two vars for each child----------------------------------------------------------------------

fid_tp_fill <- c(
  "ethnicity",
  
  # These ethnicity.suffix are from dem_p pilot data in merger_04_add_vars
  "ethnicity.selected.first", 
  "ethnicity.selected.second", 
  "ethnicity.written.other",
  "country.born.in.selected",
  "country.born.in.other",
  "dob",
  "gender",
  "education",
  "marital.status",
  "lives.with.partner",
  "number.children",
  "number.children.18mo.to.5yr",
  "household.income", 
  "household.number.adults"
)

p_select <- function(df, letter, id){
  df %>% 
    select(
      FID,
      PID, 
      time_point,
      all_of(fid_tp_fill)
    ) %>% 
    filter(
      grepl(letter, PID)
    ) %>% 
    rename(
      "PID_parent_{id}" := PID
    ) %>% 
    rename_with(
      ~ paste0(., "_parent_", id),
      all_of(fid_tp_fill)
    )
  
}

p_data1 <- p_select(df, "P", "one")
p_data2 <- p_select(df, "S", "two")

fid_tp <- c("FID", "time_point")

p_data <- p_data1 %>% 
  full_join(p_data2, by = fid_tp)

df <- df %>% 
  full_join(p_data, by = fid_tp)


## --Rename vars to use _ over .---------------------------------------------------------------------
fid_tp_fill <- str_replace_all(fid_tp_fill, "\\.", "_")
names(df) <- str_replace_all(names(df), "\\.", "_")

names(df) <- gsub("_$", "\\.", names(df))

## --Add vars for secondary ID -------------

df <- df %>% 
  mutate(
    SID_parent_sv = parent_sv,
    SID_phase_sv = phase_sv,
    SID_progress_sv = progress_sv,
    SID_date_sv = date_sv,
    SID_did_sv_on_child = parent_did_sv_on_child
  ) %>% 
  rename(
    bmi_z_score = bfaz
  ) 

df <- hei(df)

## --Remove extraneous rows from child 1-5 ----

pspr <- df %>% 
  distinct(
    PID, 
    SID,
    time_point, 
    .keep_all = T
    ) %>% 
  arrange(
    phase, 
    time_point, 
    FID, 
    FID_order
    )

vroom_write(
  pspr,
  paste0(cle_p, "pspr.csv")
)

# vroom_write(
#   df,
#   paste0(cle_p, "df.csv")
# )
