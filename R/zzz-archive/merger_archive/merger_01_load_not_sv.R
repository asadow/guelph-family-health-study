## --------- Lifelabs [LL] Blood measures ---------
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
    time_point = str_replace_all(time_point, ntp)
  )

##  ---------ASA-24 [ASA] 24-hour diet intakes and HEI calculation---------

asa <- vroom(
  paste0(cle_p, "asa_totals.csv")
  )

## --------- Health Assessment [HA] Health measures, date of measures---------

ha <- vroom(
  paste0(cle_p, 
         "ha.csv"),
  col_types = c(.default = "c")
  )

names(ha) <- str_replace_all(names(ha), "flag", "i")

##  ---------At-Home Health Assessment [AHHA]----

# code that height is to be replaced by the measured height in the lag time point from data source ha
# ahha <- read.csv(paste0(cle_p, "ahha.csv"),
#   header = TRUE,
#   check.names = FALSE
# )
# 


##  ---------Pilot Demographics---------------

dem_p <- paste0(ref_p, "Demographics - Pilot 1 and Pilot 2", sl)

pdemo <- vroom(
  paste0(dem_p, "t1_phase_1_2_parent_demographics.csv"),
  col_types = c(.default = "c")
)
cdemo <- vroom(
  paste0(dem_p, "t1_phase_1_2_child_demographics.csv"),
  col_types = c(.default = "c")
)
ethn <- plyr::rbind.fill(pdemo, cdemo)

names(ethn)[10:12] <- c("ethnicity.selected.first", "ethnicity.selected.second", "ethnicity.written.other")
names(ethn)[8:9] <- c("country.born.in.selected", "country.born.in.other")

ethn$time_point <- "t1"
ethn <- ethn %>%
  mutate(
    time_point = "t1",
    household.income = select(
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
