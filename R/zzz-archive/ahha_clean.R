# Import ####
## Raw data ####

ahha <- read.csv(paste0(cle_p, "ahha_matched.csv"))

## Review files ####

folder_path <- paste0(ref_p, "AHHA", sl)

file_paths <- list.files(path = folder_path,
                         pattern = "*.csv",
                         full.names = T)

file_df <- data.frame(path_name = file_paths) %>%
  mutate(
    data = map(
      path_name,
      ~ as_tibble(
        vroom(
          .x,
          col_types = c(.default = "c")
        )
      )
    )
  )

df <- file_df %>%
  unnest(data)

# Merge ####

df <- ahha %>%
  full_join(df,
            by = c("PID", "time_point"),
            suffix = c("", "_review"))

# Decision Column ####
## Unit Conversions ####

df <- df %>%
  mutate(
    wc_cm = case_when(
      convert_wc_inches_to_cm == "1" ~ wc_cm * 2.4,
      TRUE ~ wc_cm
    ),
    bm_kg = case_when(
      convert_bm_lb_to_kg == "1" ~ bm_kg * 0.45359237,
      TRUE ~ bm_kg
    ),
    ht_cm = case_when(
      remove_ht == "1" ~ ".",
      TRUE ~ as.character(ht_cm)
    ),
    wc_cm = case_when(
      remove_wc == "1" ~ ".",
      TRUE ~ as.character(wc_cm)
    )
  )

# "Cleaned" indicator for time points  ####

clear_mn <- function(x){
  case_when(
    str_detect(
      {{ x }},
      "by MN"
      ) ~ NA_character_,
    TRUE ~ {{ x }}
  )
}

df <- df %>%
  mutate(
    cleaned_ahha = case_when(
      time_point %in% c("t2") ~ 1,
      TRUE ~ 0
    ),
    MN_note = case_when(
      str_detect(comments_ahha, "by MN") ~ comments_ahha,
      TRUE ~ NA_character_
    ),
    MN_child_note = case_when(
      if_any(
        ends_with("_notes"),
        ~ str_detect(.x, "by MN")
        ) ~ paste(
        "ht_note:", ht_notes, "----",
        "wc_note:", wc_notes, "----",
        "bm_note:", bm_notes
        ),
      TRUE ~ NA_character_),
    across(
      c(comments_ahha,
        ht_notes,
        wc_notes,
        bm_notes),
      ~ clear_mn(.x)
      )
  ) %>%
  rename(SID_ahha = parent_pid,
        flag_pregnant_ahha = pregnant,
        flag_breastfeeding_ahha = breastfeeding,
        flag_comments_ahha = comments_ahha,
        ht_cm_ahha = ht_cm,
        wc_cm_ahha = wc_cm,
        bm_kg_ahha = bm_kg)

df <- df %>%
  select(FID,
        PID,
        cleaned_ahha,
        SID_ahha,
        phase_ahha,
        date_ahha,
        time_point,
        time_point_ahha,
        flag_pregnant_ahha,
        flag_breastfeeding_ahha,
        flag_comments_ahha,
        ht_cm_ahha,
        wc_cm_ahha,
        bm_kg_ahha,
        ht_notes,
        wc_notes,
        bm_notes,
        MN_note,
        MN_child_note)

# Export ####

df %>% write_csv(paste0(cle_p, "ahha.csv"))


