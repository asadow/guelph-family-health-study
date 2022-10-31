study_sv <- "gfhs"
survey_type <- "ahha"

nested_surveys <- read_rds(
    here("data",
         "processed",
         "surveys",
         glue("{study_sv}_{survey_type}_nested.rds")
    )
  )

# redundant <- "_(feet|height|to_nearest_0_1_cm_text|waist_circumference|weight_1_decimal_kg_text|ahha|my_weight_kg_with_1_decimal_text)"

redundant <- c("feet",
                "height",
                "to_nearest_0_1_cm_text",
                "waist_circumference",
                "weight_1_decimal_kg_text",
                "ahha",
                "my_weight_kg_with_1_decimal_text")

redundant <- redundant %>% glue_collapse("|") %>% {glue("_({.})")}

ahha <- nested_surveys %>%
  unnest_and_clean_surveys %>%
  rename_with(
    ~ .x %>% str_replace("sv", "ahha"),
    ends_with("sv")
  ) %>%
  rename(on_who_ahha = on_who) %>%
  rename_with(~ str_remove_all(.x, redundant),
              - time_point_ahha)

ahha <- ahha %>%
  mutate(time_point = case_when(
    fid %in% c("521", "516") & survey_type == "ahha" &
      time_point_ahha == "t2" ~ "t3",
    TRUE ~ time_point
  ))

ahha <- ahha %>% pivot


## These variables on selected choice are
## artifacts of how the survey was made...
## Only wt's choice variable contains I prefer not to answer

ahha <- ahha %>%
  select(- matches("choice")) %>%
  rename_with(~ str_replace(.x, "wt", "bm"))

ahha <- ahha %>%
  mutate(
    pregnant = str_replace_all(
      pregnant,
      c("Not applicable" = "N/A",
        "No" = "N",
        "Yes" = "Y")
    ),
    bf = str_replace_all(
      bf,
      c("Not applicable" = "N/A",
        "No" = "N",
        "Yes" = "Y")
    )
  )

# Check for characters before making numeric ----------------------------------
regex_expect_numeric <- c("bm_kg", "bm_lb", "ht_cm", "ht_ft", "wc_cm")
cols_needing_correction <- ahha %>%
  select(matches(regex_expect_numeric)) %>%
  map(~ str_detect(.x, "[A-z]") %>% any) %>%
  unlist %>%
  which %>%
  names

ahha %>%
  select(all_of(cols_needing_correction)) %>%
  filter(if_any(everything(), ~ str_detect(.x, "[A-z]")))

ahha <- ahha %>%
  mutate(
    across(
      c(bm_kg_v2, child_bm_kg_v2),
      ~ str_remove_all(.x, "[A-z]")
    )
  )

# Conversions
# Inch is 2.54 cm
# Foot is 30.48 cm
## Keep all columns for data review
ahha <- ahha %>%
  mutate(
    across(
      c(bm_kg, child_bm_kg),
      ~ coalesce(.x, get(glue("{cur_column()}_v2")))),
    across(c(ht_ft, ht_ft_inches, bm_kg),
           ~ coalesce(.x, get(glue("{cur_column()}_2")))),
    ht_cm_converted = 30.48*as.numeric(ht_ft) + 2.54*as.numeric(ht_ft_inches),
    ht_cm = coalesce(as.numeric(ht_cm), ht_cm_converted)) %>%
  select(- matches("_(2|1)$"))

ahha <- ahha %>%
  mutate(
    across(matches(regex_expect_numeric), as.numeric),
    across(
      c(bm_lb, child_bm_lb),
      ~ .x*0.45359237,
      .names = "{col}_converted_to_kg"),
    across2(matches("bm_kg$"),
            matches("bm_lb_converted_to_kg"),
            coalesce,
            .names = "{xcol}")
    )

## units are redundant; separate columns exist on ft and cm
## however, they do contain the rare response: "I prefer not to say"
ahha <- ahha %>%
  select(- c(ends_with("v2"),
             ends_with("units"),
             matches("ht_ft|lb|converted")))

# -- ----------------------------------------------------------------------

# Split Data by Role

cdf <- ahha %>%
  rename(parent_pid = pid) %>%
  select(
    fid, parent_pid,
    time_point, time_point_ahha,
    date,
    starts_with("child_")
  ) %>%
  rename_with(~ str_remove(.x, "child_")) %>%
  filter(!is.na(pid))

pdf <- ahha %>% distinct(pid, time_point, .keep_all = T)

# Bind and Label Qualtrics Type

ahha <- bind_rows(cdf, pdf)

## Review files ####
folder_path <- here("data", "processed",
                    "health-assessments",
                    "at-home",
                    "review-decisions")

# folder_path <- paste0(ref_p, "AHHA", sl)

file_paths <- list.files(path = folder_path,
                         pattern = "*.csv",
                         full.names = T)

file_df <- data.frame(path_name =  file_paths) %>%
  mutate(
    data = map(
      path_name,
      ~ as_tibble(
        read_csv(.x, col_types = c(.default = "c"))
      )
    )
  )

review_df <- file_df %>%
  unnest(data) %>%
  mutate(cleaned = 1,
         pid = coalesce(pid, PID),
         time_point = coalesce(time_point, time))

## These families accidentally received a t2 survey
## And this bled into the review
## Now they are corrected in data; and were reviewed properly
## so review of t2 can be discarded

review_df <- review_df %>%
  add_fid %>%
  filter(
    !(fid %in% c("516", "521") & time_point == "t2")
  )


# Done Redo-Reviews -------------------------------------------------------

redone <- read_csv(here("data", "processed",
                        "health-assessments",
                        "at-home",
                        "review-decisions",
                        "redone", "redone.csv"))

# Merge -------------------------------------------------------
df <- ahha %>%
  full_join(review_df,
            by = c("pid", "time_point"),
            suffix = c("", "_review"))

df <- redone %>%
  rename(done_review = redo_review) %>%
  right_join(df, by = c("pid", "time_point")) %>%
  mutate(
    redo_review = case_when(
      done_review == "1" ~ "0",
      TRUE ~ redo_review
    )
  )


# Decision Column ####
## Unit Conversions ####

df <- df %>%
  mutate(
    ## -- Decisions on re-do review on 10-19-2022 w/ Maddy and Andrea
    convert_bm_lb_to_kg = case_when(
      pid == "S0521" & time_point == "t3" ~ "1",
      TRUE ~ convert_bm_lb_to_kg
    ),
    remove_ht = case_when(
      pid == "A0509" & time_point == "t5" ~ "1",
      TRUE ~ remove_ht
    ),
    remove_wc = case_when(
      pid == "A0775" & time_point == "t2" ~ "1",
      TRUE ~ remove_wc
    ),
    ## --
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

# clear_mn <- function(x){
#   case_when(
#     str_detect(
#       {{ x }},
#       "by MN"
#     ) ~ NA_character_,
#     TRUE ~ {{ x }}
#   )
# }

# df <- df %>%
#   mutate(
#     MN_note = case_when(str_detect(comments, "by MN") ~ comments,
#                         TRUE ~ NA_character_),
#     MN_child_note = case_when(
#       if_any(ends_with("_notes"), ~ str_detect(.x, "by MN"))
#       ~ glue("ht_note: {ht_notes}--wc_note: {wc_notes}--bm_note: {bm_notes}"),
#       TRUE ~ NA_character_),
#     across(c(comments, ht_notes, wc_notes, bm_notes), ~ clear_mn(.x))
#   )
df <- df %>%
  unite(notes_united, c(comments, ends_with("notes")),
        remove = FALSE, na.rm = TRUE, sep = ".") %>%
  mutate(notes_united = glue("{notes_united}."),
         sentences = map(notes_united, ~ str_split(.x, "(?<=[.?!])(?=.)")),
         notes_dupl = map(sentences,
                       ~ unlist(.) %>%
                         .[duplicated(.) == TRUE & . != "."] %>%
                         unique %>%
                         str_remove_all("\\.")),
         notes_dupl = map(notes_dupl,
                        ~ glue_collapse(., sep = "|")) %>% as.character,
         across(c(comments, ends_with("notes")),
                ~ map2_chr(.x,
                           notes_dupl,
                            ~ .x %>%
                             str_remove_all(.y))),
         across(c(comments, ends_with("notes")),
                ~ if_else(.x %in% c(".", "..", "") | is.na(.x),
                          NA_character_,
                          .x)),
         notes_dupl = notes_dupl %>%
           if_else(. == "character(0)", NA_character_, .) %>%
           trimws
         )

## Add ahha suffix
# df <- df %>%
#   rename_with(~ glue("{.x}_ahha"), - c(time_point, time_point_ahha, pid, fid))

df <- df %>%
  select(fid,
         pid,
         cleaned,
         progress,
         parent_pid,
         date,
         survey_type,
         time_point,
         time_point_ahha,
         pregnant,
         bf,
         comments,
         ht_cm,
         wc_cm,
         bm_kg,
         ht_notes,
         wc_notes,
         bm_notes,
         notes_dupl,
         redo_review) %>%
  rename(date_ha = date,
         i_pregnant_ha = pregnant,
         i_breastfeeding_ha = bf,
         comments_ha = comments,
         progress_ha = progress)

# Export ####

df %>% write_csv(here("data", "processed", "incomplete", "ahha.csv"))
