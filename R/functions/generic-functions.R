fill_ids_per_time <- function(df){
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
              !mi(pid) & !mi(fid)
            ) %>%
            select(pid, fid, phase),
          by = c("pid", "fid", "phase")
        )
      )
    )

  df <- dfn %>%
    group_by(time_point) %>%
    unnest(
      cols = c(data)
    )

  df
}

## For renaming files
# fs::file_move(
#   paste0(path, list.files(path, pattern = "gfhs-.*\\.csv")),
#   paste0(
#     path,
#     str_replace(
#       list.files(path, pattern = "gfhs-.*\\.csv"),
#       pattern = "(_|-)sv.*\\.csv",
#       ".csv"
#     )
#   )
# )

read_csv_in_df <- function(path, names_characteristics, n_skip, n_max = Inf){
  files <- list.files(path, pattern = "csv")
  file_paths <- list.files(path, pattern = "csv", full.names = TRUE)

  m <- str_split_fixed(files, "[._]", length(names_characteristics))
  colnames(m) <- names_characteristics

  df <- m %>%
    as_tibble %>%
    mutate(
      data = map(
        file_paths,
        ~ read_csv(
          .x,
          col_types = c(.default = "c"),
          name_repair = "minimal",
          n_max = n_max,
          skip = n_skip
        )
      )
    )

  df
}

write_for <- function(x,
                      path,
                      data_type,
                      file_type,
                      file_date_needed,
                      needs_unnest_asa = TRUE){

  if(file_type == "rds"){
    return(write_rds(x, path))
           }

  if("asa" %in% data_type & needs_unnest_asa){
    x <- x %>% unnest(asa_totals, keep_empty = TRUE)
    }

  if(file_type == "csv"){
    write_csv(x, path, na = "")
  }
}

## Use across(where(is.numeric), ~ round(.x, 2)) instead
round_df <- function(df, digits) {

  nums <- map_lgl(df, is.numeric)

  df[, nums] <- format(
    round(df[, nums], digits),
    nsmall = digits
  )

  (df)
}

an <- function(x) as.numeric(x)

## You can write your own list() function
## so it behaves like data.frame(),
## i.e., uses the un-evaluated arg names
## as entry names:

List <- function(...) {
  names <- as.list(substitute(list(...)))[-1L]
  setNames(list(...), names)
}

## Used in asa cleaning ####

list.csvs <- function(x){
    list.files(
      path = x,
      pattern = ".*csv",
      full.names = TRUE
      )
}

derive_names <- function(df){
  df %>%
    mutate(
      folder = sub(paste0(".*\\", sl, "(.*)"), "\\1", path_name),
      time_point = sub("^(t\\d*)_?.*", "\\1", folder),
      phase = sub(".*phase_(\\d{1}).*", "\\1", folder),
      version = sub(".*([^_]*\\d{4}.*)", "\\1", folder),
      data_path = map(
        path_name,
        ~list.csvs(.)
      )
    )
}

catalog_foldered_files <- function(path_folders) {

  file_list <- list.files(
    path = path_folders,
    full.names = T
  )

  ndf <- tibble(path_name = file_list) %>%
    derive_names

  df <- ndf %>%
    relocate(
      path_name,
      .after = last_col()
    ) %>%
    unnest(data_path) %>%
    mutate(
      time_last_modified = file.mtime(data_path)
    )
}

milist <- function(df, var) {
  map(
    df,
    ~ .x %>%
      filter(
        mi(
          {{ var }}
        )
      ) %>%
      select(
        where(
          ~ !all(
            mi(.x)
            )
          )
        )
    )
}

master_join <- function(df, master){

  df %>%
    full_join(
      master %>%
        filter(
          !mi(pid)
          ) %>%
        select(pid, name),
      by = "pid"
      )
}

id_only <- function(df) {
  df %>%
    filter(!mi(pid)) %>%
    select(pid, fid)
}

add_role <- function(df) {
  df %>%
    mutate(
      role = case_when(
        grepl("P|S", pid) ~ "parent",
        TRUE ~ "child"
      )
    )
}

add_phase <- function(df) {
  df %>%
    mutate(
      phase = case_when(
        fid > 400 ~ "3",
        fid < 401 & fid > 299 ~ "2",
        fid < 300 ~ "1",
        TRUE ~ NA_character_
      )
    )
}

add_fid <- function(df){
  df %>%
    mutate(
      fid = pid %>%
        str_extract("\\d+") %>%
        as.numeric
      )
}

ids_of <- function(df, x) {
  df %>%
    filter(role == x) %>%
    id_only()
}


# Note x for primary, y for secondary
create_combos <- function(x, y, new.name) {
  x %>%
    full_join(
      y %>%
        rename(
          {{ new.name }} := pid
        ),
      by = "fid"
    ) %>%
    arrange(fid, pid) %>%
    select(pid, {{ new.name }})
}

time_to_from <- function(x, y, t) {
  time_length(
    interval(y, x),
    t
  )
}

## Other ####


code_hh <- function(df){
  df %>%
    rename(
      hh = household_income_parent_one
    ) %>%
    mutate(
      household_income_n = case_when(
        hh == "$150,000 or more" ~ 17.5,
        hh == "$100,000 to $149,999" ~ 12.5,
        hh == "$90,000 to $99,999" ~ 9.5,
        hh == "$80,000 to $89,999" ~ 8.5,
        hh == "$70,000 to $79,999" ~ 7.5,
        hh == "$60,000 to $69,999" ~ 6.5,
        hh == "$50,000 to $59,999" ~ 5.5,
        hh == "$40,000 to $49,999" ~ 4.5,
        hh == "$30,000 to $39,999" ~ 3.5,
        hh == "$20,000 to $29,999" ~ 2.5,
        hh == "$10,000 to $19,999" ~ 1.5,
        hh == "Less than $10,000" ~ 0.5,
        hh == "I am not comfortable answering this question" ~ NA_real_,
        hh == "I don????s?\"ôt know" ~ NA_real_,
        TRUE ~ NA_real_
      ),
      household_income = case_when(
        hh %in% c(
          "$150,000 or more",
          "$100,000 to $149,999",
          "$90,000 to $99,999"
        )
        ~ "$90,000 or more",
        hh %in% c(
          "$80,000 to $89,999",
          "$70,000 to $79,999",
          "$60,000 to $69,999"
        )
        ~ "$60,000 to $89,999",
        hh %in% c(
          "$50,000 to $59,999",
          "$40,000 to $49,999",
          "$30,000 to $39,999"
        )
        ~ "$30,000 to $59,999",
        hh == "I am not comfortable answering this question"
        | str_detect("I don.*know", hh)
        ~ "could not disclose",
        TRUE ~ NA_character_
      )
    )
}

add_ethnicity <- function(df, name, e1, e2){
  df %>%
    mutate(
      "{name}" := case_when(

        #---

        {{ e1 }}  %in% c(
          "South Asian",
          "South Asian (for example: East Indian, Pakistani, Sri Lanken, etc.)",
          "South Asian (for example: East Indian, Pakistani, Sri Lankan, etc.)"
        )
        ~ "South Asian (e.g. East Indian, Pakistani, Sri Lankan, etc.)",

        #---

        {{ e1 }} %in% c(
          "Chinese",
          "Korean or Japanese",
          "Southeast Asian (for example: Vietnamese, Cambodian, Filipino, Malaysian, Laotian, etc.)"
        )
        ~ "Chinese, Korean, Japanese, or Southeast Asian (e.g. Vietnamese, Cambodian, Filipino, Malaysian, Laotian, etc.)",

        #---

        {{ e2 }} %>%
          str_detect(
          "mix|Mix|Asian/Caucasian|white, phillipino scottish|Caucasian.*African"
        )
        | {{ e1 }} %>%
          str_detect(
          "White,(South|East|West|Latin|Korean|Chinese|Black|Aboriginal).*"
        )
        ~ "Mixed ethnicity",

        #---

        {{ e1 }} %>%
          str_detect(
          "I am not comfortable answering"
        )
        | {{ e2 }} %>%
          str_detect(
          "question infuriates me|human|Coloured|unknown"
        )
        ~ "Explicitly did not disclose",

        #---

        mi({{ e1 }})
        ~ as.character(NA),

        #---

        {{ e1 }} %>% str_detect("White")
        ~ "White",

        #---
        {{ e1 }} == "DNR"
        ~ NA_character_,

        #---
        TRUE
        ~ "Other (e.g. Black, West Asian, Latin American)")
    )
}
# Show that all selected ethnicities have been categorized correctly
view_ethnicity <- function(df) {
 df %>%
  group_by(
    ethnicity_selected,
    ethnicity_other,
    ethnicity
    ) %>%
  summarise(count = n()) %>%
  select(
    count,
    ethnicity_selected,
    ethnicity_other,
    ethnicity
    ) %>%
  arrange(ethnicity)
}

mi <- function(x) {is.na(x) | x %in% ""}

substrRight <- function(x, n){
  substr(x,
         nchar(x) - n + 1,
         nchar(x)
         )
}

ttrim <- function(x) {
  str_to_title(
    trimws(x)
    )
}

hei <- function(df){
  df <- as.data.frame(df)
  # sums and names
  sum.in.asa <- c("F_TOTAL",
                  "G_WHOLE",
                  "D_TOTAL")
  intakes <- c("SODI",
               "G_REFINED",
               "SFAT",
               "ADD_SUGARS")

  to.numeric <- c(sum.in.asa,
                  "KCAL",
                  intakes,
                  "F_CITMLB",
                  "F_OTHER",
                  "MFAT",
                  "PFAT",
                  "V_TOTAL",
                  "V_LEGUMES",
                  "V_DRKGR",
                  "PF_MPS_TOTAL",
                  "PF_EGGS",
                  "PF_NUTSDS",
                  "PF_SOY",
                  "PF_LEGUMES",
                  "PF_SEAFD_HI",
                  "PF_SEAFD_LOW")

  df[, to.numeric] <- sapply(df[, to.numeric],
                             as.numeric)

  df$FWHOLEFRT <- with(df, F_CITMLB+
                         F_OTHER)
  df$MONOPOLY <- with(df, MFAT+
                        PFAT)
  df$VTOTALLEG <- with(df, V_TOTAL+
                         V_LEGUMES)
  df$VDRKGRLEG <- with(df, V_DRKGR+
                         V_LEGUMES)
  df$PFALLPROTLEG <- with(df,
                          PF_MPS_TOTAL+
                            PF_EGGS+
                            PF_NUTSDS+
                            PF_SOY+
                            PF_LEGUMES)
  df$PFSEAPLANTLEG <- with(df,
                           PF_SEAFD_HI+
                             PF_SEAFD_LOW+
                             PF_NUTSDS+
                             PF_SOY+
                             PF_LEGUMES)

  # components 1 to 8
  tnames <- c("VTOTALLEG",
              "VDRKGRLEG",
              "F_TOTAL",
              "FWHOLEFRT",
              "G_WHOLE",
              "D_TOTAL",
              "PFALLPROTLEG",
              "PFSEAPLANTLEG")
  dnames <- c("VEGDEN",
              "GRBNDEN",
              "FRTDEN",
              "WHFRDEN",
              "WGRNDEN",
              "DAIRYDEN",
              "PROTDEN",
              "SEAPLDEN")
  cnames1to8 <- c("C1_TOTALVEG",
                  "C2_GREEN_AND_BEAN",
                  "C3_TOTALFRUIT",
                  "C4_WHOLEFRUIT",
                  "C5_WHOLEGRAIN",
                  "C6_TOTALDAIRY",
                  "C7_TOTPROT",
                  "C8_SEAPLANT_PROT")

  # loop
  dfac <- c(1.1,
            0.2,
            0.8,
            0.4,
            1.5,
            1.3,
            2.5,
            0.8)

  for (i in seq(dnames)){
    kcal <- df$KCAL
    den <- df[, tnames[i]]/(kcal/1000)
    mfac <- ifelse(dnames[i] %in% c("WGRNDEN", "DAIRYDEN"),
                   10,
                   5)
    c <- mfac*(den/dfac[i])
    c <- ifelse(c > mfac,
                mfac,
                ifelse(den %in% 0,
                       0,
                       c))
    df[, paste0("HEI2015_", cnames1to8[i])] <- c
    df[, dnames[i]] <- den
  }

  # component 9
  FARATIO <-  df$MONOPOLY/df$SFAT
  FARMIN <- 1.2
  FARMAX <- 2.5
  c9 <- paste0("HEI2015_",
               "C9_FATTYACID")
  df[, c9] <- with(df,
                   ifelse(SFAT == 0 & MONOPOLY == 0, 0,
                          ifelse(SFAT == 0 & MONOPOLY > 0, 10,
                                 ifelse(FARATIO >= FARMAX, 10,
                                        ifelse(FARATIO <= FARMIN, 0,
                                               10*((FARATIO - FARMIN)/(FARMAX - FARMIN)))))))

  # components 10 to 13
  mins <- c(1.1,
            1.8,
            8,
            6.5)

  maxs <- c(2.0,
            4.3,
            16,
            26)

  cnames10to13 <- c("C10_SODIUM",
                    "C11_REFINEDGRAIN",
                    "C12_SFAT",
                    "C13_ADDSUG")

  for (i in seq(intakes)){
    intake <- intakes[i]
    percentify <- ifelse(intake %in% c("SODI", "G_REFINED"),
                         1,
                         100)
    k.units <- ifelse(intake %in% c("G_REFINED"),
                      1000,
                      1)
    mfac <- ifelse(intake %in% c("SFAT"), 9,
                   ifelse(intake %in% c("ADD_SUGARS"), 16,
                          1))

    den <- percentify*mfac*df[, intakes[i]]/(kcal/k.units)
    df[, paste0("HEI2015_", cnames10to13[i])] <- ifelse(den <= mins[i], 10,
                                                        ifelse(den >= maxs[i], 0,
                                                               10 - (10*(den - mins[i])/(maxs[i] - mins[i]))))
  }

  components <- c(paste0("HEI2015_",
                         c(cnames1to8,
                           "C9_FATTYACID",
                           cnames10to13)))
  df[df$KCAL %in% 0, components] <- 0
  df$HEI2015_TOTAL_SCORE <- rowSums(df[, components])

  df
}

# ASA Items and Responses ####

read_asa <- function(ui){

  folders_files <- ui %>%
    rename(folder_path = path_name_asa) %>%
    distinct(folder_path) %>%
    mutate(
      file_path = map(
        folder_path,
        ~ list.csvs(.)
      )
    ) %>%
    unnest(file_path) %>%
    filter(
      str_detect(
        file_path,
        "(Totals|Items|INF|Responses|MS).csv"
      )
    )

  nested_data <- folders_files %>%
    mutate(
      asa_file_type = file_path %>%
        str_replace("^.+_(.*)\\.csv", "\\1"),
      data = map(
        file_path,
        ~ vroom(.x)
      ),
      columns = map(data, names)
    ) %>%
    select(- folder_path, - file_path)

}

write_which <- function(df, type_wanted, time_point, phase, role) {

  z <- c("items_responses", "items", "responses")
  types_not_wanted <- z[!z %in% type_wanted]

  df <- df %>%
          select(
            -all_of(types_not_wanted)
          ) %>%
          unnest({{ type_wanted }})

  write_csv(
    df,
    paste0(
      here("data", "processed", "requests"),
      time_point,
      "_phase-",
      phase,
      "_",
      glue_collapse(r, "-and-"),
      "_",
      type_wanted,
      "_",
      currentDate,
      ".csv"
      )
  )
}

write_asa <- function(df, ...){
  write_which(df, "totals", ...)
  write_which(df, "items_responses", ...)
  write_which(df, "items", ...)
  write_which(df, "responses", ...)
}

process_asa <- function(ui, time_point, phase, role) {

  ui <- ui %>%
    filter(
      time_point == {{ time_point }}
      & phase == {{ phase }}
      & role == {{ role }}
    )

  nested_data <- read_asa(ui)
  nested_wider <- pivot_asa(nested_data)
  df <- merge_asa(ui, nested_wider)
  df <- clean_asa(df)
}

only_these_nutrients <- function(df, nutrients){

  regex <- glue_collapse(nutrients, "|")
  df %>%
    pivot_longer(
      cols = c(items, items_responses, totals, responses),
      names_to = c("asa_file_type"),
      values_to = c("data")
    ) %>%
    mutate(
      data = map2(
        data,
        asa_file_type,
        function(x, y)
          if(y != "responses")
            x %>%
          select(
            - all_of(
              asa_nutrients %>%
                str_subset(regex, negate = TRUE)
            )
          )
        else x
      )
    ) %>%
    pivot_wider(
      names_from = asa_file_type,
      values_from = data
    )
}


# could integrate the below with the above

write_tot <- function(df, time_point, phase, role){

  df <- df %>%
    filter(
      time_point %in% {{ time_point }}
      & phase == {{ phase }}
      & role %in% {{ role }}
    ) %>%
    arrange(fid, pid) %>%
    select(
      phase,
      time_point,
      fid,
      pid,
      user_name,
      date_asa,
      i_note_asa,
      i_outlier_asa,
      i_outlier_asa_note,
      everything(),
      - c(
        age,
        folder_asa,
        data_path_asa,
        path_name_asa,
        time_last_modified_asa,
        i_low_KCAL_asa,
        responses_asa,
        time_point_asa,
        phase_asa,
        role,
        ends_with("_login"),
        matches("outlier|breastfed|Recall|Amt|CFG"),
        UserID
      )
    )

  df <- hei(df)

  write_csv(
    df,
    paste0(
      here("data", "processed", "requests"),
      glue_collapse(time_point, "_"),
      "_phase_",
      phase,
      "_",
      glue_collapse(role, "_and_"),
      "_",
      "totals",
      "_",
      currentDate,
      ".csv"
    )
  )
}


## AHHA_FS ####
numdiff <- function(x, y){
  as.numeric(x) - as.numeric(y)
}

compare <- function(df){
  df %>%
    group_by(pid) %>%
    mutate(
      across(c(location,
               time,
               pregnant,
               bm_kg,
               ht_cm,
               wc_cm,
               date,
               contains("note")),
             list(prior = ~ lag(.x, order_by = pid)),
             .names = "{col}_{fn}"),
      across(c(bm_kg, ht_cm, wc_cm),
             ~ numdiff(.x, get(glue("{cur_column()}_prior"))),
             .names = "{.col}_change")
    )

}
clean <- function(df){
  df %>%
    group_by(pid) %>%
    fill(dob, gender, .direction = c("downup")) %>%
    ungroup() %>%
    mutate(
      age = time_length(difftime(date, ymd(dob)),
                        "years"),
      months_elapsed = time_length(difftime(date, date_prior),
                                   "months")
    ) %>%
    distinct(pid, time, .keep_all = TRUE) %>%
    mutate(
      across(where(is.numeric),
             ~ round(.x, 1)),
      across(matches("(time|location|pregnant|bf)$"),
             ~ as.factor(.x))
    ) %>%
    add_role() %>%
    filter(location == "at-home" & is.na(cleaned))

}

dt <- function(x){
  datatable(
    x,
    filter = 'top',
    extensions = c('Buttons', "FixedColumns"),
    options = list(
      scrollX = TRUE,
      fixedColumns = TRUE,
      scrollY = 300,
      scroller = TRUE,
      autoWidth = TRUE,
      pageLength = 7,
      dom = 'Bfrtip',
      buttons = list(
        I('colvis'),
        c('csv')
        ),
      columnDefs = list(
        list(
          className = 'dt-center',
          targets = "_all"
        )
      )
    ),
    rownames = F,
    class = 'cell-border stripe'
  )
}

basic_dt <- function(df){
  df %>%
    datatable(
    filter = 'top',
    extensions = 'Buttons',
    options = list(
      pageLength = 7,
      dom = 'Bfrtip',
      buttons = list(
        I('colvis'),
        c('copy', 'csv', 'excel', 'pdf')
      ),
      columnDefs = list(
        list(className = 'dt-center', targets = "_all")
      )
    ),
    rownames = F,
    class = 'cell-border stripe'
  )
}

# prep parent
prepp <- function(df){
  df %>%
    select(
      pid, role,
      time, time_prior,
      location, location_prior,

      bf, pregnant, pregnant_prior,

      age,
      months_elapsed,

      starts_with("bm"),
      # bm_kg, bm_kg_prior, bm_kg_change, bm_notes_prior,

      comments,
      # prior_note_ahha, # note is only for parent
      starts_with("notes_dupl")
      # ht_cm, prior_ht, change_ht # for parents
      ## not using ahha height due to inaccuracy
    ) %>%
    filter(role == "parent") %>%
    arrange(pid, time)
}

process <- function(df){
  df %>%
    compare() %>%
    clean() %>%
    prepp()
}
custom_dt <- function(x, role){
  cols <- c("bm_kg_change")
  if(role == "child") cols <- c(cols, "ht_cm_change", "wc_cm_change")

  datatable(
    x,
    filter = 'top',
    extensions = 'Buttons',
    options = list(
      pageLength = 7,
      dom = 'Bfrtip',
      buttons = list(
        I('colvis'),
        c('copy', 'csv', 'excel', 'pdf')
      ),
      columnDefs = list(
        list(className = 'dt-center', targets = "_all")
      )
    ),
    rownames = F,
    class = 'cell-border stripe'
  ) %>%
    formatStyle(
      columns = cols,
      color = styleInterval(
        cuts = c(- 5, 5),
        values = c("red", "black", "green")
      ),
      fontWeight = "bold"
    )
}



# Extremes ####

qrab <- function(x, y){
  unname(x[names(x) == y])
}

fences <- function(x){
  # Traditional box plot method
  #qc <- quantile(x, na.rm = T)
  #iqr <- IQR(x, na.rm=T)
  #lw <- qrab(qc, "25%") - 1.5*iqr
  #hw <- qrab(qc, "75%") + 1.5*iqr

  # Adjusted box plot method (Hubert and Vandervieren, 2008)
  adj <- robustbase::adjboxStats(x)
  fences <- adj$fence
  lw <- min(fences)
  hw <- max(fences)

  whiskers <- c(lw, hw)
  names(whiskers) <- c("LW",
                       "HW")
  whiskers
}

cuts <- function(x){
  c(
    quantile(x, na.rm = TRUE),
    fences(x)
  )
}
