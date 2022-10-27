import_questions <- function(df){
  df %>%
    mutate( # also import child first name
      survey_questions = map(
        id,
        possibly(survey_questions, "Error")
      )
    )
}

import_headers <- function(df){
  df %>%
    mutate(
      data = map(
        id,
        # qid_first_name, #for map2
        possibly(
          ~ fetch_survey(
            .x,
            col_types = cols(.default = "c"),
            convert = FALSE,
            # include_questions = c(character(), .y),
            # force_request = TRUE,
            limit = 1,
            breakout_sets	= FALSE
          ),
          "Error"
        )
      )
    )
}

import_headers_1 <- function(df){
  df %>%
    mutate(
      data = map(
        id,
        # qid_first_name, #for map2
        possibly(
          ~ fetch_survey(
            .x,
            col_types = cols(.default = "c"),
            convert = FALSE,
            # include_questions = c(character(), .y),
            force_request = TRUE,
            limit = 1,
            breakout_sets	= FALSE
          ),
          "Error"
        )
      )
    )
}

import_needed_surveys <- function(t, p, nrows){
  ## Exceptions where ppl took wrong time point survey
  exceptions <- read_csv(
    glue(ref_p, "exceptions.csv"),
    col_types = c(.default = "c")
  )
# t <- c("t1", "t2", "t3", "t4")
# p <- c("2", "3")
warning("The exceptions code does not work!")

  tp <- crossing(t, p)
  exceptions_tp <- tp %>%
    left_join(
      exceptions,
      by = c("t" = "tp_true", "p" = "ph")
    )

  t <- exceptions_tp$t %>% unique
  p <- exceptions_tp$p %>% unique

  qp <- here("data", "raw", "qualtrics")
  df <- read_rds(here(qp, "df_qualtrics.rds"))

  df <- df %>% filter(time_point_sv %in% t & phase_sv %in% p)

  df <- df %>%
    mutate(
      data = map(
        id,
        ~ read_rds(
          here("data", "raw", "qualtrics", glue("{.x}.rds"))
        )
      )
    )
}

separate_survey_name <- function(df, name){
  df %>%
    separate(
      name,
      c("study_phase",
        "survey_type",
        "time_point_sv",
        "role_number",
        "on_who"),
      "_",
      remove = FALSE
    ) %>%
    separate_sp_rn
}

separate_sp_rn <- function(df){
  df %>%
    separate(
      study_phase,
      c("study_sv", "phase_sv"),
      "-"
    ) %>%
    separate(
      role_number,
      c("role", "parent_sv"),
      "-"
    )
}

crush_same_rows_for_different <- function(df, x){
  ## Alternate way perhaps:
  # group_by(- x) %>%
  #   mutate(x = paste(x[!is.na(x)], collapse = " | "))
  df %>%
    group_by(across(- {{ x }})) %>%
    mutate(
      "{{ x }}" := {{ x }} %>%
        ifelse(
          length(.) >= 2,
          paste(., collapse = ", "),
          .
        )
    ) %>%
    distinct
}

separate_name <- function(df){
  df %>%
    separate_survey_name %>%
    rename_with(~ .x %>% str_remove("_sv"))
}


# sv_identifiers <- c(
#   "Start Date", "End Date", "Response Type",
#   "IP Address", "Progress",
#   "Duration (in seconds)", "Finished",
#   "Recorded Date", "Response ID",
#   "Recipient Last Name", "Recipient First Name",
#   "Recipient Email",
#   "External Data Reference"
# )
#sv_identifiers %>% str_remove_all(" ")

# df_qualtrics <- df_qualtrics %>%
#   mutate(
#     column_map = map(id, ~ column_map(.x))
#   )

# an_id <- df_qualtrics$id[[2]]
# qs <- an_id %>% survey_questions
# qs %>% filter(question %>% str_detect("first name of your.*child")) %>% pull(qid)
#
# cm <- an_id %>% column_map
# m <- an_id %>% metadata
#
# fetch_survey(
#   an_id,
#   col_types = cols(.default = "c"),
#   convert = FALSE,
#   include_questions = c(character(), "QID1329"),
#   force_request = TRUE
# ) %>%
#   label_to_colnames

## exporting de-identified qualtrics ---------------------------
import_all_data_raw <- function(df){
  df %>%
    mutate(
      data = map(
        id,
        possibly(
          ~ fetch_survey(
            .x,
            col_types = cols(.default = "c"),
            convert = FALSE,
            breakout_sets	= FALSE,
            force_request = FALSE
          ),
          "Error")
      )
    )
}

import_all_data <- function(df){
  df %>%
    mutate(
      data = map(
        id,
        possibly(
        ~ fetch_survey(
          .x,
          col_types = cols(.default = "c"),
          convert = FALSE,
          breakout_sets	= FALSE
          # ,
          # force_request = TRUE
         ) %>%
          label_to_colnames,
        "Error")
      )
    )
}

select_identifiers <- function(nested_surveys){
  nested_surveys %>%
    mutate(
      data = map(
        data,
        ~ .x %>%
          select(
            ## Participant info
            matches("(Name|Email|Date)$", ignore.case = FALSE),
            starts_with("response"),
            Progress,
            Finished,
            ## child name
            starts_with("first_name"))
      )
    )
}

missing_child_first_name <- function(nested_surveys){
  ## Compare against data_status.csv to see when we stopped
  ## asking parent 2 child questions

  data_status <- here("data", "manually-entered",
                      "data_status.csv") %>%
    read_csv(col_types = c(.default = "c"))

  data_status <- data_status %>%
    pivot_longer(matches("(ahha|sv)_"),
                 names_pattern = "(ahha|sv)_(.*)",
                 names_to = c("survey_type", ".value"))

  nested_surveys %>%
    mutate(
      has_cname = map(data,
                      ~ names(.x) %>%
                        str_detect("^first_name") %>%
                        any)
    ) %>%
    select(phase_sv, time_point_sv, parent_sv,
           has_cname, on_who, survey_type) %>%
    filter(has_cname == FALSE &
             !on_who %in% "on-self") %>%
    left_join(
      data_status,
      by = c("time_point_sv" = "time_point",
             "phase_sv" = "phase",
             "survey_type")
    ) %>%
    filter(parent_2_child_questions != "--")
}

deidentify_each <- function(df){
  df %>%
    mutate(
      data = map(
        data,
        ~ .x[
          !names(.x) %in%
           c(
            .x %>%
              names %>%
              str_subset("^(Recipient|Location)|IP Address")
            )
        ]
     )
  )
}

## matching id's ---------------------------

import_embedded_data <- function(df){
  df %>%
    mutate( #also import child first name
      survey_questions = map(id, possibly(~ survey_questions(.x), "Error")),
      qid_first_name = map(
        survey_questions,
        ~ .x %>%
          filter(
            question %>% str_detect("first name of your.*child")
          ) %>%
          pull(qid)
      ),
      data = map2(
        id,
        qid_first_name,
        possibly(
          ~ fetch_survey(
          .x,
          col_types = cols(.default = "c"),
          convert = FALSE,
          #force_request = TRUE,
          include_questions = c(character(), .y)
          ),
          "Error"
        ) %>%
          label_to_colnames
      )
    )
}

add_child_fname_cols <- function(df){
  df %>%
    mutate(
      first_name_cols = map(
        data,
        ~ names(.x) %>% str_subset("^child_first_name")
      )
    )
}

add_is_it_on_child <- function(df){

  df %>%
    mutate(
      is_it_on_child = case_when(
        on_who %in% "on-self" |
        parent_sv == "2" & (
          # When we stopped asking parent 2 child questions
          (phase_sv == "3" & time_point_sv >= "t4") |
          (phase_sv == "2" & time_point_sv >= "t5") |
          (phase_sv == "1" & time_point_sv >= "t7")
        )
        ~ FALSE,
        TRUE ~ TRUE
        )
      )
}

check_child_fname_cols_exist <- function(df){
  empty_list <- list(character())

  missing_child_fname <- df %>%
    add_is_it_on_child %>%
    filter(
      first_name_cols %in% empty_list
      & is_it_on_child == TRUE
      )

  stopifnot(nrow(missing_child_fname) == 0)

  df
}

clean_embedded_names <- function(df){
  df %>%
    mutate(
      data = map(
        data,
        ~ .x %>%
          as_tibble(.name_repair = "unique") %>% # Score is duplicated in some
          clean_names %>%
          rename_with(~ .x %>% str_remove("recipient_")) %>%
          rename(date_sv = recorded_date)
      )
    )
}

remove_duplicate_surveys <- function(df){
  df %>%
    mutate(
      data = map(data, remove_sv_dups)
    )
}

unnest_surveys <- function(df){
  df %>%
    select(
      time_point_sv,
      phase_sv,
      parent_sv,
      on_who,
      survey_type,
      data,
      everything()
    ) %>%
    unnest(data)

}

clean_person_names <- function(df){
  df %>%
    mutate(
      name = case_when(
        name == "Tanya Martinez" ~ "Tanya Henderson Martinez",
        name == "Na Na" ~ NA_character_,
        TRUE ~ name
      )
    )
}

fix_time_points <- function(df){
  df %>%
    mutate(
      time_point = time_point_sv,
      time_point = case_when(

        survey_type == "survey" &
        time_point_sv %in% "t2"
        & pid %in% "S0428"
        & parent_sv %in% "1"
        ~ "t3",

        survey_type == "survey" &
        time_point_sv %in% "t5"
        & pid %in% "P0403"
        & date_sv %in% "2020-07-27 11:46:04"
        ~ "t4",

        survey_type == "survey" &
        time_point_sv %in% "t3"
        & pid %in% "P0752"
        & date_sv %in% "2020-08-30 14:46:58"
        ~ "t2",

        survey_type == "ahha" &
        time_point_sv %in% "t2"
        & pid %in% c("P0516", "S0516", "P0521", "S0521")
        ~ "t3",

        TRUE ~ time_point
      )
    )
}


remove_extra_cases <- function(df){
  df %>%
    filter(
      !(survey_type == "survey" &
          time_point %in% "t2"
        & pid %in% "S0540"
        & progress %in% 1)
      &
        !(survey_type == "survey" &
            time_point %in% "t4"
          & pid %in% "P0403"
          & progress %in% 1)
    )
}

remove_rephased <- function(df){
  ## Shows Phase 2 families that ended up redoing in phase 3, baseline
  redos <- c(
    paste0(
      40,
      c(1, 2, 3, 5, 6, 8)
    ),
    734
  )

  df %>%
    filter(
      !(survey_type == "survey" &
        fid %in% redos
        & time_point == "t1"
        & !is.na(on_who))
    )

}


check_extra_child_names <- function(df){
  ## Suggest code "if_some" (more than 1, or x) in addition to if_any and if_all

  many_first_names <- df %>%
    filter(
      rowSums(
        !is.na(
          df %>%
            select(
              matches(child_name_regex)
            )
        )
      ) > 1
    ) %>%
    select(
      matches(child_name_regex),
      time_point,
      phase_sv,
      on_who,
      pid
    ) %>%
    distinct(
      pid,
      time_point,
      .keep_all = TRUE
    )

  stopifnot(nrow(many_first_names) == 0)

  df
}

remove_duplicate_child_pid <- function(df){
  df %>%
    filter(
      !(## P0325 answered for one out of two children at t2
        ## Answered for A0325 twice at this time; we discard the 2nd entry
        ## Reason:
        ### Maddy Fri 11-Mar-22 12:47 PM
        ## The data is incomplete for the 2nd entry
        pid == "P0325"
        & child_pid == "A0325"
        & time_point %in% c("t2")
        & child == "2")
      )
}

clean_and_match_child <- function(row_per_child){
  row_per_child <- row_per_child %>%
    mutate(
      child_fname = child_first_name_first_name_only %>%
        trimws %>%
        str_to_title
    )

  row_per_child %>%
    replace_child_fnames(corrections = fid_sv_master()) %>%
    match_child
}

remove_extra_rows <- function(df){
 df %>%
    filter(
      !(mi(child_pid) & child != "1")
    ) %>%
    add_is_it_on_child %>%
    mutate(
      across(
        c(child, child_pid),
        ~ case_when(
          is_it_on_child == FALSE ~ "N/A",
          TRUE ~ .x
        )
      )
    )
}

clean_and_deidentify <-function(row_per_child_matched){
  row_per_child_matched %>%
    mutate(
      progress_sv = progress,
      child = child %>% str_extract("\\d")
    ) %>%
    de_identify %>%
    clean_names %>%
    remove_extra_rows %>%  # rows resulting from child per row pivot
    add_phase
}

## Used in Qualtrics Cleaning ####

title_trim <- function(x) {
  str_to_title(
    str_trim(x, side = "both")
  )
}

q_names <- function(df){
  df %>%
    mutate(
      first_name = !!sym("Recipient First Name"),
      last_name = !!sym("Recipient Last Name"),
      email = !!sym("Recipient Email"),
      start_date = !!sym("Start Date"),
      date_sv = !!sym("Recorded Date"),
      email = str_to_title(email)
    ) %>%
    rename(
      progress = Progress,
      finished = Finished
    )
}

unite_names <- function(df){

  df %>%
    unite(
      col = name,
      first_name,
      last_name,
      sep = " ",
      na.rm = TRUE,
      remove = FALSE
    )
}


unite_to_from <- function(df, to, from1, from2, sep = sep){

  df %>%
    unite(
      col = {{to}},
      {{from1}},
      {{from2}},
      sep = sep,
      na.rm = TRUE,
      remove = TRUE
    )
}

remove_sv_dups <- function(df){

  df <- df %>%
    mutate(
      first_name = title_trim(first_name),
      last_name = title_trim(last_name),
      email = title_trim(email),
      progress = as.numeric(
        as.character(progress)
      )
    ) %>%
    unite_names %>%
    mutate(
      name = replace(
        name,
        name == "Amy Atkinson"
        & email == "Ferguamy@Hotmail.com",
        "Amy Atkinson2"
      )
    ) %>%
    filter(
      #!grepl("Import", name) &
      !(name %in% name[duplicated(name)]
        & finished == "False"
      )
    ) %>%
    group_by(name) %>%
    filter(
      progress == max(progress)
      & start_date == max(start_date)
    ) %>% ungroup()

}

match_ID <- function(df, x){

  df <- df %>%
    mutate(
      {{ x }} := master[
        match(
          name,
          master$name,
          incomparables = NA
        ),
      ] %>%
        pull( {{ x }} ),

      {{ x }} := case_when(
        is.na( {{ x }} ) ~ master[
          match(
            email,
            master$email,
            incomparables = NA
          ),
        ] %>%
          pull( {{ x }} ),
        TRUE ~ {{ x }}
      )
    )

  #
  # df$fid <- master$fid[
  #   match(
  #     df$name,
  #     master$name,
  #     incomparables = NA
  #   )
  # ]
  #
  #
  # df$fid <- ifelse(
  #   is.na(df$fid),
  #   master$fid[
  #     match(
  #       df$email,
  #       master$email,
  #       incomparables = NA
  #     )
  #   ],
  #   master$fid[
  #     match(
  #       df$name,
  #       master$name,
  #       incomparables = NA
  #     )
  #   ]
  # )

  df

}

see_missing <- function(df, x){
  df %>%
    filter(
      is.na( {{ x }} )
      & !is.na(name)
      & ## Remove fluff of GFHS members doing trial runs
        ! name %in% c(
          "Jessica Yu",
          "Jessica Test1",
          "Sabrina's Test",
          "Katherine Eckert"
        )
    ) %>%
    select(
      matches("(P|F)ID", ignore.case = FALSE),
      name, email,
      matches("^time_point")
    )
}


df_missing <- function(df, x){
  df %>%
    filter(
      is.na( {{ x }} )
      & !is.na(name)
      & ## Remove fluff of GFHS members doing trial runs
        ! name %in% c(
          "Jessica Yu",
          "Jessica Test1",
          "Sabrina's Test",
          "Katherine Eckert"
        )
    ) %>%
    select(
      matches("(P|F)ID", ignore.case = FALSE),
      matches("(name|email)$", ignore.case = FALSE),
      matches("^time_point")
    )
}

partial_matches <- function(df, suffix){

  su <- suffix

  in_by <- function(x) {
    df_missing(df, fid) %>%
      inner_join(
        study,
        by = x,
        suffix = c(su, "_study"),
      )
  }

  pm <- bind_rows(
    in_by("first_name"),
    in_by("last_name")
  )

  cols <- cross2(
    c("name", "email"),
    c(su, "_study")
  ) %>%
    map_chr(paste0, collapse = "")

  pm %>%
    select(
      i_drop_out,
      all_of(cols),
      matches("user_name"),
      starts_with("note"),
    ) %>%
    select(
      starts_with("name"),
      everything()
    ) %>%
    filter(
      !is.na(name_study) &
        !name_study %in% c("N/A", "")
      & i_drop_out != 1
    )
}

path_to_names <- function(df){
  df %>%
    mutate(
      file = path_name %>% path_file,
      time_point_sv = sub(".*(t\\d{1}).*", "\\1", file),
      phase_sv = sub(".*phase_(\\d{1}).*", "\\1", file),
      parent_sv = sub(".*parent_(\\d{1}).*", "\\1", file),
      survey_type = sub(".*parent_\\d{1}(.*_sv|_reg)_{1}.*", "\\1", file)
    ) %>%
    select(-file)
}

## Used in matching ####
## Correct variant names
# fid, sv spelling, master spelling

fid_sv_master <- function(){
  wb <- loadWorkbook(
    here("data",
         "manually-entered",
         "fid_survey-name_master-name.xlsx"),
    password = keyring::key_get("MASTER_KEY")
  )

  fsm <- readWorksheet(
    wb,
    sheet = getSheets(wb),
    colTypes = c(XLC$DATA_TYPE.STRING)
  )

  fsm <- fsm[-1] %>% split(seq(nrow(.))) %>% map(as.character)

}

prepend_number <- function(x){
  y <- sub(
    "(.*)( - \\d{1} - )(.*)",
    "\\2\\1 \\3",
    x
  )
  sub(
    "^ - ",
    "",
    y
  )
}

label_child <- function(x){
  y <- sub(
    "^(\\d{1})( - )(.*)",
    "\\2\\1__\\3",
    x
  )
  sub(
    "^ - ",
    "child",
    y
  )
}

pivot_to_child_per_row <- function(df, regex) {

  df <- df %>%
    rename_with(
      prepend_number,
      matches(regex)
    ) %>%
    rename_with(
      label_child,
      matches("^\\d{1} - ")
    )

  cvars <- str_subset(names(df), "^child")

  df %>%
    pivot_longer(
      cols = all_of(cvars),
      names_to = c("child", ".value"),
      names_sep = "__",
      names_repair= "minimal"
    )
}

# Note without !!! coalesce treats the selection of a data frame
# as a single argument; not sure why this is needed
# as documentation on coalesce gives impression it will work
# for a dataframe input

add_child_name <- function(df, regex){

  df <- df %>%
    mutate(
      across(
        everything(),
        ~ na_if(., "")
      ),
      child_fname = str_to_title(
        trimws(
          coalesce(
            !!! select(
              .,
              matches(regex)
            )
          )
        )
      )
    )
}

match_child <- function(df){
  df %>%
    left_join(
      master_kids %>% select(- last_name),
      by = c("fid", "child_fname"),
      suffix = c("_sv", "_master")
    )
}

# match unmatched by only fid

show_unmatched <- function(df){
  t <- "time_point_sv"
  p <- "parent_sv"

  unmatched <- df %>%
    filter(
      !mi(child_fname)
      & mi(child_pid)
    ) %>%
    select(
      study_sv,
      survey_type,
      pid,
      child,
      child_fname,
      fid,
      all_of(t),
      all_of(p)
    )

  unmatched %>%
    left_join(
      master_kids,
      by = c("fid"),
      suffix = c("_sv", "_master")
    ) %>%
    select(
      pid,
      all_of(t),
      all_of(p),
      child,
      fid,
      everything(),
      - last_name
    ) %>%
    arrange(all_of(t), fid)
}

replace_child_fnames <- function(row_per_child, corrections){
  ## Note: all_of() does not work inside replace()
  ## Vector names in for loop must not be column names in df
  ls <- corrections

  fid <- map_chr(ls, 1)
  sv_name <- map_chr(ls, 2)
  master_name <- map_chr(ls, 3)
  df_names <- row_per_child$child_fname

  for (i in seq(length(ls))){
    ls_fid <- fid[i]
    ls_survey_name <- sv_name[i]
    ls_master_name <- master_name[i]

    if(ls_survey_name %in% df_names){

      row_per_child <- row_per_child %>%
        mutate(
          child_fname = replace(
            child_fname,
            fid == ls_fid
            & child_fname == ls_survey_name,
            ls_master_name
          )
        )

    } else {}

  }
  row_per_child
}

# does not remove lisa logan
# as she did a survey twice under aanis@uoguelph.ca
remove_drop_outs_and_tests <- function(df){
  df %>%
    filter(!name %in% drop_outs$name) %>%
    filter(
      !(
        !name == "Lisa Logan"
        & email %in% c(
          "Sdougl02@Uoguelph.ca",
          "Asadowsk@Uoguelph.ca",
          "Sjdouglas8@Gmail.com",
          "Swedde@Uoguelph.ca",
          "Aannis@Uoguelph.ca",
          "Maddy.nixon@Uoguelph.ca",
          "Aannishome@Gmail.com"
        )
      )
    )
}

look <- c(
  "pid",
  "survey_type",
  "name",
  "date_sv",
  "progress"
)

pid_neq_pn <- function(df){
  df <- df %>%
    mutate(
      parent = case_when(
        grepl("P", pid) ~ 1,
        grepl("S", pid) ~ 2
      )
    )

  df %>%
    filter(
      (parent == 1
       & parent_sv == "2")
      | (parent == 2
         & parent_sv == "1")
      & !survey_type  %in%  "registration"
    ) %>%
    select(
      parent_sv,
      all_of(look),
      time_point,
      parent_sv
    )
}

fid_neq_ph <- function(df){
  df %>%
    filter(phase != phase_sv) %>%
    arrange(fid) %>%
    select(
      phase,
      phase_sv,
      fid,
      time_point_sv,
      survey_type,
      name,
      progress
    )
}

misordered_dates <- function(df){
  df %>%
    arrange(pid, time_point) %>%
    filter(!on_who == "on-child") %>% # since on_self is sufficient
    group_by(pid) %>%
    filter(
      pid %in% pid[date_sv < lag(date_sv)]
    ) %>%
    select(
      all_of(look),
      phase_sv
    )
}

# find duplicate adults

get_dups <- function(df) {
  df %>%
    filter(# since on_self is sufficient
      !on_who %in% "on-child"
    ) %>%
    add_count(
      pid,
      time_point,
      name,
      name = "n" # name of count variable
    ) %>%
    filter(n > 1) %>%
    arrange(fid) %>%
    select(
      n,
      time_point,
      all_of(look)
    )
}


# find duplicate children

get_dups_c <- function(df, x){
  df %>%
    filter(# since on_self is sufficient
      # !on_who == "_on_child_sv"
      # &
      !mi( {{ x }} )
    ) %>%
    add_count(
      pid,
      time_point,
      {{ x }},
      name = "n"
    ) %>%
    filter(n > 1) %>%
    select(
      pid,
      survey_type,
      date_sv,
      phase_sv,
      child,
      time_point,
      child_pid,
      name,
      child_fname,
      progress
    )
}



de_identify <- function(df) {

  df %>%
    select(
      - c(matches("Recipient (Email|(First|Last) Name)"),
          matches("email|IP Address|Location"),
          matches("first_name|fname"),
          matches("last_name"),
          matches("^name$"))
    )
}

# 02_export-surveys -------------------------------------------------------

remove_first_digits_and_ellipses <- function(x){
  x %>% str_remove_all("^\\d_|\\.{3}.*")
  ## Reason used in rename_relabel_each_survey():
  #First number in
  #e.g. 2_considerate...3
  #represents which child.
  #Second number
  #represents a duplicate item label
  #in this case one for sdq 2-4yr and another for 5-17yr.
  #Remove first number, as we will
  #get the child number from the non-item label, not names.
  #Remove second number, as we will
  #have unique names through pasting
  #with the non-item label.
}

cut_non_item_label_if_exists <- function(data, data_names){
  if_else(
    #Don't cut non-item label where there is
    #no item label: e.g. where name is Q1 or 1_Q1
    data_names %>% str_detect("^(\\d_|)Q\\d"),
    names(data),
    #Cut non-item label where there is
    #an item label; remove all after last dash
    #and replace with item label.
    #Last dash means child number 1 in "- 1 -..."
    #will be maintained
    names(data) %>% str_replace("^(\\D.* - ).*",
                                paste0("\\1", data_names))
  )
}

rename_relabel_each_survey <- function(survey_data){
  survey_data %>%
    mutate(
      data_names = map(data,
                       ~ names(.x) %>% remove_first_digits_and_ellipses),
      data = map(data, ~ .x %>% label_to_colnames),
      data = map2(data,
                  data_names,
                  ~ `names<-`(.x, cut_non_item_label_if_exists(.x, .y)))
    )
}

coalesce_duplicate_names <- function(df, cols){

  for(j in seq(cols)){

    index_dupes <- which(names(df) %in% cols[j])

    ## Append duplicates with digits
    names(df)[index_dupes] <- make_clean_names(names(df)[index_dupes])

    new_names <- names(df)[index_dupes]

    df[cols[j]] <- coalesce(
      !!! df %>% select(
        all_of(
          names(df)[index_dupes]
        )
      )
    )

    extra_columns <- index_dupes[-1]
    df <- df[, - extra_columns]

  }
  df
}

deidentify_raw_data_and_add <- function(nested_surveys, ui){
  nested_surveys %>%
    mutate(
      ## This is needed as Qualtrics gives identical column names
      ## e.g. two "Score" columns
      data = map(data, ~ `names<-`(.x, names(.x) %>% make.unique)),
      data =
        map(
          data,
          possibly(
            ~ ui %>%
              inner_join(
                .x,
                by = c("response_id" = "Response ID")
              ) %>%
              de_identify,
            "Error")
        )
    )
}
#
# join_to <- function(nested_surveys, ui){
#   nested_surveys %>%
#     mutate(
#       data = map(
#         data,
#         ~ ui %>%
#           inner_join(
#             .x,
#             # %>% as_tibble(.name_repair = "unique"),
#             by = c("response_id" = "Response ID")
#           ) %>%
#           select(
#             - c(starts_with("Recipient"),
#                 starts_with("Location"),
#                 "IP Address")
#           )
#       )
#     )
# }

## other -----

  sv_info <- c(
    "phase", "fid", "pid", "time_point",
    "child", "child_pid", "date_sv", "progress_sv", "on_who",
    "phase_sv", "time_point_sv", "parent_sv" # bookkeeping vars
  )


## Archive ####


## Old code ####
#
# dfc <- df %>%
#   filter(
#     !mi(child_pid)
#     ) %>%
#   mutate(
#     parent_pid = pid
#   ) %>%
#   select(
#     fid, parent_pid, on_who,
#     parent_sv, phase_sv,
#     progress_sv, date_sv,
#     time_point,
#     starts_with("child_")
#   ) %>%
#   rename_with(
#     ~ gsub("child_", "", .x),
#     starts_with("child_")
#   ) %>%
#   rename(pid = pid)
#
# df <- bind_rows(df, dfc) %>%
#   distinct(
#     pid,
#     time_point,
#     .keep_all = TRUE
#   )
#
# ## De-identify
# df <- de_identify(df)
#
# dfw <- dfl %>%
#   select(
#     ## Remove all columns that do not have
#     ## equal values across each child 1 to 7
#     - matches("is the first name|last_name"),
#     ) %>%
#   pivot_wider(
#     names_from = child,
#     names_glue = "{child}__{.value}",
#     values_from = c(child_pid, child_fname)
#     )
#
# ## De-identify
# dfw <- de_identify(dfw)
#
# ## Confirm that Response ID is unique per row
#
# rid_dups <- dfw %>%
#   add_count(`Response ID`) %>%
#   filter(n > 1) %>%
#   arrange(time_point, on_who, `Response ID`) %>%
#   select(time_point, on_who, `Response ID`, pid, n)
#
#  stopifnot(nrow(rid_dups) == 0)
#
# ## Vars for SID
#   df <- df %>%
#     mutate(
#       SID_parent_sv = parent_sv,
#       SID_phase_sv = phase_sv,
#       SID_progress_sv = progress_sv,
#       SID_date_sv = date_sv
#       #,
#       #SID_did_sv_on_child = parent_did_sv_on_child
#     )


