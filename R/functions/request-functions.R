
read_and_glue_labels <- function(){
  path <- here("data",
               "manually-entered",
               "data-dictionary.xlsx")

  label_sheet <- path %>%
    readxl::read_excel(sheet = "labels")

  label_sheet %>%
    filter(!if_all(everything(), ~ is.na(.))) %>%
    fill(label) %>%
    mutate(
      across(
        c(label, item_label, extra_label),
        ~ .x %>% as.character() %>% replace_na("")
      ),
      label = glue("{label}_{item_label}", .na = ""),
      label = label %>% str_remove("(^_)|(_$)")
    )
}

relabel <- function(df){
  df %>% rename_with(~ dict, names(dict))
}

# Timepoints and id's ----------------------------------------------

join_id_pairs <- function(requested_times){
  # Study id's of parent child combinations
  parent_child_ids <- here("data",
                           "processed",
                           "parent-child-ids.csv") %>%
    read_csv %>%
    add_phase

  ## Cross join
  times_and_ids <- requested_times %>%
    left_join(
      parent_child_ids %>% select(- parent_pid),
      by = "phase"
    ) %>%
    select(- phase) %>%
    distinct(pid, child_pid, time_point, .keep_all = TRUE)
}


# Exceptions --------------------------------------------------------------

extract_needed_data <- function(qualtrics_requested_data){
  ## Where people took the wrong time point survey
  exceptions <- here("data", "manually-entered", "exceptions.csv") %>%
    read_csv(col_types = c(.default = "c")) %>%
    clean_names %>%
    rename(phase = ph,
           time_point = tp_true) %>%
    rename_with(~ glue("exception_{.}"),
                - c(phase, time_point, tp_source, data_type)
                )

  ## Join requested times, t1 for demographics, and exception times
  requested_and_exceptions <- qualtrics_requested_data %>%
    left_join(
      exceptions %>% select(phase, time_point, tp_source, data_type),
      by = c("phase", "time_point", "data_type")
    )

  assert_that(
    exceptions %>%
      filter(exception_completed_steps != "N/A"
             & exception_completed_steps != "1") %>%
      nrow == 0
    )

  ## Pivot long and filter out rows of repeated time_points
  needed <- requested_and_exceptions %>%
    rename(yes = time_point, no = tp_source) %>%
    pivot_longer(
      c(yes, no),
      names_to = "requested",
      values_to = "time_point"
    ) %>%
    add_count(phase, time_point) %>%
    filter(
      !(requested == "no" & n > 1)
      & !is.na(time_point)
    ) %>%
    select(- n) %>%
    mutate(
      time_point_sv =
        case_when(
          data_type == "survey" ~ time_point,
          TRUE ~ NA_character_
       ),

      phase_sv =
        case_when(
          data_type == "survey" ~ phase,
          TRUE ~ NA_character_
        )
    )

  needed

}

# Status of Data -------------------------------------------------------
## survey: isActive - whether survey is open or closed

qualtrics_api_credentials(
  api_key = key_get("QUALTRICS_API_KEY"),
  base_url = key_get("QUALTRICS_BASE_URL")
)

survey_info <- all_surveys() %>%
  select(name, isActive) %>%
  separate_survey_name

survey_export_info <- here("data",
                           "processed",
                           "surveys",
                           "gfhs_survey_nested.rds") %>%
  file_info %>%
  mutate(
    date_downloaded = as_date(birth_time),
    data_type = "survey"
    ) %>%
  select(date_downloaded, data_type)


## asa: birth_time - file creation date
asa_file_info <- here("data", "raw", "diet-asa") %>%
  dir_ls %>%
  file_info

asa_path_to_names <- function(df){
  df %>%
    mutate(
      file = path %>% path_file,
      time_point = sub(".*(t\\d{1}).*", "\\1", file),
      phase = sub(".*phase_(\\d{1}).*", "\\1", file),
      role = sub(".*phase_\\d{1}_(parents|children).*", "\\1", file),
      asa_year = sub(".*(\\d{4})_.*", "\\1", file),
      asa_type = sub(".*\\d{4}_(.*)$", "\\1", file)
    ) %>%
    select(-file)
}

asa_file_info <- asa_file_info %>%
  asa_path_to_names %>%
  mutate(data_type = "asa",
         birth_time = as_date(birth_time)) %>%
  select(data_type, birth_time,
         time_point, phase,
         role, asa_year, asa_type) %>%
  rename(date_downloaded = birth_time)

asa_cleaned <- here("data", "processed", "diet-asa",
                    "asa_cleaned_summary.csv") %>%
  read_csv(col_types = c(.default = "c")) %>%
  mutate(review_status = "reviewed", data_type = "asa")

survey_file_info <- here("data",
                         "raw",
                         "surveys",
                         "archive",
                         "gfhs-123_survey_deidentified_qualtrics") %>%
  dir_ls %>%
  file_info

survey_file_info <- survey_file_info %>%
  mutate(survey_name = path %>%
           path_file %>%
           str_remove("\\.csv"))

status_update <- function(requested_data){

  ## Data availability
  da <- here("data", "manually-entered", "data_status.csv") %>%
    read_csv(col_types = cols(.default = "c")) %>%
    rename(collection_status = status) %>%
    rename_with(~ .x %>% str_replace("sv", "survey"))

  ## Rename and pivot
  da_long <- da %>%
    rename_with(~ glue({"{.x}_source"}), 5:last_col()) %>%
    pivot_longer(
      cols = ends_with("_source"),
      names_pattern = "(.*)_source",
      names_to = "data_type",
      values_to = "collected"
    )

  matching_vars <- c("phase", "time_point", "data_type")

  ## Join with requested_data
  request_status <- da_long %>%
    right_join(
      requested_data,
      by = matching_vars
    ) %>%
    select(- collected)

  unreviewed <- request_status %>%
    ## Surveys are never reviewed
    filter(data_type != "survey") %>%
    left_join(asa_cleaned, by = matching_vars) %>%
    mutate(review_status = if_else(is.na(review_status),
                                   "unreviewed",
                                   review_status)
    ) %>%
    filter(review_status == "unreviewed") %>%
    select(- review_status)

  ## Create tibbles of incomplete and unreviewed statuses
  incomplete <- request_status %>%
    filter(!collection_status %in% c("complete", "N/A")) %>%
    left_join(asa_file_info, by = matching_vars) %>%
    mutate(
      date_downloaded = case_when(
        data_type == "survey" ~ survey_export_info$date_downloaded,
        TRUE ~ date_downloaded)
      )

  open_surveys <- survey_info %>%
    right_join(
      request_status,
      by = c("phase_sv" = "phase",
             "time_point_sv" = "time_point",
             "survey_type" = "data_type"
             )
    ) %>%
    filter(isActive == TRUE) %>%
    select(
      phase, time_point, collection_status, everything(),
      ## Remove columns like on_who if they are empty and unneeded
      where(
        ~ !all(is.na(.x))),
      - isActive
           ) %>%
    rename_with(~ .x %>% str_replace("_sv", "")) %>%
    mutate(date_downloaded = survey_export_info$date_downloaded) %>%
    arrange(phase, time_point)

  ## Warning
  print_and_capture <- function(x)
  {
    paste(
      capture.output(print(x)),
      collapse = "\n"
      )
  }

  warning(
  "\nSome data is incomplete:\n",
  print_and_capture(incomplete),
  "\n\nSome data is unreviewed:\n",
  print_and_capture(unreviewed),
  "\n\nSome surveys are still open to participants:\n",
  print_and_capture(open_surveys)
  )

  List(incomplete, unreviewed, open_surveys)

#
#   ## Surveys that need to be downloaded/dated
#   surveys <- here("data",
#                   "processed",
#                   "surveys",
#                   "gfhs_survey_nested.rds") %>%
#     read_rds
#
#   api_and_files <- surveys %>%
#     left_join(survey_file_info, by = "survey_name")
#
#   errored_surveys <- api_and_files %>%
#     filter(data == "Error") %>%
#     select(- data)
#
#   errored_surveys <- errored_surveys %>%
#     left_join(
#       request_status,
#       by = c("phase_sv" = "phase",
#              "time_point_sv" = "time_point",
#              "survey_type" = "data_type"
#       )
#     ) %>%
#     select(date_completed, survey_name)
#
#   warning(
#     "\nSome surveys need to be downloaded/updated:\n",
#     print_and_capture(errored_surveys)
#   )

}


write_status_for <- function(status, requester){
  names(status) <- names(status) %>% str_replace("_", "-")
  status %>%
    iwalk(
      ~ if(nrow(.x) > 0) {
        write_csv(
          .x,
          here(
            "data",
            "processed",
            "requested",
            glue("{requester}_status_{.y}_{Sys.Date()}.csv")
          )
        )
      }
    )
}


# Surveys -----------------------------------------------------------------

## Qualtrics API-accessed surveys filtered by needed surveys
api_surveys <- function(needed){
  all_surveys() %>%
    select(name, id) %>%
    separate_survey_name %>%
    right_join(
      needed,
      by = c("phase_sv" = "phase",
             "time_point_sv" = "time_point")
    )
}

## Why is the extract_labels function slow?
extract_labels <- function(df){
  df %>%
    mutate(
      labels = map(data,
                   . %>%
                     label_to_colnames %>%
                     names %>%
                     make_clean_names %>%
                     str_subset("^(recipient|location|ip_address)",
                                negate = TRUE)
      )
    ) %>%
    select(- data)
}

downloaded_survey_files <- function(){
  directory_path <- here("data",
                         "Raw Data",
                         "SV",
                         "gfhs-123_survey_deidentified_qualtrics")

  files <- list.files(directory_path, ".csv")
  m <- str_split_fixed(files, "[_.]", 6)
  colnames(m) <- c("study_phase",
                   "survey_type",
                   "time_point_sv",
                   "role_number",
                   "on_who",
                   "file_type")

  df_files <- m %>%
    as_tibble %>%
    separate_sp_rn %>%
    mutate(
      file_path = dir_ls(directory_path, glob = "*.csv"),
      on_who = if_else(on_who == "csv", NA_character_, on_who)
    )

  df_files

}

join_to_downloads <- function(df){
  df %>%
    left_join(
      downloaded_survey_files(),
      by = c("study_sv",
             "phase_sv",
             "survey_type",
             "time_point_sv",
             "role",
             "parent_sv",
             "on_who")
    ) %>%
    arrange(file_path)
}

read_downloads <- function(df){
  df %>%
    filter(!is.na(file_path)) %>%
    mutate(
      raw_data = map(file_path,
                     ~ read_csv(.x ,col_types = c(.default = "c"))
      )
    )
}

extract_names <- function(df){
  df %>%
    mutate(
      new_names = map(labels,
                      ~ .[which(. == "user_language"):length(.)]),
      old_names = map(raw_data, ~ .x %>%
                        names %>%
                        .[which(. == "User Language"):length(.)])
    ) %>%
    ## Compare a random name from old and new names to check equality
    mutate(
      rn = map_dbl(new_names, ~ sample(1:length(.), 1)),
      rn_new_names = map2_chr(new_names, rn, ~ .x[.y]),
      rn_old_names = map2_chr(old_names, rn, ~ .x[.y])
    )
}

label_downloads <- function(df){
  df %>%
    mutate(
      raw_data = pmap(
        list(raw_data,
             new_names,
             old_names),
        function(df, y, z)
          df %>%
          rename_with(
            ~ y[which(z == .x)],
            all_of(z)
          )
      )
    ) %>%
    select(- c(labels,
               new_names,
               old_names,
               starts_with("rn"))
    )
}

get_surveys <- function(requested_data, requested_survey_vars){
  needed_data <- requested_data %>%
    extract_needed_data %>%
    select(- time_point)

  surveys <- here("data",
                  "processed",
                  "surveys",
                  "gfhs_survey_nested.rds") %>%
    read_rds

  needed_surveys <- needed_data %>%
    left_join(surveys,
              by = c("phase_sv",
                     "time_point_sv"))
}

mget_surveys <- memoise(get_surveys)

prefix_mid_child_number <- function(x) {
  str_replace(x, "(.*)- (\\d) -(.*)", "child_\\2_\\1_\\3")
}

prefix_front_child_number <- function(x){
  negative_lookahead_match <- "length_of_(dog|cat)_ownership|(cat|dog)_breed"
  pattern <- glue("^(?!{negative_lookahead_match})(\\d) - (.*)")
  #dog and cat ownership questions are not on-child questions
  str_replace(x, pattern, "child_\\3_\\4")
}

prefix_child_numbers <- function(x){
  x %>% prefix_mid_child_number %>% prefix_front_child_number}

rename_child_questions <- function(survey_data){
  survey_data %>%
    rename_with(~ .x %>% prefix_child_numbers)
}

unnest_and_clean_surveys <- function(survey_data){
  survey_data <- survey_data %>%
    unnest(data, names_repair = make.unique) %>%
    ## UTC is default of fetch_survey() used in qualtRics
    mutate(date_sv = as_datetime(date_sv, tz = "UTC")) %>%
    rename_child_questions %>%
    clean_names

}

# ASA ---------------------------------------------------------------------

read_asa_data <- function(ui, requested_asa_files){
  ## from https://epi.grants.cancer.gov/asa24/researcher/sample.html
  regex_asa_file <- c(
    "totals" = "Totals",
    "items" = "Items|INF",
    "responses" = "Responses|MS",
    "all" = "Totals|Items|INF|Responses|MS"
  )
  x <- regex_asa_file[requested_asa_files]

  new_path <- here("data", "raw", "diet-asa")
  ui$path_name_asa <- ui$path_name_asa %>%
    str_replace("/Users/Adam/study1/Data/Raw Data/ASA/", new_path)

  ## Get distinct folders and files needed from asa user info
  folders_and_files <- ui %>%
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
        glue_collapse(!!x, "|")
      )
    )

  nested_data <- folders_and_files %>%
    mutate(
      asa_file_type = file_path %>%
        str_replace("^.+_(.*)\\.csv", "\\1"),
      data = map(
        file_path,
        ~ read_csv(.x)
      ),
      columns = map(data, ~ names(.x))
    ) %>%
    select(- folder_path, - file_path)

}

file_specific_cols <- function(asa_cols, y) {
  asa_cols %>%
    filter(asa_file_type == {{ y }}) %>%
    distinct(columns) %>%
    pull
}

pivot_asa <- function(nested_asa){

  asa_cols <- nested_asa %>%
    select(asa_file_type, columns) %>%
    unnest(columns) %>%
    filter(columns != "UserName")

  nested_asa <- nested_asa %>%
    select(- columns) %>%
    unnest(data) %>%
    group_by(UserName, asa_file_type) %>%
    nest() %>%
    mutate(
      data = map2(
        data,
        asa_file_type,
        ~ .x %>%
          select(
            all_of(
              file_specific_cols(asa_cols, .y)
            )
          )
      )
    )

  nested_wider <- nested_asa %>%
    pivot_wider(names_from = asa_file_type,
                values_from = data)

}

merge_asa <- function(ui, nested_wider) {

  df <- ui %>%
    left_join(
      nested_wider,
      by = c("user_name" = "UserName")
    ) %>%
    clean_names

  df <- df %>% merge_ir
}

merge_ir <- function(df){
  df %>%
    mutate(
      items_responses = map2(
        items,
        responses,
        ~ full_join(
          .x,
          .y,
          by = c("Occ_Name", "FoodNum"),
          suffix = c("_items", "_responses")
        )
      )
    )
}

clean_ir <- function(df){
  df %>%
    mutate(
      items_responses = map(
        items_responses,
        ~ select(
          .x,
          Occ_No_items,
          FoodNum,
          LineNum,
          Occ_Name,
          Occ_Time_items,
          Food_Description,
          Variable,
          Response,
          ResponseOs,
          SpinDial1,
          SpinDial2,
          everything(),
          Occ_No_responses,
          Occ_Time_responses,
        )
      )
    )
}

clean_asa <- function(df){

  df <- df %>% clean_ir

  df <- df %>%
    select(
      phase,
      time_point,
      fid,
      pid,
      user_name,
      date_asa,
      i_note_asa,
      time_point_asa,
      phase_asa,
      version_asa,
      starts_with("i"),
      any_of(
        c("totals",
          "items_responses",
          "items",
          "responses")
      )
    ) %>%
    arrange(fid, pid)

}

get_asa <- function(requested_times, requested_asa_files, roles){
  ## user_info: id's, time_point's and file source
  ui <- vroom(
    paste0(
      here("data",
           "processed",
           "diet-asa",
           "asa_user_info.csv")
      ),
    col_types = c(.default = "c")
  ) %>%
    filter(role %in% !!roles) %>%
    rename(
      ## Suffix with _asa
      i_outlier_note_asa = i_outlier_asa_note,
      amount_usual_asa = amount_usual,
      i_breastfed_note_asa = i_breastfed_note
    )

  ui <- requested_times %>%
    left_join(ui, by = c("phase", "time_point"))

  ## Use ui just for file paths
  nested_asa <- ui %>% read_asa_data(requested_asa_files)
  wider_asa <- pivot_asa(nested_asa)

  ## Use ui for joining
  asa_data <- ui %>%
    left_join(
      wider_asa,
      by = c("user_name" = "UserName")
    ) %>%
    clean_names

  if(requested_asa_files != "totals"){
    asa_data <- asa_data %>%
      merge_ir %>%
      clean_asa
  }

  asa_data %>%
    select(- any_of("age")) %>%
    mutate(date_asa = mdy(date_asa),
           fid = as.numeric(fid)) %>%
    rename_with(
      ~ glue("asa_{.x}"),
      any_of(
        c("totals",
          "items",
          "responses",
          "items_responses")
      )
    )

}

mget_asa <- memoise(get_asa)

remove_1st_of_2_consec <- function(x){
  str_replace_all(x,
                "([^_]+)(?:_\\1(?=_|$))*",
                "\\1")
}

rename_remove_1st_of_2_consec <- function(df){
  df %>%
    ## Removes first of two identical,
    ## consecutive words
    rename_with(
      ~ str_replace_all(.x,
                        "([^_]+)(?:_\\1(?=_|$))*",
                        "\\1")
    )
}

pivot_longer_children <- function(df){
  df %>%
    pivot_longer(
      cols = matches("^child_\\d"),
      names_to = c("child", ".value"),
      names_pattern = "child_(\\d)_(.*)",
      names_repair = "unique"
    ) %>%
    rename_remove_1st_of_2_consec
}


unite_first_using_others <- function(df, newcol, ...){
  df %>%
    unite({{ newcol }}, ..., na.rm = TRUE, sep = ", ")
}

## Arrange between and within families
arrange_by_fid <- function(df){
  df <- df %>%
    mutate(
      fid_order = case_when(
        grepl("P", pid) ~ 1,
        grepl("S", pid) ~ 2,
        grepl("A", pid) ~ 3,
        grepl("B", pid) ~ 4,
        grepl("C", pid) ~ 5,
        grepl("D", pid) ~ 6,
        TRUE ~ as.numeric(NA)
      )
    ) %>%
    arrange(
      fid,
      time_point,
      fid_order
    ) %>%
    select(- fid_order)
}

# Merging data -----------------------------------------------------------------
get_other_data <- function(){
  ll <- read_csv(here("data", "processed", "complete", "life-labs.csv"),
                 col_types = c(.default = "c"))
  ha <- read_csv(here("data", "processed", "incomplete", "ha_all.csv"),
                 col_types = c(.default = "c"))

  idt <- c("pid", "fid", "time_point")

  other_data <- list(ha, ll) %>%
    reduce(
      full_join,
      by = idt
    ) %>%
    select(- sex)

}

join_and_clean_names <- function(all_data, file_vars_asa){

  all_data %>%
    clean_names %>%
    rename(progress_sv = progress,
           sex = gender) %>%
    ## Remove technical vars
    select(- any_of(file_vars_asa),
           - any_of("age"),
           - study_survey,
           - trigger_survey_id)

}

get_clean_demo_phase_1 <- function(){
  demo_phase_1 <- here("data",
                       "processed",
                       "other",
                       "demographics_phase-1.rds") %>%
    read_rds %>%
    mutate(time_point = "t1") %>%
    unite_first_using_others(ethnicity_selected_choice,
                             ethnicity_selected_first,
                             ethnicity_selected_second) %>%
    rename(ethnicity_other_please_specify_text = ethnicity_written_other)

}


pull_t1_phase_1_if <- function(survey_data,
                               requesting_phase_1){
  if(!requesting_phase_1){
    return(survey_data)
  }

  demo_phase_1 <- get_clean_demo_phase_1()

  # here("data", "processed", "surveys") %>%
  #   dir_ls(regexp = "gfhs-1_survey_t(1|2)") %>%
  #   map_df(~ read_rds(.))

  t11 <- here("data",
              "processed",
              "surveys",
              "gfhs-1_survey_t1.rds") %>%
    read_rds %>%
    ## for bind_rows to work properly later:
    ## see work log July 25-29 for details
    mutate(
      phase = as.character(phase),
      date_sv = date_sv %>% ymd
    )

  t11 <- t11 %>% full_join(demo_phase_1, by = c("pid", "time_point"))

  phase_1_child_demo_regex <- "child.*(pid|gender|dob|.*ethnicity|.*related)"
  ## NB
  ## Not on-child yet
  ## t11 is row per parent child combo;
  ## Should change raw data to be row per parent
  ## Just like Qualtrics surveys
  ## For pivotting later
  ## For now: use distinct()

  if(exists("non_qualtrics_regex")){
    t11_selected <- t11 %>%
      select(phase,
             fid,
             pid,
             matches(phase_1_child_demo_regex),
             time_point,
             date_sv,
             parent_sv,
             all_of(names(demo_phase_1)),
             matches(non_qualtrics_regex)) %>%
      distinct(pid, .keep_all = TRUE)

  } else if (!exists("non_qualtrics_regex")){

    t11_selected <- t11 %>%
      select(phase,
             fid,
             pid,
             matches(phase_1_child_demo_regex),
             time_point,
             date_sv,
             parent_sv,
             all_of(names(demo_phase_1))) %>%
      distinct(pid, .keep_all = TRUE)
  }

  surveys <- list(
    get("t11_selected"),
    survey_data
  )
  survey_data <- bind_rows(surveys)

}


# Ethnicity --------------------------------------------------------------

add_ethnicities <- function(dat, requesting_phase_1){
  if(requesting_phase_1){

    demo_phase_1 <- get_clean_demo_phase_1()
    child_demo <- demo_phase_1 %>%
      add_role %>%
      filter(role == "child") %>%
      select(pid, starts_with("ethnicity")) %>%
      rename_with(~ glue("child_{.x}"))

    dat <- dat %>%
      full_join(
        child_demo,
        by = c("child_pid"),
        suffix = c("", "_2")
      ) %>%
      mutate(
        child_ethnicity_other_please_specify_text = coalesce(
          child_ethnicity_other_please_specify_text,
          child_ethnicity_other_please_specify_text_2
        ),
        child_ethnicity_selected_choice = coalesce(
          child_ethnicity_selected_choice,
          child_ethnicity_selected_choice_2
        ),
        .keep = "unused"
      )
    }

  dat %>%
    add_ethnicity_to_parent %>%
    add_ethnicity_to_child

}

add_ethnicity_to_parent <- function(dat){
  dat %>%
    add_ethnicity(name = "ethnicity",
                  ethnicity_selected_choice,
                  ethnicity_other_please_specify_text)
}

add_ethnicity_to_child <- function(dat){
  dat %>%
    add_ethnicity(name = "child_ethnicity",
                  child_ethnicity_selected_choice,
                  child_ethnicity_other_please_specify_text)
}

# Wrangle -------------------------------------------------------------------

select_given <- function(all_data, vars){

  prefixes <- c("", "parent_", "child_\\d_",
                "child_", "child_self_report_",
                "self_report_", "parent_child_")
  prefixes_regex <- glue("^({glue_collapse(prefixes, '|')})")
  gluep <- function(x){glue("{prefixes_regex}{x}")}

  all_data %>%
    select(
      any_of("experimental_group"),
      any_of("fid"),
      matches(gluep("pid")),
      matches("parent_in_study"),
      time_point,
      ends_with(glue("_{data_type}")),
      ends_with("_extra"),
      matches(gluep("(dob|sex)$")),
      matches(gluep(vars)),
      starts_with("asa")
    )
}

pivot <- function(dat){

  ## Prefix additional "child_" to wide format,
  ## on-child questions, so it is clear they are on-child
  ## variables when pivotted
  dat <- dat %>%
    rename_with(
      ~ str_replace(.x, "child_(\\d)", "child_\\1_child"),
      starts_with("child_")
    ) %>%
    pivot_longer_children %>%
    ## Remove extra rows
    filter(
      !(mi(child_pid) & child != 1)
    )

}

# dat %>%
#   add_fid %>%
#   filter(fid == 526) %>%
#   select(pid, time_point, matches("child.*pid"))
#
# missing_pairs %>%
#   add_fid %>%
#   filter(fid == 526) %>%
#   select(pid, child_pid)
#
# data_for_missing_pairs %>%
#   add_fid %>%
#   filter(fid == 526) %>%
#   select(pid, child_pid)

calculate_if <- function(dat, requesting_calculations){
  if(!requesting_calculations){return(dat)}

  dat %>% calculation
}

add_missing_pairs <- function(dat, times_and_ids){

  missing_pairs <- times_and_ids %>%
    anti_join(
      dat,
      by = c("pid", "child_pid", "time_point")
    )
  ## Parent data
  data_for_missing_pairs <- dat %>%
    select(everything(), - child_pid) %>%
    distinct(pid, time_point, .keep_all = TRUE) %>%
    right_join(missing_pairs, by = c("pid", "time_point"))

  data_for_non_missing_pairs <- dat %>%
    semi_join(
      times_and_ids,
      by = c("pid", "child_pid", "time_point")
    )

  data_for_missing_pairs %>%
    bind_rows(data_for_non_missing_pairs)
}

separate_multiple <- function(dat, cols){
  new_suffix <- "_extra"

  ## Remove the cols
  ## Then column bind the separated cols
  dat %>%
    select(- any_of(cols)) %>%
    bind_cols(
      dat %>%
        names %>%
        .[. %in% cols] %>%
        map_dfc(~ dat %>%
                  select(.x) %>%
                  separate(.x,
                           into = paste0(.x,
                                         c("", new_suffix)),
                           sep = ", ")
        )
    )
}

squish_on_child_and_self_surveys <- function(dat){
  if (all(is.na(dat$on_who_survey))){
    return(dat %>% select(- on_who_survey))
  }

  ## This function can be made more efficient
  ## If it only fills the data where on_who_survey is not missing
  ## So subset that data out, then bind it back

  suffix <- c("survey")
  suffixed <- c("time_point",
                "date",
                "on_who",
                "parent")

  cols <- glue("{suffixed}_{suffix}")

  dat <- dat %>%
    group_by(pid, time_point) %>%
    add_count

  dat_on_1 <- dat %>%
    filter(n == 1) %>%
    ungroup

  ## Fill and paste by group
  dat_on_2 <- dat %>%
    filter(n > 1) %>%
    fill(everything(), .direction = "downup")

  dat_on_2 <- dat_on_2 %>%
    arrange(pid,
            time_point,
            match(on_who_survey, c("on-self", "on-child"))
    ) %>%
    mutate(
      across(
        any_of(cols),
        ~ paste(.x[!is.na(.x)], collapse = ", ")
      )
    ) %>%
    ungroup

  dat_on_2 <- dat_on_2 %>%
    separate_multiple(cols) %>%
    distinct(pid, time_point, .keep_all = TRUE) %>%
    mutate(across(matches("date_(survey|ahha)"), ~ as_datetime(.x)))

  dat <- dat_on_1 %>% bind_rows(dat_on_2)

}

fill_by <- function(df, by, to_fill){
  df %>%
    group_by( {{ by }} ) %>%
    mutate(
      across(
        any_of(to_fill),
        ~ na_if(.x, "")
      )
    ) %>%
    fill(
      any_of(to_fill),
      .direction = c("downup")
    ) %>%
    ungroup
}

## Calculate Age from dates of all sources
add_age <- function(dat, data_type){

  dated <- dat %>%
    mutate(
      across(
        starts_with(glue("date_{data_type}")),
        list(age = ~ time_to_from(.x, dob, "years")),
        .names = "{str_replace(.col, 'date', 'age')}"
      )
    )

  if("child_dob" %in% names(dat)){
    dated <- dated %>%
      mutate(
        across(
          starts_with(glue("child_date_{data_type}")),
          list(age = ~ time_to_from(.x, child_dob, "years")),
          .names = "child_{str_replace(.col, 'date', 'age')}"
        ),
        child_age_survey = time_to_from(date_survey, child_dob, "years")
      ) %>%
      rename_with(~ str_remove(.x, "child_"), matches("child_child"))
    }
    dated
}

## Calculate bmi z from age in days, sex, and numeric bm and ht
add_bmi_z <- function(df){
  df <- df %>%
    mutate(
      age_ha_days = time_to_from(
        date_ha,
        dob,
        "days"
      ),
      sex_for_bmi = case_when(
        sex == "M" ~ 1,
        sex == "F" ~ 2,
        TRUE ~ NA_real_
      ),
      bm_kg_numeric = as.numeric(bm_kg),
      ht_cm_numeric = as.numeric(ht_cm)
    )

  df <- addWGSR(data = df,
                sex = "sex_for_bmi",
                firstPart = "bm_kg_numeric",
                secondPart = "ht_cm_numeric",
                thirdPart = "age_ha_days",
                index = "bfa") %>%
    rename(bmi_z = bfaz) %>%
    select(- any_of(
      c("age_ha_days",
       "age_years_ha",
       "sex_for_bmi",
       "ht_cm_numeric",
       "bm_kg_numeric")
      )
    )

}

add_iv_group <- function(dat){

  iv <- here("data", "processed", "iv_status.csv") %>%
    read_csv %>%
    select(fid, intervention) %>%
    rename(experimental_group = intervention)

  dat %>% left_join(iv, by = c("fid"))

}

add_parent <- function(dat){
  dat %>%
    mutate(parent_in_study = case_when(
      pid %>% str_detect("P") ~ "parent_1",
      pid %>% str_detect("S") ~ "parent_2",
      TRUE ~ NA_character_))
}

fix_dob_and_sex <- function(dat){
  dat %>%
    mutate(
      dob = case_when(
        pid == "A0720" ~ "2016-07-13",
        pid == "B0720" ~ "2018-01-16",
        ## Fix: see "Incorrect birthday for a GFHS child"
        pid == "B0609" ~ "2016-09-04",
        TRUE ~ dob
      ),
      sex = case_when(
        pid == "A0720" ~ "F",
        pid == "B0720" ~ "M",
        TRUE ~ sex
      )
    )
}

add_static_vars <- function(dat, requesting_phase_1){
  dat %>%
    add_parent %>%
    add_ethnicities(requesting_phase_1) %>%
    add_fid %>%
    add_iv_group %>%
    add_phase %>%
    add_role
}


pivot_wide <- function(dat_final, pivot_parents_wide){
  if(!pivot_parents_wide){return(dat)}

  dat_final <- dat_final %>%
    pivot_wider(names_from = parent_in_study,
                names_glue = "{parent_in_study}_{.value}",
                values_from = matches("^parent")) %>%
    ## Remove consecutive "parent" label
    rename_with(~.x %>% str_remove("_parent"),
                matches("(1|2)_parent")) %>%
    select(- contains("parent_in_study"))

  dups <- dat_final %>%
    add_count(pid, time_point, name = "n") %>%
    filter(n > 1)

  assert_that(nrow(dups) == 0)

  dat_final
}

# dat_final %>% names %>% str_subset("age")

#
# wtf <- dat_final %>%
#   filter(pid == "A0085" & time_point == "t3")
#
# wtf <- wtf %>%
#   pivot_wider(names_from = parent_in_study,
#               names_glue = "{parent_in_study}_{.value}",
#               values_from = c(matches("^parent")))
#
#
# dat_final %>%
#   add_fid %>%
#   filter(fid == "401") %>%
#   select(wc_cm, ht_cm, 1:20)
#
# 56.75/118.5


# wtf %>%
#   ## Remove consecutive "parent" label
#   rename_with(~.x %>% str_remove("_parent"),
#               matches("(1|2)_parent")) %>%
#   select(- contains("parent_in_study"))

arrange_and_relocate <- function(dat){
  ## Arrange and Relocate age and date variables to be side-by-side
  age_and_date <- glue("age_{data_type}") %>% c(glue("date_{data_type}"))
  order_by_type <- age_and_date %>% str_extract("[[:alpha:]]$") %>% order
  age_and_date <- age_and_date %>% .[order_by_type]

  dat <- dat %>%
    arrange_by_fid %>%
    relocate(phase) %>%
    relocate(role, .after = fid) %>%
    relocate(
      any_of(age_and_date),
      .after = sex
    ) %>%
    relocate(ethnicity, .after = dob)
}

fill_vars <- function(dat, pid_static_vars){
  dat <- dat %>%
    fill_by(pid, pid_static_vars) %>%
    fill_by(child_pid,
            c("child_ethnicity_selected_choice",
              "child_ethnicity_other_please_specify_text",
              "child_ethnicity")
    )
}

squish_child_rows <- function(dat,
                              roles,
                              data_type_children,
                              vars_child_self_report,
                              vars_child_not_by_parent_survey,
                              vars_exclude_parents){

  ## Get data of child self report and health assessment vars
  dat_child <- dat %>%
    filter(role == "child") %>%
    select(pid, time_point,
           dob, sex,
           matches(vars_child_self_report),
           starts_with(vars_child_not_by_parent_survey),
           ends_with(glue("_{data_type_children}")),
           - matches("pregnan|breastfeeding"),
           - parent_survey
           ) %>%
    rename_with(
      ~ glue("self_report_{.x}"),
      c(matches(vars_child_self_report),
        ends_with("survey"))
    ) %>%
    rename_with(~ glue("child_{.x}"), - c(time_point))

  if(!"parent" %in% roles){
    ## Remove some parent variables
    parent_vars_non_survey <- dat %>%
      names %>%
      str_subset("_(asa|ha|ll|esha|ahha)$") %>%
      str_subset(glue("date_{glue_collapse(data_type, '|')}"), negate = TRUE)

    dat <- dat %>% select(- all_of(parent_vars_non_survey))
    }

    dat %>%
      filter(role != "child") %>%
      select(- starts_with(vars_exclude_parents),
             - matches(vars_child_self_report)) %>%
      full_join(dat_child, by = c("child_pid", "time_point"))

}

prefix_parent_and_remove_child_prefix <- function(dat, remove_child_prefix){
  if(!remove_child_prefix){
    return(dat)
  }

  dat <- dat %>%
    rename_with(
      ~ glue("parent_{.x}"),
      c(any_of(ns),
        ## Created variables
        sex,
        dob,
        starts_with("age"),
        starts_with("child_age"),
        ethnicity,
        child_ethnicity,
        ends_with("_extra"),
        - c(child_pid, experimental_group, fid, time_point))
    )

  dat %>%
    rename_with(
      ~ str_remove(.x, "^child_"),
      starts_with("child_")
    )
}

# Read manual download ---------------------------------------------

read_all_qualtrics_csv <- function(path_to_raw_qualtrics_folder){
  files <- path_to_raw_qualtrics_folder %>% list.files(".csv")

  ## Matrix columns from file name
  m <- str_split_fixed(files, "[_.]", 6)
  colnames(m) <- c("study_phase",
                   "survey_type",
                   "time_point_sv",
                   "parent_sv",
                   "type_sv",
                   "file_type")

  file_tibble <- m %>%
    as_tibble %>%
    separate(
      study_phase,
      c("study", "phase_sv"),
      "-"
    ) %>%
    mutate(file_path = dir_ls(path_to_raw_qualtrics_folder, glob = "*.csv"))

  file_tibble %>%
    mutate(
      data = map(
        file_path,
        ~ read_csv(.,
                   skip = 1,
                   ## Avoiding name_repair in order to coalesce duplicates
                   name_repair = "minimal",
                   col_types = c(.default = "c"))
      )
    )

}



# Big functions -----------------------------------------------------------

get_data <- function(qualtrics_requested_data,
                     vars,
                     requesting_phase_1,
                     data_type,
                     requested_asa_files){

  survey_data <- qualtrics_requested_data %>%
    mget_surveys(vars) %>%
    filter(!is.na(id)) %>%
    unnest_and_clean_surveys %>%
    ## See exceptions
    filter(!(pid == "P0091" & time_point == "t1" & phase_sv == "2")) %>%
    pull_t1_phase_1_if(requesting_phase_1) %>%
    rename_with(
      ~ .x %>% str_replace("sv", "survey"),
      ends_with("sv")
    ) %>%
    rename(on_who_survey = on_who)

  if("asa" %in% data_type){
    asa_data <- requested_times %>%
      mget_asa(requested_asa_files, roles)
  }

  other_data <- get_other_data()

  not_in_other <- data_type %>%
    str_subset('ha|ahha|ll', negate = TRUE)

  all_data <- mget(
    glue("{c('other', not_in_other)}_data")
  )

}

remove_self_report_if_no <- function(dat, child_self_report){
  if(child_self_report){
    return(dat)
  }
  dat %>% select(- matches("self_report"))
}

prepare_vars <- function() source(here("R", "requests", "01_prepull.R"))
