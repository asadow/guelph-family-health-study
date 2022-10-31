# Data Pulling ####

files_as_tibble <- function(sv_p){
  ## SV data
  files <- list.files(sv_p, "*.csv")

  m <- files %>%
    cbind(
      .,
      sv_p,
      paste0(sv_p, .),
      str_split_fixed(., "[._]", 6)
    )

  colnames(m) <- c("file", "path", "file_path",
                   "study_phase", "survey_type",
                   "time_point_sv", "parent_sv",
                   "type_sv", "file_type")

  df <- m %>%
    as_tibble %>%
    separate(
      study_phase,
      c("study", "phase_sv"),
      "-"
    )
}


replace_names <- function(x, pattern, replacement){
  `names<-`(
    x,
    str_replace_all(names(x), pattern, replacement)
  )
}

read_sv <- function(t, p, nrows){
  # Read
  ## Exceptions where ppl took wrong time point survey
  xc <- read_csv(
    glue(ref_p, "exceptions.csv"),
    col_types = c(.default = "c")
  )

  need <- crossing(t, p)
  need_xc <- need %>%
    left_join(
      xc,
      by = c("t" = "tp_true", "p" = "ph")
    )

  t <- unique(need_xc$t)
  p <- unique(need_xc$p)

  df <- files_as_tibble(sv_p)

  df <- df %>%
    filter(time_point_sv %in% t & phase_sv %in% p)

  df <- df %>%
    mutate(
      data = map(
        file_path,
        ~ read_csv(
          .,
          n_max = nrows,
          col_types = c(.default = "c")
          )
      )
    ) %>%
    select(- file_path)

  df
}

name_sv <- function(df){
  df <- df %>%
    mutate(
      data = map(data, ~ as.data.frame(.x)),
      # data = map(
      #   data, # Replace qualtrics child numbering mid col
      #   ~ replace_names(.x, "(.*)- (\\d{1}) -(.*)", "\\1_child_\\2_\\3")
      # ),
      data = map(
        data, # Replace qualtrics child numbering mid col
        ~ replace_names(.x, "(.*)- (\\d{1}) -(.*)", "child_\\2_\\1_\\3")
      ),
      data = map(
        data, # Replace qualtrics child numbering start of col
        ~ replace_names(.x, "^(\\d{1})(.*)", "child_\\1_\\2")
        ),
      data = map(data, ~ janitor::clean_names(.x)),
      # data = map(
      #   data, # Place child numbering at start
      #   ~ replace_names(.x, "(.*)_(child_\\d{1})_(.*)", "\\2_\\1_\\3")
      # ),


# DONT WORRY ABOUT THESE WITH LABELS --------------------------------------


      data = map(
        data, # For cols where child numbering is repeated e.g. 1 - 1_Q333
        ~ replace_names(.x, "child_(\\d{1})_\\d{1}_", "child_\\1_")
      ),
      data = map(
        data,
        ~ replace_names(.x, "qid\\d{3,4}_choice_text_entry_value_2", "child")
      ),
      data = map(
        data,
        ~ replace_names(.x, "field_2", "child")
      ),

# end ---------------------------------------------------------------------

      data = map(
        data, # this is because de-identified files have child1__child_pid
        ~ replace_names(.x, "^child(\\d)", "child_\\1")
      )
    )

  ## Messes up 01-read-sv.R, which we will abandon
  # %>%
  #   mutate(
  #     data = pmap(
  #       list(
  #         data,
  #         time_point_sv,
  #         phase_sv,
  #         role)
  #       ,
  #       function(d, t, p, r)
  #         if(
  #           t == "t7" &
  #           p == "1" &
  #           r == "child"
  #         ) d %>%
  #         select(- matches("_2$")) ## Duplicate names end in 2 *
  #       else
  #         d
  #       )
  #     )

  ## * due to default question block holding the responses
  df
}

pivot_sv <- function(df){
  df %>%
    mutate(
      data = map(
        data,
        ~ .x %>%
          pivot_longer(
            cols = starts_with("child_"),
            names_to = c("child", ".value"),
            names_pattern = "child_(\\d)_(.*)",
            names_repair = "unique"
            # q200 col name is duplicated in t1 p3 par1
          )
      )
    )
}

# Regex Checking ####
pulls_per_regex <- function(df, data, label, regex, type) {
  df %>%
    mutate(
      label := label,
      columns := map(
          {{ data }},
          ~ str_subset(names(.x), regex)
        ),
      regex := regex,
      type := type
    ) %>% select(- data)
}

abort_bad_argument <- function(regex, arg, must, not = NULL, vals = NULL){
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    not <- length(not)
    msg <- glue(
      "Challenge with str_subset using regex {regex}. ",
      "Cannot use subset {arg} as {msg}; not {not}. ",
      "Values: \n\n", glue_collapse(vals, "\n")
    )
  }
  abort(
    "abort_bad_argument", # class
    message = msg,
    arg = arg,
    must = must,
    not = not
  )
}

ov <- function(df){
  sl <- list()
  for (i in seq(label_regex)) {
    label <- map_chr(label_regex, 1)[i]
    regex <- map_chr(label_regex, 2)[i]
    type <- names(label_regex)[i]

    sl[[i]] <- df %>% pulls_per_regex(data, label, regex, type)
  }

  df <- bind_rows(sl) %>% unnest(columns)
  df

  # ## NA columns where regexes did not match anything
  # no_matches <- df %>% filter(columns %in% NA & parent_sv %in% "1")
  #
  # ## Show how regexes intended to pull 1 column are pulling more
  # too_many_matches <- df %>%
  #   filter(type %in% c("single", "pair") & map(columns, length) > 1) %>%
  #   unnest(columns)

  ## Code to cause stops or warnings based on SV Summary showing which vars
  ## are available from which time points
  # if(nrow(no_matches) > 0){
  #   warn_bad_argument(
  #     key,
  #     "subset",
  #     must = "have length 1",
  #     not = subset,
  #     subset
  #   )

}

# Label Assignment ####

warn_bad_argument <- function(regex, arg, must, not = NULL, vals = NULL){
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    not <- length(not)
    msg <- glue(
      "Challenge with str_subset using regex {regex}. ",
      "Using first value of resulting {arg} as {msg}; not {not}. ",
      "Values: \n\n", glue_collapse(vals, "\n")
      )
  }
  rlang::warn(
    "warn_bad_argument", # class
    message = msg,
    arg = arg,
    must = must,
    not = not
  )
}

label_sv <- function(df, x){
  label <- map_chr(x, 1)
  regex <- map_chr(x, 2)
  type_var <- names(x)

  for(j in seq(label)){
    lab <- label[j]
    lab2 <- glue("{lab}_units")
    key <- regex[j]
    type <- type_var[j]
    ndf <- names(df)

    subset <- str_subset(ndf, key)
    pos1 <- str_which(ndf, key)
    pos2 <- pos1 + 1

    # stop_message <- glue(
    #   "regex \"{key}\" pulls more than 1: \n",
    #   glue_collapse(subset, "\n")
    # )

    if(type == "single"){
      if(length(subset) > 1){
        warn_bad_argument(
          key,
          "subset",
          must = "have length 1",
          not = subset,
          subset
        )
        df <- df %>%
          rename({{ lab }} := subset[1])
        }
      if(length(subset) == 1) {
        #names(df)[ndf %in% subset] <- lab
        df <- df %>%
          rename({{ lab }} := subset)
        }
    }

    if(type == "pair"){
      if(!length(pos1) == 1 | !length(pos2) == 1) {abort(stop_message)}
       {
        names(df)[pos1] <- lab
        names(df)[pos2] <- lab2
      }
    }

    if(type == "scale"){
      if(length(subset) > 0) {
        #names(df)[ndf %in% subset] <- scale_names
        df <- df %>%
          rename_with(
            ~ glue("{lab}{.x}"),
            all_of(subset)
          )
        }
    }
  }
  df
}

# Data Wrangling ####
attach_pids <- function(df, sv_ui, vars){
  df <- df %>%
    unnest(data) %>%
    select(
      response_id,
      child,
      matches({{ vars }})
    )

  df <- df %>%
    left_join(
      sv_ui %>% clean_names,
      by = c("response_id", "child")
    )
  df
}

## Not needed due to pivot in pull
# pivot_child <- function(df){
#   # # format names for pivot_longer
#   # names(df) <- sub(
#   #   "c(\\d{1})_",
#   #   "child\\1__child_",
#   #   names(df)
#   # )
#
#   child_vars <- str_subset(names(df), "child\\d")
#
#   df %>%
#     pivot_longer(
#       cols = all_of(child_vars),
#       names_to = c("child", ".value"),
#       names_sep = "__",
#       names_repair= "minimal"
#     )
#
# }

expand_sv <- function(df){
  # 2 sets: child and parent
  ## 1 where pid is for parent with child_pid column;
  ## 2 where pid is for child with parent_pid column
  ## Keep pid, child_pid rows for analyses of parent-child combinations

  # Children as pid
  ## Create pid rows using child_pid and child_variables,
  ## Create parent variable
  dfc <- df %>%
    filter(
      !mi(child_pid)
    ) %>%
    rename(parent_pid = pid) %>%
    select(
      parent_pid,
      starts_with("child_")
    ) %>%
    rename_with(
      ~ str_replace(.x, "^child_", ""),
      starts_with("child_")
    )

  # Parents
  ## Remove duplicate rows created by pivot_longer
  ## and child 1, 2, etc.

  dfp <- df %>%
    filter(
      !(mi(child_pid) & !child == "1")
    )

  df <- bind_rows(dfc, dfp) %>%
    mutate(
      parent_did_sv_on_child = "1",
      across(
        c(child_pid, parent_pid),
        ~ na_if(.x, "")
      ), # Secondary ID
      sid = coalesce(child_pid, parent_pid)
    )

  # # Vars for SID
  # df <- df %>%
  #   mutate(
  #     SID_parent_sv = parent_sv,
  #     SID_phase_sv = phase_sv,
  #     SID_progress_sv = progress_sv,
  #     SID_date_sv = date_sv,
  #     SID_did_sv_on_child = parent_did_sv_on_child
  #   )
  #
  # # Squish _on_self_sv and remove them
  #
  # used_title <- p_title[p_title %in% names(df)]
  #
  # df <- df %>%
  #   group_by(
  #     PID, time_point
  #   ) %>%
  #   fill(
  #     all_of(used_title),
  #     .direction = "downup"
  #   ) %>%
  #   mutate(
  #     type_sv = str_replace(type_sv, "_on_child_sv", "_on_child_and_self_sv")
  #   ) %>%
  #   filter(!type_sv %in% "_on_self_sv")

  df
}


## Needs more from 02-prepare-sv
process_sv <- function(sv, label_regex, vars){
  safelab <- safely(label_sv)
  df <- sv %>%
    mutate(
      data = map(
        data,
        ~ safelab(.x, x = label_regex)
      )
    )

  df <- df %>%
    mutate(
      data = map(data, ~ .x$result)
    )

  vars_v <- unlist(map(vars, 1))
  x <- glue_collapse(glue("^{vars_v}"), "|")

  ## If parent-child sv
  df <- df %>% attach_pids(sv_ui, x)

  ## Note names for children start with child_
  ## When expanded df will have a duplicate of these names without the starting child_
  source(here("R", "tidy-questions.R"))

  df <- df %>% expand_sv
  df
}



export_sv <- function(df, t, p){
  vroom_write(
    df,
    glue(
      cle_p,
      "sv_",
      glue_collapse(t, "_"),
      "_phase_",
      glue_collapse(p, "_"),
      currentDate,
      ".csv"
    )
  )
}


# archive ####

#
# singles_assign <- function(df, label, regex) {
#   for (i in seq(regex)){
#     key <- regex[i]
#     old_name <- str_subset(names(df), key)
#     lo <- length(old_name)
#     if(lo > 1){
#       stop(
#         paste0(
#           "regex[i] \"", key,
#           "\" pulls more than 1",
#           paste0(
#             "\n\n",
#             str_subset(names(df), key),
#             collapse = "\n "
#           )
#         )
#       )
#     }
#     if(lo > 0)
#     {names(df)[names(df) == old_name] <- label[i]}
#   }
#   df
# }
#
# p_singles_units_assign <- function(df, label, regex) {
#   for (i in seq(regex)){
#
#     c <- label[i]
#     c2 <- paste0(c, "_units")
#     key <- regex[i]
#
#     pos1 <- str_which(names(df), key)
#     pos2 <- pos1 + 1
#
#     if(length(pos1) > 1)
#     {stop(
#       paste0(
#         "regex[i] \"",
#         key,
#         "\" pulls more than 1",
#         paste0(
#           "\n\n",
#           str_subset(names(df), key),
#           collapse = "\n "
#         )
#       )
#     )
#     }
#
#     if(length(pos1) > 0)
#     {df[, c] <- df[, pos1]
#     df[, c2] <- df[, pos2]}
#   }
#   df
# }
#
# scale_assign <- function(df, label, regex){
#   for (i in seq(regex)){
#     the_subset <- str_subset(names(df), regex[i])
#     new_names <- paste0(label[i], the_subset)
#
#     if(!length(the_subset) == 0)
#     {df[, new_names] <- df[, the_subset]}
#   }
#   df
# }
#
# clabels <- c("c1_", "c2_", "c3_", "c4_", "c5_")
#
# c_singles_assign <- function(df, label, regex){
#   for (j in seq(regex)){
#     key <- regex[j]
#     grab <- str_subset(names(df), key)
#
#     # if more than 5 grep results, throw error
#     if(length(grab) > 5)
#     {stop(paste0("regex[j] \"", key,
#                  "\" pulls more than 5",
#                  paste0("\n\n", grab, collapse = "\n ")))}
#
#     # otherwise
#     for (k in seq(grab)){
#       c <- paste0(clabels[k], label[j])
#       if(!length(grab) == 0)
#       {df[, c] <- df[, grab[k]]}
#     }
#   }
#   df
# }
#
# c_singles_units_assign <- function(df, label, regex){
#   for (j in seq(regex)){
#     key <- regex[j]
#     grab <- str_subset(names(df), key)
#     pos1 <- str_which(names(df), key)
#     pos2 <- pos1 + 1
#
#     # if more than 5 results, throw error
#     if(length(grab) > 5)
#     {stop(paste0("regex[j] \"", key,
#                  "\" pulls more than 5",
#                  paste0("\n\n", grab, collapse = "\n ")))}
#
#     # otherwise
#     for (k in seq(grab)){
#       c <- paste0(clabels[k], label[j])
#       c2 <- paste0(clabels[k], label[j], "_units")
#       df[, c] <- df[, pos1[k]]
#       df[, c2] <- df[, pos2[k]]
#     }
#   }
#   df
# }
#
# c_scales_assign <- function(df, label, regex){
#   for(j in seq(regex)){
#
#     logical <- str_detect(names(df), regex[j])
#
#     t <- names(df)[logical]
#
#     t <- paste0(label[j], t)
#
#     new <- str_replace(
#       t,
#       "(.*[[:lower:]]_)(\\d)_([[:lower:]].*)",
#       "c\\2_\\1\\3"
#     )
#
#     names(df)[logical] <- new
#
#   }
#   df
# }


