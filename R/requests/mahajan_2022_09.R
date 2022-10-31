
# NB Request code does not include t2 phase 1 survey,  and ESHA -----------
file_date_needed <- TRUE
requesting_calculations <- FALSE
pivot_parents_wide <- TRUE

requester <- "mahajan"
roles <- c("child")
data_type <- c("survey", "asa")
requested_asa_files <- c("all")
data_type_children <- "asa"


requested_times <- tibble(phase = "3", time_point = "t1")
scales <- NULL

file_type = "rds"
vars_specific <- c("asa_totals",
                  "asa_items",
                  "asa_responses",
                  "asa_items_responses")

vars_child_self_report <- "\\b\\B"
vars_child_not_by_parent_survey <- vars_specific
vars_exclude_parents <- vars_specific


# Post processing ---------------------------------------------------------

dat <- here("data", "processed", "requested", "mahajan_2022-09-20.rds") %>%
  read_rds

totals <- dat %>%
  select(- matches("asa_(items|responses)")) %>%
  unnest(asa_totals, keep_empty = TRUE)

totals %>% write_for(glue("{requester}_asa_totals"),
                     data_type,
                     file_type = "csv",
                     file_date_needed,
                     needs_unnest_asa = FALSE)

non_totals <- dat %>% select(pid, time_point, matches("asa"), - asa_totals)

files <- c("asa_items",
          "asa_responses",
          "asa_items_responses")

dfs <- list()
for(i in seq(files)){
  file <- files[i]
  other_files <- files[!file == files]

  dfs[[i]] <- non_totals %>%
    select(- all_of(other_files)) %>%
    unnest(all_of(file), keep_empty = TRUE)

}

file_name <- glue("mahajan_{separate_files}_{Sys.Date()}.csv")
paths <- here("data", "processed", "requested", file_name)
pwalk(list(dfs, paths),
      write_csv)
