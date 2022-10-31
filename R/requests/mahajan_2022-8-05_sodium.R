requester <- "mahajan"
data_type <- c("survey", "asa")
roles <- c("child")
on_children <- FALSE
children_as_pid <- TRUE
requested_asa_files <- "all"
## Can make this depend on requested_asa_files
## if you only want rds when including asa files
file_type <- "rds"


# non_qualtrics_regex <- "grocery_shopping|family_meals"
requested_times <- tibble(
  phase = c(
    "3"
  ),
  time_point = c(
    "t1"
  )
)

scales <- NULL
specific_vars <- NULL

apply_calculations <- function(df){df}

#
# d <- here("data", "processed", "requested", "mahajan_2022-08-05.rds") %>%
#   read_rds
# d
