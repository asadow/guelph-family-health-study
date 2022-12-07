library(here)
library(magrittr)
library(fs)

## Loads/installs packages and functions
here("R", "packages.R") %>% source
fnc_files <- here("R", "functions") %>% dir_ls %>% as.character()
fnc_files %>% str_subset("generic|request") %>% map(source)
here("R", "requests", "names.R") %>% source

## Miscellaneous
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")
options(warnPartialMatchArgs = TRUE)
fpath <- "/Users/Adam/study1/"

# Qualtrics Labels --------------------------------------------------------
#
# ql <- here("data",
#            "instructions_qualtrics-labelling",
#            "qualtrics_labels_and_coding_July_19_2022.xlsx") %>%
#   readxl::read_excel()
#
# ql <- ql %>%
#   mutate(
#     across(
#       c(suggested_label,
#         suggested_item_label),
#       ~ recode(.x, !!!dict)
#     )
#   )
#
# survey_demographics <- ql %>%
#   filter(category_or_block %>% str_detect("demographics")) %>%
#   pull(suggested_label)


# get calls ---------------------------------------------------------------

get_calls <- function(filepath) {
  code <- parse(filepath)
  tokens <- as.list(code)
  calls <- c()
  while (TRUE) {
    any_unpacked <- FALSE
    for (ii in seq_along(tokens)) {
      part <- tokens[[ii]]
      # Calls always have the function name as the first element
      if (is.call(part)) {
        fun_token <- part[[1]]
        calls <- c(calls, deparse(fun_token))
      }
      # Expressions have a length
      if (length(part) > 1) {
        tokens[[ii]] <- as.list(part)
        any_unpacked <- TRUE
      }
    }
    tokens <- unlist(tokens)
    if (!any_unpacked) break
  }
  unique(calls)
}

