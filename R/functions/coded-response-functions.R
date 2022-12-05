# Functions used ####
likert_score <- function(x, a){
  scores <- seq_along(a) %>% as.character
  named_scores <- setNames(scores, a)
  recode(x, !!!named_scores)
}


st_quantify <- function(x){
  as.numeric(
    recode(
      x,
      "None" = 0,
      "Less than one hour per day" = 0.5,
      "1-2 hours per day" = 1.5,
      "2-3 hours per day" = 2.5,
      "4-6 hours per day" = 5,
      "7 or more hours per day" = 7
    )
  )
}

bev_quantify <- function(x){
  recode(
    x,
    "Never" = 0,
    "Once per week" = 1,
    "2-4 times per week" = 3,
    "Once per day" = 7,
    "2-4 times per day" = 21,
    "5 or more times per day" = 35,
    "I am not comfortable answering this question" = NA_real_
  )
}

tparse <- function(x) parse_time(x, format = "%I:%M %p")

add_avg_sleep <- function(df, type){
  df %>%
    mutate(
      "avg_sleep_hours_{type}" := difftime(
        tparse(!!sym(glue("wake_time_{type}"))),
        tparse(!!sym(glue("bed_time_{type}"))),
        units = "hours"
      )
    )
}

## dup_coalesce
## `pattern` should capture the start and end of names that are nearly identical
## `replacement` causes names to be duplicated
dup_coalesce <- function(df, pattern, replacement){
  to_rename <- str_which(names(df), pattern)

  ## Use base R to rename as
  ## we cannot use rename_with; it does not allow duplicate names
  names(df)[to_rename] <- str_replace(
    names(df)[to_rename], pattern, replacement)

  dupes <- names(df) %>% .[duplicated(.)]
  index_dupes <- which(names(df) %in% dupes)

  ## Append duplicates with digits
  names(df)[index_dupes] <- make_clean_names(names(df)[index_dupes])

  for(i in seq(dupes)){

    df[dupes[i]] <- coalesce(
      !!! df %>% select(
        matches(
          glue("^{dupes[i]}")
        )
      )
    )
  }

  df %>% select(- matches(glue("{dupes}_\\d")))
}

## To maintain naming in lists
combine <- function(x, y, z) {
  c(
    unlist(x[y]),
    unlist(x[z])
  )
}
