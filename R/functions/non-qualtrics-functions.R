no_3 <- function(x){
  c1 <- df$data[[x]] %>%
    names %>%
    str_subset("_\\d{1}$") %>%
    str_remove("_\\d{1}$") %>%
    unique

  # find all for which it does not end in _3
  no_3 <- c1[!glue("{c1}_3") %in% names(df$data[[x]])]
  no_3
}

show_matches <- function(x){
  map(
    no_3(x),
    ~ df$data[[x]] %>%
      names %>%
      str_subset(.x)
  )
}

add_into_nested <- function(df){
  df %>%
    mutate(
      data = pmap(
        list(
          time_point,
          parent_sv,
          data
        ),
        function(t, ps, d) {
          d %>%
            mutate(
              time_point = t,
              parent_sv = ps %>% str_extract("\\d")
            )
        }
      )
    )
}

join_parts_of <- function(df, x){
  nested_parent_data <- df %>%
    filter(parent_sv == x)

  nested_parent_data %>%
    pull(data) %>%
    reduce(full_join, by = c("time_point", "parent_sv", "new_part_id"))

}

# For t2 only -------------------------------------------------------------

fix_names <- function(df, pattern, n_lag){

  df <- df %>%
    mutate(
      data = map(
        data,
        ~ `names<-`(
          .x,
          if_else(
            names(.x) %>% make_clean_names %>% str_detect(pattern)
            & names(.x) %>% make_clean_names %>% lag(., n_lag - 1) %>%
              str_detect("time_did_you_spend_sitting", negate = TRUE),
            paste0(
              lag(names(.x), n_lag),
              "_",
              names(.x)
            ),
            names(.x)
          )
        )
      )
    )
}

add_final_vars <- function(df){
  df %>%
    add_fid %>%
    add_phase %>%
    add_role
}
