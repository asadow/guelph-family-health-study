
cleft_join <- function(x, y, by = NULL, suffix = c(".x", ".y")) {
  joined <- left_join(x, y, by = by, suffix = suffix)

  to_coalesce <- names(joined) %>% .[!. %in% union(names(x), names(y))]
  suffix_used <- suffix %>% .[if_else(str_ends(to_coalesce, .[1]), 1, 2)]

  # remove suffixes and deduplicate
  to_coalesce <- to_coalesce %>%
    str_sub(1, nchar(to_coalesce) - nchar(suffix_used)) %>%
    unique

  coalesced <- map_dfc(
    to_coalesce,
    ~ coalesce(joined[[glue("{.}{suffix[1]}")]], joined[[glue("{.}{suffix[2]}")]])
  ) %>% setNames(to_coalesce)

  bind_cols(joined, coalesced)[names(x)]

}


## Come up with better names for the suffix functions
rename_suffix <- function(df, parent_letter, suffix){
  df %>%
    filter(
      pid %>% str_detect(parent_letter)
    ) %>%
    rename_with(
      ~ paste0(.x, "_parent_", suffix),
      - c(fid, time_point)
    )
}

suffix_parent <- function(df, vars_to_suffix){
  df <- df %>% # needed due to multiple rows per PID (1 per child)
    distinct(pid, time_point, .keep_all = TRUE) %>%
    select(fid, pid, time_point, any_of({{ vars_to_suffix }}))

  df %>%
    rename_suffix("P", "one") %>%
    full_join(
      df %>% rename_suffix("S", "two"),
      by = c("fid", "time_point")
    )
}

add_parent_suffixed <- function(df, vars_to_suffix){
  suffixed <- df %>% suffix_parent(vars_to_suffix)

  df <- df %>%
    left_join(
      suffixed,
      by = c("fid", "time_point")
    )

}
