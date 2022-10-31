## UI and Timepoints -------------------------
qt_ui <- read_rds(
  here("data", "processed", "qualtrics_ui.rds")
)

qt_ui <- qt_ui %>%
  select(
    phase, fid, pid, child_pid, time_point,
    phase_sv, time_point_sv, response_id, child, # matching vars
    date_sv, progress_sv
    ) %>%
  filter(time_point %in% t & phase %in% p)

## Import, name and pivot ---------------------------------------------------------------------
## Code to ask for user input
df <- import_needed_surveys(t, p) %>% suppressMessages

df <- df %>% name_sv

df <- df %>%
  add_is_it_on_child %>%
  mutate(
    data = map2(
      data,
      is_it_on_child,
      function(d, y)
      if(y == TRUE)
        d %>%
          pivot_longer(
            cols = starts_with("child_"),
            names_to = c("child", ".value"),
            names_pattern = "child_(\\d)_(.*)",
            names_repair = "unique"
            # q200 col name is duplicated in t1 p3 par1
          )
      else d %>% mutate(child = "N/A")
    )
  )

## Regex ---------------------------------------------------------------------

df <- df %>%
  unnest(data) %>%
  right_join(
    qt_ui,
    by = c("phase_sv", "time_point_sv", "response_id", "child")
    )
df %>% names %>%  str_subset("i_involve")

## Selection ---------------------------------------------------------------------
df <- df %>%
  select(
    all_of(sv_info), matches(regex)
    )
