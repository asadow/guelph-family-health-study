## ------------------------------------------------------------------------
## Qualtrics [SV] Surveys----------

sv <- vroom(
  paste0(cle_p, "sv.csv"), 
  col_types = c(.default = "c")
)

## Transform data to 2 sets: 1 where PID is for parent with child_pid column; 2 where PID is for child with parent_pid column

# keep PID, child_pid rows for analyses of parent-child combinations

# create PID rows using child_pid and child_variables, and create parent variable 
# to indicate which parent (P or S) this is from, call data svc

sv <- sv %>% 
  mutate(
    progress_sv = Progress
  )

svc <- sv %>%
  filter(
    !mi(child_pid)
    ) %>%
  mutate(
    parent_pid = PID
  ) %>%
  select(
    FID, parent_pid, 
    parent_sv, phase_sv, 
    progress_sv, date_sv,
    time_point,
    starts_with("child_")
  ) %>%
  rename_with(
    ~ gsub("child_", "", .x),
    starts_with("child_")
  ) %>%
  rename(PID = pid)

# Remove duplicate PID-time_points where child_pid is missing. This keeps single rows with no child_pid

svp <- sv %>% 
  arrange(time_point, PID, child_pid) %>% 
  filter(
    !(
      duplicated(
        .[c("PID", "time_point")]
                 ) 
      & mi(child_pid)
      )
    )

sva <- bind_rows(svc, svp) %>%
  mutate(parent_did_sv_on_child = "1")
