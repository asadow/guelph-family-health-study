
## Get df of all pid, child_pid, parent_pid
pspr <- paste0(cle_p, "df.csv") %>%
  vroom(col_types = c(.default = "c"))

dids <-pspr %>%
  distinct(pid, fid)

kids <- dids %>%
  filter(pid %>% str_starts("A|B|C|D"))

pars <- dids %>%
  filter(!pid %>% str_starts("A|B|C|D"))

pars_kids <- pars %>%
  full_join(
    kids %>% rename(child_pid = pid),
    by = "fid"
  )

kids_pars <- kids %>%
  full_join(
    pars %>% rename(parent_pid = pid),
    by = "fid"
  )

ids <- pars_kids %>% bind_rows(kids_pars) %>% arrange(fid)

ids %>% write_csv(here("data", "parent-child-ids.csv"))
