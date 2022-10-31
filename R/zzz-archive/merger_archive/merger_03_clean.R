# ------------------------------------------------------------------------

datalist <- list(sva, ha, asa, ll)

# ------------------------------------------------------------------------
# Fix FID

datalist <- datalist %>%
  map(
    ~ .x %>%
    mutate(
      FID = as.numeric(
        str_sub(PID, start = -3)
      )
    )
    )

# Check for missing IDs after fix
datalist %>%
  milist(PID) 

datalist %>%
  milist(FID)

# --Merge sources----------------------------------------------------------------------

idt <- c("PID", "FID", "time_point")

df <- datalist %>%
  reduce(
    full_join,
    by = idt
  ) %>%
  mutate(
    phase = case_when(
      FID > 400 ~ 3,
      FID < 401 & FID > 299 ~ 2,
      FID < 300 ~ 1,
      TRUE ~ as.numeric(NA)
    )
  ) %>%
  # Order within FIDs
  mutate(
    FID_order = case_when(
      grepl("P", PID) ~ 1,
      grepl("S", PID) ~ 2,
      grepl("A", PID) ~ 3,
      grepl("B", PID) ~ 4,
      grepl("C", PID) ~ 5,
      grepl("D", PID) ~ 6,
      TRUE ~ as.numeric(NA)
    )
  ) %>%
  arrange(
    FID,
    time_point,
    FID_order
  ) %>%
  # Remove drop-out families
  filter(!FID %in% drop_outs$FID_decline) %>% 
  mutate(# needed as "" in child_pid messes up filtering and matching
    across(
      child_pid, 
      ~ na_if(.x, "")
    )
  )

# Check whether there are still any in data that are not on master
dont_belong <- df %>%
  filter(
    !FID %in% master$FID |
    !PID %in% master$PID |
    !child_pid %in% master$PID
    ) %>% 
  select(PID, time_point)

stopifnot(nrow(dont_belong) == 0)

# Joins ####
## --Join with missing PIDs per time_point----------------------------------------------------------------------
## Nest data by time_point

dfn <- df %>%
  group_by(time_point) %>%
  nest()

dfn <- dfn %>%
  mutate(
    data = map(
      data, 
      ~ master_join(.x, master)
      )
  )

df <- dfn %>%
  group_by(time_point) %>%
  unnest(
    cols = c(data)
    )

## --Join with missing parent-child combinations per time_point----------------------------------------------------------------------
## Get all master PIDs for kids and parents

master <- add_role(master)
kids <- ids_of(master, "child")
rents <- ids_of(master, "parent")

# Create and join separate df's of all PID-child.pid and PID-parent.pid combinations ------

pc <- create_combos(
  rents,
  kids,
  SID
)
cp <- create_combos(
  kids,
  rents,
  SID
)

all_combos <- as_tibble(bind_rows(pc, cp))

df <- add_role(df)

df <- df %>% 
  mutate(
    across(
      c(child_pid, parent_pid), 
      ~ na_if(.x, "")
      ),
    SID = coalesce(child_pid, parent_pid)
  )


dfn <- df %>%
  group_by(time_point) %>%
  nest()


# Join data with all missing combinations (non-matches between data and all combinations)

hmm <- right_join(
  dfn$data[[1]] %>%
    select(
      everything(), -SID
      ), 
  anti_join(
    all_combos,
    dfn$data[[1]],
    by = c("PID", "SID")
    ),
  by = c("PID")
)


as <- dfn %>% 
  mutate(
    data = map(
      data,
      ~ bind_rows(
          .x,
          right_join(
            .x %>%
              select(everything(), -SID), 
            anti_join(
              all_combos,
              .x,
              by = c("PID", "SID"),
              ),
          by = c("PID")
        )
      )
    )
  )

df <- as %>%
  group_by(time_point) %>%
  unnest(
    cols = c(data)
    ) 

df <- df %>% 
  filter(
    !mi(SID)
    )

