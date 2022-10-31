# IDs and time points ####

## Join with missing parent-child combinations per time_point
## Get all master PIDs for kids and parents

master <- add_role(master)
kids <- ids_of(master, "child")
rents <- ids_of(master, "parent")

# Create and join separate df's of all PID-child.pid and PID-parent.pid combinations

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

ids <- as_tibble(bind_rows(pc, cp))

ids <- ids %>% 
  nest(
    data = c(PID, SID)
  )

time_points <- c(paste0("t", 1:10))

ac <- tibble(time_point = time_points, ids)

# All IDs and TPs with Data ####

ac <- ac %>% unnest()

ndf <- ac %>% 
  left_join(
    df, 
    by = c("PID", "time_point")
  )


# %>% 
#   mutate(# needed as "" in child_pid messes up filtering and matching
#     across(
#       child_pid, 
#       ~ na_if(.x, "")
#     )
#   )