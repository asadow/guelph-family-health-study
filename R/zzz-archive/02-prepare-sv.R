## Variables ####

# use length: 1, 2, or n for specific scale number of items (not necessary at the moment to know exactly each n for each scale, but a future good to have; for now can use same n > 2 across scales)

source(here("R", "label_regex.R"))

lr_form <- function(x){
  label_regex <- list()
  for(i in seq(x[[1]])){
    label_regex[[i]] <- c(x[[1]][i], x[[2]][i])
  }
  label_regex
}

label_regex <- unlist(map(vars, lr_form), recursive = F)
names(label_regex) <- str_replace(names(label_regex), "[0-9]+", "")

### Regex Evaluation ####
#### Overview ####
overview <- ov(sv)
stopifnot("labels pulled nothing" = nrow(overview) > 0)

## Show which regexes are not working
overview %>% filter(columns %in% NA & parent_sv %in% "1")

## Show how regexes intended to pull 1 column are pulling more
overview %>%
  filter(
    type %in% c("single", "pair") &
      map(columns, length) > 1
  ) %>%
  unnest(columns)

#### Slice####

# sv %>% slice(1) %>% unnest(data) %>% names %>%
#   str_subset(., "family_feeding_practices_and_parental_modelling")

## df Pull and Label ####

safelab <- safely(label_sv)
df <- sv %>%
  mutate(
    data = map(
      data,
      ~ safelab(.x, x = label_regex)
    )
  )

df <- df %>%
  mutate(
    data = map(data, ~ .x$result)
  )

vars_v <- unlist(map(vars, 1))
x <- glue_collapse(glue("^{vars_v}"), "|")
# ## If parent-child sv
## old code
# df <- df %>% attach_pids(sv_ui, x)

## new code

keep <- c(
  "fid", "pid", "time_point",
  "parent_sv", "date_sv", "phase_sv",
  "time_point_sv", "type_sv", "start_date",
  "child", "child_pid", "progress_sv", "response_id"
  )

df <- df %>%
  select(- c(phase_sv, time_point_sv, parent_sv, type_sv)) %>%
  unnest(data) %>%
  select(
    all_of(keep),
    matches(x)
  )
## ignore
## Note expand_sv(df) will bind data where child is pid
## Any names in df with prefix child_
## Will after expand_sv(df), have
## additional columns without the prefix _child
## containing data where child is pid

source(here("R", "tidy-questions.R"))

df <- df %>% expand_sv

source(here("R", "tidy-acronyms.R"))

df <- df %>% select(order(names(.))) %>% clean_names

## If writing to file
# path <- "/Users/Adam/Library/CloudStorage/OneDrive-UniversityofGuelph/Analysis/maude_food-skills/data/processed/r_exports/cook_att.csv"
# df %>% write_csv(path)

## If self-sv ####
# df <- df %>%
#     unnest(data) %>%
#     select(response_id, matches(x))
#
# df <- df %>%
#   left_join
#     sv_ui %>%
#       filter(type_sv == "_on_self_sv") %>%
#       select(pid, fid, response_id, time_point) %>%  # de-identify
#       distinct(response_id, .keep_all = TRUE),
#     by = "response_id"
#   )

# ## Attaching to Anisha's data, and suffixing parents
# an <- read_csv(
#   glue(sub_p, "mahajan_cfdr_2021_12_20.csv"),
#   col_types = cols(.default = "c")
#   )
#
# df <- df %>% suffix_parent("education")
#
# try <- cleft_join(an, df, by = c("FID" = "fid", "time_point"), suffix = c(".1", ".2"))
#
# write_csv(try, glue("{sub_p}mahajan_cfdr_{Sys.Date()}.csv"))



## Process and Write #####

# df <- process_sv(sv, label_regex, vars)


