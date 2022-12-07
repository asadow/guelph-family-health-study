source(here("R", "functions", "qualtrics-functions.R"))
# library(qualtRics)
# keyring::key_get("QUALTRICS_BASE_URL")
# qualtrics_api_credentials(base_url = key_get("QUALTRICS_BASE_URL"),
#                           api_key = key_get("QUALTRICS_API_KEY"))
# fetch_survey("SV_ezFnmDF8xXheiR7")


# Merging data labels and dictionary --------------------------------------


## Data labels ------------------------------------------------------
#
# phase <- c("1", "2", "3")
#
# surveys <- all_surveys() %>%
#   select(name, id) %>%
#   filter(name %>% str_detect("gfhs"))
#
# surveys %>% write_rds(here("data", "processed", "surveys", "survey-labels.rds"))

#
# surveys <- read_rds(here("data", "processed", "surveys", "survey-labels.rds"))
# nested_surveys <- surveys %>% import_headers

nested_surveys <- read_rds(here("data", "processed", "surveys", "survey-labels.rds"))

nested_labels <- nested_surveys %>%
  rename_relabel_each_survey %>%
  mutate(label = map(data, ~ .x %>% names %>% prefix_child_numbers %>% make_clean_names)         ) %>%
  select(- data, - data_names)

labels <- nested_labels %>%
  select(name, label) %>%
  unnest(label)

### Unnest and crush -------------------------------------------------

labels <- nested_labels %>%
  select(name, label) %>%
  unnest(label) %>%
  arrange(name) %>%
  group_by(label) %>%
  separate_name

times_and_labels <- labels %>%
  select(phase, time_point, parent, label) %>%
  crush_same_rows_for_different(time_point) %>%
  crush_same_rows_for_different(parent)

times_and_labels <- times_and_labels %>%
  mutate(label = label %>% str_replace("(child_)(\\d_)(.*)", "\\1\\3")) %>%
  distinct


## la: label availability
## Create availability variable
la <- times_and_labels %>%
  mutate(
    phase = str_c("phase: ", phase),
    time_point = str_c("time_point: ", time_point),
    parent = str_c("parent: ", parent),
    data_label = label
  ) %>%
  unite(availability, phase, time_point, parent, sep = "; ") %>%
  ungroup() %>%
  select(- label)

## Remove extra rows
## Squish availability for labels
la <- la %>%
  distinct %>%
  group_by(data_label) %>%
  mutate(availability = paste(availability[!is.na(availability)],
                              collapse = " | ")) %>%
  distinct


## Dictionary  -------------------------------------------------

### Prefix child label  -------------------------------------------------
## Need to find which labels are for both parent and child
## One option:
## Find duplicates after removing child_ prefix
## Keep original
## Manually determine or use the category_or_block column in labels sheet
## to find which
## OR **
## Find all labels in sheet that have [Child Name] and prefix label with child_,
## then match to labels sheet
## Proceed with second option

dict <- read_and_glue_labels() %>% fill(everything())

## Add child prefix
dict <- dict %>%
  mutate(
    on_child = case_when(
      str_detect(question_or_scale_intro, "Child's Name") ~ TRUE,
      TRUE ~ na_lgl
    )
  ) %>%
  group_by(category_or_block) %>%
  fill(on_child, .direction = "downup") %>%
  mutate(
    data_label = case_when(
      on_child ~ glue("child_{label}"),
      TRUE ~ label)
  )


## Match: Ideally do fuzzy match matching start of label ----------------------------------

dict %>%
  left_join(la, by = "data_label") %>%
  write_csv("/Users/Adam/Library/CloudStorage/OneDrive-UniversityofGuelph/gfhs/GFHS Team OneDrive/Requesting Data/Data Analysis Plans/Data Dictionary/data-dictionary_times-available.csv")
















# Old ---------------------------------------------------------------------

## Future work -------------------------------------------------------------

## We have duplicate label times from on-child and on-self surveys
# vars_specific, scales
## Jess: Can add counts
## But first prioritize making derived variables
## Like ethnicity, screen_time


la %>%
  filter(data_label %>% str_detect(vars_specific)) %>%
  View





















# # Reactable ------------------------------------------------------------------
#
# reactable(
#   data_dictionary,
#   groupBy = c("category_or_block"),
#   onClick = "expand",
#   highlight = TRUE,
#   striped = TRUE,
#   compact = TRUE,
#   wrap = FALSE,
#   height = 500,
#   bordered = TRUE,
#   searchable = TRUE,
#
#   columns = list(
#     question_or_scale_intro = colDef(
#       aggregate = "count",
#       format = list(
#         aggregated = colFormat(suffix = " Questions")
#       )
#     ),
#
#     item_label = colDef(name = "label"),
#     item_and_number = colDef(name = "item"),
#     equivalent_item_label = colDef(name = "equivalent_item"),
#     extra_label = colDef(name = "label_prefix")
#   ),
#
#   columnGroups = list(
#     colGroup(name = "Scale", columns = c("item_label",
#                                          "item_and_number",
#                                          "equivalent_item_label",
#                                          "extra_label")
#     )
#   ),
#
#   details = function(index) {
#     times <- times_and_labels[times_and_labels$label == data_dictionary$label[index], ]
#     htmltools::div(style = "padding: 1rem",
#                    reactable(times,
#                              outlined = TRUE,
#                              fullWidth = FALSE))
#     }
#   )
#
#
#
#
#
#
#
#
#
#
#
# ## Subset surveys that are on-child so you can remove this info
# ## from the data dictionary
#
# on_child_surveys <- surveys %>%
#   separate_name %>%
#   filter(on_who == "on-child") %>%
#   select(phase, time_point, parent)
