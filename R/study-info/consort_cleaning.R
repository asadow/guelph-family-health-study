## Paths for Consort Working Documents
#wb_source <- paste0(fpath, "CONSORT Diagrams_Sept 2020", sl)
wb_source  <- "/Volumes/cbsshared/HHNS/David_Ma/study1/CONSORT Diagrams_Sept 2020/"
phases <- c("Full Study", "Pilot 2", "Pilot 1")
paths <- paste0(wb_source, phases, " Working Document.xlsx")

read_sheets <- function(x){
  wb <- loadWorkbook(x, key_get("MASTER_KEY"))
  readWorksheet(
    wb,
    sheet = getSheets(wb)
  )
}

map_sheets <- function(df){
  df %>%
    mutate(
      file = sub(glue(".*\\{sl}(.*)"), "\\1", path_name),
      sheets = map(path_name, ~ read_sheets(.x)),
      sheet_name = map(
        sheets,
        ~str_trim(names(.x))
      )
    ) %>%
    unnest(c(sheets, sheet_name)) %>%
    rename(data = sheets)
}

clean_listed_data <- function(df){
  df %>%
    filter(!sheet_name == "GFHS Master List") %>%
    mutate(
      data = map(
        data,
        ~ .x %>%
          mutate(
            across(everything(), ~ as.character(.)),
            row = row_number()
          ) %>%
          clean_names
      ),
      sheet_name = replace(sheet_name, sheet_name == "t6ASA", "t6 ASA")
    ) %>%
    separate(
      sheet_name,
      c("time_point", "source"),
      " "
    )
}

unnest_and_process <- function(df){
  df %>%
    unnest(data) %>%
    rename(pid = participant_id) %>%
    filter(!is.na(first_name) & !is.na(pid)) %>%
    mutate(
      fid = pid %>% str_sub(3),
      # Maddy used gender to indicate that pid did get their data collected
      data_collected = case_when(!mi(gender) ~ "yes", TRUE ~ "no"),
      # These ASA named sheets are where parents did ASA but kids did ESHA
      source = case_when(
        pid %>% str_starts("P|S", negate = TRUE)
        & source == "ASA"
        & fid < 401
        & time_point %in% c("t1", "t2")
        ~ "ESHA",
        TRUE ~ source
      ),
      source = glue("{source}_consort") %>% tolower
    ) %>%
    select(file, time_point, source,  data_collected, pid) %>%
    pivot_wider(names_from = source, values_from = data_collected)
}

# data.frame(path_name = paths) %>%
#   map_sheets %>%
#   clean_listed_data %>%
#   unnest_and_process %>%
#   vroom_write(glue("{ref_p}consort.csv"))
flist <- data.frame(path_name = paths)
df <- flist %>%
  map_sheets %>%
  clean_listed_data %>%
  filter(!is.na(source)) %>%
  unnest_and_process
#
# df %>% filter(pid == "P0300")

# df <- flist %>%
#   map_sheets %>%
#   clean_listed_data
#
# df %>%
#   filter(time_point == "t4" & str_detect(file, "Pilot 2") & source == "ASA") %>%
#   unnest(data) %>% filter(participant_id == "P0300") %>% View()


df %>% vroom_write(glue("{ref_p}consort.csv"))

## ARchive####
# sources <- c("asa", "sv", "ha", "ahha", "esha")
#
# pivot_source_wide <- function(df){
#   df %>%
#     group_by(source) %>%
#
#     pivot_wider(
#       names_from = source,
#       values_from = data_collected
#     ) %>%
#     rename_with(
#       ~ glue("{.}_consort"),
#       all_of(sources)
#     )
# }
#
# df <- df %>%
#   mutate(
#     across(
#       all_of(sources),
#       ~ if_else(is.na(.) | is_empty(.), "no", .)
#     )
#   ) %>%
#   rename_with(
#     ~ glue("{.}_consort"),
#     all_of(sources)
#   )
# #
# cs <- cs %>%
#   unnest(ends_with("consort")) %>%
#   select(!contains("name"))





