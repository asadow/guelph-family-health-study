
qi_path <- here("prose", "instructions_qualtrics-labelling")
old <- readxl::read_excel(here(qi_path, "qualtrics_labels_and_coding_ASaher_July_4_2022.xlsx"))
newl <- readxl::read_excel(here(qi_path, "qualtrics_labels_and_coding_AS_July_4_2022.xlsx"))

together <- old %>% full_join(newl, by = c("question_or_scale_intro", "coding"), suffix = c("_old", "_new"))
#together <- together %>% distinct(question_or_scale_intro, .keep_all = TRUE)
changed <- together %>% filter(suggested_label_old != suggested_label_new)
changed <- changed %>% select(starts_with("suggested_label"))

changed %>%
  filter(!suggested_label_old %in% suggested_label_new) %>%
  write_csv(here(qi_path, "changed_labels.csv"))
