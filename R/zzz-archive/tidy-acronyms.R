name_acronym <- list(
  c("family_feeding_practices_and_parental_modelling",
    "ffppm"),
  c("child_eating_behaviour", "ceb"),
  c("eating_behaviour", "eb"),
  c("coparenting_relationship", "cr"),
  c("food_parenting", "fp")
)

for(i in seq_along(name_acronym)){
  pattern <- glue("^{name_acronym[[i]][1]}")
  replacement <- name_acronym[[i]][2]
  df <- df %>% replace_names(pattern, replacement)
}
