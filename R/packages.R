## Packages
## does not include tidyverse
## since it has readxl
## which conflicts with XLConnect
## relating to java
pkgs <- c(
  "here",
  "snakecase",
  "XLConnect",
  "usethis",
  "assertthat",
  "readr",
  "tidyr",
  "vroom",
  "stringi",
  "stringr",
  "anytime",
  "purrr",
  "tibble",
  "dplyr",
  "glue",
  "memoise",
  "pryr",
  "janitor",
  "rlang",
  "lubridate",
  "zscorer",
  "pander",
  "formattable",
  "DT",
  "data.table",
  "qualtRics", ## May need to specify lib in install.packages()
  "sjlabelled",
  "fs", ## May need to specify lib in install.packages()
  "magrittr",
  "keyring",
  "dplyover"
)

#inst = lapply(pkgs, library, character.only = TRUE) # load them

for (i in pkgs){
  if(! i %in% installed.packages()){
    install.packages(i, dependencies = TRUE)
  }
  require(i, character.only = TRUE)
}

## Reinstall readxl if getting error for XLConnect
# install.packages("readxl")
