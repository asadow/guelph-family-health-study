
## Read UI ####

## old code
# sv_ui <- vroom(
#   paste0(cle_p, "svl_matched.csv"),
#   col_types = c(.default = "c")
# )

## ignore
# svw <- svw %>%
#   filter(
#     time_point_sv %in% t
#     & phase_sv %in% p
#   )


## Read data ####
prepare_sv <- function(t, p, typ){
  # add option for parent per row format or child per row
  # the former will help investigate why some child columns dont pop up in regex pulls

  df <- suppressMessages(read_sv(t, p, nrows = -1))
  # name_sv <- memoise(name_sv)
  df <- name_sv(df)

  ## Add option for on_self; these can be pulled separately and matched later

  df <- if(typ == "_on_self_sv"){
    df %>%
      filter(type_sv == typ)
  } else{
    df %>%
      filter(!type_sv == "_on_self_sv") %>%
      pivot_sv
    }
}

sv <- prepare_sv(c("t1"), "3", "csv")

#
# sv$data[[1]] %>% names %>% str_subset("planning_a_health_meal_for_your_family")
#
# sv$data[[1]] %>% select(matches("planning_a_health_meal_for_your_family")) %>% unique
