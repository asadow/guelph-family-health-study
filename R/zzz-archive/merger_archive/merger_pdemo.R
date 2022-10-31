# 
# df <- df %>%
#   left_join(
#     ethn,
#     by = c("PID", "time_point"),
#     suffix =c("", "_pilot_demo")
#   )
# 
# # May want to adjust this code later if not using _pilot_demo
# 
# df <- df %>%
#   mutate(
#     across(
#       all_of(
#         c("household_income", "education")
#         ),
#       ~ na_if(.x, "")
#       ),
#     household_income = coalesce(household_income, 
#                                 household_income_pilot_demo),
#     education = coalesce(education, 
#                          education_pilot_demo),
#     parent_sv = coalesce(parent_sv, 
#                          parent_sv_pilot_demo)
#     )

