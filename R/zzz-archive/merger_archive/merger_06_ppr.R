

## --Screen Time----------------------------------------------------------------------

### screen time quantified
# 
# st.quantify <- function(x) {as.numeric(plyr::revalue(x, 
#                                                      c("None" = 0, 
#                                                        "Less than one hour per day" = 0.5,
#                                                        "1-2 hours per day" = 1.5,
#                                                        "2-3 hours per day" = 2.5,
#                                                        "4-6 hours per day" = 5,
#                                                        "7 or more hours per day" = 7))
# )
# }
# 
# df$avg.week.day.screen.hours.quant <- st.quantify(df$avg.week.day.screen.time)
# 
# df$avg.weekend.day.screen.hours.quant <- st.quantify(df$avg.weekend.day.screen.time)
# 
# 
# df$avg.day.screen.hours <- 1/7*(5*df$avg.week.day.screen.hours.quant + 2*df$avg.weekend.day.screen.hours.quant)



## --One PID per time.point or row----------------------------------------------------------------------

ppr <- df %>% 
  distinct(PID, 
           time.point, 
           .keep_all = T) %>% 
  arrange(phase, 
          time.point, 
          FID, 
          FID.order)

##--Age group--------------------------------------------------------------------------

ppr <- ppr %>% 
  mutate(
    ag = case_when(
      age.ha > 1.5 & age.ha < 5 ~ "preschooler",
      age.ha <= 1.5 ~ "toddler",
      age.ha > 5 & age.ha < 13 ~ "preteen", 
      age.ha > 13 & age.ha < 20 ~ "teen",
      TRUE ~ as.character(NA)
    )
  )

