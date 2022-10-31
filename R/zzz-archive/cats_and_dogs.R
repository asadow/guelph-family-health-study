
## -----this was before pivot_longer:
# cats_and_dogs <- c(
#   union(
#     nsv[svgrep("dog| cat", F)], 
#     nsv[svgrep("dog| cat", F) + 1]
#     )
# )

#new_cds <- paste0("cds_", cats_and_dogs)

# sv2 <- sv %>% 
#   rename_with(
#     move_number, 
#     matches("QID|OR - \\d{1}|\\[Field-2\\] child")
#   ) %>% 
#   rename_with(
#     ~new_cds[which(cats_and_dogs == .x)], 
#     .cols = cats_and_dogs
#     ) %>% 
#   rename_with(
#     replace_number, 
#     matches("^\\d{1} - ")
#   )

##----- after pivot:
# grep(
#   " cat", 
#   names(sv2), 
#   value = T, 
#   perl = T
#   )
# 
# h <- grep(
#   "\\d{1} - ", 
#   names(sv2), 
#   value = T, 
#   perl = T
#   )
#h[1:100]
#h[grepl("Field",  h)]
#h[300:720]

# 
# h[!grepl("QID",  h)]
# names(sv)[grep("dog", names(sv))+1]
# 
# h <- grep("Kilograms", names(sv),  perl = T)
# names(sv)[c(h-1, h, h+1, h+2,h+3, h+4,h+5,h+6)]
# 
# h[30:80]