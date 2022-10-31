
#_______________________________________________________________________________
# Time points----

otp <- c("BL", "6M", "1Y", "2Y", "3Y", "4Y", "5Y")
ntp <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7")

names(ntp) <- otp

#_______________________________________________________________________________
# Book-keeping----

other_ahha <- c("cleaned_ahha",
                "SID_ahha",
                "phase_ahha",
                "date_ahha",
                "time_point_ahha")

#_______________________________________________________________________________
# ASA----

nasa <- c("user_name", "date_asa", "amount_usual",
          "i_note_asa", "IntakeStartDateTime", "IntakeEndDateTime",
          "ReportingDate", "Lang", "NumFoods", "NumCodes", "AMTUsual",
          "SaltType", "SaltFreq", "SaltUsed", "KCAL", "PROT", "TFAT", "CARB",
          "MOIS", "ALC", "CAFF", "THEO", "SUGR", "FIBE", "CALC", "IRON",
          "MAGN", "PHOS", "POTA", "SODI", "ZINC", "COPP", "SELE", "VC",
          "VB1", "VB2", "NIAC", "VB6", "FOLA", "FA", "FF", "FDFE", "VB12",
          "VARA", "RET", "BCAR", "ACAR", "CRYP", "LYCO", "LZ", "ATOC",
          "VK", "CHOLE", "SFAT", "S040", "S060", "S080", "S100", "S120",
          "S140", "S160", "S180", "MFAT", "M161", "M181", "M201", "M221",
          "PFAT", "P182", "P183", "P184", "P204", "P205", "P225", "P226",
          "VITD", "CHOLN", "VITE_ADD", "B12_ADD", "DataComp",
          "user_name", "F_TOTAL", "F_CITMLB", "F_OTHER", "F_JUICE", "V_TOTAL",
          "V_DRKGR", "V_REDOR_TOTAL", "V_REDOR_TOMATO", "V_REDOR_OTHER",
          "V_STARCHY_TOTAL", "V_STARCHY_POTATO", "V_STARCHY_OTHER", "V_OTHER",
          "V_LEGUMES", "G_TOTAL", "G_WHOLE", "G_REFINED", "PF_TOTAL", "PF_MPS_TOTAL",
          "PF_MEAT", "PF_CUREDMEAT", "PF_ORGAN", "PF_POULT", "PF_SEAFD_HI",
          "PF_SEAFD_LOW", "PF_EGGS", "PF_SOY", "PF_NUTSDS", "PF_LEGUMES",
          "D_TOTAL", "D_MILK", "D_YOGURT", "D_CHEESE", "OILS", "SOLID_FATS",
          "ADD_SUGARS", "A_DRINKS", "RecallRecId", "AmtUsual", "CFG_MILK_ALT",
          "CFG_VEG_FRUIT", "CFG_GRAIN", "CFG_MEAT_ALT")

asa_needed <- nasa[
  which(nasa=="IntakeStartDateTime")
  : which(nasa=="DataComp")
  ]

asa_nutrients <- nasa %>%
  str_subset("[[:lower:]]|^CFG", negate = TRUE)

sum_in_asa <- c("F_TOTAL",
                "G_WHOLE",
                "D_TOTAL")
intakes <- c("SODI",
             "G_REFINED",
             "SFAT",
             "ADD_SUGARS")

asa_numeric <- c(sum_in_asa,
                 "KCAL",
                 intakes,
                 "F_CITMLB",
                 "F_OTHER",
                 "MFAT",
                 "PFAT",
                 "V_TOTAL",
                 "V_LEGUMES",
                 "V_DRKGR",
                 "PF_MPS_TOTAL",
                 "PF_EGGS",
                 "PF_NUTSDS",
                 "PF_SOY",
                 "PF_LEGUMES",
                 "PF_SEAFD_HI",
                 "PF_SEAFD_LOW")

# #_______________________________________________________________________________
# # AHHA ----
#
# measures_ahha <- c("ht_cm_ahha",
#                    "wc_cm_ahha",
#                    "bm_kg_ahha")

#_______________________________________________________________________________
# HA ----

bia_calc <- c("fat_percent_bia",
              "fat_free_mass_kg_bia",
              "fat_mass_kg_bia",
              "TBW",
              "bia_res_ohm")

measures_ha <- c("fat_percent_bia",
                 "fat_percent_bodpod",
                 "bmi_z",
                 "bmi",
                 "wc_cm",
                 "bm_kg",
                 "ht_cm",
                 "bia_res_ohm",
                 "bp_dia_mmhg",
                 "bp_sys_mmhg")

pastey <- function(x, y) paste0(x, c(1:y))


bpn <- unlist(
  map(
    c("BP Sys (mmHg) - ", "BP Dia (mmHg) - ", "HR (bpm) - "),
    ~ pastey(.x, 5)
  )
)

wc <- pastey("WC (cm) - ", 4)
ht <- pastey("Height (cm) - ", 4)
bia <- pastey("BIA_Res - ", 3)
raw_measures_ha_snake_case <- map(list(wc, ht, bia, bpn), to_snake_case) %>%
  unlist %>%
  str_replace("_res_", "_res_ohm_") %>%
  str_replace("height", "ht")



raw_ha <- c(bpn,
            bia,
            wc,
            ht,
            bia_calc)

#_______________________________________________________________________________
# IDs and Demographics----
#
# idutp <- c("FID",
#            "PID",
#            "time_point",
#            "parent_sv",
#            "phase_sv",
#            "phase",
#            "user_name_asa",
#            "version_asa")
#
# demo_parent <- c("gender_sv_selected",
#                  "gender_sv_other",
#                  "education",
#                  "personal_income",
#                  "marital_status")
#
# paste_source <- function(x) paste0(x, c("sv", "asa", "ha", "ll"))
#
# demo_any <- c("gender_ha",
#               "dob_ha",
#               paste_source("date"),
#               paste_source("age"),
#               "ethnicity")
#
# fam_size <- c("lives_with_partner",
#               "number_children",
#               "number_children_18mo_to_5yr")
#

#_______________________________________________________________________________
# Flags -----
#
# flags_asa <- c(
#   "amount_usual",
#   "i_outlier_asa",
#   "i_outlier_asa_note",
#   "i_breastfed_asa",
#   "i_breastfed_note"
# )

# flags_generic_child <- c("i_health_problem_child",
#                          "i_health_problem_specified_or_concern_child")
#
# flags_generic_parent <- c("i_illness_selected",
#                           "i_illness_cancer",
#                           "i_illness_other",
#
#                           "i_medical_condition_parent",
#                           "i_mental_health_history_parent",
#                           "i_prescription_medication_parent")
# #
# flags_ahha <- c("i_pregnant_ahha",
#                 "i_breastfeeding_ahha",
#                 "i_comments_ahha")

flags_bod_pod <- c(
  "i_eat_drink_past_2_hrs",
  "i_exercise_past_2_hrs",
  "i_eat_drink_past_30_mins",
  "i_exercise_past_30_mins",
  "i_note_bodpod",
  "i_faulty_bodpod",
  "i_implausibly_low_fat_percent"
)

flags_parent_ha <- c("i_pregnant_ha",
                     "i_pregnancy_due_date_ha",
                     "i_breastfeeding_ha")

flags_prefix <- c(
  "i_note_",
  "i_broke_sop_",
  "i_measures_"
)

flag <- function(x){paste0(flags_prefix, x)}

flags_wc <- flag("wc")
flags_bia <- flag("bia")
flags_ht <- flag("ht")
flags_bp <- flag("bp")


flags_ha <-  c("cleaned_ha",
               flags_bod_pod,
               flags_wc,
               flags_ht,
               flags_bp,
               flags_bia,
               flags_parent_ha

               # "i_extreme_used_bmi_zscore",
               # "i_extreme_used_fat_percent_bia",
               # "i_extreme_used_bmi",
               #
               # "i_extreme_bmi_zscore",
               # "i_extreme_fat_percent_bodpod",
               # "i_extreme_bmi",
               # "i_extreme_wc_mean_cm",
               # "i_extreme_body_mass_kg",
               # "i_extreme_height_mean_cm",
               # "i_extreme_bia_res_mean_ohm",
               # "i_extreme_bp_dia_mean_mmhg",
               # "i_extreme_bp_sys_mean_mmhg"
)


# LL ####

nll <- c("PAK", "GLUCFAST", "GLUCFAST_RANGE",
         "GLUCFAST_FLAG", "GLUCFAST_COMMENT", "TLO", "GLUCRAND", "GLUCRAND_RANGE",
         "GLUCRAND_FLAG", "GLUCRAND_COMMENT", "LIPHR", "TRIG", "TRIG_RANGE",
         "TRIG_FLAG", "TRIG_COMMENT", "CHOL", "CHOL_RANGE", "CHOL_FLAG",
         "CHOL_COMMENT", "HDL", "HDL_RANGE", "HDL_FLAG", "HDL_COMMENT",
         "CHOLHDL", "CHOLHDL_RANGE", "CHOLHDL_FLAG", "CHOLHDL_COMMENT",
         "LDL", "LDL_RANGE", "LDL_FLAG", "LDL_COMMENT", "NONHDL", "NONHDL_RANGE",
         "NONHDL_FLAG", "NONHDL_COMMENT", "LIPID", "LIPID_RANGE", "LIPID_FLAG",
         "LIPID_COMMENT", "CORTAM", "CORTAM_RANGE", "CORTAM_FLAG", "CORTAM_COMMENT",
         "CORTAM_DATE", "CORTAM_TIME", "CORTRAND", "CORTRAND_RANGE", "CORTRAND_FLAG",
         "CORTRAND_COMMENT", "CORTRAND_DATE", "CORTRAND_TIME", "HSCRP",
         "HSCRP_RANGE", "HSCRP_FLAG", "HSCRP_COMMENT", "INSULINFAST",
         "INSULINFAST_RANGE", "INSULINFAST_FLAG", "INSULINFAST_COMMENT",
         "INSULINRAND", "INSULINRAND_RANGE", "INSULINRAND_FLAG", "INSULINRAND_COMMENT",
         "HBA1C", "HBA1C_RANGE", "HBA1C_FLAG", "HBA1C_COMMENT")
