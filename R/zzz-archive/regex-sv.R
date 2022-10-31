# Parents ####
## Single questions ####

gls <- list(
  ### Survey Info ####
  c("^response_id$",
    "id_sv"),
  c("recipient_first_name$",
    "first_name"),
  c("recipient_last_name$",
    "last_name"),
  c("recipient_email$",
    "email"),
  c("start_date$",
    "start_date"),
  c("recorded_date$", 
    "date_sv"),
  
  ### Demographics ####
  c("marital_status", 
    "marital_status"),
  c("your_ethnicity.*selected", 
    "ethnicity_selected"),
  c("your_ethnicity.*other", 
    "ethnicity_other"),
  c("annual_income", 
    "household_income"),
  c("personal_income", 
    "personal_income"),
  c("degree", 
    "education"),
  c("which_gender.*select|please_indicate_your_gender|gender.*selected", 
    "gender_sv_selected"),
  c("which_gender.*other|gender.*other", 
    "gender_sv_other"),
  
  ### Parent Health ####
  c("illnesses.*_selected", 
    "flag_illness_selected"),
  c("illnesses.*_cancer", 
    "flag_illness_cancer"),
  c("illnesses.*apply_other",
    "flag_illness_other"),
  c("health_problem_or_condition", 
    "flag_medical_condition_parent"),
  c("mental_health_condition", 
    "flag_mental_health_history_parent"),
  c("any_prescription_medication.*currently_use", 
    "flag_prescription_medication_parent"),
  c("prescription_medication.*currently_use.*selected", 
    "flag_prescription_medication_parent_selected"),
  c("your_health_in_general", 
    "general_health"),
  
  ### Parent Stress ####
  c("no_stress|parent_stress", 
    "general_life_stress"),
  c("stress_at_home", 
    "general_home_stress"),
  
  ### House Members ####
  c("live_with_a_partner_or_spouse", 
    "lives_with_partner"),
  c("how_many_adults", 
    "household_number_adults"),
  
  ### Number of Children ####
  c("children_you_have_in_total", 
    "number_children"),
  c("children_you_have_between_the_ages.*old$", 
    "number_children_18mo_to_5yr"),
  
  ### Other ####
  c("house_sit_and_eat", 
    "family_meal_frequency"),
  c("playground", 
    "playground_near"),
  
  ### Pet Ownership ####
  c("own.*dog",
    "pet_ownership_dog"),
  c("how_many_dogs",
    "pet_ownership_number_of_dogs"),
  c("own.*cat",
    "pet_ownership_cat"),
  
  ### Parent Media Use ####
  c("hours_do_you_spend_on_screens.*weekday",
    "avg_week_day_screen_time"),
  c("hours do you spend on screens.*weekend",
    "avg_weekend_day_screen_time"),
  
  ### Parent Physical Activity ####
  ## Days Per Last 7 (at least 10 min activity)
  c("think_about_all_the_vigorous",
    "PA_days_per_last_7_vigorous"),
  c("think_about_all_the_moderate",
    "PA_days_per_last_7_moderate"),
  c("think_about_the_time_you_spent_walking",
    "PA_days_per_last_7_walking"),
  ### note: sitting is in Pairs
  
  # For t4 phase 3
  ## Hours
  c("sitting.*weekday.*hours$",
    "PA_time_spent_avg_week_day_sitting_hours"),
  c("sitting.*weekend.*hours$",
    "PA_time_spent_avg_weekend_day_sitting_hours"),
  ## Minutes
  c("sitting.*weekday.*minutes$",
    "PA_time_spent_avg_week_day_sitting_minutes"),
  c("sitting.*weekend.*minutes$",
    "PA_time_spent_avg_weekend_day_sitting_minutes"),
  
  ### Food skills  ####
  c("proportion_of_the_grocery_shopping",
    "proportion_grocery_shopping"),
  c("proportion_of_the_cooking",
    "proportion_cooking"),
  c("i_plan_meals_for_the_week_before_going_food_shopping",
    "before_shopping_plan_meals"),
  c("i_write_a_list_of_items_that_i_need_before_going_food_shopping",
    "before_shopping_write_list")
  
)

p_single_keyword <- map_chr(gls, 1)
p_title <- map_chr(gls, 2)


## Pairs (numbers and units)  ####

contains_both_units <- c("^(?!.*(hours|minutes)).*")

gls <- list( 
  ### PA: Time Spent on One of Those Days ####
  c("you_usually_spend_doing_vigorous",
    "PA_time_spent_avg_day_vigorous"),
  c("you_usually_spend_doing_moderate",
    "PA_time_spent_avg_day_moderate"),
  
  c("you_spend_walking",
    "PA_time_spent_avg_day_walking"),
  
  c("you_spend_sitting.*week_day",
    "PA_time_spent_avg_week_day_sitting"),
  c("you_spend_sitting.*weekend_day",
    "PA_time_spent_avg_weekend_day_sitting")
  
)

p_unit_keyword <- map_chr(gls, 1)
p_unit_keyword <- paste0(
  contains_both_units, p_unit_keyword
)

p_title_quantity <- map_chr(gls, 2)

## Scales  ####

gls <- list( 
  ### Prescription Medication ####
  c("prescription_medication.*currently use.*medication", 
    "flag_prescription_medication_parent_medication -"),
  
  # # Parenting Distress ####
  # c("^(?!.*\\d).*please_indicate_how_much.*statements",
  # "Parenting Distress: "),
  
  ### Parent Depression ####
  c("felt_or_behaved|parental_depression",
    "Depressive Symptoms: "),
  
  ### Household Chaos ####
  c("^(?!.*\\d).*please_indicate_how_much.*following_options",
    "Household Chaos: "),
  
  ### Family Functioning ####
  c("family_s_general_functioning",
    "Family Functioning: "),
  
  ### Food Skills ####
  c("how_confident_do_you_feel_in_being_able_to_do_the_following",
    "Food Skills - Conceptualizing: "),
  
  c("how_confident_are_you_in_using_the_following_cooking_techniques",
    "Food Skills - Mechanical: "),
  
  c("do_the_children_in_your_household",
    "Food Skills - Child Involvement: "),
  
  c("when_we_have.*family_dinner",
    "Healthfulness of Dinner: "),
  
  ### Household Food Security ####
  c("food_eaten_in_your_household_in_the_last_30_days",
    "Household Food Security: ")
)

p_scale_keyword <- map_chr(gls, 1)

p_scale_heading <- map_chr(gls, 2)
	
p_keywords <- c(p_single_keyword, p_unit_keyword, p_scale_keyword)
p_headers <- c(p_title, p_title_quantity, p_scale_heading)

  # Children ####
  ## Single questions ####
  
  
gls <- list(
  ### Demographics  ####
  c("_s_ethnicity.*selected",
    "ethnicity_selected"),
  c("_s_ethnicity.*other",
    "ethnicity_other"),
  c("specific_health_problems",
    "flag_health_problem_child"),
  c("concerns_you_have",
    "flag_health_problem_specified_or_concern_child"),
  
  ### Parenting Physical Activity ####
  c("to_do_activities_other_than_screen_time",
    "parent_encourages_act_not_screen"),
  c("role_modelling",
    "parent_active_role_model"),
  c("to_places_where",
    "parent_takes_to_places_act"),
  
  ### Media Use ####
  c("hours_does.*spend_on_screens.*weekday",
    "avg_week_day_screen_time"),
  c("hours_does.*spend_on_screens.*end",
    "avg_weekend_day_screen_time"),
  
  ### Beverages ####
  c("often_does.*drink.*flavoured",
    "bev_freq_flav_milk"),
  c("often_does.*drink.*100_percent_juice",
    "bev_freq_100_juice"),
  c("often_does.*drink.*fruit_drinks",
    "bev_freq_fruit_drinks"),
  c("often_does.*drink.*soda_not_sugar_free",
    "bev_freq_soda"),
  c("often_does.*drink.*sugar_free_soda",
    "bev_freq_sug_free_soda"),
  c("often_does.*drink.*energy_drinks",
    "bev_freq_energy_drinks"),
  c("often_does.*drink.*sports_drinks",
    "bev_freq_sports_drinks"),
  
  ### Sleep Times ####
  c("does.*bed.*weekday",
    "bed_time_week_day"),
  c("does.*bed.*end",
    "bed_time_weekend_day"),
  c("does.*wake.*weekday",
    "wake_time_week_day"),
  c("does.*wake.*end",
    "wake_time_weekend_day"),
  
  ### Sleep Hours ####
  c("weekday.*sleep_in_a_usual_24",
    "sleep_time_week_day"),
  c("weekend_day.*sleep_in_a_usual_24",
    "sleep_time_weekend_day"),
  
  ### Day Care ####
  c("currently.*day_care",
    "attending_care"),
  c("hours.*average_week.*day_care",
    "avg_week_care_hours"),
  c("type_of_care",
    "type_care"),
  c("provides_food.*day_care",
    "food_provider_during_care")
)


c_single_keyword <- map_chr(gls, 1)
c_title <- map_chr(gls, 2)


## Pairs (numbers and units) ####

gls <- list(
  ### Child Physical Activity ####
  c("active_play.*(monday|weekdays)",
    "PA_play_time_avg_week_day"),
  c("active_play.*(saturday|weekend days)",
    "PA_play_time_avg_weekend_day"),
  
  c("play_outside.*weekday",
    "PA_outside_time_avg_week_day"),
  c("play_outside.*weekend",
    "PA_outside_time_avg_weekend_day")
)


c_unit_keyword <- map_chr(gls, 1)
c_unit_keyword <- paste0(
  contains_both_units, 
  c_unit_keyword
)

c_title_quantity <- map_chr(gls, 2)


### Scales ####

### Nutri step parts ####
ns1 <- "usually(eats|drinks|takes_supplements|watches_tv)|"
ns2 <- "food_is_expensive|problems_chewin|feeds_his|from_a_baby_bottle"
ns3 <- "|hungry_at_mealtimes|how_much_to_eat|meals_while_watching_tv|"
ns4 <- "comfortable_with_how"

# DO NOT USE BRACKETS IN NAME

gls <- list(
  ### Sleep Parenting ####
  c("sleep_parenting|about.*_s_sleep",
    "sleep_parenting_"),
  
  ### Media Parenting ####
  c("family_s_behaviour_around_screen_time",
    "media_parenting_"),
  
  ### PA Parenting ####
  c("physical_activity_parenting",
    "physical_activity_parenting_"),
  
  ### Family Feeding Practices and Parental Modeling ####
  c("family_feeding_practices_and_parental_modelling",
    "family_feeding_practices_and_parental_modelling_"),
  
  ### Relation ####
  c("how_are_you_related",
    "relation_to_child_"),
  
  ### NutriStep ####
  c(paste0(ns1, ns2, ns3, ns4),
    "nutristep_"),
  
  ### Child Mental Health: Strengths and Difficulties ####
  c("strengths_and_difficulties_5_17y_",
    "sdq_5_17y_"),
  c("strengths_and_difficulties_2_4y_",
    "sdq_2_4y_")
)

c_scale_keyword <- map_chr(gls, 1)

c_scale_heading <- map_chr(gls, 2)