function(loop, response, varname, indicator) {
  r <- loop[, c("X_uuid", varname)]
  r <- r[complete.cases(r),]
  r = aggregate(r[, c(2)],
                by = list(r$X_uuid),
                FUN = sum,
                na.rm = T)
  names(r) <- c("X_uuid", indicator)
  response <- merge(response, r, by = "X_uuid", all = T)
  return(response)
}

msni_recoding <- function(df, loop) {
  df <- response
  df$stress <-
    ifelse(
      df$selling_assets %in% c("no_already_did", "yes") |
        df$borrow_debt  %in% c("no_already_did", "yes") |
        df$reduce_spending %in% c("no_already_did", "yes") |
        df$spending_savings %in% c("no_already_did", "yes")   ,
      1,
      0
    )
  df$crisis <-
    ifelse(
      df$selling_transportation_means %in% c("no_already_did", "yes") |
        df$change_place  %in% c("no_already_did", "yes") |
        df$child_work %in% c("no_already_did", "yes"),
      1,
      0
    )
  df$emergency <-
    ifelse(
      df$child_dropout_school %in% c("no_already_did", "yes") |
        df$adult_risky  %in% c("no_already_did", "yes") |
        df$family_migrating %in% c("no_already_did", "yes") |
        df$child_forced_marriage %in% c("no_already_did", "yes"),
      1,
      0
    )
  
  loop$single_hhh <-
    case_when(
      loop$marital_status %in% c("single", "separated", "widowed", "divorced") ~ 1,
      loop$marital_status == "married" ~ 0,
      TRUE ~ NA_real_
    )
  ###YS this does not look at if there is a single female headed household, this looks if the single person is a female.
  #loop %>% group_by(X_uuid) %>% summarise(sum(single_female_hhh, na.rm = T))
  loop$single_female_hhh <-
    case_when(
      loop$single_hhh == 1 & loop$sex == "female" ~ 1,
      is.na(loop$single_hhh) &
        is.na(loop$sex) ~ NA_real_,
      TRUE ~ 0
    )
  
  ###################################a #########################################
  df$a1 <-
    ifelse(
      df$reasons_for_debt %in%  c("basic_hh_expenditure",
                                  "health",
                                  "food",
                                  "education"),
      1,
      0
    )
  
  df$a2 <- df$primary_livelihood.ngo_charity_assistance
  df$a3 <- ifelse(df$crisis == 1 | df$emergency == 1, 1, 0)
  
  df <- df %>%
    mutate(coping_mechanism = sum_row(a1 + a2 + a3))
  ###################################b #########################################
  
  df$b1 <-
    ifelse(
      df$difficulty_communicating %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
        df$difficulty_hearing %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
        df$difficulty_remembering %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
        df$difficulty_seeing %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
        df$difficulty_walking %in% c("a_lot_of_difficulty", "cannot_do_at_all"),
      1,
      0
    )
  
  ###YS this is based on the previous indicators that does not compute single female hhh. 
  # this takes the sum of single women.
  #it is also not a binary/logical as values goes from 1 to 9. 
  df <- individual_to_HH_numeric(loop, df, "single_female_hhh", "b2")

  ###YS this calculate the number of person that reply yes to health_issue.chronic. 
  #it is also not a binary/logical as values goes from 1 to 9. 
  df <- individual_to_HH_numeric(loop, df, "health_issue.chronic", "b3")
  
  
  df$b4 <- ifelse(
    df$pds_card == "no" |
      df$id_card_a18 == "no" |
      df$nationality_cert_a18 == "no" |
      df$id_card_u18 == "no" |
      df$nationality_cert_u18 == "no" |
      df$birth_cert_u18 == "no",
    1,
    0
  )
  
  df$b5 <- ifelse(
    df$why_not_return.fear_trauma == 1 |
      df$why_not_return.lack_of_security_forces == 1 |
      df$why_not_return.presence_of_mines == 1 |
      df$why_not_return.discrimination == 1 |
      df$why_not_return.lack_security_women == 1 ,
    1,
    0
  )
  
  df$b6 <- ifelse(df$access_soap == "no", 1, 0)
  ###YS female_60_calc and male_60_calc is the number of ppl age 60+, not a binary that says yes and no if there 
  #are 60+ people in the hh.
  df$b7 <- ifelse(df$female_60_calc == 1 | df$male_60_calc == 1, 1, 0)
  
  #using sum_row function to sum the rows, sum_row belongs to expss library.
  
  ###YS the sum_row are adding none binaries so you need first to change all the variables to binaries
  df$vulnerability_score <- case_when(
    df$b1 == 1 ~ 4,
    df$b2 == 1 |
      sum_row(df$b3, df$b4, df$b5, df$b6, df$b7) > 3 ~ 3,
    #You are omitting if the HH scores 3
    sum_row(df$b3, df$b4, df$b5, df$b6, df$b7) >= 2 ~ 2,
    sum_row(df$b3, df$b4, df$b5, df$b6, df$b7) == 1 ~ 1,
    ###YS cannot have 0 for LSG score.
    TRUE ~ 0
  )
  df$vulnerability_1 <- ifelse(df$vulnerability_score == 1, 1, 0)
  df$vulnerability_2 <- ifelse(df$vulnerability_score == 2, 1, 0)
  df$vulnerability_3 <- ifelse(df$vulnerability_score == 3, 1, 0)
  df$vulnerability_4 <- ifelse(df$vulnerability_score == 4, 1, 0)
  ###################################c #########################################
  
  df$c1 <- case_when(
    df$reasons_not_attend.cannot_afford == 1 |
      df$reasons_not_attend.children_working == 1 ~ 1,
    is.na(df$reasons_not_attend.cannot_afford) &
      is.na(df$reasons_not_attend.children_working) ~ NA_real_,
    TRUE ~ 0
  )
  
  loop$children_not_attend_school <-
    case_when(
      loop$age < 18 &
        loop$attend_formal_ed == "no" &
        loop$attend_informal_ed == "no" ~ 1,
      loop$age >= 18 ~ NA_real_,
      TRUE ~ 0
    )
  
  ###YS this also a sum of children, not a binary see below
  df <-
    individual_to_HH_numeric(loop, df, "children_not_attend_school", "c2")
  
  temp <- loop %>%
    filter(age < 18)
  temp <- temp %>%
    group_by(X_uuid) %>%
    summarize(
      n_no_school = sum(children_not_attend_school, na.rm = TRUE),
      n_child = n()
    )
  temp <- temp %>%
    mutate(all_child_not_attend = ifelse(n_no_school == n_child, 1, 0))
  
  
  ###YS using match will probably match with the 1st case, not all of them. you are excluding some cases.
  #see next section
  df$c3 <-
    case_when(
      temp$all_child_not_attend[match(df$X_uuid, temp$X_uuid)] == 1 ~ 1,
      temp$all_child_not_attend[match(df$X_uuid, temp$X_uuid)] == 0 ~ 0,
      TRUE ~ NA_real_
    )
  
  
  df$c4 <-
    ifelse(
      df$primary_school_place == "within_2km" |
        df$secondary_school_place == "within_2km",
      0,
      1
    )
  df$education_score <- case_when(
    df$c3 == 1 ~ 4,
    ###YS c2 is a sum, not a binary. you will not take if there are 2+ children not going to school
    df$c2 == 1 |
      sum_row(df$c1, df$c4) == 2 ~ 3,
    sum_row(df$c1, df$c4) == 1 ~ 2,
    ###YS, should be sum = 0 to be severity 1
    sum_row(df$c1, df$c4) == 1 ~ 1,
    ###YS cannot have 0 for LSG score.
    TRUE ~ 0
  )
  df$education_1 <- ifelse(df$education_score == 1, 1, 0)
  df$education_2 <- ifelse(df$education_score == 2, 1, 0)
  df$education_3 <- ifelse(df$education_score == 3, 1, 0)
  df$education_4 <- ifelse(df$education_score == 4, 1, 0)
  
  ###################################d #########################################
  
  df$d1 <- case_when(df$employment_seasonal == "yes" ~ 1,
                     df$employment_seasonal == "no" ~ 0,
                     TRUE ~ NA_real_)
  
  ###YS using match will probably match with the 1st case, not all of them. you are excluding some cases.
  # aa <- loop %>% 
  #   mutate(hello = age >= 18 & actively_seek_work == "yes" & work == "no") %>%
  #   group_by(X_uuid) %>%
  #   summarise(aaa = sum(hello) > 0)
  # 
  # response %>% left_join(aa) %>% select(d2, aaa, X_uuid) %>% 
  #   filter(aaa == T,
  #          d2 == 0) -> to_check
  # 
  # loop %>% filter(X_uuid %in% to_check$X_uuid) %>% View()
  #look per uuid grouping, not per line.
  
  df$d2 <- ifelse(loop$age[match(df$X_uuid, loop$X_uuid)] >= 18 &
                    loop$actively_seek_work[match(df$X_uuid, loop$X_uuid)] == "yes" &
                    loop$work[match(df$X_uuid, loop$X_uuid)] == "no",
                  1,
                  0)
  
  ###YS the question is for the household, the indicators is per individual.
  df$d3 <- ifelse(df$inc_employment_pension < 90000, 1, 0)
  df$d4 <- ifelse(df$how_much_debt > 505000, 1, 0)
  ###YS these is a intergers question, not a select one. 
  df$d5 <-
    ifelse(df$covid_loss_job_permanent == "yes" |
             df$covid_loss_job_temp == "yes",
           1,
           0)
  df$d6 <-
    ifelse(
      df$reasons_for_debt %in%  c("basic_hh_expenditure",
                                  "health",
                                  "food",
                                  "education"),
      1,
      0
    )
  
  df$livelihoods_score <- case_when(
    df$d6 == 1 ~ 4,
    df$d2 == 1 |
      sum_row(df$d1, df$d3, df$d4, df$d5) > 2 ~ 3,
    sum_row(df$d1, df$d3, df$d4, df$d5) == 2 ~ 2,
    sum_row(df$d1, df$d3, df$d4, df$d5) == 1 ~ 1,
    ###YS cannot have 0 for LSG score.
    TRUE ~ 0
  )
  df$livelihoods_1 <- ifelse(df$livelihoods_score == 1, 1, 0)
  df$livelihoods_2 <- ifelse(df$livelihoods_score == 2, 1, 0)
  df$livelihoods_3 <- ifelse(df$livelihoods_score == 3, 1, 0)
  df$livelihoods_4 <- ifelse(df$livelihoods_score == 4, 1, 0)
  ###################################e #########################################
  
  fsc <- df %>%
    dplyr::select(X_uuid,
                  no_food,
                  no_food_freq,
                  hungry,
                  hungry_freq,
                  not_eating,
                  not_eating_freq)
  fsc$hhh1_1 <- ifelse(fsc$no_food == "yes", 1, 0)
  fsc <- fsc %>% mutate(
    hhh1_2 = case_when(
      no_food_freq == "rarely" ~ 1,
      no_food_freq == "sometimes"  ~ 1,
      no_food_freq == "often" ~ 2,
      no_food_freq == ""  ~ 0
    )
  )
  fsc$hhh1_3 <- fsc$hhh1_1 * fsc$hhh1_2
  fsc$hhh2_1 <- ifelse(fsc$hungry == "yes", 1, 0)
  fsc <- fsc %>% mutate(
    hhh2_2 = case_when(
      hungry_freq == "rarely" ~ 1,
      hungry_freq == "sometimes"  ~ 1,
      hungry_freq == "often" ~ 2,
      hungry_freq == ""  ~ 0
    )
  )
  fsc$hhh2_3 <- fsc$hhh2_1 * fsc$hhh2_2
  fsc$hhh3_1 <- ifelse(fsc$not_eating == "yes", 1, 0)
  fsc <- fsc %>% mutate(
    hhh3_2 = case_when(
      not_eating_freq == "rarely" ~ 1,
      not_eating_freq == "sometimes"  ~ 1,
      not_eating_freq == "often" ~ 2,
      not_eating_freq == ""  ~ 0
    )
  )
  fsc$hhh3_3 <- fsc$hhh3_1 * fsc$hhh3_2
  fsc$hhs <-
    rowSums(fsc[, c("hhh1_3", "hhh2_3", "hhh3_3")], na.rm = T)
  
  #FOOD EXPENDITURE SHARE
  df$food_share <- df$food_exp / df$tot_expenses
  
  #FOOD CONSUMPTIONS SCORE
  df$fcs <-
    as.numeric(df$cereals) * 2 + as.numeric(df$nuts_seed) * 3 + as.numeric(df$milk_dairy) * 4 + as.numeric(df$meat) * 4 +
    as.numeric(df$vegetables) + as.numeric(df$fruits) + as.numeric(df$oil_fats) * 0.5 + as.numeric(df$sweets) * 0.5
  
  ###YS can you confirm the thresold is not 42?
  df$e1 <- ifelse(df$fcs <= 35, 1, 0)
  df$e2 <- ifelse(df$food_share > 0.65, 1, 0)
  df$e3 <- ifelse(fsc$hhs[match(df$X_uuid, fsc$X_uuid)] >= 2, 1, 0)
  
  df$food_security_score <- case_when(df$e3 == 1 ~ 4,
                                      sum_row(df$e1, df$e2) == 2 ~ 3,
                                      sum_row(df$e1, df$e2) == 1 ~ 2,
                                      TRUE ~ 1)
  df$food_security_1 <- ifelse(df$food_security_score == 1, 1, 0)
  df$food_security_2 <- ifelse(df$food_security_score == 2, 1, 0)
  df$food_security_3 <- ifelse(df$food_security_score == 3, 1, 0)
  df$food_security_4 <- ifelse(df$food_security_score == 4, 1, 0)
  ###################################f #########################################
  
  df$f1 <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(any(loop$age[which(loop$X_uuid == x["X_uuid"])] < 18 &
                   loop$work[which(loop$X_uuid == x["X_uuid"])] == "yes"), 1, 0)
    }
  )
  
  loop$child_married <-
    ifelse(
      loop$marital_status %in% c("married", "widowed", "divorced", "separated") &
        loop$age < 18,
      1 ,
      0
    )
  ###YS sums of individuals, not binary
  df <- individual_to_HH_numeric(loop, df, "child_married", "f2")
  df$f3 <-
    ifelse(df$adult_distress_number >= 1 |
             df$child_distress_number >= 1,
           1,
           0)
  df$f4 <- ifelse(
    df$unsafe_areas.distribution_areas == 1 |
      df$unsafe_areas.facilities == 1 |
      df$unsafe_areas.markets == 1 |
      df$unsafe_areas.social_areas == 1 |
      df$unsafe_areas.water_points == 1 |
      df$unsafe_areas.way_to_centers == 1 |
      df$unsafe_areas.way_to_school == 1,
    1,
    0
  )
  
  df$f5 <-
    ifelse(
      df$pds_card == "no" |
        df$id_card_a18 == "no" |
        df$nationality_cert_a18 == "no" |
        df$id_card_u18 == "no" |
        df$nationality_cert_u18 == "no" |
        df$birth_cert_u18 == "no",
      1,
      0
    )
  ###YS indicators says under dispute or no document. not AND
  df$f6 <-
    ifelse(df$hlp_document == "no" & df$hh_dispute == "yes", 1, 0)
  df$f7 <- ifelse(df$hh_risk_eviction == "yes", 1, 0)
  df$f8 <-     case_when(df$not_residing == "yes" ~ 1,
                         df$not_residing %in% c("no") ~ 0,
                         TRUE ~ NA_real_)
  df$f9 <- ifelse(df$security_incident == "yes", 1, 0)
  
  df$protection_score <- case_when(
    ###YS df is a sum of individuals, not binary wont take 2+
    df$f1 == 1 | df$f2 == 1 ~ 4,
    df$f5 == 1 |
      sum_row(df$f3, df$f4, df$f6, df$f7,df$f8, df$f9) >= 4 ~ 3,
    sum_row(df$f3, df$f4, df$f6, df$f7,df$f8, df$f9) == 3 ~ 2,
    sum_row(df$f3, df$f4, df$f6, df$f7,df$f8, df$f9) >= 1 ~ 1,
    ###YS cannot have lsg score of 0
    TRUE ~ 0
  )
  df$protection_1 <- ifelse(df$protection_score == 1, 1, 0)
  df$protection_2 <- ifelse(df$protection_score == 2, 1, 0)
  df$protection_3 <- ifelse(df$protection_score == 3, 1, 0)
  df$protection_4 <- ifelse(df$protection_score == 4, 1, 0)
  ###################################g #########################################
  
  df$g1 <- ifelse(
    df$distance_hospital %in% c("less_15", "less_30", "less_hour") |
      df$distance_clinic %in% c("less_15", "less_30", "less_hour"),
    0,
    1
  )
  ###YS the question asks if women have acces to services, so a "yes" means they have access to, which is not
  #what you are trying to code
  df$g2 <- ifelse(df$women_specialised_services == "yes", 1, 0)
  
  df$health_share <- df$medical_exp / df$tot_expenses
  
  df$g3 <- ifelse(df$health_share > 0.2, 1, 0)
  
  ###YS the difficulty_accessing_services has a skip logic and is only asked to households with disability
  #41. Does any member of your household face difficulties 
  #in accessing any basic services (e.g education, health clinics, markets, etc.) due to his/her difficulty
  df$g4 <- ifelse(df$difficulty_accessing_services == "yes", 1, 0)
  
  df$health_score <-
    case_when(df$g1 == 1 |
                df$g4 == 1 | sum_row(df$g2, df$g3) == 2 ~ 3,
              sum_row(df$g2, df$g3) == 1 ~ 2,
              TRUE ~ 1)
  df$health_1 <- ifelse(df$health_score == 1, 1, 0)
  df$health_2 <- ifelse(df$health_score == 2, 1, 0)
  df$health_3 <- ifelse(df$health_score == 3, 1, 0)
  
  ###################################h #########################################
  
  df$h1 <-
    ifelse(
      ###YS DAP says 2 improvements.
      sum_row(
        df$shelter_better.improve_privacy,
        df$shelter_better.improve_safety,
        df$shelter_better.protect_climate,
        df$shelter_better.protec_hazards,
        df$shelter_better.other
      ) > 1,
      1,
      0
    )
  
  df$h2 <-
    ifelse(
      sum_row(
        df$nfi_priority_needs.bedding_items,
        df$nfi_priority_needs.mattresses_sleeping_mats,
        df$nfi_priority_needs.blankets,
        df$nfi_priority_needs.clothing,
        df$nfi_priority_needs.cooking_stove,
        df$nfi_priority_needs.cooking_utensils,
        df$nfi_priority_needs.heating_cooking_fuel,
        df$nfi_priority_needs.winter_heaters,
        df$nfi_priority_needs.other
      ) >= 1,
      1,
      0
    )
  
  df$h3 <-
    ifelse(
      df$shelter_type %in%
        c(
          "unfinished_abandoned_building",
          "damaged_building",
          "tent",
          "religious_building",
          "public_building",
          "non_residential",
          "container",
          "makeshift_shelter"
        ),
      1,
      0
    )
  
  df$snfi_score <-
    case_when(df$h3 == 1 ~ 4,
              sum_row(df$h1, df$h2) == 2 ~ 3,
              sum_row(df$h1, df$h2) == 1 ~ 2,
              TRUE ~ 1)
  df$snfi_1 <- ifelse(df$snfi_score == 1, 1, 0)
  df$snfi_2 <- ifelse(df$snfi_score == 2, 1, 0)
  df$snfi_3 <- ifelse(df$snfi_score == 3, 1, 0)
  df$snfi_4 <- ifelse(df$snfi_score == 4, 1, 0)
  
  ###################################i #########################################
  
  df$i1 <- ifelse(
    ###YS you are coding the % of people having access to those. 
    df$drinking_water_source %in%
      c(
        "borehole",
        "prot_well",
        "prot_spring",
        "bottled_water",
        "network_private",
        "network_comm"
      ),
    1,
    0
  )
  
  ###YS the indicator mention all 4, while the condition mentions only drinking, cooking and hygiene.
  df$i2 <- ifelse(
    df$sufficient_water_drinking == "yes" &
      df$sufficient_water_cooking == "yes" &
      df$sufficient_water_hygiene == "yes" &
      df$sufficient_water_other == "yes",
    0,
    1
  )
  
  df$i3 <- ifelse(df$latrines %in% c("vip_pit", "flush"), 0, 1)
  
  ###YS you are coding the ones that are treating.
  # side note: you don't need to treat water if it comes from an improved source of water.
  df$i4 <-
    ifelse(df$treat_drink_water %in% c("always", "sometimes"), 1, 0)
  
  df$wash_score <-
    case_when(
      df$i2 == 1 ~ 4,
      df$i1 == 1 |
        sum_row(df$i3, df$i4) == 2 ~ 3,
      sum_row(df$i3, df$i4) == 1 ~ 2,
      TRUE ~ 1
    )
  df$wash_1 <- ifelse(df$wash_score == 1, 1, 0)
  df$wash_2 <- ifelse(df$wash_score == 2, 1, 0)
  df$wash_3 <- ifelse(df$wash_score == 3, 1, 0)
  df$wash_4 <- ifelse(df$wash_score == 4, 1, 0)
  
  return(df)
}