msni_recoding <- function(df, loop) {
  df$stress <-
    case_when(
      df$selling_assets %in% c("no_already_did", "yes") |
        df$borrow_debt  %in% c("no_already_did", "yes") |
        df$reduce_spending %in% c("no_already_did", "yes") |
        df$spending_savings %in% c("no_already_did", "yes") ~ 1,
      TRUE ~ 0
    )
  df$crisis <-
    case_when(
      df$selling_transportation_means %in% c("no_already_did", "yes") |
        df$change_place  %in% c("no_already_did", "yes") |
        df$child_work %in% c("no_already_did", "yes") ~ 1,
      TRUE ~ 0
    )
  df$emergency <-
    case_when(
      df$child_dropout_school %in% c("no_already_did", "yes") |
        df$adult_risky  %in% c("no_already_did", "yes") |
        df$family_migrating %in% c("no_already_did", "yes") |
        df$child_forced_marriage %in% c("no_already_did", "yes") ~ 1,
      TRUE ~ 0
    )
  
  ###################################a #########################################
  df$a1 <-
    case_when(
      df$reasons_for_debt %in%  c("basic_hh_expenditure",
                                  "health",
                                  "food",
                                  "education") ~ 1,
      TRUE ~ 0
    )
  
  df$a2 <- df$primary_livelihood.ngo_charity_assistance
  df$a3 <- ifelse(df$crisis == 1 | df$emergency == 1, 1, 0)
  
  df$capacity_gap <-
    ifelse(df$a1 == 1 | df$a2 == 1 | df$a3 == 1, 1, 0)
  ###################################b #########################################
  
  df$b1 <-
    case_when(
      df$difficulty_communicating %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
        df$difficulty_hearing %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
        df$difficulty_remembering %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
        df$difficulty_seeing %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
        df$difficulty_walking %in% c("a_lot_of_difficulty", "cannot_do_at_all") ~ 1,
      TRUE ~ 0
    )
  
  loop_head <- loop %>%
    filter(relationship == "head")
  
  loop_head$female <-
    case_when(loop_head$sex == "female" & loop_head$age >= 18 ~ 1,
              TRUE ~ 0)
  
  temp <- loop_head %>%
    group_by(X_uuid) %>%
    summarize(sum_female = sum(female, na.rm = TRUE))
  
  df$female_hhh <-
    case_when(temp$sum_female[match(df$X_uuid, temp$X_uuid)] >= 1 ~ 1,
              temp$sum_female[match(df$X_uuid, temp$X_uuid)] == 0 ~ 0,
              TRUE ~ NA_real_)
  
  df$b2 <-
    df$female_hhh
  
  temp <- loop %>%
    group_by(X_uuid) %>%
    summarize(sum_health_issue = sum(health_issue.chronic, na.rm = TRUE))
  
  df$b3 <-
    case_when(temp$sum_health_issue[match(df$X_uuid, temp$X_uuid)] >= 1 ~ 1,
              temp$sum_health_issue[match(df$X_uuid, temp$X_uuid)] == 0 ~ 0,
              TRUE ~ NA_real_)
  
  df$b4 <- case_when(
    df$pds_card == "no" |
      df$id_card_a18 == "no" |
      df$nationality_cert_a18 == "no" |
      df$id_card_u18 == "no" |
      df$nationality_cert_u18 == "no" |
      df$birth_cert_u18 == "no" ~ 1,
    TRUE ~ 0
  )
  
  df$b5 <- case_when(
    df$why_not_return.fear_trauma == 1 |
      df$why_not_return.lack_of_security_forces == 1 |
      df$why_not_return.presence_of_mines == 1 |
      df$why_not_return.discrimination == 1 |
      df$why_not_return.lack_security_women == 1 ~ 1,
    TRUE ~ 0
  )
  
  df$b6 <- case_when(df$access_soap == "no" ~ 1,
                  TRUE ~ 0
  )
  df$b7 <-
    case_when(df$female_60_calc >= 1 | df$male_60_calc >= 1 ~ 1,
              TRUE ~ 0
    )
  #using sum_row function to sum the rows, sum_row belongs to expss library.
  df$vulnerability_score <- case_when(
    df$b1 == 1 ~ 4,
    df$b2 == 1 |
      sum_row(df$b3, df$b4, df$b5, df$b6, df$b7, na.rm = TRUE) >= 3 ~ 3,
    is.na(df$b2) ~ NA_real_,
    sum_row(df$b3, df$b4, df$b5, df$b6, df$b7, na.rm = TRUE) >= 2 ~ 2,
    TRUE ~ 1
  )
  
  df$vulnerability_1 <-
    case_when(df$vulnerability_score == 1 ~ 1,
              is.na(df$vulnerability_score) ~ NA_real_,
              TRUE ~ 0)
  df$vulnerability_2 <-
    case_when(df$vulnerability_score == 2 ~ 1,
              is.na(df$vulnerability_score) ~ NA_real_,
              TRUE ~ 0)
  df$vulnerability_3 <-
    case_when(df$vulnerability_score == 3 ~ 1,
              is.na(df$vulnerability_score) ~ NA_real_,
              TRUE ~ 0)
  df$vulnerability_4 <-
    case_when(df$vulnerability_score == 4 ~ 1,
              is.na(df$vulnerability_score) ~ NA_real_,
              TRUE ~ 0)
  
  df$lsg_vulnerability <-
    case_when(df$vulnerability_score >= 3 ~ 1,
              is.na(df$vulnerability_score) ~ NA_real_,
              TRUE ~ 0)
  
  
  ###################################c #########################################
  
  df$c1 <- case_when(
    df$reasons_not_attend.cannot_afford == 1 |
      df$reasons_not_attend.children_working == 1 ~ 1,
    is.na(df$reasons_not_attend.cannot_afford) &
      is.na(df$reasons_not_attend.children_working) ~ NA_real_,
    TRUE ~ 0
  )
  
  loop_child <- loop %>%
    filter(age < 18)
  
  loop_child$no_school <-
    case_when(
      loop_child$attend_formal_ed == "no" &
        loop_child$attend_informal_ed == "no" ~ 1,
      TRUE ~ 0
    )
  
  temp <- loop_child %>%
    group_by(X_uuid) %>%
    summarize(sum_no_school = sum(no_school),
              sum_child = n())
  
  temp$all_no_school <-
    ifelse(temp$sum_child - temp$sum_no_school == 0, 1, 0)
  
  df$c2 <-
    case_when(temp$sum_no_school[match(df$X_uuid, temp$X_uuid)] >= 1 ~ 1,
              temp$sum_no_school[match(df$X_uuid, temp$X_uuid)] == 0 ~ 0,
              TRUE ~ NA_real_)
  
  
  df$c3 <-
    case_when(temp$all_no_school[match(df$X_uuid, temp$X_uuid)] == 1 ~ 1,
              temp$all_no_school[match(df$X_uuid, temp$X_uuid)] == 0 ~ 0,
              TRUE ~ NA_real_)
  
  
  df$c4 <-
    case_when(
      df$primary_school_place == "within_2km" |
        df$secondary_school_place == "within_2km"  ~ 0,
      TRUE ~ 1
    )
  
  df$education_score <- case_when(
    df$c3 == 1 ~ 4,
    is.na(df$c3) ~ NA_real_,
    df$c2 == 1 |
      sum_row(df$c1, df$c4, na.rm = TRUE) == 2 ~ 3,
    is.na(df$c2) ~ NA_real_,
    sum_row(df$c1, df$c4, na.rm = TRUE) == 1 ~ 2,
    TRUE ~ 1
  )
  df$education_1 <-
    case_when(df$education_score == 1 ~ 1,
              is.na(df$education_score) ~ NA_real_,
              TRUE ~ 0)
  df$education_2 <-
    case_when(df$education_score == 2 ~ 1,
              is.na(df$education_score) ~ NA_real_,
              TRUE ~ 0)
  df$education_3 <-
    case_when(df$education_score == 3 ~ 1,
              is.na(df$education_score) ~ NA_real_,
              TRUE ~ 0)
  df$education_4 <-
    case_when(df$education_score == 4 ~ 1,
              is.na(df$education_score) ~ NA_real_,
              TRUE ~ 0)
  
  df$lsg_education <-
    case_when(df$education_score >= 3 ~ 1,
              is.na(df$education_score) ~ NA_real_,
              TRUE ~ 0)
  df$lsg_education_vulnerable <-
    case_when(df$lsg_education == 1 &
             df$vulnerability_score >= 3 ~ 1,
           TRUE ~ 0
    )
  ###################################d #########################################
  
  df$d1 <- case_when(df$employment_seasonal == "yes" ~ 1,
                     df$employment_seasonal == "no" ~ 0,
                     TRUE ~ NA_real_)
  
  loop_adult <- loop %>%
    filter(age >= 18)
  
  loop_adult$unemployed_seek_work <-
    case_when(
      loop_adult$actively_seek_work == "yes" &
        loop_adult$work == "no" ~ 1,
      loop_adult$actively_seek_work == "no" ~ 0,
      TRUE ~ NA_real_
    )
  
  temp <- loop_adult %>%
    group_by(X_uuid) %>%
    summarize(sum_unemployed = sum(unemployed_seek_work, na.rm = TRUE))
  
  df$d2 <-
    case_when(temp$sum_unemployed[match(df$X_uuid, temp$X_uuid)] >= 1 ~ 1,
              temp$sum_unemployed[match(df$X_uuid, temp$X_uuid)] == 0 ~ 0,
              TRUE ~ NA_real_)
  
  df$d3 <-
    case_when(df$inc_employment_pension / df$num_hh_member < 90000 ~ 1,
              TRUE ~ 0)
  df$d4 <- case_when(df$how_much_debt > 505000 ~ 1,
                  TRUE ~ 0)
  df$d5 <-
    case_when(df$covid_loss_job_permanent >= 1 |
                df$covid_loss_job_temp >= 1 ~ 1,
              TRUE ~ 0)
  df$d6 <-
    case_when(
      df$reasons_for_debt %in%  c("basic_hh_expenditure",
                                  "health",
                                  "food",
                                  "education") ~ 1,
      TRUE ~ 0)
  
  df$livelihoods_score <- case_when(
    df$d6 == 1 ~ 4,
    is.na(df$d6) ~ NA_real_,
    df$d2 == 1 |
      sum_row(df$d1, df$d3, df$d4, df$d5, na.rm = TRUE) > 2 ~ 3,
    is.na(df$d2) ~ NA_real_,
    sum_row(df$d1, df$d3, df$d4, df$d5, na.rm = TRUE) == 2 ~ 2,
    TRUE ~ 1
  )
  df$livelihoods_1 <- case_when(df$livelihoods_score == 1 ~ 1,
                                is.na(df$livelihoods_score) ~ NA_real_,
                                TRUE ~ 0)
  df$livelihoods_2 <- case_when(df$livelihoods_score == 2 ~ 1,
                                is.na(df$livelihoods_score) ~ NA_real_,
                                TRUE ~ 0)
  df$livelihoods_3 <- case_when(df$livelihoods_score == 3 ~ 1,
                                is.na(df$livelihoods_score) ~ NA_real_,
                                TRUE ~ 0)
  df$livelihoods_4 <- case_when(df$livelihoods_score == 4 ~ 1,
                                is.na(df$livelihoods_score) ~ NA_real_,
                                TRUE ~ 0)
  
  df$lsg_livelihoods <-
    case_when(df$livelihoods_score >= 3 ~ 1,
              is.na(df$livelihoods_score) ~ NA_real_,
              TRUE ~ 0)
  df$lsg_livelihoods_vulnerable <-
    case_when(df$lsg_livelihoods == 1 &
             df$vulnerability_score >= 3 ~ 1,
             TRUE ~ 0)
  
  ###################################e #########################################
  
  fsc <- df %>%
    dplyr::select(X_uuid,
                  no_food,
                  no_food_freq,
                  hungry,
                  hungry_freq,
                  not_eating,
                  not_eating_freq)
  fsc$hhh1_1 <- case_when(fsc$no_food == "yes" ~ 1,
                       TRUE ~ 0)
  fsc <- fsc %>% mutate(
    hhh1_2 = case_when(
      no_food_freq == "rarely" ~ 1,
      no_food_freq == "sometimes"  ~ 1,
      no_food_freq == "often" ~ 2,
      no_food_freq == ""  ~ 0
    )
  )
  fsc$hhh1_3 <- fsc$hhh1_1 * fsc$hhh1_2
  fsc$hhh2_1 <- case_when(fsc$hungry == "yes" ~ 1,
                          TRUE ~ 0)
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
  
  
  df$e1 <- case_when(df$fcs <= 42 ~ 1,
                  TRUE ~ 0)
  df$e2 <- case_when(df$food_share > 0.65 ~ 1,
                  TRUE ~ 0)
  df$e3 <- case_when(fsc$hhs[match(df$X_uuid, fsc$X_uuid)] >= 2 ~ 1,
                  TRUE ~ 0)
  
  df$food_security_score <- case_when(
    df$e3 == 1 ~ 4,
    is.na(df$e3) ~ NA_real_,
    sum_row(df$e1, df$e2, na.rm = TRUE) == 2 ~ 3,
    sum_row(df$e1, df$e2, na.rm = TRUE) == 1 ~ 2,
    TRUE ~ 1
  )
  df$food_security_1 <- case_when(df$food_security_score == 1 ~ 1,
                                  is.na(df$food_security_score) ~ NA_real_,
                                  TRUE ~ 0)
  df$food_security_2 <- case_when(df$food_security_score == 2 ~ 1,
                                  is.na(df$food_security_score) ~ NA_real_,
                                  TRUE ~ 0)
  df$food_security_3 <- case_when(df$food_security_score == 3 ~ 1,
                                  is.na(df$food_security_score) ~ NA_real_,
                                  TRUE ~ 0)
  df$food_security_4 <- case_when(df$food_security_score == 4 ~ 1,
                                  is.na(df$food_security_score) ~ NA_real_,
                                  TRUE ~ 0)
  
  df$lsg_food <-
    case_when(df$food_security_score >= 3 ~ 1,
              is.na(df$food_security_score) ~ NA_real_,
              TRUE ~ 0)
  df$lsg_food_vulnerable <-
    case_when(df$lsg_food == 1 & df$vulnerability_score >= 3 ~ 1,
              TRUE ~ 0)
  
  ###################################f #########################################
  
  df$f1 <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(any(loop$age[which(loop$X_uuid == x["X_uuid"])] < 18 &
                   loop$work[which(loop$X_uuid == x["X_uuid"])] == "yes"), 1, 0)
    }
  )
  
  loop_child$married <-
    case_when(
      loop_child$marital_status %in% c("married", "widowed", "divorced", "separated") ~ 1,
      TRUE ~ 0)
  temp <- loop_child %>%
    group_by(X_uuid) %>%
    summarize(sum_married_child = sum(married))
  
  df$f2 <-
    case_when(temp$sum_married_child[match(df$X_uuid, temp$X_uuid)] >= 1 ~ 1,
              temp$sum_married_child[match(df$X_uuid, temp$X_uuid)] == 0 ~ 0,
              TRUE ~ NA_real_)
  df$f3 <-
    case_when(df$adult_distress_number >= 1 |
                df$child_distress_number >= 1 ~ 1,
              TRUE ~ 0)
  
  df$f4 <- case_when(
    df$unsafe_areas.distribution_areas == 1 |
      df$unsafe_areas.facilities == 1 |
      df$unsafe_areas.markets == 1 |
      df$unsafe_areas.social_areas == 1 |
      df$unsafe_areas.water_points == 1 |
      df$unsafe_areas.way_to_centers == 1 |
      df$unsafe_areas.way_to_school == 1 ~ 1,
    TRUE ~ 0
  )
  
  df$f5 <-
    case_when(
      df$pds_card == "no" |
        df$id_card_a18 == "no" |
        df$nationality_cert_a18 == "no" |
        df$id_card_u18 == "no" |
        df$nationality_cert_u18 == "no" |
        df$birth_cert_u18 == "no" ~ 1,
      TRUE ~ 0)
  df$f6 <-
    case_when(df$hlp_document == "no" | df$hh_dispute == "yes" ~ 1,
           TRUE ~ 0
    )
  df$f7 <- case_when(df$hh_risk_eviction == "yes" ~ 1,
                  TRUE ~ 0
  )
  df$f8 <-     case_when(df$not_residing == "yes" ~ 1,
                         df$not_residing %in% c("no") ~ 0,
                         TRUE ~ NA_real_)
  df$f9 <- case_when(df$security_incident == "yes" ~ 1,
                  TRUE ~ 0
  )
  
  df$protection_score <- case_when(
    df$f1 == 1 | df$f2 == 1 ~ 4,
    is.na(df$f1) & is.na(df$f2) ~ NA_real_,
    df$f5 == 1 |
      sum_row(df$f3, df$f4, df$f6, df$f7, df$f8, df$f9, na.rm = TRUE) >= 4 ~ 3,
    is.na(df$f5) ~ NA_real_,
    sum_row(df$f3, df$f4, df$f6, df$f7, df$f8, df$f9, na.rm = TRUE) == 3 ~ 2,
    TRUE ~ 1
  )
  df$protection_1 <- case_when(df$protection_score == 1 ~ 1,
                               is.na(df$protection_score) ~ NA_real_,
                               TRUE ~ 0)
  df$protection_2 <- case_when(df$protection_score == 2 ~ 1,
                               is.na(df$protection_score) ~ NA_real_,
                               TRUE ~ 0)
  df$protection_3 <- case_when(df$protection_score == 3 ~ 1,
                               is.na(df$protection_score) ~ NA_real_,
                               TRUE ~ 0)
  df$protection_4 <- case_when(df$protection_score == 4 ~ 1,
                               is.na(df$protection_score) ~ NA_real_,
                               TRUE ~ 0)
  
  df$lsg_protection <-
    case_when(df$protection_score >= 3 ~ 1,
              is.na(df$protection_score) ~ NA_real_,
              TRUE ~ 0)
  df$lsg_protection_vulnerable <-
    case_when(df$lsg_protection == 1 &
             df$vulnerability_score >= 3 ~ 1,
             TRUE ~ 0
    )
  ###################################g #########################################
  
  df$g1 <- case_when(
    df$distance_hospital %in% c("less_15", "less_30", "less_hour") |
      df$distance_clinic %in% c("less_15", "less_30", "less_hour") ~ 0,
    TRUE ~ 1
  )
  
  df$g2 <- case_when(df$women_specialised_services == "no" ~ 1,
                  TRUE ~ 0
  )
  
  df$health_share <- df$medical_exp / df$tot_expenses
  
  df$g3 <- case_when(df$health_share > 0.2~ 1,
                     TRUE ~ 0
  )
  df$g4 <-
    case_when(
      df$difficulty_accessing_services == "yes" ~ 1,
      df$difficulty_accessing_services == "no" ~ 0,
      TRUE ~ NA_real_
    )
  
  df$health_score <-
    case_when(
      df$g1 == 1 |
        df$g4 == 1 | sum_row(df$g2, df$g3, na.rm = TRUE) == 2 ~ 3,
      is.na(df$g1) & is.na(df$g4) ~ NA_real_,
      sum_row(df$g2, df$g3, na.rm = TRUE) == 1 ~ 2,
      TRUE ~ 1
    )
  df$health_1 <- case_when(df$health_score == 1 ~ 1,
                           is.na(df$health_score) ~ NA_real_,
                           TRUE ~ 0)
  df$health_2 <- case_when(df$health_score == 2 ~ 1,
                           is.na(df$health_score) ~ NA_real_,
                           TRUE ~ 0)
  df$health_3 <- case_when(df$health_score == 3 ~ 1,
                           is.na(df$health_score) ~ NA_real_,
                           TRUE ~ 0)
  
  df$lsg_health <-
    case_when(df$health_score >= 3 ~ 1,
              is.na(df$health_score) ~ NA_real_,
              TRUE ~ 0)
  df$lsg_health_vulnerable <-
    case_when(df$lsg_health == 1 & df$vulnerability_score >= 3 ~ 1, TRUE ~ 0)
  ###################################h #########################################
  
  df$h1 <-
    case_when(
      sum_row(
        df$shelter_better.improve_privacy,
        df$shelter_better.improve_safety,
        df$shelter_better.protect_climate,
        df$shelter_better.protec_hazards,
        df$shelter_better.other,
        na.rm = TRUE
      ) > 1 ~ 1, TRUE ~ 0)
  
  df$h2 <-
    case_when(
      sum_row(
        df$nfi_priority_needs.bedding_items,
        df$nfi_priority_needs.mattresses_sleeping_mats,
        df$nfi_priority_needs.blankets,
        df$nfi_priority_needs.clothing,
        df$nfi_priority_needs.cooking_stove,
        df$nfi_priority_needs.cooking_utensils,
        df$nfi_priority_needs.heating_cooking_fuel,
        df$nfi_priority_needs.winter_heaters,
        df$nfi_priority_needs.other,
        na.rm = TRUE
      ) >= 1 ~ 1, TRUE ~ 0)
  
  df$h3 <-
    case_when(
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
        ) ~ 1, TRUE ~ 0)
  
  df$snfi_score <-
    case_when(
      df$h3 == 1 ~ 4,
      is.na(df$h3) ~ NA_real_,
      sum_row(df$h1, df$h2, na.rm = TRUE) == 2 ~ 3,
      sum_row(df$h1, df$h2, na.rm = TRUE) == 1 ~ 2,
      TRUE ~ 1
    )
  df$snfi_1 <- case_when(df$snfi_score == 1 ~ 1,
                         is.na(df$snfi_score) ~ NA_real_,
                         TRUE ~ 0)
  df$snfi_2 <- case_when(df$snfi_score == 2 ~ 1,
                         is.na(df$snfi_score) ~ NA_real_,
                         TRUE ~ 0)
  df$snfi_3 <- case_when(df$snfi_score == 3 ~ 1,
                         is.na(df$snfi_score) ~ NA_real_,
                         TRUE ~ 0)
  df$snfi_4 <-  case_when(df$snfi_score == 4 ~ 1,
                          is.na(df$snfi_score) ~ NA_real_,
                          TRUE ~ 0)
  
  df$lsg_snfi <-
    case_when(df$snfi_score >= 3 ~ 1, is.na(df$snfi_score) ~ NA_real_, TRUE ~ 0)
  df$lsg_snfi_vulnerable <-
    case_when(df$lsg_snfi == 1 & df$vulnerability_score >= 3 ~ 1, TRUE ~ 0)
  ###################################i #########################################
  
  df$i1 <- case_when(
    !df$drinking_water_source %in%
      c(
        "borehole",
        "prot_well",
        "prot_spring",
        "bottled_water",
        "network_private",
        "network_comm"
      ) ~ 1, TRUE ~ 0)
  
  df$i2 <-
    case_when(
      df$sufficient_water_drinking == "no" |
        df$sufficient_water_cooking == "no" |
        df$sufficient_water_hygiene == "no"  ~ 1,
      TRUE ~ 0
    )
  
  df$i3 <- case_when(df$latrines %in% c("vip_pit", "flush") ~ 0, TRUE ~ 1)
  
  df$i4 <-
    case_when(
      df$treat_drink_water %in% c("always", "sometimes") &
        !df$drinking_water_source %in%
        c(
          "borehole",
          "prot_well",
          "prot_spring",
          "bottled_water",
          "network_private",
          "network_comm"
        ) ~ 1, TRUE ~ 0)
  
  
  
  df$wash_score <-
    case_when(
      df$i1 == 1 ~ 4,
      is.na(df$i1) ~ NA_real_,
      df$i3 == 1 | sum_row(df$i2, df$i4, na.rm = TRUE) == 2 ~ 3,
      sum_row(df$i2, df$i4, na.rm = TRUE) == 1 ~ 2,
      TRUE ~ 1
    )
  df$wash_1 <- case_when(df$wash_score == 1 ~ 1,
                         is.na(df$wash_score) ~ NA_real_,
                         TRUE ~ 0)
  df$wash_2 <- case_when(df$wash_score == 2 ~ 1,
                         is.na(df$wash_score) ~ NA_real_,
                         TRUE ~ 0)
  df$wash_3 <- case_when(df$wash_score == 3 ~ 1,
                         is.na(df$wash_score) ~ NA_real_,
                         TRUE ~ 0)
  df$wash_4 <- case_when(df$wash_score == 4 ~ 1,
                         is.na(df$wash_score) ~ NA_real_,
                         TRUE ~ 0)
  
  df$lsg_wash <-
    case_when(df$wash_score >= 3 ~ 1, is.na(df$wash_score) ~ NA_real_, TRUE ~ 0)
  df$lsg_wash_vulnerable <-
    case_when(df$lsg_wash == 1 & df$vulnerability_score >= 3 ~ 1, TRUE ~ 0)
  
  df$msni_score <-
    case_when(
      df$education_score == 4 |
        df$livelihoods_score == 4 |
        df$food_security_score == 4 |
        df$health_score == 4 |
        df$protection_score == 4 |
        df$snfi_score == 4 |
        df$wash_score == 4 ~ 4,
      df$education_score == 3 |
        df$livelihoods_score == 3 |
        df$food_security_score == 3 |
        df$health_score == 3 |
        df$protection_score == 3 |
        df$snfi_score == 3 |
        df$wash_score == 3 ~ 3,
      df$education_score == 2 |
        df$livelihoods_score == 2 |
        df$food_security_score == 2 |
        df$health_score == 2 |
        df$protection_score == 2 |
        df$snfi_score == 2 |
        df$wash_score == 2 ~ 2,
      df$education_score == 1 |
        df$livelihoods_score == 1 |
        df$food_security_score == 1 |
        df$health_score == 1 |
        df$protection_score == 1 |
        df$snfi_score == 1 |
        df$wash_score == 1 ~ 1,
      TRUE ~ NA_real_
    )
  
  df$msni_score_4 <-
    case_when(df$msni_score == 4 ~ 1, TRUE ~ 0)
  
  df$msni_score_3 <-
    case_when(df$msni_score == 3 ~ 1, TRUE ~ 0)
  
  df$msni_score_2 <-
    case_when(df$msni_score == 2 ~ 1, TRUE ~ 0)
  
  df$msni_score_1 <-
    case_when(df$msni_score == 1 ~ 1, TRUE ~ 0)
  
  df$lsg_all <- case_when(df$msni_score >= 3 ~ 1, TRUE ~ 0)
  
  
  
  
  df$female_hhh_education <-
    case_when(df$female_hhh == 1 & df$lsg_education == 1 ~ 1,
              is.na(df$female_hhh) ~ NA_real_,
              TRUE ~ 0)
  df$female_hhh_food <-
    case_when(df$female_hhh == 1 & df$lsg_food == 1 ~ 1,
              is.na(df$female_hhh) ~ NA_real_,
              TRUE ~ 0)
  df$female_hhh_health <-
    case_when(df$female_hhh == 1 & df$lsg_health == 1 ~ 1,
              is.na(df$female_hhh) ~ NA_real_,
              TRUE ~ 0)
  df$female_hhh_protection <-
    case_when(df$female_hhh == 1 & df$lsg_protection == 1 ~ 1,
              is.na(df$female_hhh) ~ NA_real_,
              TRUE ~ 0)
  df$female_hhh_shelter <-
    case_when(df$female_hhh == 1 & df$lsg_snfi == 1 ~ 1,
              is.na(df$female_hhh) ~ NA_real_,
              TRUE ~ 0)
  df$female_hhh_wash <-
    case_when(df$female_hhh == 1 & df$lsg_wash == 1 ~ 1,
              is.na(df$female_hhh) ~ NA_real_,
              TRUE ~ 0)
  df$female_hhh_lsgs <- case_when(
    sum_row(
      df$lsg_education,
      df$lsg_food,
      df$lsg_health,
      df$lsg_livelihoods,
      df$lsg_protection,
      df$lsg_snfi,
      df$lsg_wash,
      na.rm = TRUE
    ) >= 1 & df$female_hhh == 1 ~ 1,
    is.na(df$female_hhh) ~ NA_real_,
    TRUE ~ 0
  )
  df$female_hhh_cg <-
    case_when(df$female_hhh == 1 & df$capacity_gap == 1 ~ 1,
              is.na(df$female_hhh) ~ NA_real_,
              TRUE ~ 0)
  
  df$vulnerability_score_1 <-
    case_when(
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash,
        na.rm = TRUE
      ) >= 1 & df$vulnerability_1 == 1 ~ 1, TRUE ~ 0)
  
  df$vulnerability_score_2 <-
    case_when(
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash,
        na.rm = TRUE
      ) >= 1 & df$vulnerability_2 == 1 ~ 1, TRUE ~ 0)
  
  df$vulnerability_score_3 <-
    case_when(
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash,
        na.rm = TRUE
      ) >= 1 & df$vulnerability_3 == 1 ~ 1, TRUE ~ 0)
  df$vulnerability_score_4 <-
    case_when(
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash,
        na.rm = TRUE
      ) >= 1 & df$vulnerability_4 == 1 ~ 1, TRUE ~ 0)
  
  df$cg_no_lsg <-
    case_when(
      df$capacity_gap == 1 &
        sum_row(
          df$lsg_education,
          df$lsg_food,
          df$lsg_health,
          df$lsg_livelihoods,
          df$lsg_protection,
          df$lsg_snfi,
          df$lsg_wash,
          na.rm = TRUE
        ) == 0 ~ 1, TRUE ~ 0)
  
  df$cg_no_lsg_but_vulnerable <- case_when(
    df$capacity_gap == 1 &
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash,
        na.rm = TRUE
      ) == 0 & df$vulnerability_1 == 1 ~ 1, TRUE ~ 0)
  
  df$cg_and_one_lsg <-
    case_when(
      df$capacity_gap == 1 &
        sum_row(
          df$lsg_education,
          df$lsg_food,
          df$lsg_health,
          df$lsg_livelihoods,
          df$lsg_protection,
          df$lsg_snfi,
          df$lsg_wash,
          na.rm = TRUE
        ) >= 1 ~ 1, TRUE ~ 0)
  
  df$cg_or_one_lsg <-
    case_when(
      df$capacity_gap == 1 |
        sum_row(
          df$lsg_education,
          df$lsg_food,
          df$lsg_health,
          df$lsg_livelihoods,
          df$lsg_protection,
          df$lsg_snfi,
          df$lsg_wash,
          na.rm = TRUE
        ) >= 1 ~ 1, TRUE ~ 0)
  
  df$no_cg_and_no_lsg <-
    case_when(
      df$capacity_gap == 0 &
        sum_row(
          df$lsg_education,
          df$lsg_food,
          df$lsg_health,
          df$lsg_livelihoods,
          df$lsg_protection,
          df$lsg_snfi,
          df$lsg_wash,
          na.rm = TRUE
        ) == 0 ~ 1, TRUE ~ 0)
  
  df$lsg_but_no_cg <-
    case_when(
      df$capacity_gap == 0 &
        sum_row(
          df$lsg_education,
          df$lsg_food,
          df$lsg_health,
          df$lsg_livelihoods,
          df$lsg_protection,
          df$lsg_snfi,
          df$lsg_wash,
          na.rm = TRUE
        ) >= 1 ~ 1, TRUE ~ 0)
  
  df$seven_lsg   <-
    case_when(
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash,
        na.rm = TRUE
      ) == 7 ~ 1, TRUE ~ 0)
  df$six_lsg   <-
    case_when(
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash,
        na.rm = TRUE
      ) == 6 ~ 1, TRUE ~ 0)
  df$five_lsg   <-
    case_when(
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash,
        na.rm = TRUE
      ) == 5 ~ 1, TRUE ~ 0)
  df$four_lsg   <-
    case_when(
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash,
        na.rm = TRUE
      ) == 4 ~ 1, TRUE ~ 0)
  df$three_lsg   <-
    case_when(
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash,
        na.rm = TRUE
      ) == 3 ~ 1, TRUE ~ 0)
  df$two_lsg   <-
    case_when(
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash,
        na.rm = TRUE
      ) == 2 ~ 1, TRUE ~ 0)
  df$one_lsg   <-
    case_when(
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash,
        na.rm = TRUE
      ) == 1 ~ 1, TRUE ~ 0)
  df$zero_lsg   <-
    case_when(
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash,
        na.rm = TRUE
      ) == 0 ~ 1, TRUE ~ 0)
  df$lsg_vulnerable <-
    case_when(
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash,
        na.rm = TRUE
      ) >= 1 & df$vulnerability_score >= 3 ~ 1, TRUE ~ 0)
  
  return(df)
}