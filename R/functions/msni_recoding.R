msni_recoding <- function(df, loop) {
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
  
  loop$single_female_hhh <-
    case_when(
      loop$relationship == "head" &
        loop$marital_status %in% c("single", "separated", "widowed", "divorced") &
        loop$sex == "female" ~ 1,
      loop$relationship == "head" ~ 0,
      TRUE ~ NA_real_
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
  
  df$coping_mechanism <-
    ifelse(df$a1 == 1 | df$a2 == 1 | df$a3 == 1, 1, 0)
  
  df$cg_score <- sum_row(df$a1, df$a2, df$a3, 2)
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
  
  temp <- loop %>%
    group_by(X_uuid) %>%
    summarize(sum_fhhh = sum(single_female_hhh, na.rm = TRUE))
  
  df$b2 <-
    case_when(temp$sum_fhhh[match(df$X_uuid, temp$X_uuid)] >= 1 ~ 1,
              temp$sum_fhhh[match(df$X_uuid, temp$X_uuid)] == 0 ~ 0,
              TRUE ~ NA_real_)
  
  temp <- loop %>%
    group_by(X_uuid) %>%
    summarize(sum_health_issue = sum(health_issue.chronic, na.rm = TRUE))
  
  df$b3 <-
    case_when(temp$sum_health_issue[match(df$X_uuid, temp$X_uuid)] >= 1 ~ 1,
              temp$sum_health_issue[match(df$X_uuid, temp$X_uuid)] == 0 ~ 0,
              TRUE ~ NA_real_)
  
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
  df$b7 <-
    ifelse(df$female_60_calc >= 1 | df$male_60_calc >= 1, 1, 0)
  
  #using sum_row function to sum the rows, sum_row belongs to expss library.
  df$vulnerability_score <- case_when(
    df$b1 == 1 ~ 4,
    df$b2 == 1 |
      sum_row(df$b3, df$b4, df$b5, df$b6, df$b7) >= 3 ~ 3,
    is.na(df$b2) ~ NA_real_,
    sum_row(df$b3, df$b4, df$b5, df$b6, df$b7) >= 2 ~ 2,
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
    ifelse(
      df$primary_school_place == "within_2km" |
        df$secondary_school_place == "within_2km",
      0,
      1
    )
  
  df$education_score <- case_when(
    df$c3 == 1 ~ 4,
    is.na(df$c3) ~ NA_real_,
    df$c2 == 1 |
      sum_row(df$c1, df$c4) == 2 ~ 3,
    is.na(df$c2) ~ NA_real_,
    sum_row(df$c1, df$c4) == 1 ~ 2,
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
  df$lsg_education_vulnerable <- ifelse(df$lsg_education == 1 & df$vulnerability_score >= 3, 1, 0)
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
    ifelse(df$inc_employment_pension / df$num_hh_member < 90000, 1, 0)
  df$d4 <- ifelse(df$how_much_debt > 505000, 1, 0)
  df$d5 <-
    ifelse(df$covid_loss_job_permanent >= 1 |
             df$covid_loss_job_temp >= 1,
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
    is.na(df$d6) ~ NA_real_,
    df$d2 == 1 |
      sum_row(df$d1, df$d3, df$d4, df$d5) > 2 ~ 3,
    is.na(df$d2) ~ NA_real_,
    sum_row(df$d1, df$d3, df$d4, df$d5) == 2 ~ 2,
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
  df$lsg_livelihoods_vulnerable <- ifelse(df$lsg_livelihoods == 1 & df$vulnerability_score >= 3, 1, 0)
  
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
  
  
  df$e1 <- ifelse(df$fcs <= 42, 1, 0)
  df$e2 <- ifelse(df$food_share > 0.65, 1, 0)
  df$e3 <- ifelse(fsc$hhs[match(df$X_uuid, fsc$X_uuid)] >= 2, 1, 0)
  
  df$food_security_score <- case_when(
    df$e3 == 1 ~ 4,
    is.na(df$e3) ~ NA_real_,
    sum_row(df$e1, df$e2) == 2 ~ 3,
    sum_row(df$e1, df$e2) == 1 ~ 2,
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
  df$lsg_food_vulnerable <- ifelse(df$lsg_food == 1 & df$vulnerability_score >= 3, 1, 0)
  
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
    ifelse(
      loop_child$marital_status %in% c("married", "widowed", "divorced", "separated"),
      1 ,
      0
    )
  temp <- loop_child %>%
    group_by(X_uuid) %>%
    summarize(sum_married_child = sum(married))
  
  df$f2 <-
    case_when(temp$sum_married_child[match(df$X_uuid, temp$X_uuid)] >= 1 ~ 1,
              temp$sum_married_child[match(df$X_uuid, temp$X_uuid)] == 0 ~ 0,
              TRUE ~ NA_real_)
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
  df$f6 <-
    ifelse(df$hlp_document == "no" | df$hh_dispute == "yes", 1, 0)
  df$f7 <- ifelse(df$hh_risk_eviction == "yes", 1, 0)
  df$f8 <-     case_when(df$not_residing == "yes" ~ 1,
                         df$not_residing %in% c("no") ~ 0,
                         TRUE ~ NA_real_)
  df$f9 <- ifelse(df$security_incident == "yes", 1, 0)
  
  df$protection_score <- case_when(
    df$f1 == 1 | df$f2 == 1 ~ 4,
    is.na(df$f1) & is.na(df$f2) ~ NA_real_,
    df$f5 == 1 |
      sum_row(df$f3, df$f4, df$f6, df$f7, df$f8, df$f9) >= 4 ~ 3,
    is.na(df$f5) ~ NA_real_,
    sum_row(df$f3, df$f4, df$f6, df$f7, df$f8, df$f9) == 3 ~ 2,
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
  df$lsg_protection_vulnerable <- ifelse(df$lsg_protection == 1 & df$vulnerability_score >= 3, 1, 0)
  ###################################g #########################################
  
  df$g1 <- ifelse(
    df$distance_hospital %in% c("less_15", "less_30", "less_hour") |
      df$distance_clinic %in% c("less_15", "less_30", "less_hour"),
    0,
    1
  )
  
  df$g2 <- ifelse(df$women_specialised_services == "no", 1, 0)
  
  df$health_share <- df$medical_exp / df$tot_expenses
  
  df$g3 <- ifelse(df$health_share > 0.2, 1, 0)
  df$g4 <-
    case_when(
      df$difficulty_accessing_services == "yes" ~ 1,
      df$difficulty_accessing_services == "no" ~ 0,
      TRUE ~ NA_real_
    )
  
  df$health_score <-
    case_when(
      df$g1 == 1 |
        df$g4 == 1 | sum_row(df$g2, df$g3) == 2 ~ 3,
      is.na(df$g1) & is.na(df$g4) ~ NA_real_,
      sum_row(df$g2, df$g3) == 1 ~ 2,
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
  df$lsg_health_vulnerable <- ifelse(df$lsg_health == 1 & df$vulnerability_score >= 3, 1, 0)
  ###################################h #########################################
  
  df$h1 <-
    ifelse(
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
    case_when(
      df$h3 == 1 ~ 4,
      is.na(df$h3) ~ NA_real_,
      sum_row(df$h1, df$h2) == 2 ~ 3,
      sum_row(df$h1, df$h2) == 1 ~ 2,
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
  df$lsg_snfi_vulnerable <- ifelse(df$lsg_snfi == 1 & df$vulnerability_score >= 3, 1, 0)
  ###################################i #########################################
  
  df$i1 <- ifelse(
    !df$drinking_water_source %in%
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
  
  df$i2 <- ifelse(
    df$sufficient_water_drinking == "yes" &
      df$sufficient_water_cooking == "yes" &
      df$sufficient_water_hygiene == "yes",
    0,
    1
  )
  
  df$i3 <- ifelse(df$latrines %in% c("vip_pit", "flush"), 0, 1)
  
  df$i4 <-
    ifelse(
      df$treat_drink_water %in% c("always", "sometimes") &
        !df$drinking_water_source %in%
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
  
  df$wash_score <-
    case_when(
      df$i2 == 1 ~ 4,
      is.na(df$i2) ~ NA_real_,
      df$i1 == 1 |
        sum_row(df$i3, df$i4) == 2 ~ 3,
      is.na(df$i1) ~ NA_real_,
      sum_row(df$i3, df$i4) == 1 ~ 2,
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
  df$lsg_wash_vulnerable <- ifelse(df$lsg_wash == 1 & df$vulnerability_score >= 3, 1, 0)
  
  df$msni_score <- max(df$education_score, df$livelihoods_score, df$food_score, df$health_score, df$protection_score, df$snfi_score, df$wash_score)
  
  df$msni_score_4 <-
    ifelse(
        df$msni_score == 4,
      1,
      0
    )

  df$msni_score_3 <-
    ifelse(
      df$msni_score == 3,
      1,
      0
    )
  
  df$msni_score_2 <-
    ifelse(
      df$msni_score == 2,
      1,
      0
    )
  
  df$msni_score_1 <-
    ifelse(
      df$msni_score == 1,
      1,
      0
    )
  
  df$lsg_all <- ifelse(df$msni_score >= 3, 1, 0)
  
  loop_head <- loop %>%
    filter(relationship == "head")
  
  loop_head$male <-
    case_when(
      loop_head$sex == "male" & loop_head$age >= 18 ~ 1,
      TRUE ~ 0
    )
  loop_head$female <-
    case_when(
      loop_head$sex == "female" & loop_head$age >= 18 ~ 1,
      TRUE ~ 0
    )
  loop_head$adult <-
    case_when(
      loop_head$age >= 18 ~ 1,
      TRUE ~ 0
    )
  loop_head$child <-
    case_when(
      loop_head$age < 18 ~ 1,
      TRUE ~ 0
    )
  temp <- loop_head %>%
    group_by(X_uuid) %>%
    summarize(sum_male = sum(male, na.rm = TRUE),
              sum_female = sum(female, na.rm = TRUE),
              sum_adult = sum(adult, na.rm = TRUE),
              sum_child = sum(child, na.rm = TRUE))
  
  df$male_hhh <-
    case_when(temp$sum_male[match(df$X_uuid, temp$X_uuid)] >= 1 ~ 1,
              temp$sum_male[match(df$X_uuid, temp$X_uuid)] == 0 ~ 0,
              TRUE ~ NA_real_)
  
  df$female_hhh <-
    case_when(temp$sum_female[match(df$X_uuid, temp$X_uuid)] >= 1 ~ 1,
              temp$sum_female[match(df$X_uuid, temp$X_uuid)] == 0 ~ 0,
              TRUE ~ NA_real_)
  
  df$adult_hhh <-
    case_when(temp$sum_adult[match(df$X_uuid, temp$X_uuid)] >= 1 ~ 1,
              temp$sum_adult[match(df$X_uuid, temp$X_uuid)] == 0 ~ 0,
              TRUE ~ NA_real_)
  
  df$child_hhh <-
    case_when(temp$sum_child[match(df$X_uuid, temp$X_uuid)] >= 1 ~ 1,
              temp$sum_child[match(df$X_uuid, temp$X_uuid)] == 0 ~ 0,
              TRUE ~ NA_real_)

  df$male_hhh_education <-
    ifelse(
      df$male_hhh == 1 & df$lsg_education == 1,
      1,
      0
    )
  df$male_hhh_food <-
    ifelse(
      df$male_hhh == 1 & df$lsg_food == 1,
      1,
      0
    )
  df$male_hhh_health <-
    ifelse(
      df$male_hhh == 1 & df$lsg_health == 1,
      1,
      0
    )
  df$male_hhh_protection <-
    ifelse(
      df$male_hhh == 1 & df$lsg_protection == 1,
      1,
      0
    )
  df$male_hhh_shelter <-
    ifelse(
      df$male_hhh == 1 & df$lsg_snfi == 1,
      1,
      0
    )
  df$male_hhh_wash <-
    ifelse(
      df$male_hhh == 1 & df$lsg_wash == 1,
      1,
      0
    )
  df$male_hhh_lsgs <- ifelse(
    sum_row(
      df$lsg_education,
      df$lsg_food,
      df$lsg_health,
      df$lsg_livelihoods,
      df$lsg_protection,
      df$lsg_snfi,
      df$lsg_wash
    ) >= 1 & df$male_hhh == 1, 1, 0
  )
  df$male_hhh_cg <- ifelse(df$male_hhh == 1 & df$coping_mechanism == 1, 1, 0)

  df$female_hhh_education <-
    ifelse(
      df$female_hhh == 1 & df$lsg_education == 1,
      1,
      0
    )
  df$female_hhh_food <-
    ifelse(
      df$female_hhh == 1 & df$lsg_food == 1,
      1,
      0
    )
  df$female_hhh_health <-
    ifelse(
      df$female_hhh == 1 & df$lsg_health == 1,
      1,
      0
    )
  df$female_hhh_protection <-
    ifelse(
      df$female_hhh == 1 & df$lsg_protection == 1,
      1,
      0
    )
  df$female_hhh_shelter <-
    ifelse(
      df$female_hhh == 1 & df$lsg_snfi == 1,
      1,
      0
    )
  df$female_hhh_wash <-
    ifelse(
      df$female_hhh == 1 & df$lsg_wash == 1,
      1,
      0
    )
  df$female_hhh_lsgs <- ifelse(
    sum_row(
      df$lsg_education,
      df$lsg_food,
      df$lsg_health,
      df$lsg_livelihoods,
      df$lsg_protection,
      df$lsg_snfi,
      df$lsg_wash
    ) >= 1 & df$female_hhh == 1, 1, 0
  )
  df$female_hhh_cg <- ifelse(df$female_hhh == 1 & df$coping_mechanism == 1, 1, 0)

  df$child_hhh_education <-
    ifelse(
      df$child_hhh == 1 & df$lsg_education == 1,
      1,
      0
    )
  df$child_hhh_food <-
    ifelse(
      df$child_hhh == 1 & df$lsg_food == 1,
      1,
      0
    )
  df$child_hhh_health <-
    ifelse(
      df$child_hhh == 1 & df$lsg_health == 1,
      1,
      0
    )
  df$child_hhh_protection <-
    ifelse(
      df$child_hhh == 1 & df$lsg_protection == 1,
      1,
      0
    )
  df$child_hhh_shelter <-
    ifelse(
      df$child_hhh == 1 & df$lsg_snfi == 1,
      1,
      0
    )
  df$child_hhh_wash <-
    ifelse(
      df$child_hhh == 1 & df$lsg_wash == 1,
      1,
      0
    )
  df$child_hhh_lsgs <- ifelse(
    sum_row(
      df$lsg_education,
      df$lsg_food,
      df$lsg_health,
      df$lsg_livelihoods,
      df$lsg_protection,
      df$lsg_snfi,
      df$lsg_wash
    ) >= 1 & df$child_hhh == 1, 1, 0
  )
  df$child_hhh_cg <- ifelse(df$child_hhh == 1 & df$coping_mechanism == 1, 1, 0)

  df$adult_hhh_education <-
    ifelse(
      df$adult_hhh == 1 & df$lsg_education == 1,
      1,
      0
    )
  df$adult_hhh_food <-
    ifelse(
      df$adult_hhh == 1 & df$lsg_food == 1,
      1,
      0
    )
  df$adult_hhh_health <-
    ifelse(
      df$adult_hhh == 1 & df$lsg_health == 1,
      1,
      0
    )
  df$adult_hhh_protection <-
    ifelse(
      df$adult_hhh == 1 & df$lsg_protection == 1,
      1,
      0
    )
  df$adult_hhh_shelter <-
    ifelse(
      df$adult_hhh == 1 & df$lsg_snfi == 1,
      1,
      0
    )
  df$adult_hhh_wash <-
    ifelse(
      df$adult_hhh == 1 & df$lsg_wash == 1,
      1,
      0
    )
  df$adult_hhh_lsgs <- ifelse(
    sum_row(
      df$lsg_education,
      df$lsg_food,
      df$lsg_health,
      df$lsg_livelihoods,
      df$lsg_protection,
      df$lsg_snfi,
      df$lsg_wash
    ) >= 1 & df$adult_hhh == 1, 1, 0
  )
  df$adult_hhh_cg <- ifelse(df$adult_hhh == 1 & df$coping_mechanism == 1, 1, 0)
  
  df$b1_education <-
    ifelse(
      df$b1 == 1 & df$lsg_education == 1,
      1,
      0
    )
  df$b1_food <-
    ifelse(
      df$b1 == 1 & df$lsg_food == 1,
      1,
      0
    )
  df$b1_health <-
    ifelse(
      df$b1 == 1 & df$lsg_health == 1,
      1,
      0
    )
  df$b1_protection <-
    ifelse(
      df$b1 == 1 & df$lsg_protection == 1,
      1,
      0
    )
  df$b1_shelter <-
    ifelse(
      df$b1 == 1 & df$lsg_snfi == 1,
      1,
      0
    )
  df$b1_wash <-
    ifelse(
      df$b1 == 1 & df$lsg_wash == 1,
      1,
      0
    )
  df$b1_lsgs <- ifelse(
    sum_row(
      df$lsg_education,
      df$lsg_food,
      df$lsg_health,
      df$lsg_livelihoods,
      df$lsg_protection,
      df$lsg_snfi,
      df$lsg_wash
    ) >= 1 & df$b1 == 1, 1, 0
  )
  df$b1_cg <- ifelse(df$b1 == 1 & df$coping_mechanism == 1, 1, 0)
  
  df$b2_education <-
    case_when(
      df$b2 == 1 & df$lsg_education == 1 ~ 1,
      is.na(df$b2) ~ NA_real_,
      TRUE ~ 0
    )
  df$b2_food <-
    case_when(
      df$b2 == 1 & df$lsg_food == 1 ~ 1,
      is.na(df$b2) ~ NA_real_,
      TRUE ~ 0
    )
  df$b2_health <-
    case_when(
      df$b2 == 1 & df$lsg_health == 1 ~ 1,
      is.na(df$b2) ~ NA_real_,
      TRUE ~ 0
    )
  df$b2_protection <-
    case_when(
      df$b2 == 1 & df$lsg_protection == 1 ~ 1,
      is.na(df$b2) ~ NA_real_,
      TRUE ~ 0
    )
  df$b2_shelter <-
    case_when(
      df$b2 == 1 & df$lsg_snfi == 1 ~ 1,
      is.na(df$b2) ~ NA_real_,
      TRUE ~ 0
    )
  df$b2_wash <-
    case_when(
      df$b2 == 1 & df$lsg_wash == 1 ~ 1,
      is.na(df$b2) ~ NA_real_,
      TRUE ~ 0
    )
  df$b2_lsgs <- case_when(
    sum_row(
      df$lsg_education,
      df$lsg_food,
      df$lsg_health,
      df$lsg_livelihoods,
      df$lsg_protection,
      df$lsg_snfi,
      df$lsg_wash
    ) >= 1 & df$b2 == 1 ~ 1,
    is.na(df$b2) ~ NA_real_,
    TRUE ~ 0
  )
  df$b2_cg <- case_when(df$b2 == 1 & df$coping_mechanism == 1 ~ 1,
                     is.na(df$b2) ~ NA_real_,
                     TRUE ~ 0
  )
  
  df$b3_education <-
    case_when(
      df$b3 == 1 & df$lsg_education == 1 ~ 1,
      is.na(df$b3) ~ NA_real_,
      TRUE ~ 0
    )
  df$b3_food <-
    case_when(
      df$b3 == 1 & df$lsg_food == 1 ~ 1,
      is.na(df$b3) ~ NA_real_,
      TRUE ~ 0
    )
  df$b3_health <-
    case_when(
      df$b3 == 1 & df$lsg_health == 1 ~ 1,
      is.na(df$b3) ~ NA_real_,
      TRUE ~ 0
    )
  df$b3_protection <-
    case_when(
      df$b3 == 1 & df$lsg_protection == 1 ~ 1,
      is.na(df$b3) ~ NA_real_,
      TRUE ~ 0
    )
  df$b3_shelter <-
    case_when(
      df$b3 == 1 & df$lsg_snfi == 1 ~ 1,
      is.na(df$b3) ~ NA_real_,
      TRUE ~ 0
    )
  df$b3_wash <-
    case_when(
      df$b3 == 1 & df$lsg_wash == 1 ~ 1,
      is.na(df$b3) ~ NA_real_,
      TRUE ~ 0
    )
  df$b3_lsgs <- case_when(
    sum_row(
      df$lsg_education,
      df$lsg_food,
      df$lsg_health,
      df$lsg_livelihoods,
      df$lsg_protection,
      df$lsg_snfi,
      df$lsg_wash
    ) >= 1 & df$b3 == 1 ~ 1,
    is.na(df$b3) ~ NA_real_,
    TRUE ~ 0
  )
  df$b3_cg <- case_when(df$b3 == 1 & df$coping_mechanism == 1 ~ 1,
                     is.na(df$b3) ~ NA_real_,
                     TRUE ~ 0
  )
  
  df$b4_education <-
    case_when(
      df$b4 == 1 & df$lsg_education == 1 ~ 1,
      is.na(df$b4) ~ NA_real_,
      TRUE ~ 0
    )
  df$b4_food <-
    case_when(
      df$b4 == 1 & df$lsg_food == 1 ~ 1,
      is.na(df$b4) ~ NA_real_,
      TRUE ~ 0
    )
  df$b4_health <-
    case_when(
      df$b4 == 1 & df$lsg_health == 1 ~ 1,
      is.na(df$b4) ~ NA_real_,
      TRUE ~ 0
    )
  df$b4_protection <-
    case_when(
      df$b4 == 1 & df$lsg_protection == 1 ~ 1,
      is.na(df$b4) ~ NA_real_,
      TRUE ~ 0
    )
  df$b4_shelter <-
    case_when(
      df$b4 == 1 & df$lsg_snfi == 1 ~ 1,
      is.na(df$b4) ~ NA_real_,
      TRUE ~ 0
    )
  df$b4_wash <-
    case_when(
      df$b4 == 1 & df$lsg_wash == 1 ~ 1,
      is.na(df$b4) ~ NA_real_,
      TRUE ~ 0
    )
  df$b4_lsgs <- case_when(
    sum_row(
      df$lsg_education,
      df$lsg_food,
      df$lsg_health,
      df$lsg_livelihoods,
      df$lsg_protection,
      df$lsg_snfi,
      df$lsg_wash
    ) >= 1 & df$b4 == 1 ~ 1,
    is.na(df$b4) ~ NA_real_,
    TRUE ~ 0
  )
  df$b4_cg <- case_when(df$b4 == 1 & df$coping_mechanism == 1 ~ 1,
                     is.na(df$b4) ~ NA_real_,
                     TRUE ~ 0
  )
  
  df$b5_education <-
    case_when(
      df$b5 == 1 & df$lsg_education == 1 ~ 1,
      is.na(df$b5) ~ NA_real_,
      TRUE ~ 0
    )
  df$b5_food <-
    case_when(
      df$b5 == 1 & df$lsg_food == 1 ~ 1,
      is.na(df$b5) ~ NA_real_,
      TRUE ~ 0
    )
  df$b5_health <-
    case_when(
      df$b5 == 1 & df$lsg_health == 1 ~ 1,
      is.na(df$b5) ~ NA_real_,
      TRUE ~ 0
    )
  df$b5_protection <-
    case_when(
      df$b5 == 1 & df$lsg_protection == 1 ~ 1,
      is.na(df$b5) ~ NA_real_,
      TRUE ~ 0
    )
  df$b5_shelter <-
    case_when(
      df$b5 == 1 & df$lsg_snfi == 1 ~ 1,
      is.na(df$b5) ~ NA_real_,
      TRUE ~ 0
    )
  df$b5_wash <-
    case_when(
      df$b5 == 1 & df$lsg_wash == 1 ~ 1,
      is.na(df$b5) ~ NA_real_,
      TRUE ~ 0
    )
  df$b5_lsgs <- case_when(
    sum_row(
      df$lsg_education,
      df$lsg_food,
      df$lsg_health,
      df$lsg_livelihoods,
      df$lsg_protection,
      df$lsg_snfi,
      df$lsg_wash
    ) >= 1 & df$b5 == 1 ~ 1,
    is.na(df$b5) ~ NA_real_,
    TRUE ~ 0
  )
  df$b5_cg <- case_when(df$b5 == 1 & df$coping_mechanism == 1 ~ 1,
                     is.na(df$b5) ~ NA_real_,
                     TRUE ~ 0
  )
  
  df$b6_education <-
    case_when(
      df$b6 == 1 & df$lsg_education == 1 ~ 1,
      is.na(df$b6) ~ NA_real_,
      TRUE ~ 0
    )
  df$b6_food <-
    case_when(
      df$b6 == 1 & df$lsg_food == 1 ~ 1,
      is.na(df$b6) ~ NA_real_,
      TRUE ~ 0
    )
  df$b6_health <-
    case_when(
      df$b6 == 1 & df$lsg_health == 1 ~ 1,
      is.na(df$b6) ~ NA_real_,
      TRUE ~ 0
    )
  df$b6_protection <-
    case_when(
      df$b6 == 1 & df$lsg_protection == 1 ~ 1,
      is.na(df$b6) ~ NA_real_,
      TRUE ~ 0
    )
  df$b6_shelter <-
    case_when(
      df$b6 == 1 & df$lsg_snfi == 1 ~ 1,
      is.na(df$b6) ~ NA_real_,
      TRUE ~ 0
    )
  df$b6_wash <-
    case_when(
      df$b6 == 1 & df$lsg_wash == 1 ~ 1,
      is.na(df$b6) ~ NA_real_,
      TRUE ~ 0
    )
  df$b6_lsgs <- case_when(
    sum_row(
      df$lsg_education,
      df$lsg_food,
      df$lsg_health,
      df$lsg_livelihoods,
      df$lsg_protection,
      df$lsg_snfi,
      df$lsg_wash
    ) >= 1 & df$b6 == 1 ~ 1,
    is.na(df$b6) ~ NA_real_,
    TRUE ~ 0
  )
  df$b6_cg <- case_when(df$b6 == 1 & df$coping_mechanism == 1 ~ 1,
                     is.na(df$b6) ~ NA_real_,
                     TRUE ~ 0
  )
  
  df$b7_education <-
    case_when(
      df$b7 == 1 & df$lsg_education == 1 ~ 1,
      is.na(df$b7) ~ NA_real_,
      TRUE ~ 0
    )
  df$b7_food <-
    case_when(
      df$b7 == 1 & df$lsg_food == 1 ~ 1,
      is.na(df$b7) ~ NA_real_,
      TRUE ~ 0
    )
  df$b7_health <-
    case_when(
      df$b7 == 1 & df$lsg_health == 1 ~ 1,
      is.na(df$b7) ~ NA_real_,
      TRUE ~ 0
    )
  df$b7_protection <-
    case_when(
      df$b7 == 1 & df$lsg_protection == 1 ~ 1,
      is.na(df$b7) ~ NA_real_,
      TRUE ~ 0
    )
  df$b7_shelter <-
    case_when(
      df$b7 == 1 & df$lsg_snfi == 1 ~ 1,
      is.na(df$b7) ~ NA_real_,
      TRUE ~ 0
    )
  df$b7_wash <-
    case_when(
      df$b7 == 1 & df$lsg_wash == 1 ~ 1,
      is.na(df$b7) ~ NA_real_,
      TRUE ~ 0
    )
  df$b7_lsgs <- case_when(
    sum_row(
      df$lsg_education,
      df$lsg_food,
      df$lsg_health,
      df$lsg_livelihoods,
      df$lsg_protection,
      df$lsg_snfi,
      df$lsg_wash
    ) >= 1 & df$b7 == 1 ~ 1,
    is.na(df$b7) ~ NA_real_,
    TRUE ~ 0
  )
  df$b7_cg <- case_when(df$b7 == 1 & df$coping_mechanism == 1 ~ 1,
                     is.na(df$b7) ~ NA_real_,
                     TRUE ~ 0
  )
  
  df$vulnerability_score_1 <-
    ifelse(        
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash
    ) >= 1 & df$vulnerability_1 == 1, 1, 0)
  
  df$vulnerability_score_2 <-
    ifelse(        
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash
      ) >= 1 & df$vulnerability_2 == 1, 1, 0)
  
  df$vulnerability_score_3 <-
    ifelse(        
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash
      ) >= 1 & df$vulnerability_3 == 1, 1, 0)
  df$vulnerability_score_4 <-
    ifelse(        
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash
      ) >= 1 & df$vulnerability_4 == 1, 1, 0)

  df$cg_lsg_1 <-
    ifelse(
      df$coping_mechanism == 1 &
        sum_row(
          df$lsg_education,
          df$lsg_food,
          df$lsg_health,
          df$lsg_livelihoods,
          df$lsg_protection,
          df$lsg_snfi,
          df$lsg_wash
        ) == 0,
      1,
      0
    )
  
  df$cg_lsg_2 <- ifelse(
    df$coping_mechanism == 1 &
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash
      ) == 0 & df$vulnerability_1 == 1,
    1,
    0
  )
  
  df$cg_lsg_3 <- 
    ifelse(
      df$coping_mechanism == 1 |
        sum_row(
          df$lsg_education,
          df$lsg_food,
          df$lsg_health,
          df$lsg_livelihoods,
          df$lsg_protection,
          df$lsg_snfi,
          df$lsg_wash
        ) >= 1,
      1,
      0
    )
  
  df$cg_lsg_4 <- 
    ifelse(
      df$coping_mechanism == 0 &
        sum_row(
          df$lsg_education,
          df$lsg_food,
          df$lsg_health,
          df$lsg_livelihoods,
          df$lsg_protection,
          df$lsg_snfi,
          df$lsg_wash
        ) >= 1,
      1,
      0
    )
  df$seven_lsg   <- 
    ifelse(        
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash
      ) == 7,
      1,
      0
    )
  df$six_lsg   <- 
    ifelse(        
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash
      ) == 6,
      1,
      0
    )
  df$five_lsg   <- 
    ifelse(        
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash
      ) == 5,
      1,
      0
    )
  df$four_lsg   <- 
    ifelse(        
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash
      ) == 4,
      1,
      0
    )
  df$three_lsg   <- 
    ifelse(        
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash
      ) == 3,
      1,
      0
    )
  df$two_lsg   <- 
    ifelse(        
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash
      ) == 2,
      1,
      0
    )
  df$one_lsg   <- 
    ifelse(        
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash
      ) == 1,
      1,
      0
    )
  df$zero_lsg   <- 
    ifelse(        
      sum_row(
        df$lsg_education,
        df$lsg_food,
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash
      ) == 0,
      1,
      0
    )
  df$lsg_vulnerable <- 
    ifelse(      
      sum_row(
        df$lsg_education,
        df$lsg_food,    
        df$lsg_health,
        df$lsg_livelihoods,
        df$lsg_protection,
        df$lsg_snfi,
        df$lsg_wash
        ) >= 1 & df$vulnerability_score >= 3, 1, 0)
  
  
  df$lsg_education_livelihoods    <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1, 1, 0)
  df$lsg_education_food           <- ifelse(df$lsg_education == 1 & df$lsg_food == 1, 1, 0)
  df$lsg_education_protection     <- ifelse(df$lsg_education == 1 & df$lsg_protection == 1, 1, 0)
  df$lsg_education_health         <- ifelse(df$lsg_education == 1 & df$lsg_health == 1, 1, 0)
  df$lsg_education_snfi           <- ifelse(df$lsg_education == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_education_wash           <- ifelse(df$lsg_education == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_livelihoods_food         <- ifelse(df$lsg_livelihoods == 1 & df$lsg_food == 1, 1, 0)
  df$lsg_livelihoods_protection   <- ifelse(df$lsg_livelihoods == 1 & df$lsg_protection == 1, 1, 0)
  df$lsg_livelihoods_health       <- ifelse(df$lsg_livelihoods == 1 & df$lsg_health == 1, 1, 0)
  df$lsg_livelihoods_snfi         <- ifelse(df$lsg_livelihoods == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_livelihoods_wash         <- ifelse(df$lsg_livelihoods == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_food_protection          <- ifelse(df$lsg_food == 1 & df$lsg_protection == 1, 1, 0)
  df$lsg_food_health              <- ifelse(df$lsg_food == 1 & df$lsg_health == 1, 1, 0)
  df$lsg_food_snfi                <- ifelse(df$lsg_food == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_food_wash                <- ifelse(df$lsg_food == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_protection_health        <- ifelse(df$lsg_protection == 1 & df$lsg_health == 1, 1, 0)
  df$lsg_protection_snfi          <- ifelse(df$lsg_protection == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_protection_wash          <- ifelse(df$lsg_protection == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_health_snfi              <- ifelse(df$lsg_health == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_health_wash              <- ifelse(df$lsg_health == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_snfi_wash                <- ifelse(df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  
  df$lsg_education_livelihoods_food      <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_food == 1, 1, 0)
  df$lsg_education_livelihoods_protection<- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_protection == 1, 1, 0)
  df$lsg_education_livelihoods_health    <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_health == 1, 1, 0)
  df$lsg_education_livelihoods_snfi      <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_education_livelihoods_wash      <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_food_protection       <- ifelse(df$lsg_education == 1 & df$lsg_food == 1 & df$lsg_protection == 1, 1, 0)
  df$lsg_education_food_health           <- ifelse(df$lsg_education == 1 & df$lsg_food == 1 & df$lsg_health == 1, 1, 0)
  df$lsg_education_food_snfi             <- ifelse(df$lsg_education == 1 & df$lsg_food == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_education_food_wash             <- ifelse(df$lsg_education == 1 & df$lsg_food == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_protection_health     <- ifelse(df$lsg_education == 1 & df$lsg_protection == 1 & df$lsg_health == 1, 1, 0)
  df$lsg_education_protection_snfi       <- ifelse(df$lsg_education == 1 & df$lsg_protection == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_education_protection_wash       <- ifelse(df$lsg_education == 1 & df$lsg_protection == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_health_snfi           <- ifelse(df$lsg_education == 1 & df$lsg_health == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_education_health_wash           <- ifelse(df$lsg_education == 1 & df$lsg_health == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_snfi_wash             <- ifelse(df$lsg_education == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_livelihoods_food_protection     <- ifelse(df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_protection == 1, 1, 0)
  df$lsg_livelihoods_food_health         <- ifelse(df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_health == 1, 1, 0)
  df$lsg_livelihoods_food_snfi           <- ifelse(df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_livelihoods_food_wash           <- ifelse(df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_livelihoods_protection_health   <- ifelse(df$lsg_livelihoods == 1 & df$lsg_protection == 1 & df$lsg_health == 1, 1, 0)
  df$lsg_livelihoods_protection_snfi     <- ifelse(df$lsg_livelihoods == 1 & df$lsg_protection == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_livelihoods_protection_wash     <- ifelse(df$lsg_livelihoods == 1 & df$lsg_protection == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_livelihoods_health_snfi         <- ifelse(df$lsg_livelihoods == 1 & df$lsg_health == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_livelihoods_health_wash         <- ifelse(df$lsg_livelihoods == 1 & df$lsg_health == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_livelihoods_snfi_wash           <- ifelse(df$lsg_livelihoods == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_food_protection_health          <- ifelse(df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_health == 1, 1, 0)
  df$lsg_food_protection_snfi            <- ifelse(df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_food_protection_wash            <- ifelse(df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_food_health_snfi                <- ifelse(df$lsg_food == 1 & df$lsg_health == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_food_health_wash                <- ifelse(df$lsg_food == 1 & df$lsg_health == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_food_snfi_wash                  <- ifelse(df$lsg_food == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_protection_health_snfi          <- ifelse(df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_protection_health_wash          <- ifelse(df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_protection_snfi_wash            <- ifelse(df$lsg_protection == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_health_snfi_wash                <- ifelse(df$lsg_health == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  
  df$lsg_education_livelihoods_food_protection    <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_protection == 1, 1, 0)
  df$lsg_education_livelihoods_food_health        <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_health == 1, 1, 0)
  df$lsg_education_livelihoods_food_snfi          <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_education_livelihoods_food_wash          <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_livelihoods_protection_health  <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_protection == 1 & df$lsg_health == 1, 1, 0)
  df$lsg_education_livelihoods_protection_snfi    <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_protection == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_education_livelihoods_protection_wash    <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_protection == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_livelihoods_health_snfi        <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_health == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_education_livelihoods_health_wash        <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_health == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_livelihoods_snfi_wash          <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_food_protection_health         <- ifelse(df$lsg_education == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_health == 1, 1, 0)
  df$lsg_education_food_protection_snfi           <- ifelse(df$lsg_education == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_education_food_protection_wash           <- ifelse(df$lsg_education == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_food_health_snfi               <- ifelse(df$lsg_education == 1 & df$lsg_food == 1 & df$lsg_health == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_education_food_health_wash               <- ifelse(df$lsg_education == 1 & df$lsg_food == 1 & df$lsg_health == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_food_snfi_wash                 <- ifelse(df$lsg_education == 1 & df$lsg_food == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_protection_health_snfi         <- ifelse(df$lsg_education == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_education_protection_health_wash         <- ifelse(df$lsg_education == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_protection_snfi_wash           <- ifelse(df$lsg_education == 1 & df$lsg_protection == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_health_snfi_wash               <- ifelse(df$lsg_education == 1 & df$lsg_health == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_livelihoods_food_protection_health       <- ifelse(df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_health == 1, 1, 0)
  df$lsg_livelihoods_food_protection_snfi         <- ifelse(df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_livelihoods_food_protection_wash         <- ifelse(df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_livelihoods_food_health_snfi             <- ifelse(df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_health == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_livelihoods_food_health_wash             <- ifelse(df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_health == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_livelihoods_food_snfi_wash               <- ifelse(df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_livelihoods_protection_health_snfi       <- ifelse(df$lsg_livelihoods == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_livelihoods_protection_health_wash       <- ifelse(df$lsg_livelihoods == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_livelihoods_protection_snfi_wash         <- ifelse(df$lsg_livelihoods == 1 & df$lsg_protection == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_livelihoods_health_snfi_wash             <- ifelse(df$lsg_livelihoods == 1 & df$lsg_health == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_food_protection_health_snfi              <- ifelse(df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_food_protection_health_wash              <- ifelse(df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_food_protection_snfi_wash                <- ifelse(df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_food_health_snfi_wash                    <- ifelse(df$lsg_food == 1 & df$lsg_health == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_protection_health_snfi_wash              <- ifelse(df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  
  
  df$lsg_education_livelihoods_food_protection_health     <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_health == 1, 1, 0)
  df$lsg_education_livelihoods_food_protection_snfi       <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_education_livelihoods_food_protection_wash       <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_livelihoods_food_health_snfi           <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_health == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_education_livelihoods_food_health_wash           <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_health == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_livelihoods_food_snfi_wash             <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_livelihoods_protection_health_snfi     <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_education_livelihoods_protection_health_wash     <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_livelihoods_protection_snfi_wash       <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_protection == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_livelihoods_health_snfi_wash           <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_health == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_food_protection_health_snfi            <- ifelse(df$lsg_education == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_education_food_protection_health_wash            <- ifelse(df$lsg_education == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_food_protection_snfi_wash              <- ifelse(df$lsg_education == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_food_health_snfi_wash                  <- ifelse(df$lsg_education == 1 & df$lsg_food == 1 & df$lsg_health == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_protection_health_snfi_wash            <- ifelse(df$lsg_education == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_livelihoods_food_protection_health_snfi          <- ifelse(df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_livelihoods_food_protection_health_wash          <- ifelse(df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_livelihoods_food_protection_snfi_wash            <- ifelse(df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_livelihoods_food_health_snfi_wash                <- ifelse(df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_health == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_livelihoods_protection_health_snfi_wash          <- ifelse(df$lsg_livelihoods == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_food_protection_health_snfi_wash                 <- ifelse(df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  
  df$lsg_education_livelihoods_food_protection_health_snfi      <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_snfi == 1, 1, 0)
  df$lsg_education_livelihoods_food_protection_health_wash      <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_livelihoods_food_protection_snfi_wash        <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_livelihoods_food_health_snfi_wash            <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_health == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_livelihoods_protection_health_snfi_wash      <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_education_food_protection_health_snfi_wash             <- ifelse(df$lsg_education == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  df$lsg_livelihoods_food_protection_health_snfi_wash           <- ifelse(df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  
  df$lsg_education_livelihoods_food_protection_health_snfi_wash <- ifelse(df$lsg_education == 1 & df$lsg_livelihoods == 1 & df$lsg_food == 1 & df$lsg_protection == 1 & df$lsg_health == 1 & df$lsg_snfi == 1 & df$lsg_wash == 1, 1, 0)
  
  
  return(df)
}