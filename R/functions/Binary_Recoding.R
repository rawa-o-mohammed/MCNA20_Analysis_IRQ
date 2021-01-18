round2 <- function(x, n = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10 ^ n
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10 ^ n
  z <- z * posneg
  return(z)
}

#r <- response
#loop <- loop


recoding_preliminary <- function(r, loop) {
  # get sex column form member sheet
  loop_hoh <- loop[which(loop$relationship == "head"),]
  
  
  loop$plw <- case_when(
    loop$pregnant_lactating == "yes" ~ 1,
    loop$pregnant_lactating == "no"  ~ 0,
    TRUE ~ NA_real_
  )
  
  r <- r %>%
    mutate(sex = loop_hoh$sex[match(r$X_uuid, loop_hoh$`X_uuid`)])
  
  loop_children <- loop[which(loop$age < 18 & loop$age > 5),]
  female_headed <-
    response[which(response$X_uuid %in% loop$X_uuid[which(loop$sex == "female" &
                                                            loop$relationship == "head")]), ]
  r$gender_hhh <- loop_hoh$sex[match(r$X_uuid, loop_hoh$X_uuid)]
  
  ############################################### a #############################
  r$a7        <- as.numeric(r$num_hh_member)
  r$a8        <- as.numeric(r$num_family_member)
  r$a9_male   <-
    as.numeric(r$tot_male) / (as.numeric(r$tot_male) + as.numeric(r$tot_female))
  r$a9_female <-
    as.numeric(r$tot_female) / (as.numeric(r$tot_male) + as.numeric(r$tot_female))
  
  r$a10_child <-
    as.numeric(r$tot_child) / (as.numeric(r$tot_male) +  as.numeric(r$tot_female))
  r$a10_adult <-
    (as.numeric(r$male_18_59_calc) +  as.numeric(r$female_18_59_calc)) / (as.numeric(r$tot_male) +  as.numeric(r$tot_female))
  r$a10_elder <-
    (as.numeric(r$male_60_calc) + as.numeric(r$female_60_calc)) / (as.numeric(r$tot_male) + as.numeric(r$tot_female))
  
  r$a11 <-
    ifelse(
      loop_hoh$marital_status[match(r$X_uuid, loop_hoh$`X_uuid`)] %in%
        c("single", "separated", "widowed", "divorced"),
      1,
      ifelse(loop_hoh$marital_status[match(r$X_uuid, loop_hoh$`X_uuid`)] %in%
               c(NA, ""), NA, 0)
    )
  
  r$a12 <-
    ifelse(
      loop_children$marital_status[match(r$X_uuid, loop_children$`X_uuid`)] %in% c("married", "widowed", "divorced", "separated") ,
      1,
      0
    )
  
  r$a13 <- apply(
    r,
    1,
    FUN = function(x) {
      ifelse(any(loop$age[which(loop$X_uuid == x["X_uuid"])] < 18 &
                   loop$work[which(loop$X_uuid == x["X_uuid"])] == "yes"), 1, 0)
    }
  )
  
  r$a14 <- ifelse(r$gender_hhh == "female", 1, 0)
  
  
  r$a15_i     <- r$why_not_return.fear_trauma
  r$a15_ii    <- r$why_not_return.lack_of_security_forces
  r$a15_iii   <- r$why_not_return.presence_of_mines
  r$a15_iv    <- r$why_not_return.discrimination
  r$a15_v     <- r$why_not_return.lack_security_women
  r$a15_vi    <- r$why_not_return.movement_restrictions
  r$a15_vii   <- r$why_not_return.no_personal_id
  r$a15_viii  <- r$why_not_return.no_transport_return
  r$a15_ix    <- r$why_not_return.no_money_return
  r$a15_x     <- r$why_not_return.lack_livelihoods_aoo
  r$a15_xi    <- r$why_not_return.hh_assets_stolen_damaged
  r$a15_xii   <- r$why_not_return.house_land_occupied
  r$a15_xiii  <- r$why_not_return.house_damaged_destroyed
  r$a15_xiv   <- r$why_not_return.lack_court
  r$a15_xv    <- r$why_not_return.local_markets_not_working
  r$a15_xvi   <- r$why_not_return.basic_services_not_enough
  r$a15_xvii  <- r$why_not_return.lack_of_education_oppotunities
  r$a15_xviii <- r$why_not_return.immediate_family_wont_return
  r$a15_xix   <- r$why_not_return.health_conditions
  r$a15_xx    <- r$why_not_return.children_enrolled_in_displacement
  r$a15_xxi   <- r$why_not_return.living_conditions_better
  r$a15_xxii  <- r$why_not_return.other
  r$a15_xxiii <- r$why_not_return.do_not_know
  r$a15_xxiv  <- r$why_not_return.decline_to_answer
  
  r$a16 <- ifelse(
    r$shelter_type %in%
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
  
  r$a17 <- r$enclosure_issues.none
  
  r$a22 <-
    case_when(r$displaced_again == "yes" ~ 1,
              is.na(r$displaced_again) ~ NA_real_,
              TRUE ~  0)
  
  r$a24 <-
    ifelse(
      r$movement_intentions_3 %in% c("remain") &
        r$population_group == "idp_out_camp",
      1,
      ifelse(
        r$movement_intentions_12 %in% c(
          "return",
          "move_inside_iraq",
          "move_outside_iraq",
          "wait_to_decide"
        ) &
          r$population_group == "idp_out_camp",
        0,
        NA_real_
      )
    )
  
  r$a25 <-
    ifelse(
      r$movement_intentions_12 %in% c("current") &
        r$population_group == "idp_out_camp",
      1,
      ifelse(
        r$movement_intentions_12 %in% c("return", "move_iraq", "move_other") &
          r$population_group == "idp_out_camp",
        0,
        NA_real_
      )
    )

  r$a26_i    <- r$reason_to_return_to_aoo.security_stable
  r$a26_ii   <- r$reason_to_return_to_aoo.uxo
  r$a26_iii  <- r$reason_to_return_to_aoo.other_members_returned
  r$a26_iv   <-
    r$reason_to_return_to_aoo.livelihood_availability_there
  r$a26_v    <- r$reason_to_return_to_aoo.basic_services
  r$a26_vi   <- r$reason_to_return_to_aoo.emotional_desire
  r$a26_vii  <- r$reason_to_return_to_aoo.secure_house_land
  r$a26_viii <- r$reason_to_return_to_aoo.secure_civil_doc
  r$a26_ix   <- r$reason_to_return_to_aoo.limited_livelihoods_ao
  r$a26_x    <- r$reason_to_return_to_aoo.limited_services
  r$a26_xi   <- r$reason_to_return_to_aoo.no_safe_aod
  r$a26_xii  <- r$reason_to_return_to_aoo.no_integrated_aod
  r$a26_xiii <- r$reason_to_return_to_aoo.facing_eviction
  r$a26_xiv  <- r$reason_to_return_to_aoo.forced_security
  r$a26_xv   <- r$reason_to_return_to_aoo.lack_security_women
  
  r$a27 <-
    ifelse(
      r$movement_intentions_3 %in% c("remain") &
        r$population_group == "returnee",
      1,
      ifelse(
        r$movement_intentions_12 %in% c("move_inside_iraq", "move_outside_iraq", "wait_to_decide") &
          r$population_group == "returnee",
        0,
        NA_real_
      )
    )
  
  r$a28 <-
    ifelse(
      r$movement_intentions_b12 %in% c("remain") &
        r$population_group == "returnee",
      1,
      ifelse(
        r$movement_intentions_b12 %in% c("move_inside_iraq", "move_outside_iraq", "wait_to_decide") &
          r$population_group == "returnee",
        0,
        NA_real_
      )
    )
  
  r$a29 <- case_when(r$local_integration == "yes" ~ 1,
                     r$local_integration %in% c("no") ~ 0,
                     TRUE ~ NA_real_)
  
  
  # ################################################### b #######################################
  
  r$b1     <- ifelse(r$inc_employment_pension < 480000, 1, 0)
  
  r$b2     <-
    ifelse(r$primary_livelihood.ngo_charity_assistance == 1, 1, 0)
  
  r$b3     <-
    case_when(
      r$inc_employment_pension < 480000 &
        r$gender_hhh == "female" &
        r$a11 == 1 ~ 1,
      r$inc_employment_pension >= 480000 &
        r$gender_hhh == "female" &
        r$a11 == 1 ~ 0,
      TRUE ~ NA_real_
    )
  
  r$b5     <-
    ifelse(
      r$selling_assets %in% c("no_already_did", "yes") |
        r$borrow_debt  %in% c("no_already_did", "yes") |
        r$reduce_spending %in% c("no_already_did", "yes"),
      1,
      0
    )
  r$b6     <-
    ifelse(
      r$selling_transportation_means %in% c("no_already_did", "yes") |
        r$change_place  %in% c("no_already_did", "yes") |
        r$child_work %in% c("no_already_did", "yes"),
      1,
      0
    )
  r$b7     <-
    ifelse(
      r$child_dropout_school %in% c("no_already_did", "yes") |
        r$adult_risky  %in% c("no_already_did", "yes") |
        r$family_migrating %in% c("no_already_did", "yes") |
        r$child_forced_marriage %in% c("no_already_did", "yes"),
      1,
      0
    )
  
  # ################################################### c #######################################
  
  
  r$c2     <-
    ifelse(r$injured_explosive %in% c("killed", "injured"), 1, 0)
  
  r$c3   <-
    ifelse(
      r$difficulty_seeing %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
        r$difficulty_hearing %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
        r$difficulty_walking %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
        r$difficulty_remembering %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
        r$difficulty_washing %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
        r$difficulty_communicating %in% c("a_lot_of_difficulty", "cannot_do_at_all"),
      1,
      0
    )
  
  r$c9     <- ifelse(r$difficulty_accessing_services == "yes", 1, 0)
  
  PLW <-
    as.data.frame(loop %>% dplyr::group_by(X_uuid) %>% dplyr::summarize(sum(plw, na.rm = T)))
  
  
  
  r$c11    <- PLW[match(r$X_uuid, PLW$X_uuid), 2]
  # rm(PLW)
  
  # ################################################### d ########################################
  
  r$d1_i    <- r$info_aid.aid
  r$d1_ii   <- r$info_aid.safety
  r$d1_iii  <- r$info_aid.housing
  r$d1_iv   <- r$info_aid.livelihoods
  r$d1_v    <- r$info_aid.water
  r$d1_vi   <- r$info_aid.electricity
  r$d1_vii  <- r$info_aid.education
  r$d1_viii <- r$info_aid.healthcare
  r$d1_ix   <- r$info_aid.legal
  r$d1_x    <- r$info_aid.property
  r$d1_xi   <- r$info_aid.uxo
  r$d1_xii  <- r$info_aid.documentation
  r$d1_xiii <- r$info_aid.none
  
  
  r$d2_i    <- r$info_provider.ngo
  r$d2_ii   <- r$info_provider.friends_in_aoo
  r$d2_iii  <- r$info_provider.friends_visited_aoo
  r$d2_iv   <- r$info_provider.friends_not_been_in_aoo
  r$d2_v    <- r$info_provider.local_authorities
  r$d2_vi   <- r$info_provider.national_authorities
  r$d2_vii  <- r$info_provider.religious
  r$d2_viii <- r$info_provider.mukhtars
  r$d2_ix   <- r$info_provider.sector_leaders
  r$d2_x    <- r$info_provider.schools
  
  r$d8_i    <- r$info_provider_ret.friends
  r$d8_ii    <- r$info_provider_ret.local_authorities
  r$d8_iii    <- r$info_provider_ret.mukhtars
  r$d8_iv    <- r$info_provider_ret.national_authorities
  r$d8_v    <- r$info_provider_ret.ngo
  r$d8_vi    <- r$info_provider_ret.religious
  r$d8_vii    <- r$info_provider_ret.schools
  r$d8_viii    <- r$info_provider_ret.sector_leaders
  
  r$d3_i    <- r$info_mode.mobile
  r$d3_ii   <- r$info_mode.direct_obs
  r$d3_iii  <- r$info_mode.face_cmmunic
  r$d3_iv   <- r$info_mode.television
  r$d3_v    <- r$info_mode.telephone
  r$d3_vi   <- r$info_mode.facebook_app
  r$d3_vii  <- r$info_mode.facebook_messenger
  r$d3_viii <- r$info_mode.whatsapp
  r$d3_ix   <- r$info_mode.viber
  r$d3_x    <- r$info_mode.other_social
  r$d3_xi   <- r$info_mode.notice_board
  r$d3_xii  <- r$info_mode.newspapers
  r$d3_xiii <- r$info_mode.leaflet
  r$d3_xiv  <- r$info_mode.loud_speakers
  r$d3_xv   <- r$info_mode.radio
  
  r$d4      <- ifelse(r$aid_received == "yes", 1, 0)
  
  r$d5_i    <- r$aid_type.cash
  r$d5_ii   <- r$aid_type.food
  r$d5_iii  <- r$aid_type.water
  r$d5_iv   <- r$aid_type.fuel
  r$d5_v    <- r$aid_type.shelter
  r$d5_vi   <- r$aid_type.seasonal_items
  r$d5_vii  <- r$aid_type.healthcare
  r$d5_viii <- r$aid_type.other_nfi
  r$d5_ix   <- r$aid_type.education
  r$d5_x    <- r$aid_type.protection
  
  r$d6      <-
    case_when(r$aid_satisfaction == "yes" ~ 1,
              is.na(r$aid_satisfaction) ~ NA_real_ ,
              TRUE ~ 0)
  
  r$d7      <- case_when(
    r$aid_not_satisfied.quantity == 1 ~ 1,
    (r$aid_received == "yes" &
       r$aid_satisfaction == "yes") |
      r$aid_not_satisfied.quantity == 0 ~ 0,
    TRUE ~  NA_real_
  )
  
  r$d10      <- case_when(
    r$aid_workers_satisfied == "no" ~ 1,
    r$aid_received == "yes" &
      r$aid_workers_satisfied == "yes" ~ 0,
    TRUE ~  NA_real_
  )
  
  r$d12      <- case_when(r$complaint_mechanisms == "yes" ~ 1,
                          r$complaint_mechanisms == "no"  ~ 0,
                          TRUE ~  NA_real_)
  
  r$d15   <-
    ifelse(r$covid_info_need == 'yes' &
             r$covid_info_type.prevention == 1,
           1,
           0)
  
  ##################################################### F ########################################
  r$f4 <-
    ifelse(r$distance_clinic %in% c("less_15", "less_30", "less_hour"),
           1,
           0)
  
  
  r$f7 <- ifelse(
    r$property_damaged == "yes" &
      r$aware_compensation == "yes" &
      r$applied_compensation == "yes",
    ifelse(r$received_compensation == "yes", 1, 0),
    NA_real_
  )
  
  r$f7b <- case_when(
    r$complaint_mechanisms == "yes" ~ 1,
    is.na(r$complaint_mechanisms) ~ NA_real_,
    TRUE ~ 0
  )
  # ################################################### g ########################################
  
  
  children_attend_ed <- loop_children %>%
    mutate(
      not_attending_either =
        case_when(attend_formal_ed == "no" &
                    attend_informal_ed == "no" ~ 1, TRUE ~ 0),
      attending_either =
        case_when(
          attend_formal_ed == "yes" |
            attend_informal_ed == "yes" ~ 1,
          TRUE ~ 0
        ),
      attend_formal_ed =
        case_when(
          attend_formal_ed == "yes" ~ 1,
          attend_formal_ed %in% c("no") ~ 0,
          TRUE ~ NA_real_
        ),
      attend_informal_ed =
        case_when(
          attend_informal_ed == "yes" ~ 1,
          attend_informal_ed %in% c("no") ~ 0,
          TRUE ~ NA_real_
        )
    )
  
  children_attend_ed <- children_attend_ed %>%
    dplyr::group_by(X_uuid) %>%
    dplyr::summarize(
      num_attend_formal = sum(attend_formal_ed, na.rm = TRUE),
      num_attend_informal = sum(attend_informal_ed, na.rm = TRUE),
      num_not_attend_either = sum(not_attending_either, na.rm = TRUE),
      num_attending_either = sum(attending_either, na.rm = TRUE),
      num_schoolage_child = n()
    )
  
  r <- left_join(r, children_attend_ed, by = "X_uuid")
  
  r$g4 <-
    ifelse(r$num_not_attend_either > 0, 1, 0)
  
  r <- r %>%
    mutate(
      perc_attend_formal = num_attend_formal / num_schoolage_child,
      perc_attend_either = num_attending_either / num_schoolage_child
    )
  
  r$g5 <- ifelse(r$perc_attend_formal >= 1, 1, 0)
  r$g6 <- ifelse(r$perc_attend_either >= 1, 1, 0)
  
  
  r$g7_i    <- r$reasons_not_attend.school_closed
  r$g7_ii   <- r$reasons_not_attend.not_safe
  r$g7_iii  <- r$reasons_not_attend.cannot_afford
  r$g7_iv   <- r$reasons_not_attend.impossible_to_enrol
  r$g7_v    <- r$reasons_not_attend.cannot_go_physically
  r$g7_vi   <- r$reasons_not_attend.overcrowded
  r$g7_vii  <- r$reasons_not_attend.lack_of_staff
  r$g7_viii <- r$reasons_not_attend.poor_infrastructure
  r$g7_ix   <- r$reasons_not_attend.curriculum
  r$g7_x    <- r$reasons_not_attend.children_working
  r$g7_xi   <- r$reasons_not_attend.parental_refusal
  r$g7_xii  <- r$reasons_not_attend.uninterested
  r$g7_xiii <- r$reasons_not_attend.lack_doc
  r$g7_xiv  <- r$reasons_not_attend.other
  
  
  r$g8 <-
    ifelse(
      r$primary_school_place %in% c("between_2_5", "within_2km") &
        r$secondary_school_place %in% c("between_2_5", "within_2km"),
      1,
      0
    )
  
  r$g9 <- ifelse(r$covid_dropout > 0, 1, 0)
  
  r$stress <-
    ifelse(
      r$selling_assets %in% c("no_already_did", "yes") |
        r$borrow_debt  %in% c("no_already_did", "yes") |
        r$reduce_spending %in% c("no_already_did", "yes") |
        r$spending_savings %in% c("no_already_did", "yes")   ,
      1,
      0
    )
  r$crisis <-
    ifelse(
      r$selling_transportation_means %in% c("no_already_did", "yes") |
        r$change_place  %in% c("no_already_did", "yes") |
        r$child_work %in% c("no_already_did", "yes"),
      1,
      0
    )
  r$emergency <-
    ifelse(
      r$child_dropout_school %in% c("no_already_did", "yes") |
        r$adult_risky  %in% c("no_already_did", "yes") |
        r$family_migrating %in% c("no_already_did", "yes") |
        r$child_forced_marriage %in% c("no_already_did", "yes"),
      1,
      0
    )
  
  #FOOD EXPENDITURE SHARE
  r$food_share <-
    as.numeric(r$food_exp) / as.numeric(r$tot_expenses)
  
  #FOOD CONSUMPTIONS SCORE
  r$fcs <-
    (as.numeric(r$cereals) * 2) + (as.numeric(r$nuts_seed) * 3) + (as.numeric(r$milk_dairy) * 4) + (as.numeric(r$meat) * 4) +
    as.numeric(r$vegetables) + as.numeric(r$fruits) + (as.numeric(r$oil_fats) * 0.5) + (as.numeric(r$sweets) * 0.5)
  
  r$livelihood_strategies <-
    case_when(r$emergency == 1 ~ 4, r$crisis == 1 ~ 3, r$stress == 1 ~ 2, TRUE ~ 1)
  r$food_share_strategies <-
    case_when(
      r$food_share < 0.5 ~ 1,
      between(r$food_share, 0.5, 0.6499) ~ 2,
      between(r$food_share, 0.65, 0.7499) ~ 3,
      r$food_share >= 0.75 ~ 4
    )
  r$fcs_strategies <-
    case_when(r$fcs < 21 ~ 4, between(r$fcs, 21, 35) ~ 3, TRUE ~ 1)
  
  r$mean_coping_capacity <-
    mean_row(r$livelihood_strategies, r$food_share_strategies, na.rm = TRUE)
  r$g14 <-
    round2(mean_row(r$mean_coping_capacity, r$fcs_strategies, na.rm = TRUE))
  r <-
    dplyr::select(
      r,
      -c(
        "stress",
        "crisis",
        "emergency",
        "food_share",
        "fcs",
        "food_share_strategies",
        "fcs_strategies",
        "mean_coping_capacity"
      )
    )
  
  fsc <- r %>%
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
  
  r$g15_i <-
    ifelse(fsc$hhs[match(r$X_uuid, fsc$X_uuid)] <= 1, 1, 0)
  
  r$g15_ii <-
    ifelse(between(fsc$hhs[match(r$X_uuid, fsc$X_uuid)], 2, 3), 1, 0)
  
  r$g15_iii <-
    ifelse(fsc$hhs[match(r$X_uuid, fsc$X_uuid)] >= 4, 1, 0)
  
  # rm(fsc)
  r$g19 <-
    ifelse((
      loop_children$attend_formal_ed[match(r$X_uuid, loop_children$X_uuid)] == "yes" |
        loop_children$attend_informal_ed[match(r$X_uuid, loop_children$X_uuid)] == "yes"
    ) &
      loop_children$work[match(r$X_uuid, loop_children$X_uuid)] == "yes",
    1,
    0
    )
  
  
  r$g25 <-
    ifelse(
      r$distance_hospital %in% c("less_15", "less_30", "less_hour") |
        r$distance_clinic %in% c("less_15", "less_30", "less_hour"),
      1,
      0
    )
  
  r$g26 <-
    ifelse(
      r$distance_hospital %in% c("less_15", "less_30", "less_hour") &
        r$hospital_emergency_ser == "yes" &
        r$hospital_maternity_ser == "yes" &
        r$hospital_surgical_ser == "yes" &
        r$hospital_pediatric_ser == "yes",
      1,
      0
    )
  
  r$g28 <-
    ifelse(r$distance_clinic %in% c("less_15", "less_30", "less_hour"),
           1,
           0)
  r$g29 <-
    ifelse(r$distance_hospital %in% c("less_15", "less_30", "less_hour"),
           1,
           0)
  
  r$g32 <- ifelse(r$women_specialised_services == "yes", 1, 0)
  
  r$g34_i    <- r$health_barriers.cost
  r$g34_ii   <- r$health_barriers.unqualified_staff
  r$g34_iii  <- r$health_barriers.civ_docs_problems
  r$g34_iv   <- r$health_barriers.no_referral_phc
  r$g34_v    <- r$health_barriers.phc_closed
  r$g34_vi   <- r$health_barriers.distance_to_treatmentcenter
  r$g34_vii  <- r$health_barriers.refused_treatment
  r$g34_viii <- r$health_barriers.no_medicine
  r$g34_ix   <- r$health_barriers.no_offered_treatment
  r$g34_x    <- r$health_barriers.not_inclusive
  r$g34_xi   <- r$health_barriers.no_fem_staff
  
  r$g35 <- apply(
    r,
    1,
    FUN = function(x) {
      ifelse(sum(loop$health_issue.chronic[which(loop$X_uuid == x["X_uuid"])], na.rm = T) > 0, 1, 0)
    }
  )
  
  r$g37 <- ifelse(r$how_much_debt > 505000, 1, 0)
  
  r$g38 <-
    ifelse(
      r$reasons_for_debt %in% c("basic_hh_expenditure", "education", "food", "health"),
      1,
      0
    )
  
  r$g39 <- ifelse(r$covid_loss_job == "yes"  ,
                  1,
                  0)
  
  r$g41 <- ifelse(r$market_place %in% c("less_15", "less_30"), 1, 0)
  
  r$g44 <-
    ifelse(loop$age[match(r$X_uuid, loop$`X_uuid`)] >= 18 &
             loop$actively_seek_work[match(r$X_uuid, loop$`X_uuid`)] == "yes", 1, 0)
  
  
  r$g45_i    <- r$employment_primary_barriers.increased_competition
  r$g45_ii   <- r$employment_primary_barriers.jobs_far
  r$g45_iii  <- r$employment_primary_barriers.only_low_available
  r$g45_iv   <-
    r$employment_primary_barriers.underqualified_for_jobs
  r$g45_v    <- r$employment_primary_barriers.lack_of_connections
  r$g45_vi   <- r$employment_primary_barriers.lack_jobs_women
  r$g45_vii  <- r$employment_primary_barriers.none
  r$g45_viii <- r$employment_primary_barriers.other
  
  
  r$g46 <- ifelse(r$employment_seasonal == "yes", 1, 0)
  
  r$g47_i <-
    ifelse(between(loop$age[match(r$X_uuid, loop$`X_uuid`)], 18, 59) &
             loop$sex[match(r$X_uuid, loop$`X_uuid`)] == "female" &
             loop$work[match(r$X_uuid, loop$`X_uuid`)] == "yes",
           1,
           0)
  
  r$g51 <- ifelse(
    r$pds_card == "no" |
      r$id_card_a18 == "no" |
      r$nationality_cert_a18 == "no" |
      r$id_card_u18 == "no" |
      r$nationality_cert_u18 == "no" |
      r$birth_cert_u18 == "no",
    1,
    0
  )
  
  r$g51b <-
    ifelse(r$id_card_u18 == "no" |
             r$nationality_cert_u18 == "no" |
             r$birth_cert_u18 == "no",
           1,
           0)
  
  r$g52 <- ifelse(
    r$`disciplinary_measures.shouted` == 1 |
      r$`disciplinary_measures.spanked` == 1 ,
    1,
    0
  )
  
  
  r$g53a <-
    case_when(r$not_residing == "yes" ~ 1,
              r$not_residing %in% c("no") ~ 0,
              TRUE ~ NA_real_)
  r$g53b_i <-
    case_when(
      r$g53a == 1 &
        r$not_residing_reason.married > 0 ~ 1,
      r$g53a == 0 | r$not_residing_reason.married == 0 ~ 0,
      TRUE ~ NA_real_
    )
  r$g53b_ii <-
    case_when(
      r$g53a == 1 &
        r$not_residing_reason.seek_employment > 0 ~ 1,
      r$g53a == 0 |
        r$not_residing_reason.seek_employment == 0 ~ 0,
      TRUE ~ NA_real_
    )
  r$g53b_iii <-
    case_when(
      r$g53a == 1 &
        r$not_residing_reason.study > 0 ~ 1,
      r$g53a == 0 | r$not_residing_reason.study == 0 ~ 0,
      TRUE ~ NA_real_
    )
  r$g53b_iv <-
    case_when(
      r$g53a == 1 &
        r$not_residing_reason.armed_actors > 0 ~ 1,
      r$g53a == 0 |
        r$not_residing_reason.armed_actors == 0 ~ 0,
      TRUE ~ NA_real_
    )
  
  r$g53b_v <-
    case_when(
      r$g53a == 1 &
        r$not_residing_reason.kidnapped > 0 ~ 1,
      r$g53a == 0 |
        r$not_residing_reason.kidnapped == 0 ~ 0,
      TRUE ~ NA_real_
    )
  r$g53b_vi <-
    case_when(
      r$g53a == 1 &
        r$not_residing_reason.missing > 0 ~ 1,
      r$g53a == 0 | r$not_residing_reason.missing == 0 ~ 0,
      TRUE ~ NA_real_
    )
  r$g53b_vii <-
    case_when(
      r$g53a == 1 &
        r$not_residing_reason.detained > 0 ~ 1,
      r$g53a == 0 | r$not_residing_reason.detained == 0 ~ 0,
      TRUE ~ NA_real_
    )
  
  
  r$g54_i   <-
    case_when(
      r$restriction_clearance == "yes" &
        (
          r$restriction_clearance_covid == "no" |
            r$restriction_clearance_covid == "similar"
        ) ~ 1,
      r$restriction_clearance == "yes" &
        r$restriction_clearance_covid %in% c("yes") ~ 0,
      r$restriction_clearance == "no" ~ 0,
      TRUE ~ NA_real_
    )
  
  r$g54_ii   <-
    case_when(
      r$restriction_documents == "yes" &
        (
          r$restriction_documents_covid == "no" |
            r$restriction_documents_covid == "similar"
        ) ~ 1,
      r$restriction_documents == "yes" &
        r$restriction_documents_covid %in% c("yes") ~ 0,
      r$restriction_documents == "no" ~ 0,
      TRUE ~ NA_real_
    )
  
  r$g54_iii   <-
    case_when(
      r$restriction_time == "yes" &
        (
          r$restriction_time_covid == "no" |
            r$restriction_time_covid == "similar"
        ) ~ 1,
      r$restriction_time == "yes" &
        r$restriction_time_covid %in% c("yes") ~ 0,
      r$restriction_time == "no" ~ 0,
      TRUE ~ NA_real_
    )
  
  r$g54_iv   <-
    case_when(
      r$restriction_reason == "yes" &
        (
          r$restriction_reason_covid == "no" |
            r$restriction_reason_covid == "similar"
        ) ~ 1,
      r$restriction_reason == "yes" &
        r$restriction_reason_covid %in% c("yes") ~ 0,
      r$restriction_reason == "no" ~ 0,
      TRUE ~ NA_real_
    )
  
  r$g54_v   <-
    case_when(
      r$restriction_physical == "yes" &
        (
          r$restriction_physical_covid == "no" |
            r$restriction_physical_covid == "similar"
        ) ~ 1,
      r$restriction_physical == "yes" &
        r$restriction_physical_covid %in% c("yes") ~ 0,
      r$restriction_physical == "no" ~ 0,
      TRUE ~ NA_real_
    )
  
  r$g54_vi   <-
    case_when(r$restriction_other == "yes" ~ 1,
              r$restriction_other == "no" ~ 0,
              TRUE ~ NA_real_)
  
  r$g54_vi <-
    ifelse(r$restriction_other %in% c('no'),
           0,
           1)
  r$g54 <-
    ifelse(r$g54_i == 1 |
             r$g54_ii == 1 |
             r$g54_iii == 1 |
             r$g54_iv == 1 |
             r$g54_v == 1 |
             r$g54_vi == 1,
           1,
           0)
  
  r$g56 <- case_when(
    r$child_distress_number > 0 ~ 1,
    r$child_distress_number == 0 |
      r$hh_member_distress == "no" ~ 0,
    TRUE ~ NA_real_
  )
  
  r$g57 <- case_when(
    r$adult_distress_number > 0 ~ 1,
    r$adult_distress_number == 0 |
      r$hh_member_distress == "no" ~ 0,
    TRUE ~ NA_real_
  )
  
  r$g61 <-
    case_when(r$security_incident == "yes" ~ 1,
              r$security_incident %in% c('no') ~ 0,
              TRUE ~ NA_real_)
  
  r$g62_i <-
    case_when(
      r$security_incident_gender %in% c("male", "both") ~ 1,
      r$security_incident_gender == "female" |
        r$security_incident == "no" ~ 0,
      TRUE ~ NA_real_
    )
  r$g62_ii <-
    case_when(
      r$security_incident_gender %in% c('female', "both") ~ 1,
      r$security_incident_gender == "male" |
        r$security_incident == "no" ~ 0,
      TRUE ~ NA_real_
    )
  
  r$g63 <-
    case_when(r$feel_unsafe == "yes" ~ 1,
              r$feel_unsafe %in% c('no') ~ 0,
              TRUE ~ NA_real_)
  
  r$g64 <- case_when(r$hh_risk_eviction == "yes" ~ 1,
                     r$hh_risk_eviction %in% c('no') ~ 0,
                     TRUE ~ NA_real_)
  
  r$g65_i    <- r$hh_main_risks.lack_funds
  r$g65_ii   <- r$hh_main_risks.no_longer_hosted
  r$g65_iii  <- r$hh_main_risks.unaccepted_by_community
  r$g65_iv   <- r$hh_main_risks.authorities_request
  r$g65_v    <- r$hh_main_risks.owner_request
  r$g65_vi   <- r$hh_main_risks.no_agreement
  r$g65_vii  <- r$hh_main_risks.inadequate
  r$g65_viii <- r$hh_main_risks.occupied
  r$g65_ix   <- r$hh_main_risks.confiscation
  r$g65_x    <- r$hh_main_risks.dispute
  
  r$g66 <- ifelse(r$hlp_document == "no", 1, 0)
  
  r$g67 <- case_when(
    r$why_not_return.house_land_occupied == 1 |
      r$why_not_return.house_damaged_destroyed == 1 ~ 1,
    r$why_not_return.house_land_occupied == 0 &
      r$why_not_return.house_damaged_destroyed == 0 ~ 0,
    TRUE ~ NA_real_
  )
  
  r$g68 <-
    case_when(r$hh_dispute == "yes" ~ 1,
              r$hh_dispute %in% c('no') ~ 0,
              TRUE ~ NA_real_)
  
  r$g73 <- r$why_not_return.presence_of_mines
  
  r$g74 <- ifelse(r$risk_education %in% c('no'), 0, 1)
  
  
  r$g85 <- ifelse(
    r$`nfi_priority_needs.bedding_items` == 1 |
      r$`nfi_priority_needs.mattresses_sleeping_mats` == 1 |
      r$`nfi_priority_needs.blankets` == 1 |
      r$`nfi_priority_needs.cooking_utensils` == 1 |
      r$`nfi_priority_needs.cooking_stove` == 1 |
      r$`nfi_priority_needs.winter_heaters` == 1 |
      r$`nfi_priority_needs.clothing` == 1 |
      r$`nfi_priority_needs.heating_cooking_fuel` == 1 |
      r$`nfi_priority_needs.other` == 1,
    1,
    0
  )
  r$g85_i <- r$nfi_priority_needs.bedding_items
  r$g85_ii <- r$nfi_priority_needs.mattresses_sleeping_mats
  r$g85_iii <- r$nfi_priority_needs.blankets
  r$g85_iv <- r$nfi_priority_needs.cooking_utensils
  r$g85_v <- r$nfi_priority_needs.cooking_stove
  r$g85_vi <- r$nfi_priority_needs.winter_heaters
  r$g85_vii <- r$nfi_priority_needs.clothing
  r$g85_viii <- r$nfi_priority_needs.heating_cooking_fuel
  r$g85_ix <- r$nfi_priority_needs.other
  r$g85_x <- ifelse(
      r$`nfi_priority_needs.mattresses_sleeping_mats` == 1 |
      r$`nfi_priority_needs.winter_heaters` == 1 |
      r$`nfi_priority_needs.clothing` == 1,
    1,
    0
  )
  
  r$g89 <-
    ifelse(rowSums(
      cbind(
        as.numeric(r$`shelter_better.protec_hazards`),
        as.numeric(r$`shelter_better.improve_safety`),
        as.numeric(r$`shelter_better.improve_privacy`),
        as.numeric(r$`shelter_better.protect_climate`),
        as.numeric(r$`shelter_better.other`)
      ),
      na.rm = T
    ) >= 2, 1, 0)
  r$g90 <-
    ifelse(rowSums(
      cbind(
        as.numeric(r$`shelter_better.protec_hazards`),
        as.numeric(r$`shelter_better.improve_safety`),
        as.numeric(r$`shelter_better.improve_privacy`),
        as.numeric(r$`shelter_better.protect_climate`),
        as.numeric(r$`shelter_better.other`)
      ),
      na.rm = T
    ) >= 1, 1, 0)
  
  r$g94 <- ifelse(
    r$sufficient_water_drinking == "yes" &
      r$sufficient_water_cooking == "yes" &
      r$sufficient_water_hygiene == "yes" &
      r$sufficient_water_other == "yes",
    1,
    0
  )
  
  r$g95 <- ifelse(
    r$drinking_water_source %in%
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
  
  r$g96 <-
    ifelse(r$treat_drink_water %in% c("always", "sometimes"), 1, 0)
  r$g97 <- ifelse(r$latrines %in% c("vip_pit", "flush"), 1, 0)
  r$g98 <-
    ifelse(r$access_hygiene_items  %in% c("satisfied", "very_satisfied"),
           1,
           0)
  r$g99 <-
    ifelse(r$access_soap == "yes" &
             r$`use_of_soap.handwashing` == 1,
           1,
           0)
  
  r$g100_i <-
    as.numeric(r$food_exp) / as.numeric(r$tot_expenses) * 100
  r$g100_i <- ifelse(r$g100_i > 100, NA,
                     r$g100_i)
  r$g100_ii <-
    as.numeric(r$rent_exp) / as.numeric(r$tot_expenses) * 100
  r$g100_i <- ifelse(r$g100_ii > 100, NA,
                     r$g100_ii)
  r$g100_iii <-
    as.numeric(r$medical_exp) / as.numeric(r$tot_expenses) * 100
  r$g100_i <- ifelse(r$g100_iii > 100, NA,
                     r$g100_iii)
  
  r$g102 <-
    ifelse(as.numeric(r$tot_expenses) * 0.4 <= r$food_exp, 1, 0)
  
  return(r)
}
