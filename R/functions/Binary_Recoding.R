round2 <- function(x, n=0) {
  posneg <- sign(x)
  z <- abs(x)*10^n
  z <- z + 0.5
  z <- trunc(z)
  z <- z/10^n
  z <- z*posneg
  return(z)
}

#r <- response
#loop <- loop


recoding_preliminary <- function(r, loop) {

  # get sex column form member sheet
  loop_hoh <- loop[which(loop$relationship == "head"), ]
  loop_females <- loop[which(loop$sex == "female"), ]
  loop_females <-
    loop %>% mutate(plw = ifelse(pregnant_lactating == "yes", 1, 0))
  
  r <- r %>%
    mutate(sex = loop_hoh$sex[match(r$X_uuid, loop_hoh$`X_uuid`)])
  
  loop_children <- loop[which(loop$age < 18), ]
  female_headed <- response[which(response$X_uuid %in% loop$X_uuid[which(loop$sex == "female" & loop$relationship == "head")]),]
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
    ifelse(loop_children$marital_status[match(r$X_uuid, loop_children$`X_uuid`)] %in% c("married", "widowed", "divorced") ,
           1,
           0)
  
  r$a13 <-
    ifelse(loop$age[match(r$X_uuid, loop_hoh$`X_uuid`)] < 18 &
             loop$work[match(r$X_uuid, loop_hoh$`X_uuid`)] == "yes", 1, 0)
  
  r$a14 <- ifelse(r$gender_hhh == "female", 1, 0)
  
  
  r$a15_i     <-
    ifelse(r$`why_not_return.fear_trauma` %in% c(NA, 0), 0, 1)
  r$a15_ii    <-
    ifelse(r$`why_not_return.lack_of_security_forces` %in% c(NA, 0), 0, 1)
  r$a15_ii    <-
    ifelse(r$`why_not_return.presence_of_mines` %in% c(NA, 0), 0, 1)
  r$a15_iv    <-
    ifelse(r$`why_not_return.discrimination` %in% c(NA, 0), 0, 1)
  r$a15_v     <-
    ifelse(r$`why_not_return.lack_security_women` %in% c(NA, 0), 0, 1)
  r$a15_vi    <-
    ifelse(r$`why_not_return.movement_restrictions` %in% c(NA, 0), 0, 1)
  r$a15_vii   <-
    ifelse(r$`why_not_return.no_personal_id` %in% c(NA, 0), 0, 1)
  r$a15_viii  <-
    ifelse(r$`why_not_return.no_transport_return` %in% c(NA, 0), 0, 1)
  r$a15_ix    <-
    ifelse(r$`why_not_return.no_money_return` %in% c(NA, 0), 0, 1)
  r$a15_x     <-
    ifelse(r$`why_not_return.lack_livelihoods_aoo` %in% c(NA, 0), 0, 1)
  r$a15_xi    <-
    ifelse(r$`why_not_return.hh_assets_stolen_damaged` %in% c(NA, 0),
           0,
           1)
  r$a15_xii   <-
    ifelse(r$`why_not_return.house_land_occupied` %in% c(NA, 0), 0, 1)
  r$a15_xiii  <-
    ifelse(r$`why_not_return.house_damaged_destroyed` %in% c(NA, 0), 0, 1)
  r$a15_xiv   <-
    ifelse(r$`why_not_return.lack_court` %in% c(NA, 0), 0, 1)
  r$a15_xv    <-
    ifelse(r$`why_not_return.local_markets_not_working` %in% c(NA, 0),
           0,
           1)
  r$a15_xvi   <-
    ifelse(r$`why_not_return.basic_services_not_enough` %in% c(NA, 0),
           0,
           1)
  r$a15_xvii  <-
    ifelse(r$`why_not_return.lack_of_education_oppotunities` %in% c(NA, 0),
           0,
           1)
  r$a15_xviii <-
    ifelse(r$`why_not_return.immediate_family_wont_return` %in% c(NA, 0),
           0,
           1)
  r$a15_xix   <-
    ifelse(r$`why_not_return.health_conditions` %in% c(NA, 0), 0, 1)
  r$a15_xx    <-
    ifelse(r$`why_not_return.children_enrolled_in_displacement` %in% c(NA, 0),
           0,
           1)
  r$a15_xxi   <-
    ifelse(r$`why_not_return.living_conditions_better` %in% c(NA, 0),
           0,
           1)
  r$a15_xxii  <-
    ifelse(r$`why_not_return.other` %in% c(NA, 0), 0, 1)
  r$a15_xxiii <-
    ifelse(r$`why_not_return.do_not_know` %in% c(NA, 0), 0, 1)
  r$a15_xxiv  <-
    ifelse(r$`why_not_return.decline_to_answer` %in% c(NA, 0), 0, 1)
  
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
  
  r$a17 <- ifelse(r$enclosure_issues %in% c("none"), 1, 0)
  
  r$a22 <-
    ifelse(r$displaced_again %in% c("decline_to_answer", 'no', NA), 0, 1)
  r$a24 <- ifelse(r$movement_intentions_3 %in% c("remain"), 1, 0)
  r$a25 <- ifelse(r$movement_intentions_12 %in% c("remain"), 1, 0)
  
  r$a26_i    <-
    ifelse(r$`reason_to_return_to_aoo.security_stable` %in% c(NA, "0"),
           0,
           1)
  r$a26_ii   <-
    ifelse(r$`reason_to_return_to_aoo.uxo` %in% c(NA, "0"), 0, 1)
  r$a26_iii  <-
    ifelse(r$`reason_to_return_to_aoo.other_members_returned` %in% c(NA, "0"),
           0,
           1)
  r$a26_iv   <-
    ifelse(r$`reason_to_return_to_aoo.livelihood_availability_there` %in% c(NA, "0"),
           0,
           1)
  r$a26_v    <-
    ifelse(r$`reason_to_return_to_aoo.basic_services` %in% c(NA, "0"),
           0,
           1)
  r$a26_vi   <-
    ifelse(r$`reason_to_return_to_aoo.emotional_desire` %in% c(NA, "0"),
           0,
           1)
  r$a26_vii  <-
    ifelse(r$`reason_to_return_to_aoo.secure_house_land` %in% c(NA, "0"),
           0,
           1)
  r$a26_viii <-
    ifelse(r$`reason_to_return_to_aoo.secure_civil_doc` %in% c(NA, "0"),
           0,
           1)
  r$a26_ix   <-
    ifelse(r$`reason_to_return_to_aoo.limited_livelihoods_aod` %in% c(NA, "0"),
           0,
           1)
  r$a26_x    <-
    ifelse(r$`reason_to_return_to_aoo.limited_services` %in% c(NA, "0"),
           0,
           1)
  r$a26_xi   <-
    ifelse(r$`reason_to_return_to_aoo.no_safe_aod` %in% c(NA, "0"), 0, 1)
  r$a26_xii  <-
    ifelse(r$`reason_to_return_to_aoo.no_integrated_aod` %in% c(NA, "0"),
           0,
           1)
  r$a26_xiii <-
    ifelse(r$`reason_to_return_to_aoo.facing_eviction` %in% c(NA, "0"),
           0,
           1)
  r$a26_xiv  <-
    ifelse(r$`reason_to_return_to_aoo.forced_security` %in% c(NA, "0"),
           0,
           1)
  r$a26_xv   <-
    ifelse(r$`reason_to_return_to_aoo.fam_released` %in% c(NA, "0"),
           0,
           1)
  
  r$a27    <- ifelse(r$movement_intentions_b3 %in% c("remain"), 1, 0)
  r$a28    <-
    ifelse(r$movement_intentions_b12 %in% c("remain"), 1, 0)
  r$a29    <-
    ifelse(r$local_integration %in% c("do_not_know", "decline_to_answer", "no"),
           0,
           1)
  
  
  # ################################################### b #######################################
  
  r$b1     <- ifelse(r$inc_employment_pension < 480000, 1, 0)
  r$b2     <-
    ifelse(r$`primary_livelihood.ngo_charity_assistance` == 1, 1, 0)
  r$b3     <-
    ifelse(r$inc_employment_pension < 480000, 1, 0)
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
  
  PLW       <-
    as.data.frame(loop_females %>% dplyr::group_by(`X_uuid`) %>% dplyr::summarize(sum(plw)))
  r$c11    <- PLW[match(r$X_uuid, PLW$`X_uuid`), 2]
  rm(PLW)
  
  # ################################################### d ########################################
  
  r$d1_i    <- ifelse(r$`info_aid.aid` %in% c(NA, 0), 0, 1)
  r$d1_ii   <- ifelse(r$`info_aid.safety`  %in% c(NA, 0), 0, 1)
  r$d1_iii  <- ifelse(r$`info_aid.housing`  %in% c(NA, 0), 0, 1)
  r$d1_iv   <- ifelse(r$`info_aid.livelihoods` %in% c(NA, 0), 0, 1)
  r$d1_v    <- ifelse(r$`info_aid.water` %in% c(NA, 0), 0, 1)
  r$d1_vi   <- ifelse(r$`info_aid.electricity` %in% c(NA, 0), 0, 1)
  r$d1_vii  <- ifelse(r$`info_aid.education` %in% c(NA, 0), 0, 1)
  r$d1_viii <- ifelse(r$`info_aid.healthcare` %in% c(NA, 0), 0, 1)
  r$d1_ix   <- ifelse(r$`info_aid.legal` %in% c(NA, 0), 0, 1)
  r$d1_x    <- ifelse(r$`info_aid.property` %in% c(NA, 0), 0, 1)
  r$d1_xi   <- ifelse(r$`info_aid.uxo` %in% c(NA, 0), 0, 1)
  r$d1_xii  <-
    ifelse(r$`info_aid.documentation` %in% c(NA, 0), 0, 1)
  r$d1_xiii <- ifelse(r$`info_aid.none` %in% c(NA, 0), 0, 1)
  
  r$d2_i    <- ifelse(r$`info_provider.ngo` %in% c(NA, 0), 0, 1)
  r$d2_ii   <-
    ifelse(r$`info_provider.friends_in_aoo` %in% c(NA, 0), 0, 1)
  r$d2_iii  <-
    ifelse(r$`info_provider.friends_visited_aoo` %in% c(NA, 0), 0, 1)
  r$d2_iv   <-
    ifelse(r$`info_provider.friends_not_been_in_aoo` %in% c(NA, 0), 0, 1)
  r$d2_v    <-
    ifelse(r$`info_provider.local_authorities` %in% c(NA, 0), 0, 1)
  r$d2_vi   <-
    ifelse(r$`info_provider.national_authorities` %in% c(NA, 0), 0, 1)
  r$d2_vii  <-
    ifelse(r$`info_provider.religious` %in% c(NA, 0), 0, 1)
  r$d2_viii <-
    ifelse(r$`info_provider.mukhtars` %in% c(NA, 0), 0, 1)
  r$d2_ix   <-
    ifelse(r$`info_provider.sector_leaders` %in% c(NA, 0), 0, 1)
  r$d2_x    <- ifelse(r$`info_provider.schools` %in% c(NA, 0), 0, 1)
  
  r$d3_i    <- ifelse(r$`info_mode.mobile` %in% c(NA, 0), 0, 1)
  r$d3_ii   <- ifelse(r$`info_mode.direct_obs` %in% c(NA, 0), 0, 1)
  r$d3_iii  <-
    ifelse(r$`info_mode.face_cmmunic` %in% c(NA, 0), 0, 1)
  r$d3_iv   <- ifelse(r$`info_mode.television` %in% c(NA, 0), 0, 1)
  r$d3_v    <- ifelse(r$`info_mode.telephone` %in% c(NA, 0), 0, 1)
  r$d3_vi   <-
    ifelse(r$`info_mode.facebook_app` %in% c(NA, 0), 0, 1)
  r$d3_vii  <-
    ifelse(r$`info_mode.facebook_messenger` %in% c(NA, 0), 0, 1)
  r$d3_viii <- ifelse(r$`info_mode.whatsapp` %in% c(NA, 0), 0, 1)
  r$d3_ix   <- ifelse(r$`info_mode.viber` %in% c(NA, 0), 0, 1)
  r$d3_x    <-
    ifelse(r$`info_mode.other_social` %in% c(NA, 0), 0, 1)
  r$d3_xi   <-
    ifelse(r$`info_mode.notice_board` %in% c(NA, 0), 0, 1)
  r$d3_xii  <- ifelse(r$`info_mode.newspapers` %in% c(NA, 0), 0, 1)
  r$d3_xiii <- ifelse(r$`info_mode.leaflet` %in% c(NA, 0), 0, 1)
  r$d3_xiv  <-
    ifelse(r$`info_mode.loud_speakers` %in% c(NA, 0), 0, 1)
  r$d3_xv   <- ifelse(r$`info_mode.radio` %in% c(NA, 0), 0, 1)
  
  r$d4      <- ifelse(r$aid_received == "yes", 1, 0)
  
  r$d5_i    <- ifelse(r$`aid_type.cash` %in% c(NA, 0), 0, 1)
  r$d5_ii   <- ifelse(r$`aid_type.food`  %in% c(NA, 0), 0, 1)
  r$d5_iii  <- ifelse(r$`aid_type.water`  %in% c(NA, 0), 0, 1)
  r$d5_iv   <- ifelse(r$`aid_type.fuel` %in% c(NA, 0), 0, 1)
  r$d5_v    <- ifelse(r$`aid_type.shelter` %in% c(NA, 0), 0, 1)
  r$d5_vi   <-
    ifelse(r$`aid_type.seasonal_items`  %in% c(NA, 0), 0, 1)
  r$d5_vii  <- ifelse(r$`aid_type.healthcare` %in% c(NA, 0), 0, 1)
  r$d5_viii <- ifelse(r$`aid_type.other_nfi` %in% c(NA, 0), 0, 1)
  r$d5_ix   <- ifelse(r$`aid_type.education` %in% c(NA, 0), 0, 1)
  r$d5_x    <- ifelse(r$`aid_type.protection` %in% c(NA, 0), 0, 1)
  
  r$d6      <-
    ifelse(r$aid_satisfaction == "yes",
           1,
           ifelse(
             r$aid_satisfaction %in% c("decline_to_answer", "do_not_know", "no"),
             0,
             NA
           ))
  r$d7      <-
    ifelse(r$`aid_not_satisfied.quantity`  %in% c(NA, 0), 0, 1)
  r$d10     <-
    ifelse(r$aid_workers_satisfied == "",
           NA,
           ifelse(r$aid_workers_satisfied == "no", 1, 0))
  r$d12     <- ifelse(r$complaint_mechanisms == "yes", 1, 0)
  r$d15   <-
    ifelse(
      r$covid_info_need == 'yes' &
        r$covid_info_type %in% c("prevention measures", "prevention_measures"),
      1,
      0
    )
  
  ##################################################### F ########################################
  r$f7 <- ifelse(
    r$property_damaged == "yes" &
      r$aware_compensation == "yes" &
      r$applied_compensation == "yes",
    ifelse(r$received_compensation == "yes", 1, 0),
    NA
  )
  r$f7b <- ifelse(r$complaint_mechanisms != "yes", 0, 1)
  # ################################################### g ########################################
  
  r$g4 <-
    ifelse(
      loop_children$attend_informal_ed[match(r$X_uuid, loop$`X_uuid`)] %in%
        c("do_not_know", "decline_to_answer", "yes", NA) &
        loop_children$attend_formal_ed[match(r$X_uuid, loop$`X_uuid`)] %in%
        c("do_not_know", "decline_to_answer", "yes", NA),
      0,
      1
    )
  children_attend_ed <- loop_children %>%
    mutate(
      attend_formal_ed =
        case_when(
          attend_formal_ed %in% c("do_not_know", "decline_to_answer", "no", NA) ~ 0,
          TRUE ~ 1
        ),
      attend_informal_ed =
        case_when(
          attend_informal_ed %in% c("do_not_know", "decline_to_answer", "no", NA) ~ 0,
          TRUE ~ 1
        )
    )
  children_attend_ed <- children_attend_ed %>%
    dplyr::select(`X_uuid`, attend_informal_ed, attend_formal_ed)
  children_attend_ed <- children_attend_ed %>%
    group_by(`X_uuid`) %>%
    summarize(
      num_attend_formal = sum(attend_formal_ed),
      num_attend_informal = sum(attend_informal_ed)
    )
  
  r$g5 <-
    ifelse(
      is.na(children_attend_ed$num_attend_formal[match(r$X_uuid, children_attend_ed$`X_uuid`)]),
      NA,
      children_attend_ed$num_attend_formal
    )
  
  r$g6 <-
    ifelse(
      is.na(children_attend_ed$num_attend_informal[match(r$X_uuid, children_attend_ed$`X_uuid`)]),
      NA,
      children_attend_ed$num_attend_informal
    )
  rm(children_attend_ed)
 # rm(loop_children)
 # rm(loop_females)
  #rm(loop_hoh)
  r$g7_i    <-
    ifelse(r$`reasons_not_attend.school_closed` %in% c(NA, 0), 0, 1)
  r$g7_ii   <-
    ifelse(r$`reasons_not_attend.not_safe` %in% c(NA, 0), 0, 1)
  r$g7_iii  <-
    ifelse(r$`reasons_not_attend.cannot_afford` %in% c(NA, 0), 0, 1)
  r$g7_iv   <-
    ifelse(r$`reasons_not_attend.impossible_to_enrol` %in% c(NA, 0), 0, 1)
  r$g7_v    <-
    ifelse(r$`reasons_not_attend.cannot_go_physically` %in% c(NA, 0),
           0,
           1)
  r$g7_vi   <-
    ifelse(r$`reasons_not_attend.overcrowded` %in% c(NA, 0), 0, 1)
  r$g7_vii  <-
    ifelse(r$`reasons_not_attend.lack_of_staff` %in% c(NA, 0), 0, 1)
  r$g7_viii <-
    ifelse(r$`reasons_not_attend.poor_infrastructure` %in% c(NA, 0), 0, 1)
  r$g7_ix   <-
    ifelse(r$`reasons_not_attend.curriculum` %in% c(NA, 0), 0, 1)
  r$g7_x    <-
    ifelse(r$`reasons_not_attend.children_working` %in% c(NA, 0), 0, 1)
  r$g7_xi   <-
    ifelse(r$`reasons_not_attend.parental_refusal` %in% c(NA, 0), 0, 1)
  r$g7_xii  <-
    ifelse(r$`reasons_not_attend.uninterested` %in% c(NA, 0), 0, 1)
  r$g7_xiii <-
    ifelse(r$`reasons_not_attend.lack_doc` %in% c(NA, 0), 0, 1)
  r$g7_xiv  <-
    ifelse(r$`reasons_not_attend.other` %in% c(NA, 0), 0, 1)
  
  
  r$g8 <-
    ifelse(
      r$primary_school_place %in% c("between_2_5", "within_2km") &
        r$secondary_school_place %in% c("between_2_5", "within_2km"),
      1,
      0
    )
  r$g9 <- ifelse(r$covid_dropout %in% c(NA, 0), 0, 1)
  
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
    dplyr::select(`X_uuid`,
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
      no_food_freq == ""  ~ 0,
    )
  )
  fsc$hhh1_3 <- fsc$hhh1_1 * fsc$hhh1_2
  fsc$hhh2_1 <- ifelse(fsc$hungry == "yes", 1, 0)
  fsc <- fsc %>% mutate(
    hhh2_2 = case_when(
      hungry_freq == "rarely" ~ 1,
      hungry_freq == "sometimes"  ~ 1,
      hungry_freq == "often" ~ 2,
      hungry_freq == ""  ~ 0,
    )
  )
  fsc$hhh2_3 <- fsc$hhh2_1 * fsc$hhh2_2
  fsc$hhh3_1 <- ifelse(fsc$not_eating == "yes", 1, 0)
  fsc <- fsc %>% mutate(
    hhh3_2 = case_when(
      not_eating_freq == "rarely" ~ 1,
      not_eating_freq == "sometimes"  ~ 1,
      not_eating_freq == "often" ~ 2,
      not_eating_freq == ""  ~ 0,
    )
  )
  fsc$hhh3_3 <- fsc$hhh3_1 * fsc$hhh3_2
  fsc$hhs <-
    rowSums(fsc[, c("hhh1_3", "hhh2_3", "hhh3_3")], na.rm = T)
  
  r$g15_i <-
    ifelse(fsc$hhs[match(r$X_uuid, fsc$`_uuid`)] <= 1, 1, 0)
  r$g15_ii <-
    ifelse(between(fsc$hhs[match(r$X_uuid, fsc$`_uuid`)], 2, 3), 1, 0)
  r$g15_iii <-
    ifelse(fsc$hhs[match(r$X_uuid, fsc$`_uuid`)] >= 4, 1, 0)
  rm(fsc)
  r$g19 <-
    ifelse((
      loop_children$attend_formal_ed[match(r$X_uuid, loop_children$`X_uuid`)] == "yes" |
        loop_children$attend_informal_ed[match(r$X_uuid, loop_children$`X_uuid`)] == "yes"
    ) &
      loop_children$work[match(r$X_uuid, loop_children$`X_uuid`)] == "yes",
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
  
  r$g34_i    <- ifelse(r$`health_barriers.cost` %in% c(NA, 0), 0, 1)
  r$g34_ii   <-
    ifelse(r$`health_barriers.unqualified_staff` %in% c(NA, 0), 0, 1)
  r$g34_iii  <-
    ifelse(r$`health_barriers.civ_docs_problems` %in% c(NA, 0), 0, 1)
  r$g34_iv   <-
    ifelse(r$`health_barriers.no_referral_phc` %in% c(NA, 0), 0, 1)
  r$g34_v    <-
    ifelse(r$`health_barriers.phc_closed` %in% c(NA, 0), 0, 1)
  r$g34_vi   <-
    ifelse(r$`health_barriers.distance_to_treatmentcenter` %in% c(NA, 0),
           0,
           1)
  r$g34_vii  <-
    ifelse(r$`health_barriers.refused_treatment` %in% c(NA, 0), 0, 1)
  r$g34_viii <-
    ifelse(r$`health_barriers.no_medicine` %in% c(NA, 0), 0, 1)
  r$g34_ix   <-
    ifelse(r$`health_barriers.no_offered_treatment` %in% c(NA, 0), 0, 1)
  r$g34_x    <-
    ifelse(r$`health_barriers.not_inclusive` %in% c(NA, 0), 0, 1)
  r$g34_xi   <-
    ifelse(r$`health_barriers.no_fem_staff` %in% c(NA, 0), 0, 1)
  
  r$g35 <-
    ifelse(loop$`health_issue.chronic`[match(r$X_uuid, loop$`X_uuid`)] %in% c(NA, 0), 0, 1)
  
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
  
  r$g45_i    <-
    ifelse(r$`employment_primary_barriers.increased_competition` == 1,
           1,
           0)
  r$g45_ii   <-
    ifelse(r$`employment_primary_barriers.jobs_far` == 1, 1, 0)
  r$g45_iii  <-
    ifelse(r$`employment_primary_barriers.only_low_available` == 1, 1, 0)
  r$g45_iv   <-
    ifelse(r$`employment_primary_barriers.underqualified_for_jobs` == 1,
           1,
           0)
  r$g45_v    <-
    ifelse(r$`employment_primary_barriers.lack_of_connections` == 1,
           1,
           0)
  r$g45_vi   <-
    ifelse(r$`employment_primary_barriers.lack_jobs_women` == 1, 1, 0)
  r$g45_vii  <-
    ifelse(r$`employment_primary_barriers.none` == 1, 1, 0)
  r$g45_viii <-
    ifelse(r$`employment_primary_barriers.other` == 1, 1, 0)
  
  
  r$g46 <- ifelse(r$employment_seasonal == "yes", 1, 0)
  
  r$g47_i <-
    ifelse(between(loop$age[match(r$X_uuid, loop$`X_uuid`)], 18, 59) &
             loop$sex[match(r$X_uuid, loop$`X_uuid`)] == "female" &
             loop$work[match(r$X_uuid, loop$`X_uuid`)] == "yes",
           1,
           0)
  
  r$g47_ii <-
    ifelse(between(loop$age[match(r$X_uuid, loop$`X_uuid`)], 18, 59) &
             loop$sex[match(r$X_uuid, loop$`X_uuid`)] == "male" &
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
    ifelse(r$not_residing %in% c(NA, "decline_to_answer", 'no', "do_not_know"),
           0,
           1)
  r$g53b_i <-
    ifelse(r$not_residing %in% c("yes") &
             r$not_residing_reason.married > 0, 1, 0)
  r$g53b_ii <-
    ifelse(r$not_residing %in% c("yes") &
             r$not_residing_reason.seek_employment > 0, 1, 0)
  r$g53b_iii <-
    ifelse(r$not_residing %in% c("yes") &
             r$not_residing_reason.study > 0, 1, 0)
  r$g53b_iv <-
    ifelse(r$not_residing %in% c("yes") &
             r$not_residing_reason.family > 0, 1, 0)
  r$g53b_v <-
    ifelse(r$not_residing %in% c("yes") &
             r$not_residing_reason.armed_actors > 0, 1, 0)
  r$g53b_vi <-
    ifelse(r$not_residing %in% c("yes") &
             r$not_residing_reason.kidnapped > 0, 1, 0)
  r$g53b_vii <-
    ifelse(r$not_residing %in% c("yes") &
             r$not_residing_reason.missing > 0, 1, 0)
  r$g53b_ix <-
    ifelse(r$not_residing %in% c("yes") &
             r$not_residing_reason.detained > 0, 1, 0)
  
  r$g54_i   <-
    ifelse(
      r$restriction_clearance %in% c(NA, "decline_to_answer", 'no', "do_not_know") &
        r$restriction_clearance_covid %in% c(NA, "dnt_know", "no_answer", "yes"),
      0,
      1
    )
  r$g54_ii  <-
    ifelse(
      r$restriction_documents %in% c(NA, "decline_to_answer", 'no', "do_not_know") &
        r$restriction_documents_covid %in% c(NA, "dnt_know", "no_answer", "yes"),
      0,
      1
    )
  r$g54_iii <-
    ifelse(
      r$restriction_time %in% c(NA, "decline_to_answer", 'no', "do_not_know") &
        r$restriction_time_covid %in% c(NA, "dnt_know", "no_answer", "yes"),
      0,
      1
    )
  r$g54_iv   <-
    ifelse(
      r$restriction_reason %in% c(NA, "decline_to_answer", 'no', "do_not_know") &
        r$restriction_reason_covid %in% c(NA, "dnt_know", "no_answer", "yes"),
      0,
      1
    )
  r$g54_v  <-
    ifelse(
      r$restriction_physical %in% c(NA, "decline_to_answer", 'no', "do_not_know") &
        r$restriction_physical_covid %in% c(NA, "dnt_know", "no_answer", "yes"),
      0,
      1
    )
  r$g54_vi <-
    ifelse(r$restriction_other %in% c(NA, "decline_to_answer", 'no', "do_not_know"),
           0,
           1)
  r$g54 <-
    ifelse(
      r$g54_i == 1 |
        r$g54_ii == 1 |
        r$g54_iii == 1 |
        r$g54_iv == 1 |
        r$g54_v == 1 |
        r$g54_vi == 1,
      1,
      0
    )
  
  r$g56 <-
    ifelse(r$child_distress_number < 1 |
             is.na(r$child_distress_number),
           0,
           1)
  r$g57 <-
    ifelse(r$adult_distress_number < 1 |
             is.na(r$adult_distress_number),
           0,
           1)
  
  r$g61 <-
    ifelse(r$security_incident %in% c(NA, "decline_to_answer", 'no', "do_not_know"),
           0,
           1)
  r$g62_i <-
    ifelse(r$security_incident_gender %in% c("male", "both"), 1, 0)
  r$g62_ii <-
    ifelse(r$security_incident_gender %in% c('female', "both"), 1, 0)
  
  r$g63 <-
    ifelse(r$feel_unsafe %in% c(NA, "decline_to_answer", 'no', "do_not_know"),
           0,
           1)
  
  r$g64 <- ifelse(r$hh_risk_eviction == "yes", 1, 0)
  
  r$g65_i    <- ifelse(r$`hh_main_risks.lack_funds` == 1, 1, 0)
  r$g65_ii   <-
    ifelse(r$`hh_main_risks.no_longer_hosted` == 1, 1, 0)
  r$g65_iii  <-
    ifelse(r$`hh_main_risks.unaccepted_by_community` == 1, 1, 0)
  r$g65_iv   <-
    ifelse(r$`hh_main_risks.authorities_request` == 1, 1, 0)
  r$g65_v    <- ifelse(r$`hh_main_risks.owner_request` == 1, 1, 0)
  r$g65_vi   <- ifelse(r$`hh_main_risks.no_agreement` == 1, 1, 0)
  r$g65_vii  <- ifelse(r$`hh_main_risks.inadequate` == 1, 1, 0)
  r$g65_viii <- ifelse(r$`hh_main_risks.occupied` == 1, 1, 0)
  r$g65_ix   <- ifelse(r$`hh_main_risks.confiscation` == 1, 1, 0)
  r$g65_x    <- ifelse(r$`hh_main_risks.dispute` == 1, 1, 0)
  
  r$g66 <- ifelse(r$hlp_document == "yes", 1, 0)
  
  r$g67 <- ifelse(
    r$`why_not_return.house_land_occupied` %in% c(NA, 0) |
      r$`why_not_return.house_damaged_destroyed` %in% c(NA, 0),
    0,
    1
  )
  
  r$g68 <-
    ifelse(r$hh_dispute %in% c("decline_to_answer", 'no', "do_not_know", NA),
           0,
           1)
  r$g73 <-
    ifelse(r$`why_not_return.presence_of_mines` %in% c(NA, 0), 0, 1)
  r$g74 <-
    ifelse(r$risk_education %in% c('no', "do_not_know", NA), 0, 1)
  
  r$g85 <- ifelse(
    r$`nfi_priority_needs.bedding_items` == 1 |
      r$`nfi_priority_needs.mattresses_sleeping_mats` == 1 |
      r$`nfi_priority_needs.blankets` == 1 |
      r$`nfi_priority_needs.cooking_utensils` == 1 |
      r$`nfi_priority_needs.cooking_stove` == 1 |
      r$`nfi_priority_needs.winter_heaters` == 1 |
      r$`nfi_priority_needs.clothing` == 1 |
      r$`nfi_priority_needs.heating_cooking_fuel` == 1 |
      r$`nfi_priority_needs.other` == 1 |
      r$`nfi_priority_needs.none` == 1 ,
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
        as.numeric(r$`shelter_better.other`),
        as.numeric(r$`shelter_better.none`)
      ),
      na.rm = T
    ) >= 2, 1, 0)
  
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

