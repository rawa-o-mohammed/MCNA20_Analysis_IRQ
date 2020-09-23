compare_columns_valuesLimit_col <- function(df,
                                            cn,
                                            condition,
                                            question_label,
                                            issue_label,
                                            columns,
                                            old_value,
                                            action_label,
                                            index) {
  varname <- paste("", cn , sep = "")
  data <- df %>%
    mutate(
      issue = issue_label,
      flag_index = index,
      log_number = 0,
      question.name = question_label,
      changed = NA,
      old.value = case_when(eval(parse(text = condition)) ~ as.character(eval(
        parse(text = old_value)
      )),
      TRUE ~ "Okay"),
      new.value = NA,!!varname := case_when(eval(parse(text = condition)) ~ action_label,
                                            TRUE ~ "Okay")
    )
  
  filterDate <- data %>%
    dplyr::select(columns) %>%  filter(eval(parse(text =  paste("data$", cn , sep =
                                                                  ""))) == 'Flagged' |
                                         eval(parse(text =  paste("data$", cn , sep = ""))) == 'Deleted'
                                       # |
                                       #   eval(
                                       #     parse(text =  paste("data$", cn , sep="")
                                       #     )) == 'Translate')
                                       
                                       return(filterDate)
}

compare_columns_values <- function(df, cn, condition) {
  varname <- paste("", cn , sep = "")
  
  data <- df %>%
    mutate(!!varname := case_when(eval(parse(text = condition)) ~ "Flagged",
                                  TRUE ~ "Okay"))
  
  return(data)
}

# this function is handling datetime diffirence
interview_time_handler <- function(df) {
  new_df <- df %>%
    mutate(time_spend = difftime(
      as.POSIXct(df$end, format = "%Y-%m-%dT%H:%M:%OS"),
      as.POSIXct(df$start, format = "%Y-%m-%dT%H:%M:%OS"),
      units = 'mins'
    ))
  return(new_df)
}

# this function is handling date diffirence
interview_date_handler <- function(df) {
  new_df <- df %>%
    mutate(arrival_displace_date_diff = difftime(
      as.POSIXct(df$arrival_date_idp, format = "%Y-%m-%d"),
      as.POSIXct(df$displace_date_idp, format = "%Y-%m-%d"),
      units = 'days'
    ))
  
  return(new_df)
}

read_conditions_from_excel_column <- function(df, conditionDf) {
  counter <- 0
  for (row in 1:nrow(conditionDf)) {
    result_col <- conditionDf[row, "new_column_name"]
    conditions  <- conditionDf[row, "conditions"]
    type  <- conditionDf[row, "type"]
    
    if (type == "date-time") {
      df_result <- interview_time_handler(df_result)
      df_result <-
        compare_columns_values(df_result, result_col, paste(conditions))
      
    } else if (type == "normal") {
      if (counter == 0) {
        df_result <-
          compare_columns_values(df, result_col, paste(conditions))
        
        counter <- counter + 1
        
      } else {
        df_result <-
          compare_columns_values(df_result, result_col, paste(conditions))
        
      }
    } else if (type == "date") {
      df_result <- interview_date_handler(df_result)
      df_result <-
        compare_columns_values(df_result, result_col, paste(conditions))
      
    }
  }
  
  return(df_result)
}

read_conditions_from_excel_limited_row <-
  function(df, conditionDf, idf) {
    df_total = data.frame()
    
    columns <-
      c(
        "log_number",
        "_uuid",
        "cluster_location_id",
        "date_assessment",
        "governorate_mcna_r",
        "district_mcna",
        "enumerator_num",
        "question.name",
        "issue",
        "action",
        "changed",
        "old.value",
        "new.value",
        "flag_index"
      )
    
    for (row in 1:nrow(conditionDf)) {
      result_col <- conditionDf[row, "result_column_name"]
      conditions  <- conditionDf[row, "conditions"]
      type  <- conditionDf[row, "type"]
      
      question_label  <- conditionDf[row, "question.name"]
      
      issue_label  <- conditionDf[row, "issue_label"]
      
      old_value  <- conditionDf[row, "old_value"]
      
      action_label  <- conditionDf[row, "action_label"]
      
      
      if (type == "date-time") {
        df <- interview_time_handler(df)
        flagged_rows <-
          compare_columns_valuesLimit_col(
            df,
            result_col,
            paste(conditions),
            question_label,
            issue_label,
            columns,
            old_value,
            action_label,
            row
          )
        
        new_df <- data.frame(flagged_rows)
        df_total <- rbind(df_total, new_df)
      } else if (type == "normal") {
        flagged_rows <-
          compare_columns_valuesLimit_col(
            df,
            result_col,
            paste(conditions),
            question_label,
            issue_label,
            columns,
            old_value,
            action_label,
            row
          )
        
        new_df <- data.frame(flagged_rows)
        df_total <- rbind(df_total, new_df)
      } else if (type == "date") {
        df <- interview_date_handler(df)
        flagged_rows <-
          compare_columns_valuesLimit_col(
            df,
            result_col,
            paste(conditions),
            question_label,
            issue_label,
            columns,
            old_value,
            action_label,
            row
          )
        
        new_df <- data.frame(flagged_rows)
        df_total <- rbind(df_total, new_df)
      }
    }
    
    return(df_total)
  }

read_logs <- function(df, logDF, conditionDf) {
  df_total = data.frame()
  for (row in 1:nrow(logDF)) {
    flag_index <- logDF[row, "flag_index"]
    action  <- logDF[row, "action"]
    changed  <- logDF[row, "changed"]
    new_value  <- logDF[row, "new.value"]
    question_name <- logDF[row, "question.name"]
    
    current_condtion <-
      conditionDf[as.numeric(flag_index), "conditions"]
    current_type <- conditionDf[as.numeric(flag_index), "type"]
    old_value <- conditionDf[as.numeric(flag_index), "old_value"]
    
    varname <- paste("", question_name , sep = "")
    
    if (current_type == "date") {
      df <- interview_date_handler(df)
      
      df <- df %>%
        mutate(!!varname := case_when(
          eval(parse(text = current_condtion)) ~ as.character(new_value),
          TRUE ~ as.character(eval(parse(text = old_value)))
        ))
    } else {
      df <- df %>%
        mutate(!!varname := case_when(
          eval(parse(text = current_condtion)) ~ as.character(new_value),
          TRUE ~ as.character(eval(parse(text = old_value)))
        ))
    }
  }
  
  return(df)
}

recoded_handler <- function(df, indiv_df) {
  # get sex column form member sheet
  loop_hoh <- indiv_df[which(indiv_df$relationship == "head"), ]
  loop_females <- indiv_df[which(indiv_df$sex == "female"), ]
  loop_females <-
    indiv_df %>% mutate(plw = ifelse(pregnant_lactating == "yes", 1, 0))
  
  df <- df %>%
    mutate(sex = loop_hoh$sex[match(df$`_uuid`, loop_hoh$`_submission__uuid`)])
  
  loop_children <- indiv_df[which(indiv_df$age < 18), ]
  df$femal_hh   <- ifelse(df$sex == "female", 1, 0)
  
  ############################################### a #############################
  df$a7        <- as.numeric(df$num_hh_member)
  df$a8        <- as.numeric(df$num_family_member)
  df$a9_male   <-
    as.numeric(df$tot_male) / (as.numeric(df$tot_male) + as.numeric(df$tot_female))
  df$a9_female <-
    as.numeric(df$tot_female) / (as.numeric(df$tot_male) + as.numeric(df$tot_female))
  
  df$a10_child <-
    as.numeric(df$tot_child) / (as.numeric(df$tot_male) +  as.numeric(df$tot_female))
  df$a10_adult <-
    (as.numeric(df$male_18_59_calc) +  as.numeric(df$female_18_59_calc)) / (as.numeric(df$tot_male) +  as.numeric(df$tot_female))
  df$a10_elder <-
    (as.numeric(df$male_60_calc) + as.numeric(df$female_60_calc)) / (as.numeric(df$tot_male) + as.numeric(df$tot_female))
  
  df$a11 <-
    ifelse(
      loop_hoh$marital_status[match(df$`_uuid`, loop_hoh$`_submission__uuid`)] %in%
        c("single", "separated", "widowed", "divorced"),
      1,
      ifelse(loop_hoh$marital_status[match(df$`_uuid`, loop_hoh$`_submission__uuid`)] %in%
               c(NA, ""), NA, 0)
    )
  
  df$a12 <-
    ifelse(loop_children$marital_status[match(df$`_uuid`, loop_children$`_submission__uuid`)] %in% c("married", "widowed", "divorced") ,
           1,
           0)
  
  df$a13 <-
    ifelse(indiv_df$age[match(df$`_uuid`, loop_hoh$`_submission__uuid`)] < 18 &
             indiv_df$work[match(df$`_uuid`, loop_hoh$`_submission__uuid`)] == "yes", 1, 0)
  
  df$a14 <-
    ifelse(
      df$femal_hh == 1 &
        df$gender_respondent == "female" &
        indiv_df$relationship[match(df$`_uuid`, indiv_df$`_submission__uuid`)] == "head",
      1,
      0
    )
  
  df$a15_i     <-
    ifelse(df$`why_not_return/fear_trauma` %in% c(NA, 0), 0, 1)
  df$a15_ii    <-
    ifelse(df$`why_not_return/lack_of_security_forces` %in% c(NA, 0), 0, 1)
  df$a15_ii    <-
    ifelse(df$`why_not_return/presence_of_mines` %in% c(NA, 0), 0, 1)
  df$a15_iv    <-
    ifelse(df$`why_not_return/discrimination` %in% c(NA, 0), 0, 1)
  df$a15_v     <-
    ifelse(df$`why_not_return/lack_security_women` %in% c(NA, 0), 0, 1)
  df$a15_vi    <-
    ifelse(df$`why_not_return/movement_restrictions` %in% c(NA, 0), 0, 1)
  df$a15_vii   <-
    ifelse(df$`why_not_return/no_personal_id` %in% c(NA, 0), 0, 1)
  df$a15_viii  <-
    ifelse(df$`why_not_return/no_transport_return` %in% c(NA, 0), 0, 1)
  df$a15_ix    <-
    ifelse(df$`why_not_return/no_money_return` %in% c(NA, 0), 0, 1)
  df$a15_x     <-
    ifelse(df$`why_not_return/lack_livelihoods_aoo` %in% c(NA, 0), 0, 1)
  df$a15_xi    <-
    ifelse(df$`why_not_return/hh_assets_stolen_damaged` %in% c(NA, 0),
           0,
           1)
  df$a15_xii   <-
    ifelse(df$`why_not_return/house_land_occupied` %in% c(NA, 0), 0, 1)
  df$a15_xiii  <-
    ifelse(df$`why_not_return/house_damaged_destroyed` %in% c(NA, 0), 0, 1)
  df$a15_xiv   <-
    ifelse(df$`why_not_return/lack_court` %in% c(NA, 0), 0, 1)
  df$a15_xv    <-
    ifelse(df$`why_not_return/local_markets_not_working` %in% c(NA, 0),
           0,
           1)
  df$a15_xvi   <-
    ifelse(df$`why_not_return/basic_services_not_enough` %in% c(NA, 0),
           0,
           1)
  df$a15_xvii  <-
    ifelse(df$`why_not_return/lack_of_education_oppotunities` %in% c(NA, 0),
           0,
           1)
  df$a15_xviii <-
    ifelse(df$`why_not_return/immediate_family_wont_return` %in% c(NA, 0),
           0,
           1)
  df$a15_xix   <-
    ifelse(df$`why_not_return/health_conditions` %in% c(NA, 0), 0, 1)
  df$a15_xx    <-
    ifelse(df$`why_not_return/children_enrolled_in_displacement` %in% c(NA, 0),
           0,
           1)
  df$a15_xxi   <-
    ifelse(df$`why_not_return/living_conditions_better` %in% c(NA, 0),
           0,
           1)
  df$a15_xxii  <-
    ifelse(df$`why_not_return/other` %in% c(NA, 0), 0, 1)
  df$a15_xxiii <-
    ifelse(df$`why_not_return/do_not_know` %in% c(NA, 0), 0, 1)
  df$a15_xxiv  <-
    ifelse(df$`why_not_return/decline_to_answer` %in% c(NA, 0), 0, 1)
  
  df$a16 <- ifelse(
    df$shelter_type_inperson %in%
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
  
  df$a17 <- ifelse(df$enclosure_issues %in% c("none"), 1, 0)
  
  df$a22 <-
    ifelse(df$displaced_again %in% c("decline_to_answer", 'no', NA), 0, 1)
  df$a24 <- ifelse(df$movement_intentions %in% c("remain"), 1, 0)
  df$a25 <- ifelse(df$movement_intentions12 %in% c("remain"), 1, 0)
  
  df$a26_i    <-
    ifelse(df$`reason_to_return_to_aoo/security_stable` %in% c(NA, "0"),
           0,
           1)
  df$a26_ii   <-
    ifelse(df$`reason_to_return_to_aoo/uxo` %in% c(NA, "0"), 0, 1)
  df$a26_iii  <-
    ifelse(df$`reason_to_return_to_aoo/other_members_returned` %in% c(NA, "0"),
           0,
           1)
  df$a26_iv   <-
    ifelse(df$`reason_to_return_to_aoo/livelihood_availability_there` %in% c(NA, "0"),
           0,
           1)
  df$a26_v    <-
    ifelse(df$`reason_to_return_to_aoo/basic_services` %in% c(NA, "0"),
           0,
           1)
  df$a26_vi   <-
    ifelse(df$`reason_to_return_to_aoo/emotional_desire` %in% c(NA, "0"),
           0,
           1)
  df$a26_vii  <-
    ifelse(df$`reason_to_return_to_aoo/secure_house_land` %in% c(NA, "0"),
           0,
           1)
  df$a26_viii <-
    ifelse(df$`reason_to_return_to_aoo/secure_civil_doc` %in% c(NA, "0"),
           0,
           1)
  df$a26_ix   <-
    ifelse(df$`reason_to_return_to_aoo/limited_livelihoods_aod` %in% c(NA, "0"),
           0,
           1)
  df$a26_x    <-
    ifelse(df$`reason_to_return_to_aoo/limited_services` %in% c(NA, "0"),
           0,
           1)
  df$a26_xi   <-
    ifelse(df$`reason_to_return_to_aoo/no_safe_aod` %in% c(NA, "0"), 0, 1)
  df$a26_xii  <-
    ifelse(df$`reason_to_return_to_aoo/no_integrated_aod` %in% c(NA, "0"),
           0,
           1)
  df$a26_xiii <-
    ifelse(df$`reason_to_return_to_aoo/facing_eviction` %in% c(NA, "0"),
           0,
           1)
  df$a26_xiv  <-
    ifelse(df$`reason_to_return_to_aoo/forced_security` %in% c(NA, "0"),
           0,
           1)
  df$a26_xv   <-
    ifelse(df$`reason_to_return_to_aoo/lack_security_women` %in% c(NA, "0"),
           0,
           1)
  
  df$a27    <- ifelse(df$movement_intentions_b %in% c("remain"), 1, 0)
  df$a28    <-
    ifelse(df$movement_intentions_b12 %in% c("remain"), 1, 0)
  df$a29    <-
    ifelse(df$local_integration %in% c("do_not_know", "decline_to_answer", "no"),
           0,
           1)
  
  
  # ################################################### b #######################################
  
  df$b1     <- ifelse(df$inc_employment_pension < 480000, 1, 0)
  df$b2     <-
    ifelse(df$`primary_livelihood/ngo_charity_assistance` == 1, 1, 0)
  df$b3     <-
    ifelse(df$inc_employment_pension < 480000 &
             df$g14 == 1 , 1, 0)
  df$b5     <-
    ifelse(
      df$selling_assets %in% c("no_already_did", "yes") |
        df$borrow_debt  %in% c("no_already_did", "yes") |
        df$reduce_spending %in% c("no_already_did", "yes"),
      1,
      0
    )
  df$b6     <-
    ifelse(
      df$selling_transportation_means %in% c("no_already_did", "yes") |
        df$change_place  %in% c("no_already_did", "yes") |
        df$child_work %in% c("no_already_did", "yes"),
      1,
      0
    )
  df$b7     <-
    ifelse(
      df$child_dropout_school %in% c("no_already_did", "yes") |
        df$adult_risky  %in% c("no_already_did", "yes") |
        df$family_migrating %in% c("no_already_did", "yes") |
        df$child_forced_marriage %in% c("no_already_did", "yes"),
      1,
      0
    )
  
  # ################################################### c #######################################
  
  
  df$c2     <-
    ifelse(df$injured_explosive %in% c("killed", "injured"), 1, 0)
  df$c3   <-
    ifelse(
      df$difficulty_seeing %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
        df$difficulty_hearing %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
        df$difficulty_walking %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
        df$difficulty_remembering %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
        df$difficulty_washing %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
        df$difficulty_communicating %in% c("a_lot_of_difficulty", "cannot_do_at_all"),
      1,
      0
    )
  
  df$c9     <- ifelse(df$difficulty_accessing_services == "yes", 1, 0)
  
  PLW       <-
    as.data.frame(loop_females %>% dplyr::group_by(`_submission__uuid`) %>% dplyr::summarize(sum(plw)))
  df$c11    <- PLW[match(df$`_uuid`, PLW$`_submission__uuid`), 2]
  rm(PLW)
  
  # ################################################### d ########################################
  
  df$d1_i    <- ifelse(df$`info_aid/aid` %in% c(NA, 0), 0, 1)
  df$d1_ii   <- ifelse(df$`info_aid/safety`  %in% c(NA, 0), 0, 1)
  df$d1_iii  <- ifelse(df$`info_aid/housing`  %in% c(NA, 0), 0, 1)
  df$d1_iv   <- ifelse(df$`info_aid/livelihoods` %in% c(NA, 0), 0, 1)
  df$d1_v    <- ifelse(df$`info_aid/water` %in% c(NA, 0), 0, 1)
  df$d1_vi   <- ifelse(df$`info_aid/electricity` %in% c(NA, 0), 0, 1)
  df$d1_vii  <- ifelse(df$`info_aid/education` %in% c(NA, 0), 0, 1)
  df$d1_viii <- ifelse(df$`info_aid/healthcare` %in% c(NA, 0), 0, 1)
  df$d1_ix   <- ifelse(df$`info_aid/legal` %in% c(NA, 0), 0, 1)
  df$d1_x    <- ifelse(df$`info_aid/property` %in% c(NA, 0), 0, 1)
  df$d1_xi   <- ifelse(df$`info_aid/uxo` %in% c(NA, 0), 0, 1)
  df$d1_xii  <-
    ifelse(df$`info_aid/documentation` %in% c(NA, 0), 0, 1)
  df$d1_xiii <- ifelse(df$`info_aid/none` %in% c(NA, 0), 0, 1)
  
  df$d2_i    <- ifelse(df$`info_provider/ngo` %in% c(NA, 0), 0, 1)
  df$d2_ii   <-
    ifelse(df$`info_provider/friends_in_aoo` %in% c(NA, 0), 0, 1)
  df$d2_iii  <-
    ifelse(df$`info_provider/friends_visited_aoo` %in% c(NA, 0), 0, 1)
  df$d2_iv   <-
    ifelse(df$`info_provider/friends_not_been_in_aoo` %in% c(NA, 0), 0, 1)
  df$d2_v    <-
    ifelse(df$`info_provider/local_authorities` %in% c(NA, 0), 0, 1)
  df$d2_vi   <-
    ifelse(df$`info_provider/national_authorities` %in% c(NA, 0), 0, 1)
  df$d2_vii  <-
    ifelse(df$`info_provider/religious` %in% c(NA, 0), 0, 1)
  df$d2_viii <-
    ifelse(df$`info_provider/mukhtars` %in% c(NA, 0), 0, 1)
  df$d2_ix   <-
    ifelse(df$`info_provider/sector_leaders` %in% c(NA, 0), 0, 1)
  df$d2_x    <- ifelse(df$`info_provider/schools` %in% c(NA, 0), 0, 1)
  
  df$d3_i    <- ifelse(df$`info_mode/mobile` %in% c(NA, 0), 0, 1)
  df$d3_ii   <- ifelse(df$`info_mode/direct_obs` %in% c(NA, 0), 0, 1)
  df$d3_iii  <-
    ifelse(df$`info_mode/face_cmmunic` %in% c(NA, 0), 0, 1)
  df$d3_iv   <- ifelse(df$`info_mode/television` %in% c(NA, 0), 0, 1)
  df$d3_v    <- ifelse(df$`info_mode/telephone` %in% c(NA, 0), 0, 1)
  df$d3_vi   <-
    ifelse(df$`info_mode/facebook_app` %in% c(NA, 0), 0, 1)
  df$d3_vii  <-
    ifelse(df$`info_mode/facebook_messenger` %in% c(NA, 0), 0, 1)
  df$d3_viii <- ifelse(df$`info_mode/whatsapp` %in% c(NA, 0), 0, 1)
  df$d3_ix   <- ifelse(df$`info_mode/viber` %in% c(NA, 0), 0, 1)
  df$d3_x    <-
    ifelse(df$`info_mode/other_social` %in% c(NA, 0), 0, 1)
  df$d3_xi   <-
    ifelse(df$`info_mode/notice_board` %in% c(NA, 0), 0, 1)
  df$d3_xii  <- ifelse(df$`info_mode/newspapers` %in% c(NA, 0), 0, 1)
  df$d3_xiii <- ifelse(df$`info_mode/leaflet` %in% c(NA, 0), 0, 1)
  df$d3_xiv  <-
    ifelse(df$`info_mode/loud_speakers` %in% c(NA, 0), 0, 1)
  df$d3_xv   <- ifelse(df$`info_mode/radio` %in% c(NA, 0), 0, 1)
  
  df$d4      <- ifelse(df$aid_received == "yes", 1, 0)
  
  df$d5_i    <- ifelse(df$`aid_type/cash` %in% c(NA, 0), 0, 1)
  df$d5_ii   <- ifelse(df$`aid_type/food`  %in% c(NA, 0), 0, 1)
  df$d5_iii  <- ifelse(df$`aid_type/water`  %in% c(NA, 0), 0, 1)
  df$d5_iv   <- ifelse(df$`aid_type/fuel` %in% c(NA, 0), 0, 1)
  df$d5_v    <- ifelse(df$`aid_type/shelter` %in% c(NA, 0), 0, 1)
  df$d5_vi   <-
    ifelse(df$`aid_type/seasonal_items`  %in% c(NA, 0), 0, 1)
  df$d5_vii  <- ifelse(df$`aid_type/healthcare` %in% c(NA, 0), 0, 1)
  df$d5_viii <- ifelse(df$`aid_type/other_nfi` %in% c(NA, 0), 0, 1)
  df$d5_ix   <- ifelse(df$`aid_type/education` %in% c(NA, 0), 0, 1)
  df$d5_x    <- ifelse(df$`aid_type/protection` %in% c(NA, 0), 0, 1)
  
  df$d6      <-
    ifelse(df$aid_satisfaction == "yes",
           1,
           ifelse(
             df$aid_satisfaction %in% c("decline_to_answer", "do_not_know", "no"),
             0,
             NA
           ))
  df$d7      <-
    ifelse(df$`aid_not_satisfied/quantity`  %in% c(NA, 0), 0, 1)
  df$d10     <-
    ifelse(df$aid_workers_satisfied == "",
           NA,
           ifelse(df$aid_workers_satisfied == "no", 1, 0))
  df$d12     <- ifelse(df$complaint_mechanisms == "yes", 1, 0)
  df$d15   <-
    ifelse(
      df$covid_info_need == 'yes' &
        df$covid_info_type %in% c("prevention measures", "prevention_measures"),
      1,
      0
    )
  
  ##################################################### F ########################################
  df$f7 <- ifelse(
    df$property_damaged == "yes" &
      df$aware_compensation == "yes" &
      df$applied_compensation == "yes",
    ifelse(df$received_compensation == "yes", 1, 0),
    NA
  )
  df$f7b <- ifelse(df$complaint_mechanisms != "yes", 0, 1)
  # ################################################### g ########################################
  
  df$g4 <-
    ifelse(
      loop_children$attend_informal_ed[match(df$`_uuid`, indiv_df$`_submission__uuid`)] %in%
        c("do_not_know", "decline_to_answer", "yes", NA) &
        loop_children$attend_formal_ed[match(df$`_uuid`, indiv_df$`_submission__uuid`)] %in%
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
    dplyr::select(`_submission__uuid`, attend_informal_ed, attend_formal_ed)
  children_attend_ed <- children_attend_ed %>%
    group_by(`_submission__uuid`) %>%
    summarize(
      num_attend_formal = sum(attend_formal_ed),
      num_attend_informal = sum(attend_informal_ed)
    )
  
  df$g5 <-
    ifelse(
      is.na(children_attend_ed$num_attend_formal[match(df$`_uuid`, children_attend_ed$`_submission__uuid`)]),
      NA,
      children_attend_ed$num_attend_formal
    )
  
  df$g6 <-
    ifelse(
      is.na(children_attend_ed$num_attend_informal[match(df$`_uuid`, children_attend_ed$`_submission__uuid`)]),
      NA,
      children_attend_ed$num_attend_informal
    )
  rm(children_attend_ed)
  rm(loop_children)
  rm(loop_females)
  rm(loop_hoh)
  df$g7_i    <-
    ifelse(df$`reasons_not_attend/school_closed` %in% c(NA, 0), 0, 1)
  df$g7_ii   <-
    ifelse(df$`reasons_not_attend/not_safe` %in% c(NA, 0), 0, 1)
  df$g7_iii  <-
    ifelse(df$`reasons_not_attend/cannot_afford` %in% c(NA, 0), 0, 1)
  df$g7_iv   <-
    ifelse(df$`reasons_not_attend/impossible_to_enrol` %in% c(NA, 0), 0, 1)
  df$g7_v    <-
    ifelse(df$`reasons_not_attend/cannot_go_physically` %in% c(NA, 0),
           0,
           1)
  df$g7_vi   <-
    ifelse(df$`reasons_not_attend/overcrowded` %in% c(NA, 0), 0, 1)
  df$g7_vii  <-
    ifelse(df$`reasons_not_attend/lack_of_staff` %in% c(NA, 0), 0, 1)
  df$g7_viii <-
    ifelse(df$`reasons_not_attend/poor_infrastructure` %in% c(NA, 0), 0, 1)
  df$g7_ix   <-
    ifelse(df$`reasons_not_attend/curriculum` %in% c(NA, 0), 0, 1)
  df$g7_x    <-
    ifelse(df$`reasons_not_attend/children_working` %in% c(NA, 0), 0, 1)
  df$g7_xi   <-
    ifelse(df$`reasons_not_attend/parental_refusal` %in% c(NA, 0), 0, 1)
  df$g7_xii  <-
    ifelse(df$`reasons_not_attend/uninterested` %in% c(NA, 0), 0, 1)
  df$g7_xiii <-
    ifelse(df$`reasons_not_attend/lack_doc` %in% c(NA, 0), 0, 1)
  df$g7_xiv  <-
    ifelse(df$`reasons_not_attend/other` %in% c(NA, 0), 0, 1)
  
  
  df$g8 <-
    ifelse(
      df$primary_school_place %in% c("between_2_5", "within_2km") &
        df$secondary_school_place %in% c("between_2_5", "within_2km"),
      1,
      0
    )
  df$g9 <- ifelse(df$covid_dropout %in% c(NA, 0), 0, 1)
  
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
  
  #FOOD EXPENDITURE SHARE
  df$food_share <-
    as.numeric(df$food_exp) / as.numeric(df$tot_expenses)
  
  #FOOD CONSUMPTIONS SCORE
  df$fcs <-
    (as.numeric(df$cereals) * 2) + (as.numeric(df$nuts_seed) * 3) + (as.numeric(df$milk_dairy) * 4) + (as.numeric(df$meat) * 4) +
    as.numeric(df$vegetables) + as.numeric(df$fruits) + (as.numeric(df$oil_fats) * 0.5) + (as.numeric(df$sweets) * 0.5)
  
  df$livelihood_strategies <-
    case_when(df$emergency == 1 ~ 4, df$crisis == 1 ~ 3, df$stress == 1 ~ 2, TRUE ~ 1)
  df$food_share_strategies <-
    case_when(
      df$food_share < 0.5 ~ 1,
      between(df$food_share, 0.5, 0.6499) ~ 2,
      between(df$food_share, 0.65, 0.7499) ~ 3,
      df$food_share >= 0.75 ~ 4
    )
  df$fcs_strategies <-
    case_when(df$fcs < 21 ~ 4, between(df$fcs, 21, 35) ~ 3, TRUE ~ 1)
  
  df$mean_coping_capacity <-
    mean_row(df$livelihood_strategies, df$food_share_strategies, na.rm = TRUE)
  df$g14 <-
    round2(mean_row(df$mean_coping_capacity, df$fcs_strategies, na.rm = TRUE))
  df <-
    dplyr::select(
      df,
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
  
  fsc <- df %>%
    dplyr::select(`_uuid`,
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
  
  df$g15_i <-
    ifelse(fsc$hhs[match(df$`_uuid`, fsc$`_uuid`)] <= 1, 1, 0)
  df$g15_ii <-
    ifelse(between(fsc$hhs[match(df$`_uuid`, fsc$`_uuid`)], 2, 3), 1, 0)
  df$g15_iii <-
    ifelse(fsc$hhs[match(df$`_uuid`, fsc$`_uuid`)] >= 4, 1, 0)
  rm(fsc)
  df$g19 <-
    ifelse((
      loop_children$attend_formal_ed[match(df$`_uuid`, loop_children$`_submission__uuid`)] == "yes" |
        loop_children$attend_informal_ed[match(df$`_uuid`, loop_children$`_submission__uuid`)] == "yes"
    ) &
      loop_children$work[match(df$`_uuid`, loop_children$`_submission__uuid`)] == "yes",
    1,
    0
    )
  
  
  df$g25 <-
    ifelse(
      df$distance_hospital %in% c("less_15", "less_30", "less_hour") |
        df$distance_clinic %in% c("less_15", "less_30", "less_hour"),
      1,
      0
    )
  
  df$g26 <-
    ifelse(
      df$distance_hospital %in% c("less_15", "less_30", "less_hour") &
        df$hospital_emergency_ser == "yes" &
        df$hospital_maternity_ser == "yes" &
        df$hospital_surgical_ser == "yes" &
        df$hospital_pediatric_ser == "yes",
      1,
      0
    )
  
  df$g28 <-
    ifelse(df$distance_clinic %in% c("less_15", "less_30", "less_hour"),
           1,
           0)
  df$g29 <-
    ifelse(df$distance_hospital %in% c("less_15", "less_30", "less_hour"),
           1,
           0)
  df$g32 <- ifelse(df$women_specialised_services == "yes", 1, 0)
  
  df$g34_i    <- ifelse(df$`health_barriers/cost` %in% c(NA, 0), 0, 1)
  df$g34_ii   <-
    ifelse(df$`health_barriers/unqualified_staff` %in% c(NA, 0), 0, 1)
  df$g34_iii  <-
    ifelse(df$`health_barriers/civ_docs_problems` %in% c(NA, 0), 0, 1)
  df$g34_iv   <-
    ifelse(df$`health_barriers/no_referral_phc` %in% c(NA, 0), 0, 1)
  df$g34_v    <-
    ifelse(df$`health_barriers/phc_closed` %in% c(NA, 0), 0, 1)
  df$g34_vi   <-
    ifelse(df$`health_barriers/distance_to_treatmentcenter` %in% c(NA, 0),
           0,
           1)
  df$g34_vii  <-
    ifelse(df$`health_barriers/refused_treatment` %in% c(NA, 0), 0, 1)
  df$g34_viii <-
    ifelse(df$`health_barriers/no_medicine` %in% c(NA, 0), 0, 1)
  df$g34_ix   <-
    ifelse(df$`health_barriers/no_offered_treatment` %in% c(NA, 0), 0, 1)
  df$g34_x    <-
    ifelse(df$`health_barriers/not_inclusive` %in% c(NA, 0), 0, 1)
  df$g34_xi   <-
    ifelse(df$`health_barriers/no_fem_staff` %in% c(NA, 0), 0, 1)
  
  df$g35 <-
    ifelse(indiv_df$`health_issue/chronic`[match(df$`_uuid`, indiv_df$`_submission__uuid`)] %in% c(NA, 0), 0, 1)
  
  df$g37 <- ifelse(df$how_much_debt > 505000, 1, 0)
  df$g38 <-
    ifelse(
      df$reasons_for_debt %in% c("basic_hh_expenditure", "education", "food", "health"),
      1,
      0
    )
  
  df$g39 <- ifelse(df$covid_loss_job == "yes"  ,
                   1,
                   0)
  
  df$g41 <- ifelse(df$market_place %in% c("less_15", "less_30"), 1, 0)
  
  df$g44 <-
    ifelse(indiv_df$age[match(df$`_uuid`, indiv_df$`_submission__uuid`)] >= 18 &
             indiv_df$actively_seek_work[match(df$`_uuid`, indiv_df$`_submission__uuid`)] == "yes", 1, 0)
  
  df$g45_i    <-
    ifelse(df$`employment_primary_barriers/increased_competition` == 1,
           1,
           0)
  df$g45_ii   <-
    ifelse(df$`employment_primary_barriers/jobs_far` == 1, 1, 0)
  df$g45_iii  <-
    ifelse(df$`employment_primary_barriers/only_low_available` == 1, 1, 0)
  df$g45_iv   <-
    ifelse(df$`employment_primary_barriers/underqualified_for_jobs` == 1,
           1,
           0)
  df$g45_v    <-
    ifelse(df$`employment_primary_barriers/lack_of_connections` == 1,
           1,
           0)
  df$g45_vi   <-
    ifelse(df$`employment_primary_barriers/lack_jobs_women` == 1, 1, 0)
  df$g45_vii  <-
    ifelse(df$`employment_primary_barriers/none` == 1, 1, 0)
  df$g45_viii <-
    ifelse(df$`employment_primary_barriers/other` == 1, 1, 0)
  
  
  df$g46 <- ifelse(df$employment_seasonal == "yes", 1, 0)
  
  df$g47_i <-
    ifelse(between(indiv_df$age[match(df$`_uuid`, indiv_df$`_submission__uuid`)], 18, 59) &
             indiv_df$sex[match(df$`_uuid`, indiv_df$`_submission__uuid`)] == "female" &
             indiv_df$work[match(df$`_uuid`, indiv_df$`_submission__uuid`)] == "yes",
           1,
           0)
  
  df$g47_ii <-
    ifelse(between(indiv_df$age[match(df$`_uuid`, indiv_df$`_submission__uuid`)], 18, 59) &
             indiv_df$sex[match(df$`_uuid`, indiv_df$`_submission__uuid`)] == "male" &
             indiv_df$work[match(df$`_uuid`, indiv_df$`_submission__uuid`)] == "yes",
           1,
           0)
  
  
  
  df$g51 <- ifelse(
    df$pds_card == "no" |
      df$id_card_a18 == "no" |
      df$nationality_cert_a18 == "no" |
      df$id_card_u18 == "no" |
      df$nationality_cert_u18 == "no" |
      df$birth_cert_u18 == "no",
    1,
    0
  )
  df$g51b <-
    ifelse(df$id_card_u18 == "no" |
             df$nationality_cert_u18 == "no" |
             df$birth_cert_u18 == "no",
           1,
           0)
  df$g52 <- ifelse(
    df$`disciplinary_measures/shouted` == 1 |
      df$`disciplinary_measures/spanked` == 1 ,
    1,
    0
  )
  
  df$g53a <-
    ifelse(df$not_residing %in% c(NA, "decline_to_answer", 'no', "do_not_know"),
           0,
           1)
  df$g53b_i <-
    ifelse(df$not_residing %in% c(NA, "decline_to_answer", 'no', "do_not_know") &
             df$married)

  
  df$g54_i   <-
    ifelse(
      df$restriction_clearance %in% c("yes") &
        df$restriction_clearance_covid %in% c("similar", "no"),
      1,
      0
    )
  df$g54_ii  <-
    ifelse(
      df$restriction_documents %in% c("yes") &
        df$restriction_documents_covid %in% c("similar", "no"),
      1,
      0
    )
  df$g54_iii <-
    ifelse(
      df$restriction_time %in% c("yes") &
        df$restriction_time_covid %in% c("similar", "no"),
      1,
      0
    )
  df$g54_iv   <-
    ifelse(
      df$restriction_reason %in% c("yes") &
        df$restriction_reason_covid %in% c("similar", "no"),
      1,
      0
    )
  df$g54_v  <-
    ifelse(
      df$restriction_physical %in% c("yes") &
        df$restriction_physical_covid %in% c("similar", "no"),
      1,
      0
    )
  df$g54_vi <-
    ifelse(df$restriction_other %in% c("yes"),
           1,
           0)
  df$g54 <-
    ifelse(
      df$g54_i == 1 |
        df$g54_ii == 1 |
        df$g54_iii == 1 |
        df$g54_iv == 1 |
        df$g54_v == 1 |
        df$g54_vi == 1,
      1,
      0
    )
  
  df$g56 <-
    ifelse(df$child_distress_number < 1 |
             is.na(df$child_distress_number),
           0,
           1)
  df$g57 <-
    ifelse(df$adult_distress_number < 1 |
             is.na(df$adult_distress_number),
           0,
           1)
  
  df$g61 <-
    ifelse(df$security_incident %in% c(NA, "decline_to_answer", 'no', "do_not_know"),
           0,
           1)
  df$g62_i <-
    ifelse(df$security_incident_gender %in% c("male", "both"), 1, 0)
  df$g62_ii <-
    ifelse(df$security_incident_gender %in% c('female', "both"), 1, 0)
  
  df$g63 <-
    ifelse(df$feel_unsafe %in% c(NA, "decline_to_answer", 'no', "do_not_know"),
           0,
           1)
  
  df$g64 <- ifelse(df$hh_risk_eviction == "yes", 1, 0)
  
  df$g65_i    <- ifelse(df$`hh_main_risks/lack_funds` == 1, 1, 0)
  df$g65_ii   <-
    ifelse(df$`hh_main_risks/no_longer_hosted` == 1, 1, 0)
  df$g65_iii  <-
    ifelse(df$`hh_main_risks/unaccepted_by_community` == 1, 1, 0)
  df$g65_iv   <-
    ifelse(df$`hh_main_risks/authorities_request` == 1, 1, 0)
  df$g65_v    <- ifelse(df$`hh_main_risks/owner_request` == 1, 1, 0)
  df$g65_vi   <- ifelse(df$`hh_main_risks/no_agreement` == 1, 1, 0)
  df$g65_vii  <- ifelse(df$`hh_main_risks/inadequate` == 1, 1, 0)
  df$g65_viii <- ifelse(df$`hh_main_risks/occupied` == 1, 1, 0)
  df$g65_ix   <- ifelse(df$`hh_main_risks/confiscation` == 1, 1, 0)
  df$g65_x    <- ifelse(df$`hh_main_risks/dispute` == 1, 1, 0)
  
  df$g66 <- ifelse(df$hlp_document == "yes", 1, 0)
  
  df$g67 <- ifelse(
    df$`why_not_return/house_land_occupied` %in% c(NA, 0) |
      df$`why_not_return/house_damaged_destroyed` %in% c(NA, 0),
    0,
    1
  )
  
  df$g68 <-
    ifelse(df$hh_dispute %in% c("decline_to_answer", 'no', "do_not_know", NA),
           0,
           1)
  df$g73 <-
    ifelse(df$`why_not_return/presence_of_mines` %in% c(NA, 0), 0, 1)
  df$g74 <-
    ifelse(df$risk_education %in% c('no', "do_not_know", NA), 0, 1)
  
  df$g85 <- ifelse(
    df$`nfi_priority_needs/bedding_items` == 1 |
      df$`nfi_priority_needs/mattresses_sleeping_mats` == 1 |
      df$`nfi_priority_needs/blankets` == 1 |
      df$`nfi_priority_needs/cooking_utensils` == 1 |
      df$`nfi_priority_needs/cooking_stove` == 1 |
      df$`nfi_priority_needs/winter_heaters` == 1 |
      df$`nfi_priority_needs/clothing` == 1 |
      df$`nfi_priority_needs/heating_cooking_fuel` == 1 |
      df$`nfi_priority_needs/other` == 1 |
      df$`nfi_priority_needs/none` == 1 ,
    1,
    0
  )
  
  df$g89 <-
    ifelse(rowSums(
      cbind(
        as.numeric(df$`shelter_better/protec_hazards`),
        as.numeric(df$`shelter_better/improve_safety`),
        as.numeric(df$`shelter_better/improve_privacy`),
        as.numeric(df$`shelter_better/protect_climate`),
        as.numeric(df$`shelter_better/other`),
        as.numeric(df$`shelter_better/none`)
      ),
      na.rm = T
    ) >= 2, 1, 0)
  
  df$g94 <- ifelse(
    df$sufficient_water_drinking == "yes" &
      df$sufficient_water_cooking == "yes" &
      df$sufficient_water_hygiene == "yes" &
      df$sufficient_water_other == "yes",
    1,
    0
  )
  
  df$g95 <- ifelse(
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
  
  df$g96 <-
    ifelse(df$treat_drink_water %in% c("always", "sometimes"), 1, 0)
  df$g97 <- ifelse(df$latrines %in% c("vip_pit", "flush"), 1, 0)
  df$g98 <-
    ifelse(df$access_hygiene_items  %in% c("satisfied", "very_satisfied"),
           1,
           0)
  df$g99 <-
    ifelse(df$access_soap == "yes" &
             df$`use_of_soap/handwashing` == 1,
           1,
           0)
  
  df$g100_i <-
    as.numeric(df$food_exp) / as.numeric(df$tot_expenses) * 100
  df$g100_ii <-
    as.numeric(df$rent_exp) / as.numeric(df$tot_expenses) * 100
  df$g100_iii <-
    as.numeric(df$medical_exp) / as.numeric(df$tot_expenses) * 100
  
  df$g102 <-
    ifelse(as.numeric(df$tot_expenses) * 0.4 <= df$food_exp, 1, 0)
  
  return(df)
}
round2 <- function(x, n=0) {
  posneg <- sign(x)
  z <- abs(x)*10^n
  z <- z + 0.5
  z <- trunc(z)
  z <- z/10^n
  z <- z*posneg
  return(z)
}
