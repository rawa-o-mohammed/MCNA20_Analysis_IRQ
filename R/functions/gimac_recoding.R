recoding_covid19 <- function(r){
  
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
r$food_share <- r$food_exp / r$tot_expenditure

#FOOD CONSUMPTIONS SCORE
r$fcs <-
  as.numeric(r$cereals) * 2 + as.numeric(r$nuts_seed) * 3 + as.numeric(r$milk_dairy) * 4 + as.numeric(r$meat) * 4 +
  as.numeric(r$vegetables) + as.numeric(r$fruits) + as.numeric(r$oil_fats) * 0.5 + as.numeric(r$sweets) * 0.5


r$livelihood_strategies <-
  case_when(r$emergency == 1 ~ 4,
            r$crisis == 1 ~ 3,
            r$stress == 1 ~ 2,
            TRUE ~ 1)
r$food_share_strategies <-
  case_when(
    r$food_share <= 0.5 ~ 1,
    (r$food_share > 0.5 & r$food_share <= 0.65) ~ 2,
    (r$food_share > 0.65 & r$food_share <= 0.75) ~ 3,
    TRUE ~ 4
  )
r$fcs_strategies <-
  case_when(r$fcs <= 28 ~ 4, (r$fcs > 28 &
                                   r$fcs <= 42) ~ 3, TRUE ~ 1)
r$fcs_poor <- ifelse(r$fcs >= 0 & r$fcs <= 21, 1,0)
r$fcs_borderline <- ifelse(r$fcs > 25 & r$fcs <= 35 , 1,0)
r$fcs_acceptable <- ifelse(r$fcs > 35, 1,0)



r$mean_coping_capacity <-
  rowMeans(r[, c("livelihood_strategies", "food_share_strategies")])
r$c14 <-
  rowMeans(r[, c("mean_coping_capacity", "fcs_strategies")])

r$c16 <-
  ifelse(
    r$selling_assets %in% c("no_already_did", "yes") |
      r$borrow_debt  %in% c("no_already_did", "yes") |
      r$reduce_spending %in% c("no_already_did", "yes") |
      r$child_work %in% c("no_already_did", "yes") |
      r$adult_risky  %in% c("no_already_did", "yes"),
    1,
    0
  )


r$c24 <- ifelse(r$tot_income < 480000, 1, 0)
r$c29 <- ifelse(r$how_much_debt > 505000, 1, 0)

r$c30_1 <-
  ifelse(
    r$reasons_for_debt %in%  c("basic_hh_expenditure",
                                  "health",
                                  "food",
                                  "education"),
    1,
    0
  )

r$c33_i   <- ifelse(r$hh_main_risks.lack_funds == 1, 1, 0)
r$c33_ii  <-
  ifelse(r$hh_main_risks.no_longer_hosted == 1, 1, 0)
r$c33_iii <- ifelse(r$hh_main_risks.no_agreement == 1, 1, 0)

return(r)
}



recoding_covid20 <- function(r){
  
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
  r$food_share <- r$food_exp / r$tot_expenses
  
  #FOOD CONSUMPTIONS SCORE
  r$fcs <-
    (as.numeric(r$cereals) * 2) + (as.numeric(r$nuts_seed) * 3) + (as.numeric(r$milk_dairy) * 4) + (as.numeric(r$meat) * 4) +
    as.numeric(r$vegetables) + as.numeric(r$fruits) + (as.numeric(r$oil_fats) * 0.5) + (as.numeric(r$sweets) * 0.5)
  
  r$livelihood_strategies <-
    case_when(r$emergency == 1 ~ 4,
              r$crisis == 1 ~ 3,
              r$stress == 1 ~ 2,
              TRUE ~ 1)
  r$food_share_strategies <-
    case_when(
      r$food_share < 0.5 ~ 1,
      between(r$food_share, 0.5, 0.6499) ~ 2,
      between(r$food_share, 0.65, 0.7499) ~ 3,
      r$food_share >= 0.75 ~ 4
    )
  r$fcs_strategies <-
    case_when(r$fcs < 21 ~ 4, between(r$fcs, 21, 35) ~ 3, TRUE ~ 1)
  r$fcs_poor <- ifelse(r$fcs >= 0 & r$fcs <= 21, 1,0)
  r$fcs_borderline <- ifelse(r$fcs > 25 & r$fcs <= 35 , 1,0)
  r$fcs_acceptable <- ifelse(r$fcs > 35, 1,0)
  
  r$mean_coping_capacity <-
    mean_row(r$livelihood_strategies,
             r$food_share_strategies,
             na.rm = TRUE)
  r$c14 <-
    round2(mean_row(r$mean_coping_capacity, r$fcs_strategies, na.rm = TRUE))
  
  
  r$c16 <-
    ifelse(
      r$selling_assets %in% c("no_already_did", "yes") |
        r$borrow_debt  %in% c("no_already_did", "yes") |
        r$reduce_spending %in% c("no_already_did", "yes") |
        r$child_work %in% c("no_already_did", "yes") |
        r$adult_risky  %in% c("no_already_did", "yes"),
      1,
      0
    )
  
  r$c24 <- ifelse(r$inc_employment_pension < 480000, 1, 0)
  
  r$c29 <- ifelse(r$how_much_debt > 505000, 1, 0)
  
  r$c30_1 <-
    ifelse(
      r$reasons_for_debt %in%  c("basic_hh_expenditure",
                                 "health",
                                 "food",
                                 "education"),
      1,
      0
    )
  
  r$c33_i   <- r$hh_main_risks.lack_funds
  r$c33_ii  <- r$hh_main_risks.no_longer_hosted
  r$c33_iii <- r$hh_main_risks.no_agreement
  
  return(r)
}