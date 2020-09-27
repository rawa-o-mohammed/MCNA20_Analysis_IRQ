round2 = function(x, n=0) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

#r <- response
#l <- loop
individual_to_HH_numeric <- function(loop, response, varname, indicator) {
  r <- loop[,c("X_uuid", varname)]
  r <-r[complete.cases(r), ]
  r = aggregate(r[,c(2)],
                by = list(r$X_uuid),
                FUN = sum, na.rm = T)
  names(r) <- c("X_uuid", indicator)
  response <- merge(response, r, by="X_uuid", all = T)
  return(response)
}


recoding_hno <- function(r, loop){
 
#AAP
##############
r <- r %>% mutate(s_2 = case_when(
  r$aid_satisfaction == "yes" & r$complaint_mechanisms == "yes" ~ 1,
  r$aid_satisfaction == "yes" & r$complaint_mechanisms == "no" ~ 2,
  r$aid_satisfaction == "no" & r$complaint_mechanisms == "yes" ~ 3,
  r$aid_satisfaction == "no" & r$complaint_mechanisms == "no" ~ 4,
    ))

#DISABILITY
#############
count_difficulty_level <- function(df) {
  diff <-  df[c(which(startsWith(names(df), "difficulty_")))]                   
  diff$no_diff <- rowSums(diff == "no_difficulty")
  diff$some_diff <- rowSums(diff == "some_difficulty")
  diff$lot_diff <- rowSums(diff == "a_lot_of_difficulty")
  diff$cannot_diff <- rowSums(diff == "cannot_do_at_all")
  diff <- diff[, c("no_diff", "some_diff", "lot_diff", "cannot_diff")]
  df <- cbind(df, diff)
  return(df)
}
r <- count_difficulty_level(r)
#difficulty <- difficulty[, c("no_diff", "some_diff", "lot_diff", "cannot_diff")]
#r <- cbind(r, difficulty)

r <- r %>% mutate(s_3 = case_when(
  r$some_diff == 0 & r$lot_diff == 0 & r$cannot_diff == 0 ~ 1,
  r$lot_diff == 0 & r$cannot_diff == 0 & r$some_diff >0 & 
    r$some_diff <= 3 ~ 2,
  (r$cannot_diff == 0 & r$lot_diff > 0 & r$lot_diff <= 3) |
    (r$lot_diff == 0 & r$cannot_diff == 0 & r$some_diff >= 4) ~ 3,
  (r$cannot_diff > 0 & r$cannot_diff <= 3) | 
    r$lot_diff >= 4 ~ 4,
  r$cannot_diff >= 4 ~ 5
))


#EDUCATION
############
r$perc_edu <- apply(r, 1, FUN=function(x){
  (loop %>% filter(X_uuid == x["X_uuid"] & (attend_formal_ed == "yes" | attend_informal_ed == "yes")) %>% nrow) / 
    (loop %>% filter(X_uuid == x["X_uuid"] & attend_formal_ed != "") %>% nrow)
})
r <- r %>% mutate(s_4 = case_when(
  r$perc_edu >= 0.9 ~ 1,
  r$perc_edu >= 0.7 & r$perc_edu < 0.9 ~ 2,
  r$perc_edu >= 0.55 & r$perc_edu < 0.7 ~ 3,
  r$perc_edu >= 0.3 & r$perc_edu < 0.55 ~ 4,
  r$perc_edu >= 0 & r$perc_edu < 0.3 ~ 5
))


#LIVELIHOODS
############
r$perc_unemp <- apply(r, 1, FUN=function(x){
  (loop %>% filter(X_uuid == x["X_uuid"] & age > 17 & work == "no" & actively_seek_work %in% c("yes")) %>% nrow) / 
    (loop %>% filter(X_uuid == x["X_uuid"] & age > 17) %>% nrow)
})
r <- r %>% mutate(s_6 = case_when(
  r$perc_unemp == 0 ~ 1,
  r$perc_unemp > 0 & r$perc_unemp <= 0.5 ~ 2,
  r$perc_unemp > 0.5 & r$perc_unemp <= 0.7 ~ 3,
  r$perc_unemp > 0.7 & r$perc_unemp <= 0.9 ~ 4,
  r$perc_unemp > 0.9 ~ 5
))


r <- r %>% mutate(s_7 = case_when(
  is.na(r$reasons_for_debt) ~ 1,
  r$reasons_for_debt %in% c("", "clothing", "other", "purchase_pro_assets") ~ 1,
  r$reasons_for_debt %in% c("education", "basic_hh_expenditure") ~ 3,
  r$reasons_for_debt == "health" ~ 4,
  r$reasons_for_debt == "food" ~ 5
))

#FOOD SECURITY
##############
r$stress <- ifelse(r$selling_assets %in% c("no_already_did", "yes") |
                     r$borrow_debt  %in% c("no_already_did", "yes") |
                     r$reduce_spending %in% c("no_already_did", "yes") |
                     r$spending_savings %in% c("no_already_did", "yes")   , 1, 0)
r$crisis <- ifelse(r$selling_transportation_means %in% c("no_already_did", "yes") |
                     r$change_place  %in% c("no_already_did", "yes") |
                     r$child_work %in% c("no_already_did", "yes"), 1, 0)
r$emergency <- ifelse(r$child_dropout_school %in% c("no_already_did", "yes") |
                        r$adult_risky  %in% c("no_already_did", "yes") |
                        r$family_migrating %in% c("no_already_did", "yes") |
                        r$child_forced_marriage %in% c("no_already_did", "yes"), 1, 0)
r$emergency_1 <- ifelse(r$child_dropout_school %in% c("no_already_did", "yes"), 1, 0)
r$emergency_2 <- ifelse(r$adult_risky %in% c("no_already_did", "yes"), 1, 0)
r$emergency_3 <- ifelse(r$family_migrating %in% c("no_already_did", "yes"), 1, 0)
r$emergency_4 <- ifelse(r$child_forced_marriage %in% c("no_already_did", "yes"), 1, 0)
r$emergency_count <- rowSums(r[, c("emergency_1", "emergency_2", "emergency_3", "emergency_4")], na.rm = T)

#r$s9_5 <- ifelse(r$stress == 1 & r$crisis == 0 & r$emergency == 0, 1,0)
r <- r %>% mutate(s_9 = case_when(
  r$stress == 0 & r$crisis == 0 & r$emergency == 0 ~ 1,
  r$stress == 1 & r$crisis == 0 & r$emergency == 0 ~ 2,
  r$crisis == 1 & r$emergency == 0 ~ 3,
  r$emergency_count == 1 ~ 4, 
  r$emergency_count > 1 ~ 5
))


r$food_share <- r$food_exp / r$tot_expenses
r$food_share <- ifelse(r$food_share > 1, NA, 
                       r$food_share)
r <- r %>% mutate(s_10 = case_when(
  r$food_share < 0.5 ~ 1,
  r$food_share >= 0.5 & r$food_share < 0.65 ~ 2,
  r$food_share >= 0.65 & r$food_share <0.75 ~ 3,
  r$food_share >= 0.75 & r$food_share <0.85 ~ 4,
  r$food_share >= 0.85 & r$food_share <1 ~ 5
))

r$hhh1_1 <- ifelse(r$no_food == "yes",1,0)
r <- r %>% mutate(hhh1_2 = case_when(
  no_food_freq == "rarely" ~ 1,
  no_food_freq == "sometimes"  ~ 1,
  no_food_freq == "often" ~ 2,
  no_food_freq == ""  ~ 0,
))
r$hhh1_3 <- r$hhh1_1 * r$hhh1_2
r$hhh2_1 <- ifelse(r$hungry == "yes",1,0)
r <- r %>% mutate(hhh2_2 = case_when(
  hungry_freq == "rarely" ~ 1,
  hungry_freq == "sometimes"  ~ 1,
  hungry_freq == "often" ~ 2,
  hungry_freq == ""  ~ 0,
))
r$hhh2_3 <- r$hhh2_1 * r$hhh2_2
r$hhh3_1 <- ifelse(r$not_eating == "yes",1,0)
r <- r %>% mutate(hhh3_2 = case_when(
  not_eating_freq == "rarely" ~ 1,
  not_eating_freq == "sometimes"  ~ 1,
  not_eating_freq == "often" ~ 2,
  not_eating_freq == ""  ~ 0,
))
r$hhh3_3 <- r$hhh3_1 * r$hhh3_2
r$hhs <- rowSums(r[,c("hhh1_3", "hhh2_3", "hhh3_3")], na.rm=T)
r <- r %>% mutate(s_12 = case_when(
  r$hhs == 0 ~ 1,
  r$hhs == 1 ~ 2,
  r$hhs == 2 | r$hhs == 3 ~ 3,
  r$hhs == 4 ~ 4,
  r$hhs == 5 | r$hhs == 6 ~ 5
))

r$fcs <- r$cereals * 2 + r$nuts_seed * 3 + r$milk_dairy * 4 + r$meat * 4 + 
  r$vegetables + r$fruits + r$oil_fats * 0.5 + r$sweets * 0.5
#r$poor_fcs <- ifelse(r$fcs <= 21, 1,0)
#r$borderline_fcs <- ifelse(r$fcs > 21 & r$fcs <=35,1,0)
#r$acceptable_fcs <- ifelse(r$fcs > 35,1,0)

r <- r %>% mutate(s_11 = case_when(
  r$fcs >= 42.5 & r$hhs == 0 ~ 1,
  r$fcs >= 42.5 & r$hhs > 0 ~ 2,
  r$fcs <= 42 & r$fcs >= 28.5 ~ 3,
  r$fcs <= 28 & r$hhs <= 4 ~ 4,
  r$fcs <= 28 & r$hhs >= 5 ~ 5
))

#r <- r %>% mutate(s_11_old = case_when(
#  r$acceptable_fcs == 1 ~ 1,
#  r$borderline_fcs == 1 ~ 3,
#  r$poor_fcs == 1 ~ 4
#))

#HEALTH
##########
r <- r %>% mutate(dist_clinic_num = case_when(
  r$distance_clinic == "less_15" ~ 1,
  r$distance_clinic == "less_30" ~ 2,
  r$distance_clinic == "less_hour" ~ 3,
  r$distance_clinic == "less_3hours" ~ 4,
  r$distance_clinic == "more_3hours" ~ 5,
))
r <- r %>% mutate(dist_hospital_num = case_when(
  r$distance_hospital == "less_15" ~ 1,
  r$distance_hospital == "less_30" ~ 2,
  r$distance_hospital == "less_hour" ~ 3,
  r$distance_hospital == "less_3hours" ~ 4,
  r$distance_hospital == "more_3hours" ~ 5,
))
r <- r %>% mutate(distance_healthcare = case_when(
  r$dist_hospital_num >= r$dist_clinic_num ~ r$distance_clinic,
  r$dist_hospital_num <= r$dist_clinic_num ~ r$distance_hospital,
))
r <- r %>% mutate(s_13 = case_when(
  r$distance_healthcare == "less_15" ~ 1,
  r$distance_healthcare == "less_30" ~ 2,
  r$distance_healthcare == "less_hour" ~ 3,
  r$distance_healthcare == "less_3hours" ~ 4,
  r$distance_healthcare == "more_3hours" ~ 5,
))


#r$above_50 <- apply(r, 1, FUN=function(x){
#  ifelse(any(loop$age[which(loop$X_uuid == x["X_uuid"])] > 50), 1, 0)
#})

names(loop)[names(loop) == "X_uuid"] <- "X_uuid"
loop$above_50 <- ifelse(loop$age > 50,1,0)
loop$chronic_and_age <- ifelse(loop$age > 50 & loop$health_issue.chronic == 1, 1,0)
r <- individual_to_HH_numeric(loop, r, "above_50", "above_50")
r <- individual_to_HH_numeric(loop, r, "health_issue.chronic", "health_issue.chronic")

r <- r %>% mutate(s_14 = case_when(
  rowSums(cbind(r$health_issue.chronic, r$above_50), na.rm = T) == 0 ~ 1,
  rowSums(cbind(r$health_issue.chronic, r$above_50), na.rm = T) == 1 ~ 2,
  rowSums(cbind(r$health_issue.chronic, r$above_50), na.rm = T) == 2 ~ 3,
  rowSums(cbind(r$health_issue.chronic, r$above_50), na.rm = T) == 3 ~ 4,
  rowSums(cbind(r$health_issue.chronic, r$above_50), na.rm = T) >= 4 ~ 5
))

#PROTECTION - GBV
#################
r$nr_unsafe <- rowSums(r[,c("unsafe_areas.distribution_areas", 
                           "unsafe_areas.facilities", "unsafe_areas.markets",
                           "unsafe_areas.social_areas", "unsafe_areas.water_points",
                           "unsafe_areas.way_to_centers", "unsafe_areas.way_to_school")], na.rm=T)
r <- r %>% mutate(s_15 = case_when(
  r$feel_unsafe == "no" ~ 1,
  r$nr_unsafe == 1 ~ 3,
  r$nr_unsafe > 1 ~ 4,
))


#PROTECTION - GP
###############
r$doc_pds <- ifelse(r$pds_card == "no",1,0)
r$doc_nat <- ifelse(r$nationality_cert_a18 == "no" | r$nationality_cert_u18 == "no",1,0)
r$doc_bir <- ifelse(r$birth_cert_u18 == "no",1,0)
r$doc_id <- ifelse(r$id_card_a18 == "no" | r$id_card_u18 == "no",1,0)
r$nr_docs <- rowSums(r[,c("doc_pds", "doc_nat", "doc_bir",
                            "doc_id")], na.rm=T)
r <- r %>% mutate(s_16 = case_when(
  r$nr_docs == 0 ~ 1,
  r$nr_docs == 1 ~ 2,
  r$nr_docs == 2 ~ 3,
  r$nr_docs == 3 ~ 4,
  r$nr_docs == 4 ~ 5,
))


#PROTECTION - HLP
##############
r <- r %>% mutate(s_18 = case_when(
  r$hh_risk_eviction == "no" ~ 1,
  (r$hh_risk_eviction == "yes" & r$hh_main_risks.lack_funds == 1) |
    (r$hh_risk_eviction == "yes" & r$hh_main_risks.no_agreement == 1) |
    (r$hh_risk_eviction == "yes" & r$hh_main_risks.owner_request == 1) |
    (r$hh_risk_eviction == "yes" & r$hh_main_risks.no_longer_hosted == 1) |
    (r$hh_risk_eviction == "yes" & r$hh_main_risks.dispute == 1) ~ 3,
  (r$hh_risk_eviction == "yes" & r$hh_main_risks.inadequate == 1) |
    (r$hh_risk_eviction == "yes" & r$hh_main_risks.unaccepted_by_community == 1) |
    (r$hh_risk_eviction == "yes" & r$hh_main_risks.authorities_request == 1) |
    (r$hh_risk_eviction == "yes" & r$hh_main_risks.confiscation == 1) |
    (r$hh_risk_eviction == "yes" & r$hh_main_risks.occupied == 1) ~ 4,
))


#CHILD PROTECTION COMPOSITE
################
loop_children <- loop[which(loop$age < 18),]
r$child_marriage <- apply(r, 1, FUN=function(x){
  ifelse(any(loop_children$marital_status[which(loop_children$X_uuid == x["X_uuid"])] == "married"), 1, 0)
})
r$child_labor <- apply(r, 1, FUN=function(x){
  ifelse(any(loop$age[which(loop$X_uuid == x["X_uuid"])] < 18 & 
               loop$work[which(loop$X_uuid == x["X_uuid"])] == "yes"), 1, 0)
})
r$child_education <- apply(r, 1, FUN=function(x){
  ifelse(any(loop$attend_formal_ed[which(loop$X_uuid == x["X_uuid"])] == "no" & 
               loop$attend_informal_ed[which(loop$X_uuid == x["X_uuid"])] == "no"), 1, 0)
})
r$child_documents <- ifelse(r$id_card_u18 == "no" | r$birth_cert_u18 == "no" | 
                              r$nationality_cert_u18 == "no", 1, 0)
r$child_distress <- ifelse(r$child_distress_number < 1 | is.na(r$child_distress_number), 0, 1)
r$child_composite <- rowSums(r[,c("child_marriage", "child_labor", "child_documents", "child_distress", "child_education")], na.rm = T)
r <- r %>% mutate(s_20 = case_when(
  r$child_composite == 0 ~ 1,
  r$child_composite == 1 ~ 3,
  r$child_composite == 2 ~ 4,
  r$child_composite >= 3 ~ 5,
))
r$s_20 <- ifelse(r$tot_child == 0, NA, 
                 r$s_20)

#SHELTER & NFI
###############
r <- r %>% mutate(s_21 = case_when(
  r$shelter_better.none == 1 ~ 1,
  r$shelter_better.other == 1 ~ 2,
  r$shelter_better.improve_privacy | r$shelter_better.protect_climate == 1 ~ 3,
  r$shelter_better.protec_hazards | r$shelter_better.improve_safety == 1 ~ 4,
))

#WASH
#############
r <- r %>% mutate(s_23 = case_when(
  (r$drinking_water_source == "network_private") == 1 ~ 1,
  (r$drinking_water_source == "network_comm" | r$drinking_water_source == "borehole" |
    r$drinking_water_source == "prot_well" | r$drinking_water_source == "prot_spring") ~ 2,
  (r$drinking_water_source == "bottled_water" | r$drinking_water_source == "water_trucking") ~ 3,
  (r$drinking_water_source == "unprot_spring" | r$drinking_water_source == "unprot_well") ~ 4,
  (r$drinking_water_source == "surface_water") ~ 5
))



r <- r %>% mutate(s_24 = case_when(
  (r$sufficient_water_cooking == "yes" & r$sufficient_water_drinking == "yes" &
    r$sufficient_water_hygiene == "yes" & r$sufficient_water_other == "yes") ~ 1,
  (r$sufficient_water_cooking == "yes" & r$sufficient_water_drinking == "yes" &
    r$sufficient_water_hygiene == "yes" & r$sufficient_water_other == "no") ~ 2,
  (r$sufficient_water_drinking == "yes" & (r$sufficient_water_cooking == "yes" & 
  r$sufficient_water_hygiene == "no") | (r$sufficient_water_cooking == "no" & 
                                           r$sufficient_water_hygiene == "yes")) ~ 3,
  (r$sufficient_water_drinking == "yes" & r$sufficient_water_cooking == "no" & 
    r$sufficient_water_hygiene == "no") ~ 4,
  (r$sufficient_water_drinking == "no") ~ 5,
))

#WATER SUFFICIENCY INDICATOR FOR INCAMP IDPs
r$water_quantity <- r$tank_capacity * r$refill_times / r$people_share_tank / 7
r <- r %>% mutate(s_24_1 = case_when(
  r$water_quantity  >= 50 ~ 1,
  r$water_quantity >= 15 & r$water_quantity <= 50 ~ 2,
  r$water_quantity >= 9 & r$water_quantity < 15 ~ 3,
  r$water_quantity >=3 & r$water_quantity < 9 ~ 4,
  r$water_quantity < 3 ~ 5,
))

#MERGE INCAMP AND OUTCAMP WATER SUFFICIENCY INDICATORS 
r$s_24 <- ifelse(r$population_group == "idp_in_camp", r$s_24_1, 
                 r$s_24)
r$s_24_1 <- NULL


r$impr_san <- ifelse(r$latrines == "flush" | r$latrines == "vip_pit",1,0)
r <- r %>% mutate(s_25 = case_when(
  r$impr_san == 1 & r$shared_sanitation == "no" ~ 1,
  r$impr_san == 1 & r$shared_sanitation == "yes" ~ 2,
  r$impr_san == 0 & r$shared_sanitation == "no" ~ 3,
  r$impr_san == 0 & r$shared_sanitation == "yes" ~ 4,
  r$latrines == "open" ~ 5,
))


r <- r %>% mutate(s_26 = case_when(
  r$access_soap == "yes" & r$use_of_soap.handwashing == 1 ~ 1,
  r$access_soap == "yes" & r$use_of_soap.handwashing == 0 ~ 3,
  r$access_soap == "no" ~ 5
))

r <- merge(r, ila_analysis, by="strata", all.x = T)
r <- merge(r, explosive_analysis, by="district_mcna", all.x = T)


#MEAN OF MAX 50% CALCULATION
hno <-  r[c(which(startsWith(names(r), "s_")))]                   
hno$mean <-  apply(hno, 1, function(y) {
  round2(mean(tail(sort(y), (floor(ncol(hno)/2)))))
})
#d <- density(hno$mean) 
#plot(d)
#abline(v=c(2,2.5), col=c("black", "black"), lty=c(2,2), lwd=c(1, 1))


#CRITICAL INDICATORS
hno$critical <-  apply(hno, 1, function(y) {
  max(y[c("s_3", "s_11", "s_24")], na.rm = T)
})
hno$final_severity <- ifelse(hno$critical > hno$mean, hno$critical, hno$mean)
#hno$final_severity <- as.character(as.numeric(hno$final_severity))
r$final_severity <- hno$final_severity
r$hno_severity_1 <- ifelse(r$final_severity == 1, 1,0)
r$hno_severity_2 <- ifelse(r$final_severity == 2, 1,0)
r$hno_severity_3 <- ifelse(r$final_severity == 3, 1,0)
r$hno_severity_4 <- ifelse(r$final_severity == 4, 1,0)
r$hno_severity_5 <- ifelse(r$final_severity == 5, 1,0)

#r <- r %>% mutate(final_severity = case_when(
#  r$final_severity == "1" ~ "one",
#  r$final_severity == "2" ~ "two",
#  r$final_severity == "3" ~ "three",
#  r$final_severity == "4" ~ "four",
#  r$final_severity == "5" ~ "five",
#))

return(r)
}
