#PREPARE INCAMP AND OUTOFCAMP DATASETS TO BE MERGED
idp_in_camp$district <- to_alphanumeric_lowercase(samplingframe_in_camp$district[match(idp_in_camp$camp_name, samplingframe_in_camp$camp)])


#MERGE COLUMNS MCNA REMOTE AND INPERSON
response$age_respondent <- ifelse(is.na(response$age_respondent),
                                  response$age_respondent_r, response$age_respondent)
response$age_respondent_r <- NULL

response$gender_respondent <- ifelse(is.na(response$gender_respondent), 
                                  response$gender_respondent_r, response$gender_respondent)
response$gender_respondent_r <- NULL
response$governorate_mcna <- ifelse(is.na(response$governorate_mcna), 
                                     response$governorate_mcna_r, response$governorate_mcna)
response$governorate_mcna_r <- NULL

response$shelter_type <- ifelse(response$shelter_type_inperson == "", 
                                    response$shelter_type_remote, response$shelter_type_inperson)
response$shelter_type_inperson <- NULL
response$shelter_type_remote <- NULL

idp_in_camp$dc_method <- "remote"


#RECODE SHARED SANITATION FACILITY VARIABLE INCAMP DATA COLLECTION
idp_in_camp <- idp_in_camp %>% mutate(shared_sanitation = case_when(
  (idp_in_camp$latrines.private_camp == 1 | 
    idp_in_camp$latrines.private_self == 1) ~ "no",
  idp_in_camp$latrines_phh > 0 ~ "yes",
  idp_in_camp$latrines.none == 1 ~ "no"
))

#RECODE DISPLACED_AGAIN ANSWER CHOICES INCAMP
idp_in_camp <- idp_in_camp %>% mutate(attempt_to_return = case_when(
  (idp_in_camp$attempt_to_return == "yes_return" |
     idp_in_camp$attempt_to_return == "yes_visited") ~ "yes",
  idp_in_camp$attempt_to_return == "no" ~ "no"
))

#RECODE HLP DOCUMENTATION ANSWER CHOICES INCAMP
idp_in_camp <- idp_in_camp %>% mutate(hlp_docs = case_when(
     (idp_in_camp$hlp_docs == "lost" | 
     idp_in_camp$hlp_docs == "never" |
     idp_in_camp$hlp_docs == "no_hlp") ~ "no",
  (idp_in_camp$hlp_docs == "secure_place" |
    idp_in_camp$hlp_docs == "with_member") ~ "yes",
  idp_in_camp$hlp_docs == "dnk" ~ "do_not_know"
))

#RENAME VARIABLES FOR MERGE
names(idp_in_camp)[names(idp_in_camp) == "idp_first_place_displacement"] <- "idp_first_place"
names(response)[names(response) == "movement_intentions"] <- "movement_intentions_3"
names(response)[names(response) == "movement_intentions_b"] <- "movement_intentions_b3"
names(response)[names(response) == "ox8ul79"] <- "obstacles_return"
names(response)[names(response) == "intentions"] <- "intentions_12"
names(idp_in_camp)[names(idp_in_camp) == "governorate_cccm"] <- "governorate_mcna"
names(idp_in_camp)[names(idp_in_camp) == "attempt_to_return"] <- "displaced_again"
names(idp_in_camp)[names(idp_in_camp) == "hlp_docs"] <- "hlp_document"
names(response)[names(response) == "movement_intentions12"] <- "movement_intentions_12"


#CREATE COLUMNS IN RESPONSE DATASET FOR VARIABLES ONLY INCLUDED IN INCAMP DATASET
#AND VICE VERSA
idp_in_camp$sufficient_water <- NA
idp_in_camp$sufficient_water_drinking <- NA
idp_in_camp$sufficient_water_cooking <- NA
idp_in_camp$sufficient_water_hygiene <- NA
idp_in_camp$sufficient_water_other <- NA

idp_in_camp$hh_risk_eviction <- NA
idp_in_camp$'hh_main_risks.lack_funds'<- NA
idp_in_camp$'hh_main_risks.no_longer_hosted' <- NA
idp_in_camp$'hh_main_risks.unaccepted_by_community'<- NA
idp_in_camp$'hh_main_risks.authorities_request' <- NA
idp_in_camp$'hh_main_risks.owner_request' <- NA
idp_in_camp$'hh_main_risks.no_agreement' <- NA
idp_in_camp$'hh_main_risks.inadequate' <- NA
idp_in_camp$'hh_main_risks.occupied' <- NA
idp_in_camp$'hh_main_risks.confiscation' <- NA
idp_in_camp$'hh_main_risks.dispute' <- NA
idp_in_camp$'hh_main_risks.other' <- NA


idp_in_camp$aware_compensation <- NA
idp_in_camp$applied_compensation <- NA
idp_in_camp$received_compensation <- NA
idp_in_camp$local_integration <- NA

response$camp_name <- NA
response$access_private_shared_watertank <- NA
response$tank_capacity <- NA
response$refill_times <- NA
response$people_share_tank <- NA

#ADD POPULATION GROUP TO MCNA DATASET
response <- response %>% mutate(population_group = case_when(
  response$displace_status_returnee == "yes" ~ "returnee",
     response$displace_status_idp == "yes" ~ "idp_out_camp"
))

