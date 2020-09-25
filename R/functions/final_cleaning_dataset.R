#DELETE NOTE COLUMNS
response[ ,c("note_missing_documents_above_18", "note_missing_documents_under_18", 
       "referral_contract", "note_returnee", "note_host", "note_idp", "note_ineligible", "note_idp_ineligible", 
       "note_returnee_ineligible", "calc_note", "tot_calc_note", "not_residing_reason_note", "food_security_note", 
       "disabilities_note", "covid_job_note", "exp_note", "note_inelegible", "note_returnee_inelegible")] <- list(NULL)


#DELETE REF COLUMNS
response[ ,c("ref_evic", "ref_docs", "ref_child")] <- list(NULL)


#DELETE CALC COLUMNS
response[ ,c("calc_idp", "calc_returnee", "calc_host", "calc_noteligible", "ind_level", "calc_separated", "exp_compare", 
             "calc_expenditure")] <- list(NULL)


#DELETE IDENTIFIER COLUMNS
response[ ,c("cluster_location_id")] <- list(NULL)


#CHANGE SKIPPED QUESTIONS FOR DISCIPLINARY MEASURES TO DECLINED TO ANSWER
response$disciplinary_measures <- ifelse(response$disciplinary_measures == "" | 
                                           response$disciplinary_measures == "no_answer", 
                                         "decline_to_answer", response$disciplinary_measures)

response$disciplinary_measures.explained <- ifelse(response$disciplinary_measures == "decline_to_answer", 0, 
                                                   response$disciplinary_measures.explained)

response$disciplinary_measures.forbid_privileges <- ifelse(response$disciplinary_measures == "decline_to_answer", 0, 
                                                   response$disciplinary_measures.forbid_privileges)

response$disciplinary_measures.shouted <- ifelse(response$disciplinary_measures == "decline_to_answer", 0, 
                                                           response$disciplinary_measures.shouted)

response$disciplinary_measures.spanked <- ifelse(response$disciplinary_measures == "decline_to_answer", 0, 
                                                 response$disciplinary_measures.spanked)

response$disciplinary_measures.no_answer <- ifelse(response$disciplinary_measures == "decline_to_answer", 1, 
                                                 response$disciplinary_measures.no_answer)

names(response)[names(response) == "disciplinary_measures.no_answer"] <- "disciplinary_measures.decline_to_answer"


#CHANGE NAMES FOR NOT_RESIDING_REASON VARIABLE
names(response)[names(response) == "married"] <- "not_residing_reason.married"
names(response)[names(response) == "seek_employment"] <- "not_residing_reason.seek_employment"
names(response)[names(response) == "study"] <- "not_residing_reason.study"
names(response)[names(response) == "family"] <- "not_residing_reason.family"
names(response)[names(response) == "armed_actors"] <- "not_residing_reason.armed_actors"
names(response)[names(response) == "kidnapped"] <- "not_residing_reason.kidnapped"
names(response)[names(response) == "missing"] <- "not_residing_reason.missing"
names(response)[names(response) == "detained"] <- "not_residing_reason.detained"

#VALUE FOR NOT_RESIDING_NUM = 15 BUT ONLY ONE INDICATED FOR REASONS
response$not_residing_num <- ifelse(response$not_residing_num == 15, 1, 
                                    response$not_residing_num)

