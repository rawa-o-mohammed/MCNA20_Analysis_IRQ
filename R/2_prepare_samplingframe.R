#PREPARE SAMPLINGFRAMES

##CLUSTER SAMPLING FRAME: MAKE VALUES THAT CAN BE MATCHED WITH DATA
samplingframe$popgroup[samplingframe$popgroup=="out_of_camp"]<-"idp_out_camp"
samplingframe$district <- to_alphanumeric_lowercase(samplingframe$district)
samplingframe$stratum <- paste0(samplingframe$district, samplingframe$popgroup)


##MAKE INCAMP VALUES MATCH DATA
samplingframe_in_camp$stratum<-paste0(samplingframe_in_camp$camp,"idp_in_camp")

##MAKE INCAMP COLUMNS MATCH OTHER SAMPLINGFRAM
samplingframe_in_camp <- samplingframe_in_camp %>% dplyr::select(stratum,"population" = population)
##COMBINE INCAMP AND OUTOFCAMP SAMPLING FRAME
samplingframe_strata <- rbind(samplingframe[,c("stratum", "population")],samplingframe_in_camp)

#ADD STRATA NAMES TO DATA 

##OUT OF CAMP:
response <- response %>% 
  mutate(strata = paste0(district_mcna,population_group))

##INCAMP (REPLACE DISTRICT WITH CAMP NAME FOR ALL INCAMP ENTRIES)
response <- response[!is.na(response$population_group), ]
response$strata[response$population_group=="idp_in_camp"]<- (paste0(response$camp_name,response$population_group))[response$population_group=="idp_in_camp"]


##CHECK IF ALL MATCH SAMPLINGFRAME:
`%find those not in%`<-function(x,y){x[!(x%in%y)] %>% unique}

if(any(!(response$strata %in% samplingframe_strata$stratum))){
  warning("some strata not found in samplingframe")
  warning(which(!(response$strata %in% samplingframe_strata$stratum)) %>% length)
}
response$strata %find those not in% samplingframe_strata$stratum


if(any(is.na(response$strata))){
  warning("strata can not be NA")
}

# MERGE QUESTIONNAIRES
questionnaire <- load_questionnaire(response,questions,choices, choices.label.column.to.use = "name")
