#ADD POPGROUP FOR INCAMP IDPs
idp_in_camp$population_group <- "idp_in_camp"
loop_in_camp$population_group <- "idp_in_camp"


#ONLY KEEP VARNAMES FROM INCAMP THAT MATCH WITH MCNA COLNAMES
cols_mcna <- as.vector(names(response))
names.use <- names(idp_in_camp)[(names(idp_in_camp) %in% cols_mcna)]
idp_in_camp_mcna <- idp_in_camp[, names.use]

#MERGE THE INCAMP AND OUTCAMP DATASETS
response <- plyr::rbind.fill(response, idp_in_camp_mcna)
loop <- plyr::rbind.fill(loop, loop_in_camp)


#PREPARE SAMPLINGFRAMES

##CLUSTER SAMPLING FRAME: MAKE VALUES THAT CAN BE MATCHED WITH DATA
samplingframe$popgroup[samplingframe$popgroup=="out_of_camp"]<-"idp_out_camp"
samplingframe$district <- to_alphanumeric_lowercase(samplingframe$district)
samplingframe$stratum <- paste0(samplingframe$district, samplingframe$popgroup)


#ADD DISTRICT NAMES BASED ON LOCATION IDS IN SAMPLING FRAME (to be deleted once dataset final)
strata_clusters_population <- read.csv("input/sampling_frame/Strata_clusters_population.csv",
                                       stringsAsFactors = F, check.names = T)
names(strata_clusters_population)[names(strata_clusters_population) == "psu"] <- "cluster_location_id"
district_location_ids <- strata_clusters_population[,c("district", "cluster_location_id")]
response <- merge(response, district_location_ids, by="cluster_location_id", all.x = T)  
response$district_mcna <- ifelse(response$district_mcna == "" &
                                   response$dc_method == "in_person", response$district, 
                                 response$district_mcna)
#samplingframe$cluster_ID <- cluster_lookup_table$new_ID[match(samplingframe$psu, cluster_lookup_table$psu)]
#samplingframe$cluster_strata_ID <- paste(samplingframe$cluster_ID, samplingframe$popgroup, sep = "_")


##ADD CLUSTER IDs TO DATA

##ANY POPGROUP OR CLUSTER IDs THAT CAN'T BE FOUND INDIVIDUALLY? 
`%find those not in%`<-function(x,y){x[!(x%in%y)] %>% unique}

response$population_group %find those not in% samplingframe$popgroup
#response$cluster_location_id[response$population_group != "idp_in_camp"] %find those not in% samplingframe$cluster_ID

##CREATE ID IN SAMPLINGFRAME AND IN DATA 
samplingframe$cluster_strata_ID <- paste(samplingframe$cluster_ID, samplingframe$popgroup, sep = "_")
response$cluster_id <- paste("cluster_location_id", response$cluster_location_id, response$population_group, sep = "_")

# any id that can't be found in combination?
#response$cluster_id %find those not in% samplingframe$cluster_strata_ID %>% paste0(collapse="\n") %>% cat


#CLUSTER NAMES FOR ALL REMOTELY COLLECTED DATA OUT OF CAMP
no_cluster<-paste0("NO_CLUSTER_",1:length(which(response$dc_method=="remote")))
#in_camp_cluster_sampling_frame<-data.frame(cluster_strata_ID= in_camp_ids,pop=1)


#response <- response[!is.na(response$cluster_id[in_camp]), ]
response$cluster_id <- ifelse(response$population_group == "idp_in_camp", 
                              paste0(no_cluster, "_idp_in_camp"), response$cluster_id)
response$cluster_id <- ifelse(response$population_group == "idp_out_camp" & response$dc_method == "remote", 
                              paste0(no_cluster, "_idp_out_camp"), response$cluster_id)
response$cluster_id <- ifelse(response$population_group == "returnee" & response$dc_method == "remote", 
                              paste0(no_cluster, "_returnee"), response$cluster_id)


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

if(any(!(response$strata %in% samplingframe_strata$stratum))){
  warning("some strata not found in samplingframe")
  warning(which(!(response$strata %in% samplingframe_strata$stratum)) %>% length)
}
response$strata %find those not in% samplingframe_strata$stratum

##### (TO BE DELETED ONCE FINAL DATASET READY)
response<-response[!(response$strata=="al.hawigaidp_out_camp" |
                       response$strata=="al.sulaymaniyahreturnee" | 
                       response$strata=="dokanreturnee" | 
                       response$strata == "kalarreturnee" | 
                       response$strata == "idp_in_camp" | 
                       response$strata == "NAidp_in_camp"),]
#####

if(any(is.na(response$strata))){
  warning("strata can not be NA")
}

# MERGE QUESTIONNAIRES
questionnaire <- load_questionnaire(response,questions,choices, choices.label.column.to.use = "name")
