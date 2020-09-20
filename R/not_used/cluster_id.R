##ADD CLUSTER IDs TO DATA

#ADD DISTRICT NAMES BASED ON LOCATION IDS IN SAMPLING FRAME
strata_clusters_population <- read.csv("input_modified/Strata_clusters_population.csv",
                                       stringsAsFactors = F, check.names = T)
names(strata_clusters_population)[names(strata_clusters_population) == "psu"] <- "cluster_location_id"
district_location_ids <- strata_clusters_population[,c("district", "cluster_location_id")]
response <- merge(response, district_location_ids, by="cluster_location_id", all.x = T)  
response$district_mcna <- ifelse(response$district_mcna == "" &
                                   response$dc_method == "in_person", response$district, 
                                 response$district_mcna)
#samplingframe$cluster_ID <- cluster_lookup_table$new_ID[match(samplingframe$psu, cluster_lookup_table$psu)]
#samplingframe$cluster_strata_ID <- paste(samplingframe$cluster_ID, samplingframe$popgroup, sep = "_")


##CREATE ID IN SAMPLINGFRAME AND IN DATA 
samplingframe$cluster_strata_ID <- paste(samplingframe$cluster_ID, samplingframe$popgroup, sep = "_")
#response$cluster_location_id <- ifelse(response$dc_method == "remote" & 
#                                is.na(response$cluster_location_id), "r", 
#                              response$cluster_location_id)
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
