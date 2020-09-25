#SETUP
rm(list=ls(all=T))
  library(rlang)
  library(xlsx)
  library(plyr) # rbind.fill
  library(dplyr)
  library(reshape)
  library(data.table)
  library(questionr)
  library(koboquest) # manage kobo questionnairs
  library(kobostandards) # check inputs for inconsistencies
  library(xlsformfill) # generate fake data for kobo
  library(surveyweights) # calculate weights from samplingframes
  library(hypegrammaR) # simple stats 4 complex samples
  library(composr) # horziontal operations

  source("R/functions/postprocessing_functions.R")
  source("R/functions/to_alphanumeric_lowercase.R")
  source("R/functions/analysisplan_factory.R")
  source("R/functions/HNO_Recoding.R")
  #source("R/functions/Binary_Recoding.R")

#LOAD INPUT FILES 
  source("R/1_load_inputs.R",local = T)
  names(response)[names(response) == 'ï..X_uuid'] <- "X_uuid"
  #' creates objects:
  #' 
  #'    response representative clean
  #'    response indicative clean
  #'    analysisplan
  #'    choices
  #'    questions
  #'    cluster_lookup_table
  #'    loop
  #'    samplingframe
  #'    samplingframe_in_camp

  
#PREPARE SAMPLING FRAMES AND STRATAS
  source("R/2_prepare_samplingframe.R", local = T)
  #' Prepare sampling frames and Strata names:
  #'     3.1 prepare columns in out of camp cluster level sampling frame
  #'     3.2 aggregate out-of-camp to stratum level
  #'     3.3.make strata id for in-camp sampling frame
  #'     3.4.combine the stratum sampling frames
  #'     3.5.add strata ids to the dataset
  #'     3.6. throw error if any don't match


#CALCULATE DISTRICT LEVEL SEVERITY FOR AREA-LEVEL INDICATORS (s_27, s_28, s_29)
  source("R/3_area_indicators.R", local = T)
  
  #' Indicators
  #' s_27 - % of people living under critical shelter condition
  #' s_28 - % of locations reporting incidents with explosive devices
  #' s_29 - % of children regularly attending formal or informal eduation

  
#IDENTIFY ANY FURTHER PROBLEMS WITH THE SAMPLING FRAMES MATCHING
  strata_samplingframe_issues <- as.data.frame(response[which(!response$strata %in% samplingframe_strata$stratum), c("X_uuid", "strata")])
  if(nrow(strata_samplingframe_issues)!=0){
    print(strata_samplingframe_issues)
    warning("something's not right with the strata id matching!")
  }
  

####################################
#CLUSTER WEIGHTING (NOT TO BE USED)
  
  #cluster_samplingframe_issues <- as.data.frame(response[which(!response$cluster_id[which(response$population_group != "idp_in_camp")] %in% samplingframe$cluster_strata_ID), c("X_uuid", "strata")])
  #if(nrow(cluster_samplingframe_issues)!=0){
  #  print(cluster_samplingframe_issues)
  #  warning("something's not right with the cluster id matching!")
  #}
  
  # remove records not in cluster samplingframe:
  # nrow_before<- nrow(response)
  # response<-response %>% filter((cluster_id %in% samplingframe$cluster_strata_ID) | population_group=="idp_in_camp")
  
  # if any disappeared, give a warning:
  # if(nrow(response)!=nrow_before){
  #   warning(paste("lost ",nrow_before-nrow(response), " records; their cluster id is not in the cluster sampling frame"))
  # }
  
  # clusters_weight_fun <- map_to_weighting(sampling.frame= samplingframe,
  #                                         sampling.frame.population.column = "pop",
  #                                         sampling.frame.stratum.column = "cluster_strata_ID",
  #                                         data.stratum.column = "cluster_id",
  #                                         data = response[response$population_group!="idp_in_camp",])
#######################################
  

#STRATA WEIGHTING
strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe_strata,
                                        sampling.frame.population.column = "population",
                                        sampling.frame.stratum.column = "stratum",
                                        data.stratum.column = "strata",
                                        data = response)
  
# weight_fun <- combine_weighting_functions(strata_weight_fun, clusters_weight_fun)
weight_fun <-strata_weight_fun
  
response$weights<- weight_fun(response)
  
#CHANGE WEIGHTS OF REMOTELY COLLECTED STRATAS TO 1
response$weights <- ifelse(response$dc_method == "remote", 1,
                           response$weights)

#CREATE NEW FUNCTION FOR WEIGHTING
 weight_fun<-function(df){
   df$weights
 }
  

#DISAGGREGATE MALE AND FEMALE HEADED HHs
#female_headed <- response[which(response$X_uuid %in% loop$X_submission__uuid[which(loop$sex == "female" & loop$relationship == "head")]),]
#male_headed <- response[which(response$X_uuid %in% loop$X_submission__uuid[which(loop$sex == "male" & loop$relationship == "head")]),]

#RECODING OF INDICATORS
response_with_composites <- recoding_hno(response, loop)


#LOAD ANALYSISPLAN
dap_name <- "hno"
analysisplan <- read.csv(sprintf("input/dap/dap_%s.csv",dap_name), stringsAsFactors = F)


#AGGREGATE ACROSS DISTRICTS OR/AND POPULATION GROUPS
#analysisplan <- analysisplan_nationwide(analysisplan)
#analysisplan <- analysisplan_pop_group_aggregated(analysisplan)
#analysisplan$hypothesis.type <- "group_difference"


result <- from_analysisplan_map_to_output(response_with_composites, analysisplan = analysisplan,
                                          weighting = weight_fun,
                                          questionnaire = questionnaire, confidence_level = 0.9)

name <- "hno severity_popgroup_district"
saveRDS(result,paste(sprintf("output/RDS/result_%s.RDS", name)))
#summary[which(summary$dependent.var == "g51a"),]

lookup_in_camp<-load_samplingframe("./input/sampling_frame/sampling_frame_in_camp.csv")
names(lookup_in_camp)[which(names(lookup_in_camp) == "camp")] <- "name"
names(lookup_in_camp)[which(names(lookup_in_camp) == "camp.long.name")] <- "english"
names(lookup_in_camp)[which(names(lookup_in_camp) == "governorate")] <- "filter"

summary <- bind_rows(lapply(result[[1]], function(x){x$summary.statistic}))
write.csv(summary, sprintf("output/raw_results/raw_results_%s.csv", name), row.names=F)
summary <- read.csv(sprintf("output/raw_results/raw_results_%s.csv", name), stringsAsFactors = F)
summary <- correct.zeroes(summary)
summary <- summary %>% filter(dependent.var.value %in% c(NA,1))
write.csv(summary, sprintf("output/raw_results/raw_results_%s_filtered.csv", name), row.names=F)
if(all(is.na(summary$independent.var.value))){summary$independent.var.value <- "all"}
groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]
for (i in 1:length(groups)) {
  df <- pretty.output(summary, groups[i], analysisplan, cluster_lookup_table, lookup_table, severity = name == "severity", camp = F)
  write.csv(df, sprintf("output/summary_sorted/summary_sorted_%s_%s.csv", name, groups[i]), row.names = F)
  if(i == 1){
    write.xlsx(df, file=sprintf("output/summary_sorted/summary_sorted_%s.xlsx", name), sheetName=groups[i], row.names=FALSE)
  } else {
    write.xlsx(df, file=sprintf("output/summary_sorted/summary_sorted_%s.xlsx", name), sheetName=groups[i], append=TRUE, row.names=FALSE)
  }
}

