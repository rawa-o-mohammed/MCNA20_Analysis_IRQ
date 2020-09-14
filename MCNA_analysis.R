#SETUP
rm(list=ls(all=T))
  library(rlang)
  library(xlsx)
  library(plyr) # rbind.fill
  library(dplyr)
  library(questionr)
  library(koboquest) # manage kobo questionnairs
  library(kobostandards) # check inputs for inconsistencies
  library(xlsformfill) # generate fake data for kobo
  library(surveyweights) # calculate weights from samplingframes
  library(hypegrammaR) # simple stats 4 complex samples
  library(composr) # horziontal operations
  source("postprocessing_functions.R")
  source("functions/to_alphanumeric_lowercase.R")
  source("functions/analysisplan_factory.R")
  source("functions/HNO_Recoding.R")

  
  
#LOAD INPUT FILES 
  source("load_inputs.R",local = T)
  #' creates objects:
  #' 
  #'    response
  #'    analysisplan
  #'    choices
  #'    questions
  #'    cluster_lookup_table
  #'    idp_in_camp
  #'    loop
  #'    loop_in_camp
  #'    samplingframe
  #'    samplingframe_in_camp
  
#PREPARE BOTH DATASETS
  source("prepare_datasets.R", local = T)
  #'  1. merge values from remote and in-person data collection
  #'  2. add columns to in-camp dataset which are only included in outcamp data collection

  
#MERGE INCAMP AND OUT-OF CAMP DATASETS 
#PREPARE SAMPLING FRAMES AND CLUSTER_IDs
  source("Merger.R", local = T)
  #' matching all inputs:
  #' 1. combine in and out of camp data for each, HH and loops 
  #' 2. prepare sampling frames:
  #'     3.1 prepare columns in out of camp cluster level sampling frame
  #'     3.2 aggregate out-of-camp to stratum level
  #'     3.3.make strata id for in-camp sampling frame
  #'     3.4.combine the stratum sampling frames
  #'     3.5.add strata ids to the dataset
  #'     3.6. throw error if any don't match
  
  
#IDENTIFY ANY FURTHER PROBLEMS WITH THE SAMPLING FRAMES MATCHING
  
  strata_samplingframe_issues <- as.data.frame(response[which(!response$strata %in% samplingframe_strata$stratum), c("X_uuid", "strata")])
  if(nrow(strata_samplingframe_issues)!=0){
    print(strata_samplingframe_issues)
    warning("something's not right with the strata id matching!")
  }
  
  cluster_samplingframe_issues <- as.data.frame(response[which(!response$cluster_id[which(response$population_group != "idp_in_camp")] %in% samplingframe$cluster_strata_ID), c("X_uuid", "strata")])
  if(nrow(cluster_samplingframe_issues)!=0){
    print(cluster_samplingframe_issues)
    warning("something's not right with the cluster id matching!")
  }
  

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
  
  
  
  response <- response[!is.na(response$num_hh_member), ]

  strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe_strata,
                                        sampling.frame.population.column = "population",
                                        sampling.frame.stratum.column = "stratum",
                                        data.stratum.column = "strata",
                                        data = response)
  
# weight_fun <- combine_weighting_functions(strata_weight_fun, clusters_weight_fun)
  weight_fun<-strata_weight_fun
  
  
response$weights<-weight_fun(response)
  
#CHANGE WEIGHTS OF REMOTELY COLLECTED STRATAS TO 1
response$weights <- ifelse(response$dc_method == "remote", 1,
                           response$weights)
# for speedy speed we can not recalculate weights on every run):
 weight_fun<-function(df){
   df$weights
 }
  
#female_headed <- response[which(response$X_uuid %in% loop$X_submission__uuid[which(loop$sex == "female" & loop$relationship == "head")]),]
#male_headed <- response[which(response$X_uuid %in% loop$X_submission__uuid[which(loop$sex == "male" & loop$relationship == "head")]),]

response_with_composites <- recoding_hno(response, loop)

#response_female <- recoding_preliminary(female_headed, loop)
#response_male <- recoding_preliminary(male_headed, loop)


dap_name <- "hno"
analysisplan <- read.csv(sprintf("input/dap_%s.csv",dap_name), stringsAsFactors = F)

#AGGREGATE ACROSS DISTRICTS OR/AND POPULATION GROUPS

#analysisplan <- analysisplan_nationwide(analysisplan)
#analysisplan <- analysisplan_pop_group_aggregated(analysisplan)
#analysisplan$hypothesis.type <- "group_difference"

response_with_composites<-subset(response_with_composites, response_with_composites$district_mcna!="al.hatra" | 
                is.na(response_with_composites$district_mcna))

result <- from_analysisplan_map_to_output(response_with_composites, analysisplan = analysisplan,
                                          weighting = weight_fun,
                                          cluster_variable_name = "cluster_id",
                                          questionnaire = questionnaire, confidence_level = 0.9)

name <- "preliminary_national_aggregates_popgroupagg"
saveRDS(result,paste(sprintf("output/result_%s.RDS", name)))
#summary[which(summary$dependent.var == "g51a"),]

lookup_in_camp<-load_samplingframe("./input/sampling_frame_in_camp.csv")
names(lookup_in_camp)[which(names(lookup_in_camp) == "camp")] <- "name"
names(lookup_in_camp)[which(names(lookup_in_camp) == "camp.long.name")] <- "english"
names(lookup_in_camp)[which(names(lookup_in_camp) == "governorate")] <- "filter"

summary <- bind_rows(lapply(result[[1]], function(x){x$summary.statistic}))
write.csv(summary, sprintf("output/raw_results_%s.csv", name), row.names=F)
summary <- read.csv(sprintf("output/raw_results_%s.csv", name), stringsAsFactors = F)
summary <- correct.zeroes(summary)
summary <- summary %>% filter(dependent.var.value %in% c(NA,1))
write.csv(summary, sprintf("output/raw_results_%s_filtered.csv", name), row.names=F)
if(all(is.na(summary$independent.var.value))){summary$independent.var.value <- "all"}
groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]
for (i in 1:length(groups)) {
  df <- pretty.output(summary, groups[i], analysisplan, cluster_lookup_table, lookup_table, severity = name == "severity", camp = F)
  write.csv(df, sprintf("output/summary_sorted_%s_%s.csv", name, groups[i]), row.names = F)
  if(i == 1){
    write.xlsx(df, file=sprintf("output/summary_sorted_%s.xlsx", name), sheetName=groups[i], row.names=FALSE)
  } else {
    write.xlsx(df, file=sprintf("output/summary_sorted_%s.xlsx", name), sheetName=groups[i], append=TRUE, row.names=FALSE)
  }
}

# formap <- df[-c(1:4),]
# formap$msni <- as.numeric(formap$msni)


