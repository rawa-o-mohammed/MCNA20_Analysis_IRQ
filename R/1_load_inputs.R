# LOAD QUESTIONNAIRE
questions <- read.csv("input/questionnaire/kobo_questions.csv", 
                      stringsAsFactors=T, check.names=T)
colnames(questions)[1] <- "type"


choices <- read.csv("input/questionnaire/kobo_choices.csv", 
                    stringsAsFactors=F, check.names=T)
colnames(choices)[1] <- "list_name"

# LOAD SAMPLINGFRAMES AND LOOKUP-TABLES
cluster_lookup_table <- read.csv("input/lookup_tables/combined_sample_ids.csv", 
                                 stringsAsFactors=F, check.names=F)
cluster_lookup_table <- dplyr::distinct(cluster_lookup_table)
#write.csv(cluster_lookup_table, "input/combined_sample_ids.csv")
#cluster_lookup_table <- read.csv("input/combined_sample_ids.csv", 
#                                 stringsAsFactors=F, check.names=F)

lookup_table <- read.csv("input/lookup_tables/lookup_table_names.csv", stringsAsFactors = F)


samplingframe <- load_samplingframe("./input/sampling_frame/strata_population.csv")

samplingframe_in_camp<-load_samplingframe("./input/sampling_frame/sampling_frame_in_camp.csv")


# LOAD DATA
arabic_outcamp_household <- readLines("input/datasets/outcamp_household.csv", warn = FALSE, encoding = "UTF-8")
response <- read.csv(text = arabic_outcamp_household, stringsAsFactors = F, check.names = T)
#response <- read.csv("input/datasets/outcamp_household.csv",
#                     stringsAsFactors = F, check.names = T)

arabic_outcamp_loop <- readLines("input/datasets/outcamp_loop.csv", warn = FALSE, encoding = "UTF-8")
loop <- read.csv(text = arabic_outcamp_loop, stringsAsFactors = F, check.names = T)
#loop <- read.csv("input/datasets/outcamp_loop.csv", stringsAsFactors = F)

arabic_incamp_household <- readLines("input/datasets/incamp_household.csv", warn = FALSE, encoding = "UTF-8")
idp_in_camp <- read.csv(text = arabic_incamp_household, stringsAsFactors = F, check.names = T)
#idp_in_camp <- read.csv("input/datasets/incamp_household.csv",
#                        stringsAsFactors = F, check.names = T)
idp_in_camp$district <- to_alphanumeric_lowercase(samplingframe_in_camp$district[match(idp_in_camp$camp_name, samplingframe_in_camp$camp)])

arabic_incamp_loop <- readLines("input/datasets/incamp_loop.csv", warn = FALSE, encoding = "UTF-8")
loop_in_camp <- read.csv(text = arabic_incamp_loop, stringsAsFactors = F, check.names = T)
#loop_in_camp <- read.csv("input/datasets/incamp_loop.csv", stringsAsFactors = F)

idp_in_camp <- subset(idp_in_camp, idp_in_camp$call_status == "answered")


