#SETUP
rm(list = ls(all = T))
library(rlang)
library(xlsx)
library(plyr) # rbind.fill
library(dplyr)
library(expss)
library(reshape)
library(data.table)
library(questionr)
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(surveyweights) # calculate weights from samplingframes
library(hypegrammaR) # simple stats 4 complex samples
library(composr) # horziontal operations
library(msni19)
library(Setviz)

source("R/functions/postprocessing_functions.R")
source("R/functions/to_alphanumeric_lowercase.R")
source("R/functions/analysisplan_factory.R")
source("R/functions/HNO_Recoding.R")
source("R/functions/Binary_Recoding.R")
source("R/functions/gimac_recoding.R")
source("R/functions/msni_recoding.R")


#LOAD INPUT FILES
source("R/1_load_inputs.R", local = T)
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




#STRATA WEIGHTING
strata_weight_fun <-
  map_to_weighting(
    sampling.frame = samplingframe_strata,
    sampling.frame.population.column = "population",
    sampling.frame.stratum.column = "stratum",
    data.stratum.column = "strata",
    data = response
  )

# weight_fun <- combine_weighting_functions(strata_weight_fun, clusters_weight_fun)
weight_fun <- strata_weight_fun

response$weights <- weight_fun(response)

#CREATE NEW FUNCTION FOR WEIGHTING
weight_fun <- function(df) {
  df$weights
}

#RECODING OF INDICATORS
response_with_composites <- msni_recoding(response, loop)

#LOAD ANALYSISPLAN
dap_name <- "msni"
analysisplan <-
  read.csv(sprintf("input/dap/dap_%s.csv", dap_name), stringsAsFactors = F)
response_with_composites$all <- "all"

result <-
  from_analysisplan_map_to_output(
    response_with_composites,
    analysisplan = analysisplan,
    weighting = weight_fun,
    questionnaire = questionnaire,
    confidence_level = 0.9
  )

#graph outputs
msni19::index_intersections(
  response_with_composites,
  lsg =  c(
    "lsg_education",
    "lsg_livelihoods",
    "lsg_food",
    "lsg_protection",
    "lsg_health",
    "lsg_snfi",
    "lsg_wash"
  # )
  # lsg =  c(
  #   "education_score",
  #   "livelihoods_score",
  #   "food_security_score",
  #   "protection_score",
  #   "health_score",
  #   "snfi_score",
  #   "wash_score"
  ),
  lsg_labels = c(
    "Education",
    "Livelihoods",
    "Food",
    "Protection",
    "Health",
    "Shelter",
    "WASH"
  ),
  weighting_function = strata_weight_fun,
  weight_variable = weights,
  y_label = "",
  exclude_unique = F,
  mutually_exclusive_sets = F,
  round_to_1_percent = T,
  print_plot = T,
  path = "output/graphs"
)

venn <- msni19::venn_msni(
  response_with_composites,
  lsg = c(
    "lsg_education",
    "lsg_livelihoods",
    "lsg_food",
    "lsg_protection",
    "lsg_health",
    "lsg_snfi",
    "lsg_wash"
  ),
  capacity_gaps = "coping_mechanism",
  weighting_function = weight_fun,
  print_plot = T,
  path = "output/graphs"
)

msni19::radar_graph(response_with_composites, 
                    lsg =  c(
                      "education_score",
                      "livelihoods_score",
                      "food_security_score",
                      "protection_score",
                      "health_score",
                      "snfi_score",
                      "wash_score"
                    ),
                    lsg_labels = c(
                      "Education",
                      "Livelihoods",
                      "Food Security",
                      "Protection",
                      "Health",
                      "Shelter",
                      "WASH"
                    ),
                    group = "population_group",
                    group_order = c("idp_in_camp", "idp_out_camp", "returnee"),
                    group_labels = c("In-camp IDPs", "Out of camp IDPs", "Returnees"),
                    weighting_function = weight_fun,
                    print_plot = T,
                    plot_name = "radar",
                    path = "output/graphs")


saveRDS(result, paste("output/RDS/result_msni.RDS"))
#summary[which(summary$dependent.var == "g51a"),]

lookup_in_camp <-
  load_samplingframe("./input/sampling_frame/sampling_frame_in_camp.csv")
names(lookup_in_camp)[which(names(lookup_in_camp) == "camp")] <-
  "name"
names(lookup_in_camp)[which(names(lookup_in_camp) == "camp.long.name")] <-
  "english"
names(lookup_in_camp)[which(names(lookup_in_camp) == "governorate")] <-
  "filter"

summary <-
  bind_rows(lapply(result[[1]], function(x) {
    x$summary.statistic
  }))
write.csv(summary,
          "output/raw_results/raw_results_msni.csv",
          row.names = F)
summary <-
  read.csv("output/raw_results/raw_results_msni.csv",
           stringsAsFactors = F)
summary <- correct.zeroes(summary)
summary <- summary %>% filter(dependent.var.value %in% c(NA, 1))
write.csv(summary,
          "output/raw_results/raw_results_filtered_msni.csv",
          row.names = F)
aggregation <- c("district_mcna", "all")
disaggregation <- c("population_group", "all")

for (agg in aggregation) {
  for (disagg in disaggregation) {
    name <- sprintf("msni_%s_%s", agg, disagg)
    if (all(is.na(summary$independent.var.value))) {
      summary$independent.var.value <- disagg
    }
    subset <- summary %>%
      filter(repeat.var == agg, independent.var == disagg)
    if (nrow(subset) == 0) {
      next
    }
    groups <- unique(subset$independent.var.value)
    groups <- groups[!is.na(groups)]
    library(plyr)
    for (i in 1:length(groups)) {
      df <-
        pretty.output(
          subset,
          groups[i],
          analysisplan,
          cluster_lookup_table,
          lookup_table,
          severity = name == "severity",
          camp = F
        )
      df[c(which(endsWith(names(df), "_min")))] <- NULL
      df[c(which(endsWith(names(df), "_max")))] <- NULL
      write.csv(
        df,
        sprintf(
          "output/summary_sorted/summary_sorted_%s_%s.csv",
          name,
          groups[i]
        ),
        row.names = F
      )
      if (i == 1) {
        write.xlsx(
          df,
          file = sprintf(
            "output/summary_sorted/summary_sorted_%s.xlsx",
            name
          ),
          sheetName = groups[i],
          row.names = FALSE
        )
      } else {
        write.xlsx(
          df,
          file = sprintf(
            "output/summary_sorted/summary_sorted_%s.xlsx",
            name
          ),
          sheetName = groups[i],
          append = TRUE,
          row.names = FALSE
        )
      }
    }
  }
}
