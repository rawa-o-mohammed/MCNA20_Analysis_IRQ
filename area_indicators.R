ila <- read.csv("./input/datasets/ILA5_dataset_REACH_OCHA_names.csv")
ila$district_ocha <- to_alphanumeric_lowercase(ila$district_ocha)
ila <- ila %>%
  mutate(district_ocha = tolower(district_ocha))

ila <- ila[!is.na(ila$district_ocha), ]

ila <- ila %>% mutate(idp_critical_shelter = 
                      (Q3.6.1..IDPFamiliesByRentalDestroyed + 
                         Q3.6.1..IDPFamiliesByUnfinishedBuilding + 
                         Q3.6.1..IDPFamiliesByInformalSettlement + 
                         Q3.6.1..IDPFamiliesBySchoolBuilding + 
                         Q3.6.1..IDPFamiliesByReligiousBuilding +
                         Q3.6.1..IDPFamiliesByNonResidentialStructure +
                         Q3.6.1..IDPFamiliesByOtherCollectiveSettlement + 
                         Q3.6.1..IDPFamiliesByOtherSettlement))

ila <- ila %>% mutate(returnee_critical_shelter = 
                      (Q3.7.1.RetFamiliesHabitualBadResidence +
                         Q3.7.1.RetFamiliesByUnfinishedBuilding +
                         Q3.7.1.RetFamiliesByInformalSettlement +
                         Q3.7.1.RetFamiliesBySchoolBuilding +
                         Q3.7.1.RetFamiliesByReligiousBuilding +
                         Q3.7.1.RetFamiliesNonResidentialStructure +
                         Q3.7.1.RetFamiliesByOtherCollectiveSettlement +
                         Q3.7.1.RetFamiliesByOtherSettlement +
                         Q3.7.1.RetFamiliesByUnknown))


ila_analysis <- ila %>%
  group_by(district_ocha) %>%
  summarize(sum_idp_critical = sum(idp_critical_shelter, na.rm = T),
            sum_idp = sum(Q3.1.IDPFamiliesNum, na.rm = T),
            sum_ret_critical = sum(returnee_critical_shelter, na.rm = T),
            sum_ret = sum(Q3.2.ReturneeFamiliesNum, na.rm = T)) %>%
  mutate(critical_idp = sum_idp_critical / sum_idp, critical_returnee = sum_ret_critical / sum_ret)

ila_analysis <- ila_analysis %>%
  mutate(critical_ranking_idp = case_when(critical_idp < 0.05 | is.na(critical_idp) ~ 1,
                                          critical_idp >= 0.05 & critical_idp < 0.1 ~ 2,
                                          critical_idp >= 0.1 & critical_idp < 0.3 ~ 3,
                                          critical_idp >= 0.3 & critical_idp < 0.5 ~ 4,
                                          TRUE ~ 5),
         critical_ranking_ret = case_when(critical_returnee < 0.05 | is.na(critical_returnee) ~ 1,
                                          critical_returnee >= 0.05 & critical_returnee < 0.1 ~ 2,
                                          critical_returnee >= 0.1 & critical_returnee < 0.3 ~ 3,
                                          critical_returnee >= 0.3 & critical_returnee < 0.5 ~ 4,
                                          TRUE ~ 5))
ila_analysis <- ila_analysis[,c("district_ocha", "critical_ranking_idp", "critical_ranking_ret")]
colnames(ila_analysis) <- c("district_mcna", "idp_out_camp", "returnee")

#RESHAPE FROM WIDE TO LONG
ila_analysis <- melt(setDT(ila_analysis), id.vars = c("district_mcna"), 
                    variable.name = "population_group")
names(ila_analysis)[names(ila_analysis) == "value"] <- "s_27"

#CONSTRUCT STRATAS
ila_analysis <- ila_analysis %>% 
  mutate(strata = paste0(district_mcna,population_group))
ila_analysis <- ila_analysis[,c("strata", "s_27")]

#MERGE WITH RESPONSE_WITH_COMPOSITES
#response_with_composites <- merge(response_with_composites, ila_analysis, by="strata", all.x = T)

