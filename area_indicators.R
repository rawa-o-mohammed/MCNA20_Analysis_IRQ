ila <- read.csv("./input/datasets/ILA5_dataset_REACH_OCHA_names.csv")
ila$district_ocha <- to_alphanumeric_lowercase(ila$district_ocha)

#S_27 - CRITICAL SHELTER INDICATOR
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


#s_28 - MINE ACTION INDICATOR
ila <- ila %>%
  mutate(Q5.1.2.SecIncident_ImprovisedExplosiveDevices = 
           case_when(Q5.1.2.SecIncident_ImprovisedExplosiveDevices == "Yes" ~ 1, TRUE ~ 0),
         Q5.1.3.SecIncident_Landmines = 
           case_when(Q5.1.3.SecIncident_Landmines == "Yes" ~ 1, TRUE ~ 0),
         Q5.5.1.Concernce_UXO = case_when(Q5.5.1.Concernce_UXO == "Not concerned" ~ 0, TRUE ~ 1))

explosive_analysis <- ila %>%
  group_by(district_ocha) %>%
  summarize(reported_incidents = sum(Q5.1.2.SecIncident_ImprovisedExplosiveDevices) 
            + sum(Q5.1.3.SecIncident_Landmines),
            concern_uxo = sum(Q5.5.1.Concernce_UXO),
            total_group_entries = n())

explosive_analysis <- explosive_analysis %>%
  mutate(rate_reported_incidents =  reported_incidents/total_group_entries,
         rate_concern_uxo = concern_uxo/total_group_entries)


explosive_analysis <- explosive_analysis %>%
  mutate(s_28 = 
           case_when(rate_reported_incidents == 0 & rate_concern_uxo == 0 ~ 1,
                     rate_reported_incidents == 0 & rate_concern_uxo > 0 ~ 2,
                     rate_reported_incidents > 0 & rate_reported_incidents <= 0.1 ~ 3,
                     rate_reported_incidents > 0.1 & rate_concern_uxo < 0.2 ~ 4, 
                     rate_reported_incidents > 0.1 & rate_concern_uxo >= 0.2 ~ 5))


write.csv(explosive_analysis, "output/ila_analysis_explosive_s28.csv")
