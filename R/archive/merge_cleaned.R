
indicative_hh <- read.csv("Input/datasets/cleaned/indicative_hh.csv")
representative_hh <- read.csv("Input/datasets/cleaned/representative_hh.csv")
loop <- read.csv("Input/datasets/cleaned/loop.csv")

response <- plyr::rbind.fill(representative_hh, indicative_hh)
names(response)[names(response) == "ï..X_uuid"] <- "X_uuid"
names(loop)[names(loop) == "ï..X_uuid"] <- "X_uuid"


if(any(!(loop$X_uuid %in% response$X_uuid))){
  warning("some X_uuid not found in response")
  warning(which(!(loop$X_uuid %in% response$X_uuid)) %>% length)
}

X_uuid_not <- as.vector(loop$X_uuid[!(loop$X_uuid %in% response$X_uuid)])
loop <- loop[ ! loop$X_uuid %in% X_uuid_not, ]
write.csv(loop, "Input/datasets/cleaned/loop_v2.csv")


export_indicative <- subset(response, response$dc_method == "remote" | 
                              (response$district_mcna == "al.amadiya" & 
                                 response$population_group == "idp_out_camp"))

export_representative <- subset(response, response$dc_method == "in_person" & 
                                  response$district_mcna != "al.amadiya")
library(xlsx)
write.xlsx(export_representative, file="Output/dataset/REACH_IRQ_Dataset MCNA VIII_September20.xlsx", 
           sheetName="HH data - representative", row.names=FALSE)
write.xlsx(export_indicative, file="Output/dataset/REACH_IRQ_Dataset MCNA VIII_September20.xlsx", 
           sheetName="HH data - indicative", append=TRUE, row.names=FALSE)
write.xlsx(loop, file="Output/dataset/REACH_IRQ_Dataset MCNA VIII_September20.xlsx", 
           sheetName="Ind data", append=TRUE, row.names=FALSE)



individual_to_HH_numeric_sadd <- function(loop, response, varname) {
  r <- loop[,c("X_uuid", varname)]
  r <-r[complete.cases(r), ]
  r = aggregate(r[,c(2)],
                by = list(r$X_uuid),
                FUN = sum, na.rm = T)
  names(r) <- c("X_uuid", "age_var")
  response <- merge(response, r, by="X_uuid", all = T)
  response[,c(varname)] <- response$'age_var'
  response$'age_var' <- NULL
  loop$'varname' <- NULL
  return(response)
}

#MALE
loop$male_2_calc <- ifelse(loop$sex == "male" & loop$age <= 2, 1 ,0)
loop$male_3_5_calc <- ifelse(loop$sex == "male" & loop$age <= 5 & loop$age >= 3, 1 ,0)
loop$male_6_17_calc <- ifelse(loop$sex == "male" & loop$age <= 17 & loop$age >= 6, 1 ,0)
loop$male_18_59_calc <- ifelse(loop$sex == "male" & loop$age <= 59 & loop$age >= 18, 1 ,0)
loop$male_60_calc <- ifelse(loop$sex == "male" & loop$age >= 60, 1 ,0)
loop$tot_male <- ifelse(loop$sex == "male", 1 ,0)

#FEMALE
loop$female_2_calc <- ifelse(loop$sex == "female" & loop$age <= 2, 1 ,0)
loop$female_3_5_calc <- ifelse(loop$sex == "female" & loop$age <= 5 & loop$age >= 3, 1 ,0)
loop$female_6_17_calc <- ifelse(loop$sex == "female" & loop$age <= 17 & loop$age >= 6, 1 ,0)
loop$female_18_59_calc <- ifelse(loop$sex == "female" & loop$age <= 59 & loop$age >= 18, 1 ,0)
loop$female_60_calc <- ifelse(loop$sex == "female" & loop$age >= 60, 1 ,0)
loop$tot_female <- ifelse(loop$sex == "female", 1 ,0)

#AGE
loop$tot_child_012 <- ifelse(loop$age <= 12 & loop$age >= 0, 1 ,0)
loop$tot_child_1317 <- ifelse(loop$age <= 17 & loop$age >= 13, 1 ,0)
loop$tot_child <- ifelse(loop$age <= 17 & loop$age >= 0, 1 ,0)
loop$tot_6_above <- ifelse(loop$age >= 6, 1 ,0)

#TOTAL FAMILY SIZE
loop$num_family_member <- 1


#FUN FUNCTION
#MALE
response <- individual_to_HH_numeric_sadd(loop, response, "male_2_calc")
response <- individual_to_HH_numeric_sadd(loop, response, "male_3_5_calc")
response <- individual_to_HH_numeric_sadd(loop, response, "male_6_17_calc")
response <- individual_to_HH_numeric_sadd(loop, response, "male_18_59_calc")
response <- individual_to_HH_numeric_sadd(loop, response, "male_60_calc")
response <- individual_to_HH_numeric_sadd(loop, response, "tot_male")
#FEMALE
response <- individual_to_HH_numeric_sadd(loop, response, "female_2_calc")
response <- individual_to_HH_numeric_sadd(loop, response, "female_3_5_calc")
response <- individual_to_HH_numeric_sadd(loop, response, "female_6_17_calc")
response <- individual_to_HH_numeric_sadd(loop, response, "female_18_59_calc")
response <- individual_to_HH_numeric_sadd(loop, response, "female_60_calc")
response <- individual_to_HH_numeric_sadd(loop, response, "tot_female")
#AGE
response <- individual_to_HH_numeric_sadd(loop, response, "tot_child_012")
response <- individual_to_HH_numeric_sadd(loop, response, "tot_child_1317")
response <- individual_to_HH_numeric_sadd(loop, response, "tot_child")
response <- individual_to_HH_numeric_sadd(loop, response, "tot_6_above")
#TOTAL FAMILY SIZE
response <- individual_to_HH_numeric_sadd(loop, response, "num_family_member")


sum(response$male_18_59_calc != "")
