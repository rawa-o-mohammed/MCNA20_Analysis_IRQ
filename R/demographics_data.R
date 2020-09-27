#CALCULATE TOTAL POPULATION FIGURES BY POPULATION GROUP AND BY DISTRICT

#ADD DISTRICTS AND POPGROUP TO LOOP
response_districts <- response[,c("X_uuid", "district_mcna", "population_group")]
names(loop)[which(names(loop) == "X_submission__uuid")] <- "X_uuid"
loop[,("population_group")] <- NULL

loop_pop <- merge(loop, response_districts, by="X_uuid", all.x =T)


#SUBSET IDPs OUT OF CAMP AND AGGREGATE DEMOGRAPHICS DATA
loop_out_camp <- subset(loop_pop, loop_pop$population_group == "idp_out_camp")
dem_out_camp <- dcast(data=loop_out_camp,
               district_mcna + sex ~ age,
               fun.aggregate = length,
               value.var = "age")

#SUBSET IDPs IN CAMP AND AGGREGATE DEMOGRAPHICS DATA
loop_in_camp <- subset(loop_pop, loop_pop$population_group == "idp_in_camp")
dem_in_camp <- dcast(data=loop_in_camp,
                      district_mcna + sex ~ age,
                      fun.aggregate = length,
                      value.var = "age")

#SUBSET RETURNEES AND AGGREGATE DEMOGRAPHICS DATA
loop_returnee <- subset(loop_pop, loop_pop$population_group == "returnee")
dem_returnee <- dcast(data=loop_returnee,
                     district_mcna + sex ~ age,
                     fun.aggregate = length,
                     value.var = "age")

#EXPORT FILES
write.xlsx(dem_in_camp, file="output/sadd/demographics.xlsx", sheetName="in_camp", row.names=FALSE)
write.xlsx(dem_out_camp, file="output/sadd/demographics.xlsx", sheetName="out_camp", append=TRUE, row.names=FALSE)
write.xlsx(dem_returnee, file="output/sadd/demographics.xlsx", sheetName="returnee", append=TRUE, row.names=FALSE)

