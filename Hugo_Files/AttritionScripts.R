alldata$Education <- as.factor(alldata$Education)
alldata$EnvironmentSatisfaction <- as.factor(alldata$EnvironmentSatisfaction)
alldata$JobInvolvement <- as.factor(alldata$JobInvolvement)
alldata$JobSatisfaction <- as.factor(alldata$JobSatisfaction)
alldata$PerformanceRating <- as.factor(alldata$PerformanceRating)
alldata$RelationshipSatisfaction <- as.factor(alldata$RelationshipSatisfaction)
alldata$WorkLifeBalance <- as.factor(alldata$WorkLifeBalance)
alldata$JobLevel <- as.factor(alldata$JobLevel)
alldata$StockOptionLevel <- as.factor(alldata$StockOptionLevel)

allJobRoles<-ggplot(alldata,  aes(x = JobRole, fill = Attrition)  ) + geom_bar() + xlab('Job Role') + ylab('Count')

allDepartments<-ggplot(alldata,  aes(x = Department, fill = Attrition)  ) + geom_bar() + xlab('Job Role') + ylab('Count')

alleducationFields<-ggplot(alldata,  aes(x = EducationField, fill = Attrition)  ) + geom_bar() + xlab('Educational Field') + ylab('Count')

allDistanceFromHome<-ggplot(alldata,  aes(x = DistanceFromHome, fill = Attrition)  ) + geom_bar() + xlab('DistanceFromHome') + ylab('Count')

allYearsAtCompany<-ggplot(alldata,  aes(x = YearsAtCompany, fill = Attrition)  ) + geom_bar() + xlab('DistanceFromHome') + ylab('Count')



