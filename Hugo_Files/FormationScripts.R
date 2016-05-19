alldataFormation<-WA_HR_Training
alldataFormation$Year <- as.factor(alldataFormation$Year)
alldataFormation$Position.count <- as.factor(alldataFormation$Position.count)
alldataFormation$Planned.position.count <- as.factor(alldataFormation$Planned.position.count)
alldataFormation$Expense.total <- as.numeric(alldataFormation$Expense.total)
alldataFormation$Course.cost <- as.numeric(alldataFormation$Course.cost)
alldataFormation$Course.days <- as.numeric(alldataFormation$Course.days)
alldataFormation$Terminations <- as.factor(alldataFormation$Terminations)
alldataFormation$Internal.hires <- as.factor(alldataFormation$Internal.hires)
alldataFormation$External.hires <- as.factor(alldataFormation$External.hires)
alldataFormation$Attrition <- as.factor(alldataFormation$Attrition)

allDepartment_Expense.total <- ggplot(allDataFormation,  aes(x = Department, fill = Year)  ) + geom_bar() + xlab('Department') + ylab('Count')

#Creating Attrition column on dataFrame

attrition <<- 0
attrition_column = sapply(alldataFormation$Expense.total, function(x) {
  #Logic behind attrition value
  
  if(x < 30){
    attrition = 1
  } 
  return(attrition)
})


#Droping a column
  drops <- "Position.count"

cleanedFormation <- cleanedFormation[,!(names(cleanedFormation) %in% drops)]

#Add column to a dataFrame
alldataFormation["Attrition"] <- attrition_column


allDepartment<-ggplot(alldataFormation,  aes(x = Department, fill = Attrition)  ) + geom_bar() + xlab('Department') + ylab('Count')

#Standard Deviation
sapply(alldataFormation, sd)

alldataFormation.pca <- prcomp(alldataFormation)

alldataFormation.numericonly <- alldataFormation[,unlist(lapply(alldataFormation,is.numeric))]

alldataFormation.pca <- prcomp(alldataFormation.numericonly)

#removing NAs

na.omit(cleanedFormation)

