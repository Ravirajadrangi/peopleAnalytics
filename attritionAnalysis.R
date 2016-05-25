#Libraries
library(caret)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(scales)

# Load data
originalData <- read.csv("~/sngular/peopleAnalytics/employee-attrition.csv")
dat = originalData

## See data types
str(dat)

# Clean data
dat = na.omit(dat)

# Remove unnecessary variables
dat = subset(dat, select = -c(EmployeeCount, EmployeeNumber, Over18, StandardHours, HourlyRate, DailyRate, MonthlyRate))

# Factors (Traducirlo)
dat$Education <- as.factor(dat$Education)
dat$EnvironmentSatisfaction <- as.factor(dat$EnvironmentSatisfaction)
dat$JobInvolvement <- as.factor(dat$JobInvolvement)
dat$JobSatisfaction <- as.factor(dat$JobSatisfaction)
dat$PerformanceRating <- as.factor(dat$PerformanceRating)
dat$RelationshipSatisfaction <- as.factor(dat$RelationshipSatisfaction)
dat$WorkLifeBalance <- as.factor(dat$WorkLifeBalance)
dat$JobLevel <- as.factor(dat$JobLevel)
dat$StockOptionLevel <- as.factor(dat$StockOptionLevel)

#Writing data to csv
write.csv(dat, "employee-attrition.csv", row.names = FALSE)

# Scale the data of numeric columns
# design.matrix <- dat
# numeric.columns <- design.matrix[,unlist(lapply(design.matrix,is.numeric))]
# scaled.numeric.columns <- scale(numeric.columns)
# design.matrix[,unlist(lapply(design.matrix,is.numeric))] <- scaled.numeric.columns
# dat.scaled <- design.matrix

# Plots

#Sales Representatives, Attrition rate against NumberOfCourses taken last year.
ggplot(dat[dat$Puesto=='Representante de Ventas',],  aes(x = CursosUltimoAno, fill = Abandono)  ) + geom_bar() + xlab('CursosUltimoAno') + ylab('Count')

# Attrition over job roles
ggplot(dat, aes(x = JobRole, fill = Attrition)) +
  stat_count(width = 0.5) +
  xlab("Job role") +
  ylab("Count") +
  labs(fill = "Attrition")

ggplot(dat, aes(x = JobRole)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())

# Attrition over job level
ggplot(dat, aes(x = JobLevel, fill = Attrition)) +
  stat_count(width = 0.5) +
  xlab("Job Level") +
  ylab("Count") +
  labs(fill = "Attrition")

ggplot(dat, aes(x = JobLevel)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())

# Attrition over overtime
ggplot(dat, aes(x = OverTime, fill = Attrition)) +
  stat_count(width = 0.5) +
  xlab("OverTime") +
  ylab("Count") +
  labs(fill = "Attrition")

ggplot(dat, aes(x = OverTime)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())


# Calculate standard deviations
sapply(dat, sd)

# Classification

# 0.75 training, 0.25 testing
inTraining <- createDataPartition(dat$Attrition, p=.75, list=FALSE)
training <- dat[ inTraining, ]
testing <- dat[ -inTraining, ]

# Decision tree
fit <- rpart(
  Attrition ~ 
    OverTime + TotalWorkingYears + JobLevel + MaritalStatus + 
    YearsInCurrentRole + MonthlyIncome + Age + YearsWithCurrManager + 
    StockOptionLevel , data=training, method="class") 

prp(fit)

# Random forest
rForestFit <- randomForest(
  Attrition ~ 
    OverTime + TotalWorkingYears + JobLevel + MaritalStatus + 
    YearsInCurrentRole + MonthlyIncome + Age + YearsWithCurrManager + 
    StockOptionLevel, 
  data = training,
  ntree = 100)
print(rForestFit)
varImpPlot(rForestFit)

testPred <- predict(fit, testing , type="class")

postResample(testPred, testing$Attrition)

sensitivity(testPred, testing$Attrition)

confMatrix <- confusionMatrix(testPred, testing$Attrition)

confMatrix2 <- confusionMatrix(testing$Attrition, sample(testing$Attrition))

#### Fixing Data: Representatives with low attrition, should have low training.

#Assigns a random between 0 and 1
assign_random <- function(perc) {
  return(ifelse(runif(1, 0.0, 1.0)<=perc, 1, 0))
}

#Assigns currentVal or calls Assign random between 2 and 6
assign_random2 <- function(perc, currentVal) {
  return(ifelse(runif(1, 0.0, 1.0)<=perc, currentVal, assign_min_max(2,6)))
}

#Assigns currentVal or 0
assign_random3 <- function(perc, currentVal) {
  return(ifelse(runif(1, 0.0, 1.0)<=perc, currentVal, 0))
}

#Assign random between 2 and 6
assign_min_max <- function(min,max){
  round(runif(1, min, max), digits=0)
}

#Function one: Makes Representatives with Attri=Si to 0 or 1 CursoUltimoAno
dat$CursosUltimoAno = apply(dat[,c('Abandono','CursosUltimoAno','Puesto')], 1, function(x){
  if(x['Abandono']=="Si"){
    if(x['Puesto']=="Representante de Ventas"){
      if(x['CursosUltimoAno']>1){
        return(assign_random(0.5))
      }else{
        return(x['CursosUltimoAno'])
      }
    }
  }
  return(x['CursosUltimoAno'])
})

#Function two: Moves 30% of Representatives with Attri=Si to 2 to 6 CursoUltimoAno
dat$CursosUltimoAno = apply(dat[,c('Abandono','CursosUltimoAno','Puesto')], 1, function(x){
  if(x['Abandono']=="Si"){
    if(x['Puesto']=="Representante de Ventas"){
      if(x['CursosUltimoAno']<2){
        return(assign_random2(0.7, x['CursosUltimoAno']))
      }else{
        return(x['CursosUltimoAno'])
      }
    }
  }
  return(x['CursosUltimoAno'])
})

#Function 3:Moves 5% of Representatives with Attri=No from cursosTomados 3 to 0
dat$CursosUltimoAno = apply(dat[,c('Abandono','CursosUltimoAno','Puesto')], 1, function(x){
  if(x['Abandono']=="No"){
    if(x['Puesto']=="Representante de Ventas"){
      if(x['CursosUltimoAno']==3){
        return(assign_random3(0.95, x['CursosUltimoAno']))
      }else{
        return(x['CursosUltimoAno'])
      }
    }
  }
  return(x['CursosUltimoAno'])
})
