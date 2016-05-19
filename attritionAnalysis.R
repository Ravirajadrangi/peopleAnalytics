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

# Factors
dat$Education <- as.factor(dat$Education)
dat$EnvironmentSatisfaction <- as.factor(dat$EnvironmentSatisfaction)
dat$JobInvolvement <- as.factor(dat$JobInvolvement)
dat$JobSatisfaction <- as.factor(dat$JobSatisfaction)
dat$PerformanceRating <- as.factor(dat$PerformanceRating)
dat$RelationshipSatisfaction <- as.factor(dat$RelationshipSatisfaction)
dat$WorkLifeBalance <- as.factor(dat$WorkLifeBalance)
dat$JobLevel <- as.factor(dat$JobLevel)
dat$StockOptionLevel <- as.factor(dat$StockOptionLevel)

# Scale the data of numeric columns
# design.matrix <- dat
# numeric.columns <- design.matrix[,unlist(lapply(design.matrix,is.numeric))]
# scaled.numeric.columns <- scale(numeric.columns)
# design.matrix[,unlist(lapply(design.matrix,is.numeric))] <- scaled.numeric.columns
# dat.scaled <- design.matrix

# Plots

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
