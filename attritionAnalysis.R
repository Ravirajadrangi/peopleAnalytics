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

names(dat)[names(dat)=="Age"] <- "Edad"
names(dat)[names(dat)=="BusinessTravel"] <- "Viajes"
names(dat)[names(dat)=="Department"] <- "Departamento"
names(dat)[names(dat)=="DistanceFromHome"] <- "DistanciaDesdeCasa"
names(dat)[names(dat)=="Education"] <- "Educacion"
names(dat)[names(dat)=="EducationField"] <- "CampoDeEducacion"
names(dat)[names(dat)=="Gender"] <- "Genero"
names(dat)[names(dat)=="JobInvolvement"] <- "Implicacion"
names(dat)[names(dat)=="MaritalStatus"] <- "EstadoCivil"
names(dat)[names(dat)=="MonthlyIncome"] <- "SueldoMensual"
names(dat)[names(dat)=="NumCompaniesWorked"] <- "NumEmpresasTrabajadas"
names(dat)[names(dat)=="OverTime"] <- "HorasExtra"
names(dat)[names(dat)=="PercentSalaryHike"] <- "PorcentajeAumentoSalarial"
names(dat)[names(dat)=="PerformanceRating"] <- "Rendimiento"
names(dat)[names(dat)=="RelationshipSatisfaction"] <- "RelacionSatisfaccion"
names(dat)[names(dat)=="StockOptionLevel"] <- "NivelAcciones"
names(dat)[names(dat)=="TotalWorkingYears"] <- "AñosTotalesTrabajados"
names(dat)[names(dat)=="TrainingTimesLastYear"] <- "CursosUltimoAño"
names(dat)[names(dat)=="WorkLifeBalance"] <- "EquilibrioVidaTrabajo"
names(dat)[names(dat)=="YearsAtCompany"] <- "AñosEnEmpresa"
names(dat)[names(dat)=="YearsInCurrentRole"] <- "AñosEnPuestoActual"
names(dat)[names(dat)=="YearsSinceLastPromotion"] <- "AñosDesdeUltimaPromocion"
names(dat)[names(dat)=="YearsWithCurrManager"] <- "AñosConJefeActual"
names(dat)[names(dat)=="Attrition"] <- "Abandono"
names(dat)[names(dat) == "JobRole"] <- "Cargo"
names(dat)[names(dat) == "JobSatisfaction"] <- "SatisfaccionEnTrabajo"
names(dat)[names(dat) == "JobLevel"] <- "NivelCargo"

dat[] <- lapply(dat, as.character)

dat$Abandono[dat$Abandono == "Yes"] <- "Si"
dat$HorasExtra[dat$HorasExtra == "Yes"] <- "Si"

dat$Viajes[dat$Viajes == "Travel_Rarely"] <- "Viaja_Raramente"
dat$Viajes[dat$Viajes == "Travel_Frequently"] <- "Viaja_frecuentemente"
dat$Viajes[dat$Viajes == "Non-Travel"] <- "No_Viaja"

dat$Departamento[dat$Departamento == "Research & Development"] <- "Investigacion & Desarrollo"
dat$Departamento[dat$Departamento == "Sales"] <- "Ventas"
dat$Departamento[dat$Departamento == "Human Resources"] <- "Recursos Humanos"

dat$CampoDeEducacion[dat$CampoDeEducacion == "Other"] <- "Otro"
dat$CampoDeEducacion[dat$CampoDeEducacion == "Human Resources"] <- "Recursos Humanos"
dat$CampoDeEducacion[dat$CampoDeEducacion == "Life Sciences"] <- "Ciencias de la Salud"
dat$CampoDeEducacion[dat$CampoDeEducacion == "Medical"] <- "Medicina"
dat$CampoDeEducacion[dat$CampoDeEducacion == "Technical Degree"] <- "Tecnico"

dat$Genero[dat$Genero == "Female"] <- "Mujer"
dat$Genero[dat$Genero == "Male"] <- "Hombre"

dat$Cargo[dat$Cargo == "Healthcare Representative"] <- "Representante de Salud"
dat$Cargo[dat$Cargo == "Human Resources"] <- "Recursos Humanos"
dat$Cargo[dat$Cargo == "Laboratory Technician"] <- "Tecnico de Laboratorio"
dat$Cargo[dat$Cargo == "Manufactoring Director"] <- "Director de Produccion"
dat$Cargo[dat$Cargo == "Research Director"] <- "Director de Investigacion"
dat$Cargo[dat$Cargo == "Research Scientist"] <- "Investigador Cientifico"
dat$Cargo[dat$Cargo == "Sales Executive"] <- "Ejecutivo de Ventas"
dat$Cargo[dat$Cargo == "Sales Representative"] <- "Representante de Ventas"

dat$EstadoCivil[dat$EstadoCivil == "Single"] <- "Soltero"
dat$EstadoCivil[dat$EstadoCivil == "Divorced"] <- "Divorciado"
dat$EstadoCivil[dat$EstadoCivil == "Married"] <- "Casado"
