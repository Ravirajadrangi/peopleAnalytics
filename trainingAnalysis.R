# Libraries

# Load data
#fdat = read.csv("~/sngular/peopleAnalytics/employee-training.csv")
fdat = read.csv("/Users/Hugo/Documents/EIT Master/SingularMeaning/peopleanalytics/employee-training.csv")
# Clean data
fdat = subset(fdat, select = -c(Position.count, Planned.position.count, Terminations, Internal.hires, External.hires, Department, Organization))
fdat = na.omit(fdat)

## See data types
str(fdat)


#Adding Columns
Course.name <<- 0
Course.gpa <<- 0
Course.online <<- 0
Performance.delta <<- 0
Attrition <<- 0
fdat["Course.name"] <- Course.name
fdat["Course.gpa"] <- Course.gpa
fdat["Course.online"] <- Course.online
fdat["Performance.delta"] <- Performance.delta
fdat["Attrition"] <- Attrition


write.csv(fdat, "employee-training.csv")
#-----------------------------------------------
# Columns to add:
# Input: Course.name, Course.gpa, Course.online

#Number of Courses: Not used, because every employee took 1 course only. 
#Because it is a continuation of the previous data set that suggested that employees needed more education/training to reduce the attrition. 

#Position: In order to have continuity between history 1 and 2, change position names to
#1. Sales Executive -> 73.6%
#2. Sales Manager -> 8.5%
#3. Sales Representatives 18.1%

#Courses Names:
#1. Course A 55%
#2. Course B 45%

#Course.Online
#Yes 65%
#No 35%

#Course Gpa:
#1-10

# Output: Attrition, Performance.delta

#Attrition:
#Yes/No

#Performance.delta
# -/+ %
#-----------------------------------------------

# Factors


# Functions
fdat$Attrition = sapply(fdat$Course.days, function(days) {
  ifelse(days > 5, assign_attrition(0.05), assign_attrition(0.95))
})

ggplot(fdat, aes(x = Course.days)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())


fdat$Course.name = sapply(fdat$Course.days, function(days) {
  assign_course_name(0.55)
})

fdat$Course.online = sapply(fdat$Course.days, function(days) {
  assign_course_online(0.65)
})

fdat$Course.gpa = sapply(fdat$Course.days, function(days) {
  runif(1, 4.0, 10.0)
})

assign_attrition <- function(perc) {
  return(ifelse(runif(1, 0.0, 1.0)<=perc, "Yes", "No"))
}

assign_course_name <- function(perc) {
  return(ifelse(runif(1, 0.0, 1.0)<=perc, "Course A", "Course B"))
}

assign_course_online <- function(perc) {
  return(ifelse(runif(1, 0.0, 1.0)<=perc, "Yes", "No"))
}


#Graphs

ggplot(fdat,  aes(x = Course.name, fill = Course.online)  ) + geom_bar() + xlab('Course.name') + ylab('Count')

ggplot(fdat, aes(x = Course.name)) + geom_bar(), position = 'fill') 




