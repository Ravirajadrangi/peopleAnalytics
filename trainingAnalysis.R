# Libraries

# Load data
#fdat = read.csv("~/sngular/peopleAnalytics/employee-training.csv")
fdat = read.csv("/Users/Hugo/Documents/EIT Master/SingularMeaning/peopleanalytics/employee-training.csv")
# Clean data
fdat = subset(fdat, select = -c(Position.count, Planned.position.count, Terminations, Internal.hires, External.hires, Department, Organization))
fdat = na.omit(fdat)

## See data types
str(fdat)


#-----------------------------------------------
# Columns to add:
# Input: Course.name, Course.gpa, Course.online

#Number of Courses: Not used, because every employee took 1 course only. 
#Because it is a continuation of the previous data set that suggested that employees needed more education/training to reduce the attrition. 

#Position: In order to have continuity between history 1 and 2, change position names to
#1. Sales Executive -> 73.6%
#2. Sales Manager -> 8.5%
#3. Sales Representatives 18.1%

# Attrition according to position:
#1. Sales Executive -> 17.48%
#2. Sales Manager -> 5.4%
#3. Sales Representatives 39.75%

#Courses Names:
#1. Course A
#2. Course B

#Course.Online
#Yes/No

#Course Gpa:
#1-10

# Output: Attrition, Performance.delta

#Attrition:
#Yes/No

#Performance.delta
# -/+ %
#-----------------------------------------------

fdat$Course.name = sapply(fdat$Course.days, function(days) {
  ifelse(days > 5, assign_course_name(0.50), assign_course_name(0.50))
})

fdat$Course.online = sapply(fdat$Course.days, function(days) {
  ifelse(days > 5, assign_course_online(0.46), assign_course_online(0.54))
})

fdat$Position = sapply(fdat$Position, function(position) {
  return(assign_position())
})

fdat$Attrition = sapply(fdat$Position, function(position) {
  ifelse(position == "Manager", assign_attrition(0.054),
         ifelse(position == "Executive", assign_attrition(0.1748), assign_attrition(0.3975)))
})

# Factors


# Functions
assign_attrition <- function(perc) {
  return(ifelse(runif(1, 0.0, 1.0)<=perc, "Yes", "No"))
}

assign_position <- function() {
  perc = runif(1, 0.0, 1.0)
  return(ifelse(perc <= 0.085, "Manager",
                ifelse(perc <= 0.181, "Representative", "Executive")))
}

assign_course_name <- function(perc) {
  return(ifelse(runif(1, 0.0, 1.0)<=perc, "Course A", "Course B"))
}

assign_course_online <- function(perc) {
  return(ifelse(runif(1, 0.0, 1.0)<=perc, "Yes", "No"))
}


# Plots
ggplot(fdat, aes(x = Position)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())

# Update csv file
write.csv(fdat, '~/sngular/peopleAnalytics/employee-training.csv')
