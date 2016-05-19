# Libraries

# Load data
fdat = read.csv("~/sngular/peopleAnalytics/employee-training.csv")

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





# Factors



# Functions
assign_attrition <- function(perc) {
  return(ifelse(runif(1, 0.0, 1.0)<=perc, "Yes", "No"))
}

fdat$Attrition = sapply(fdat$Course.days, function(days) {
  ifelse(days > 5, assign_attrition(0.05), assign_attrition(0.95))
})

ggplot(fdat, aes(x = Course.days)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())
