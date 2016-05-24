# Libraries

# Load data
#fdat = read.csv("~/sngular/peopleAnalytics/employee-training.csv")
fdat = read.csv("/Users/Hugo/Documents/EIT Master/SingularMeaning/peopleanalytics/employee-training.csv")
# Clean data
fdat = na.omit(fdat)

## See data types
str(fdat)


# Factors


write.csv(fdat, "~/sngular/peopleAnalytics/employee-training.csv", row.names = FALSE)

#-----------------------------------------------
# Columns to add:
# Input: Curso.name, Curso.nota, Curso.online


#Number of Cursos: Not used, because every employee took 1 Curso only. 
#Because it is a continuation of the previous data set that suggested that employees needed more education/training to reduce the Abandono. 

#Puesto
#Puesto: In order to have continuity between history 1 and 2, change Puesto names to
#1. Sales Ejecutivo -> 73.6%
#2. Sales Manager -> 8.5%
#3. Sales Representantes 18.1%

# Abandono according to Puesto:
#1. Sales Ejecutivo -> 17.48%
#2. Sales Manager -> 5.4%
#3. Sales Representantes 39.75%

#Curso
#Cursos Names:
#1. Curso A 55%
#2. Curso B 45%

#Curso.Online
#Curso.Online
#Si 65%
#No 35%

#Curso.nota
#Curso nota:
#1-10

# Output: Abandono, Performance.delta

#Abandono
#Abandono:
#Si/No

#Rendimiento.delta
#Performance.delta
# -/+ %
#-----------------------------------------------

fdat$Puesto = sapply(fdat$Puesto, function(Puesto) {
  return(assign_Puesto())
})

fdat$Abandono = sapply(fdat$Puesto, function(Puesto) {
  ifelse(Puesto == "Manager", assign_Abandono(0.054),
         ifelse(Puesto == "Ejecutivo", assign_Abandono(0.1748), assign_Abandono(0.3975)))
  
})

fdat$Tematica = sapply(fdat$Curso.dias, function(dias) {
  assign_Tematica(0.55)
})

fdat$Curso.online = sapply(fdat$Curso.dias, function(dias) {
  assign_Curso_online(0.65)
})

fdat$Curso.nota = sapply(fdat$Curso.dias, function(dias) {
  runif(1, 4.0, 10.0)
})

# Functions

assign_Abandono <- function(perc) {
  return(ifelse(runif(1, 0.0, 1.0)<=perc, "Si", "No"))
}

assign_Puesto <- function() {
  perc = runif(1, 0.0, 1.0)
  return(ifelse(perc <= 0.085, "Manager",
                ifelse(perc <= 0.181, "Representante", "Ejecutivo")))
}

assign_Tematica <- function(perc) {
  return(ifelse(runif(1, 0.0, 1.0)<=perc, "Negociacion", "Presentacion"))
}

assign_Curso_online <- function(perc) {
  return(ifelse(runif(1, 0.0, 1.0)<=perc, "Si", "No"))
}

names(fdat)[names(fdat) == 'Curso'] <- 'Tematica'
# Plots
ggplot(fdat,  aes(x = Curso.name, fill = Curso.online)  ) + geom_bar() + xlab('Curso.name') + ylab('Count')
