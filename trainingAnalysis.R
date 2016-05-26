# Libraries
library(ggplot2)

# Load data
fdat = read.csv("~/sngular/peopleAnalytics/employee-training.csv")
# fdat = read.csv("/Users/Hugo/Documents/EIT Master/SingularMeaning/peopleanalytics/employee-training.csv")
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
         ifelse(Puesto == "Ejecutivo", assign_Abandono(0.1148), assign_Abandono(0.2975)))
  
})

fdat$Tematica = sapply(fdat$Curso.dias, function(dias) {
  assign_Tematica(0.55)
})

fdat$Curso.online = sapply(fdat$Curso.dias, function(dias) {
  assign_Curso_online(0.65)
})

fdat$Curso.nota = sapply(fdat$Curso.dias, function(dias) {
  round(runif(1, 4.0, 10.0), digits = 2)
})

fdat$Curso.dias = sapply(fdat$Tematica, function (tema) {
  assign_dias(tema)
})

fdat$Curso.coste = apply(fdat[,c('Tematica','Curso.online', "Curso.dias")], 1, function (x) {
  assign_Coste(x[['Tematica']], x[['Curso.online']], x[['Curso.dias']])
})

fdat$Rendimiento.delta =apply(fdat[,c('Puesto','Tematica','Curso.online')], 1, function(x){
  if(x['Tematica']=='Presentacion'){
    if(x['Curso.online']=='Si'){
      if(x['Puesto']=='Ejecutivo'){
        return(assign_rendimiento_delta(-2.5, 2.5))
      }else if(x['Puesto']=='Manager'){
        return(assign_rendimiento_delta(-2.5, 2.5))
      }else{
        return(assign_rendimiento_delta(4, 8))
      }
      #Curso.online=No
    }else{
      if(x['Puesto']=='Ejecutivo'){
        return(assign_rendimiento_delta(2.5, 5))
      }else if(x['Puesto']=='Manager'){
        return(assign_rendimiento_delta(0,4))
      }else{
        return(assign_rendimiento_delta(5,10))
      }
    }
  #Negociacion  
  }else{
    if(x['Curso.online']=='Si'){
      if(x['Puesto']=='Ejecutivo'){
        return(assign_rendimiento_delta(4, 8))
      }else if(x['Puesto']=='Manager'){
        return(assign_rendimiento_delta(2, 6))
      }else{
        return(assign_rendimiento_delta(-3,4))
      }
      #Curso.online=No
    }else{
      if(x['Puesto']=='Ejecutivo'){
        return(assign_rendimiento_delta(7,10))
      }else if(x['Puesto']=='Manager'){
        return(assign_rendimiento_delta(3,7))
      }else{
        return(assign_rendimiento_delta(0,3))
      }
    }
  }
})


#Using Normal Distribution to generate BeneficioNeto
fdat$BeneficioNeto = apply(fdat[,c('Puesto','Tematica','Curso.online')], 1, function(x){
  currentValue<-rnorm(1, mean = 40000, sd = 30000)
  if(currentValue<0){
    currentValue<- currentValue+20000
  }
  return(currentValue)
})

#Get BeneficioNetoAbandono Total
result<<-0
BeneficioNetoAbandono = apply(fdat[,c('Abandono','BeneficioNeto')],1,function(x){
  if(x['Abandono']=='Si'){
  result=result+as.numeric(x['BeneficioNeto'])
   return(result)
  }
  return(0)
})
beneficioNetoAbandonoTotal<-sum(BeneficioNetoAbandono)

#Get BeneficioNetoAbandono Counter
counter<<-0
BeneficioNetoAbandonoCounter = apply(fdat[,c('Abandono','BeneficioNeto')],1,function(x){
  if(x['Abandono']=='Si'){
    counter=counter+1
    return(counter)
  }
  return(0)
})
beneficioNetoCounter<-sum(BeneficioNetoAbandonoCounter)

#Average Attrition Lost
avgAttritionLost<-beneficioNetoAbandonoTotal/beneficioNetoCounter

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

assign_rendimiento_delta <- function(min,max){
  round(runif(1, min, max), digits=2)
}

assign_Coste <- function (tema, online, dias) {
  dias = as.numeric(dias)
  online_cost = c(0, 100)
  names(online_cost) <- c("Si", "No")
  cost_per_day_per_person = c(150, 225)
  names(cost_per_day_per_person) <- c("Negociacion", "Presentacion")
  
  # Online cost is not multiplied by num of days because it's a one time cost
  # cost_per_day refers to the cost of teachers, installations, etc. of an specific kind of course
  return(cost_per_day_per_person[tema]*dias + online_cost[online])
}

assign_dias <- function(tema) {
  if (tema == "Negociacion") {
    return(ifelse(runif(1,0,1) < 0.5, 4, 8))
  } else {
    return(ifelse(runif(1,0,1) < 0.5, 2, 5))
  }
}

# Plots
ggplot(fdat,  aes(x = Curso.name, fill = Curso.online)  ) + geom_bar() + xlab('Curso.name') + ylab('Count')
ggplot(fdat, aes(x = Rendimiento.delta)) + geom_histogram(binwidth = 0.1)
