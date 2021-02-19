##############################################################
####Script de la actividad grupal de la asignatura
####Ingeniería para el Procesado Masivo de Datos
####Fecha: 18/02/2021
####Alumns: Constantina Bindand, Iratxe Rubio Benito del Valle
####y Luis Alejandro Tellez Godoy
##############################################################

#0.Paquetes necesarios para ejecutar el script
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(gridExtra)


#1.Lectura de datos####
df <- read.csv("https://raw.githubusercontent.com/alejotem11/grupal-ingenieria/master/attrition_data.csv", stringsAsFactors = T)
colnames(df)[1] <- "Age"

df <- select(df, -c("DailyRate",
              "HourlyRate", 
              "MonthlyRate", 
              "EmployeeCount", 
              "EmployeeNumber", 
              "PerformanceRating", 
              "StandardHours"))


#2.Exploracion inicial de los datos####
summary(df) #media, min, max, mediana, cuartiles 1 y 3

datosnum <- df[,sapply(df,is.numeric)]
sapply(datosnum, sd) #desviaciones estandard
df$AttritionNum <- as.numeric(df$Attrition)

df %>% #histogramas variables numericas y ordinales
  keep(is.numeric) %>%
  gather(key, value, -AttritionNum) %>%
  ggplot(aes(value, fill = factor(AttritionNum))) + 
    geom_histogram(alpha = 0.6, position = "identity") +
    scale_fill_discrete(name = "Attrition", labels = c("No", "Yes")) +
    facet_wrap(~key, scales = "free") +
    theme_bw() +
    xlab("") +
    ylab("Número de personas")

datosfac <- df[,sapply(df,is.factor)]

datosfac %>% #histogramas variables nominales
  gather(key = type_col, value = categories, -Attrition) %>%
  ggplot(aes(x = categories, fill = Attrition)) +
    geom_bar() + 
    facet_wrap(~ type_col, scales = "free") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    xlab("") +
    ylab("Número de personas")
    

#3.Exploracion profunda de los datos####
color.hr <- "#7EB9F8"
color.dev <- "#FD7373"
color.sales <- "#7AD067"

dep.vector <- c("Human Resources", "Research & Development", "Sales")
colors.dep.vector <- c(color.hr, color.dev, color.sales)

analyzeContinuous <- function(df, min, max, groupSize = 5, minRowsToCount = 5, field) {
  department <- "Department"
  df.inner <- df
  labels <- c(paste(seq(min, max - 1, by = groupSize),
                       seq(min + groupSize - 1, max, by = groupSize),
                       sep = "-"))
  df.inner$Group <- cut(df.inner[,field], breaks = seq(min, max + 1, by = groupSize), labels = labels, right = FALSE)
  df.inner <- df.inner %>%
    group_by(Department, Group) %>%
    summarise(TotalAttritioned = sum(Attrition=="Yes"),
              Total = n(),
              Ratio = if(Total > minRowsToCount) (TotalAttritioned / Total) else NA)

  plot1 <- df.inner %>%
    ggplot(aes(x=Group, y=Ratio, group=Department, color=Department)) +
    geom_line() +
    scale_color_manual(breaks = dep.vector,
                       values = colors.dep.vector) +
    ylab("Ratio de salidas") + theme_classic()
  
  plot2 <- df %>%
    ggplot(aes_string(x=department, y=field, fill=department)) + 
    geom_boxplot() +
    scale_fill_manual(breaks = dep.vector,
                      values = colors.dep.vector) + theme_classic()
  
  grid.arrange(plot1, plot2, nrow=2)
}

analyzeDiscrete <- function(df, field, minRowsToCount = 5) {
  department <- "Department"
  df.inner <- df
  df.inner <- df %>%
    group_by_at(c(department, field)) %>%
    summarise(TotalAttritioned = sum(Attrition=="Yes"),
              Total = n(),
              Ratio = if(Total > 3) (TotalAttritioned / Total) else NA)
  
  df.inner %>%
    ggplot(aes_string(x=field, y="Ratio", group=department, color=department)) +
    geom_line() +
    scale_color_manual(breaks = dep.vector,
                       values = colors.dep.vector) +
    ylab("Ratio de salidas") + theme_classic()
}

# Edad
minAge <- 20
maxAge <- 59

df.age <- df[(df$Age >= minAge) & (df$Age <= maxAge),]
analyzeContinuous(df = df.age, min = minAge, max = maxAge, field = "Age")

# Distancia desde casa
analyzeContinuous(df = df, min = 0, max = 30, field = "DistanceFromHome")

# Nivel educativo
analyzeDiscrete(df = df, "Education")

# Satisfacción en el entorno de trabajo
analyzeDiscrete(df = df, "EnvironmentSatisfaction")

# Monthly Income
analyzeContinuous(df = df, min = 0, max = 20000, groupSize = 2000, field = "MonthlyIncome")

# Ajuste salarial
analyzeContinuous(df = df, min = 0, max = 30, field = "PercentSalaryHike")

# Total de años trabajados
analyzeContinuous(df = df, min = 0, max = 45, field = "TotalWorkingYears")

# Años en la empresa
analyzeContinuous(df = df, min = 0, max = 45, field = "YearsAtCompany")

# Años en la posición actual
analyzeContinuous(df = df, min = 0, max = 20, field = "YearsInCurrentRole")

# Años desde el último ascenso
analyzeContinuous(df = df, min = 0, max = 18, groupSize = 3, field = "YearsSinceLastPromotion")


#4.Variables de interes seleccionadas apartir de las graficas####
#edad
#salario
#distancia
#satisfacción