library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(gridExtra)

setwd("/Users/alejandrotellez/latg/cursos/unir/master-big-data/1er_cuatrimestre/ingnieria_procesado_masivo_datos/actividades/03-analisis-exploratorio-con-r")

df <- read.csv("attrition_data.csv", stringsAsFactors = T, sep = )

df <- select(df, -c("DailyRate",
              "HourlyRate", 
              "MonthlyRate", 
              "EmployeeCount", 
              "EmployeeNumber", 
              "PerformanceRating", 
              "StandardHours"))

summary(df)

df$AttritionNum <- as.numeric(df$Attrition)

df %>%
  keep(is.numeric) %>%
  gather(key, value, -AttritionNum) %>%
  ggplot(aes(value, fill = factor(AttritionNum))) + 
  geom_histogram(alpha = 0.6, position = "identity") +
  scale_fill_discrete(name = "Attrition", labels = c("No", "Yest")) +
  facet_wrap(~key, scales = "free")

color.hr <- "darkblue"
color.dev <- "darkred"
color.sales <- "darkorange"

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
    ylab("Attritioned / Total")
  
  plot2 <- df %>%
    ggplot(aes_string(x=department, y=field, fill=department)) + 
    geom_boxplot() +
    scale_fill_manual(breaks = dep.vector,
                      values = colors.dep.vector)
  
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
    ylab("Attritioned / Total")
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





