# Cargo los datos a Dataframe Tarea
Examen2 <- read.csv("~/Desktop/datos2.csv")
View(Examen2)
attach(Examen2)
lm(Examen2)
pair(Examen2)
cor(Examen2)
summary(Examen2)