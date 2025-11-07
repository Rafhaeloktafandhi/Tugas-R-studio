install.packages("openxlsx")
install.packages("ggplot2")
install.packages("openxlsx") # kalau belum
library(openxlsx)
rm(list=ls())
data2 <- read.xlsx("Model Linier/hasil_preprocessing_modlin.xlsx")

names(data2)
x_binary <- ifelse(data2$Family_Income_Level == "Female", 0, 1)

x <- data2$Participation_Score
y <- data2$Total_Score
x1 <- data2$Family_Income_Level
x2 <- data2$`Stress_Level.(1-10)`
x4<- factor(data2$Family_Income_Level, levels = c("High", "Medium", "Low"))
x5 <- data2$Study_Hours_per_Week
x6 <-data2$Grade
model <- lm(y ~ x + x4, data = data2)
model1 <- lm(y ~ x + x4 +x4*x, data = data2)
model2 <- lm(y ~ x + I(x^2))

# Tampilkan ringkasan model (summary)
summary(model)
summary(model1)
summary(model2)

# Tampilkan estimasi koefisien (b0, b1, b2)
coefficients(model)
coefficients(model1)

