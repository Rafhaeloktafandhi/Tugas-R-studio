rm(list=ls())
data1<-read.csv(file="Model Linier/Students_Grading_Dataset.csv", sep = ";")

# Fit model regresi berganda
model2 <- lm(Total_Score ~Stress_Level..1.10. + Study_Hours_per_Week, data = data1)

# Tampilkan ringkasan
summary(model2)

# Estimasi b0, b1, b2, b3
coefficients(model2)

