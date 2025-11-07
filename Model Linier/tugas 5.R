rm(list=ls())
data1<-read.csv(file="Model Linier/Students_Grading_Dataset.csv", sep = ";")

# Fitted model regresi Total_Score ~ x_binary
model <- lm(Total_Score ~ x_binary, data = data1)
summary(model)

# Extract informasi penting
r2 <- summary(model)$r.squared
adjr2 <- summary(model)$adj.r.squared
fstat <- summary(model)$fstatistic

f_value <- fstat[1]
df1 <- fstat[2]  # numerator df
df2 <- fstat[3]  # denominator df
p_value <- pf(f_value, df1, df2, lower.tail = FALSE)

# Cetak hasil
cat("R-squared:", r2, "\n")
cat("Adjusted R-squared:", adjr2, "\n")
cat("F-statistic:", f_value, "on", df1, "and", df2, "DF\n")
cat("p-value:", p_value, "\n")

data1$x_binary <- ifelse(data1$Gender == "Female", 0, 1)

# Hapus baris yang memiliki NA pada Total_Score atau x_binary
data_clean <- na.omit(data1[, c("Total_Score", "x_binary")])

# Hitung rata-rata Total_Score untuk setiap kategori x_binary
aggregate(Total_Score ~ x_binary, data = data_clean, mean)
tapply(data_clean$Total_Score, data_clean$x_binary, mean)



