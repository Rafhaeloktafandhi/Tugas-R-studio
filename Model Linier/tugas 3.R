# Rata-rata y
y_mean <- mean(y)

# Total Sum of Squares (SST)
SST <- sum((y - y_mean)^2)

# Regression Sum of Squares (SSR)
SSR <- sum((y_hat - y_mean)^2)

# Error Sum of Squares (SSE)
SSE <- sum((y - y_hat)^2)

# R-squared
R_squared <- SSR / SST

# Derajat bebas
df1 <- 1           # Jumlah variabel independen
df2 <- n - 2       # Residual df

# Mean Square Regression dan Error
MSR <- SSR / df1
MSE <- SSE / df2

# F-statistic
F_value <- MSR / MSE

# p-value untuk F-statistic
p_value_F <- pf(F_value, df1, df2, lower.tail = FALSE)

# Output
cat("n:", n, "\n")
cat("R-squared:", R_squared, "\n")
cat("SST:", SST, "\n")
cat("SSR:", SSR, "\n")
cat("SSE:", SSE, "\n")
cat("df1 (regression):", df1, "\n")
cat("df2 (residual):", df2, "\n")
cat("F-statistic:", F_value, "\n")
cat("p-value:", p_value_F, "\n")

