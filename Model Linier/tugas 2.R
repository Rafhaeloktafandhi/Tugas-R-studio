y_hat <- b0 + b1 * x1

# Residual (selisih antara nilai asli dan prediksi)
residuals <- y - y_hat
n <- length(x1)
# Variansi error (σ² atau residual variance)
sigma_squared <- sum(residuals^2) / (n - 2)

# Standard error dari b1
SE_b1 <- sqrt(sigma_squared / sum((x1 - mean(x1))^2))

# Nilai t
t_value <- b1 / SE_b1

# p-value (dua sisi)
p_value <- 2 * pt(-abs(t_value), df = n - 2)

# Selang kepercayaan 95% untuk b1
alpha <- 0.05
t_critical <- qt(1 - alpha/2, df = n - 2)
CI_lower <- b1 - t_critical * SE_b1
CI_upper <- b1 + t_critical * SE_b1

# Output hasil
cat("Variansi error (σ²):", sigma_squared, "\n")
cat("Standard Error b1:", SE_b1, "\n")
cat("t value:", t_value, "\n")
cat("p-value:", p_value, "\n")
cat("95% Confidence Interval for b1: [", CI_lower, ",", CI_upper, "]\n")

