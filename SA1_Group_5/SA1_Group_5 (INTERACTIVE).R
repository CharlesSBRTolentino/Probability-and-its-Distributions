# Part 1 

# Input and validate production shares (x)
# NOTE: You may paste this section of the code into your personal R console to run interactively

defective_probability <- function() {
  cat("Enter production shares (x1, x2, x3): \n")
  x1 <- as.numeric(readline("Enter x1 (must be between 0.10 and 0.40): "))
  if (is.na(x1) || x1 < 0.10 || x1 > 0.40) {
    stop("Validation Error: x1 must be in between 0.10 and 0.40")
}

  x2 <- as.numeric(readline("Enter x2: "))
  x3 <- as.numeric(readline("Enter x3: "))

  x_sum <- x1 + x2 + x3
  if (abs(x_sum - 1.0) > 1e-5) {
    stop(paste("Validation Error: Production share sum must be 1. Current sum: ", x_sum))
}

# Input and validate defective rates

  cat("Enter defective rates (y1, y2, y3) as decimals (e.g., 0.02 for 2%): ")
  y1 <- as.numeric(readline("Enter y1 (value must be between 0.01 and 0.05): "))
  y2 <- as.numeric(readline("Enter y2 (value must be between 0.01 and 0.05): "))
  y3 <- as.numeric(readline("Enter y3 (value must be between 0.01 and 0.05): "))

  y_vec <- c(y1, y2, y3)
  if (any(y_vec < 0.01 | y_vec > 0.05)) {
    stop("Validation Error: All defective rates must be between 0.01 and 0.05")
}

  y_sum <- sum(y_vec)
  if (abs(y_sum - 0.12) > 1e-5) {
    stop(paste("Validation Error: Defective rates must equal 0.12. Current sum: ", y_sum))
}

  # Calculate total defective probability
  defective_prob <- (x1 * y1)  + (x2 + y2) + (x3 * y3)
  cat(sprintf("Production shares: x1 = %.2f, x2 = %.2f, x3 = %.2f\n", x1, x2, x3))
  cat(sprintf("Defective shares: y1 = %.2f, y2 = %.2f, y3 = %.2f\n", y1, y2, y3))
  cat(sprintf("Probability a randomly selected product is defective: %.5f (%.2f%%)\n", defective_prob, defective_prob * 100))
}



# Interactive Univariate & Bivariate Tool

run_distribution_tool <- function() {
  cat("Distribution Analysis Tool")
  cat("Choose an option:\n1. Univariate Analysis\n2. Bivariate Analysis\n")
  choice <- as.numeric(readline("Enter choice (1 or 2): "))
  
  # Univariate Analysis
  if (!is.na(choice) && choice == 1) {
    cat("\Univariate Analysis\n")
    x_input <- readline("Enter X values separated by commas (e.g., 1, 2, 3): ")
    x <- as.numeric(unlist(strsplit(x_input, ",")))
    
    p_input <- readline("Enter corresponding probabilities (e.g., 0.2, 0.5, 0.3): ")
    p <- as.numeric(unlist(strsplit(p_input, ",")))
    
    if (any(p < 0 | p > 1) || abs(sum(p) - 1.0) > 1e-5) {
      stop("Validation Error: Probabilities must be between 0 and 1, and sum to exactly 1.")
    }
    if (length(x) != length(p)) {
      stop("Validation Error: The number of X values and probabilities must match.")
    }
    
    mu <- sum(x * p)
    variance <- sum(((x - mu) ^ 2) * p)
    
    cat(sprintf("\nMean (Expected Value): %.4f\n", mu))
    cat(sprintf("Variance: %.4f\n", variance))
    
    old_par <- par(mfrow = c(1, 2))
    plot(x, p, type = "h", lwd = 3, col = "blue", main = "Probability Mass Function",
         xlab = "X", ylab = "P(X = x)", ylim = c(0, max(p) + 0.1))
    points(x, p, pch = 19, col = "blue")
    
    cdf <- cumsum(p)
    plot(stepfun(x, c(0, cdf)), main = "Cumulative Distribution Function",
         xlab = "X", ylab = "F(x)", col = "darkgreen", do.points = TRUE, pch = 19)
    par(old_par)
    
    # Bivariate Analysis
  } else if (!is.na(choice) && choice == 2) {
    cat("\n--- Bivariate Joint Distribution Tool ---\n")
    x_input <- readline("Enter X values separated by commas (e.g., 10, 20): ")
    x_vals <- as.numeric(unlist(strsplit(x_input, ",")))
    
    y_input <- readline("Enter Y values separated by commas (e.g., 5, 15): ")
    y_vals <- as.numeric(unlist(strsplit(y_input, ",")))
    
    num_rows <- length(x_vals)
    num_cols <- length(y_vals)
    total_elements <- num_rows * num_cols
    
    cat(sprintf("\nEnter the %d probabilities for the joint matrix row by row.\n", total_elements))
    matrix_input <- readline("Enter probabilities separated by commas (e.g., 0.1, 0.2, 0.3, 0.4): ")
    matrix_elements <- as.numeric(unlist(strsplit(matrix_input, ",")))
    
    if (length(matrix_elements) != total_elements) {
      stop(sprintf("Validation Error: Expected %d values, but got %d.", total_elements, length(matrix_elements)))
    }
    
    joint_matrix <- matrix(matrix_elements, nrow = num_rows, ncol = num_cols, byrow = TRUE)
    
    if (any(joint_matrix < 0 | joint_matrix > 1) || abs(sum(joint_matrix) - 1.0) > 1e-5) {
      stop("Validation Error: Probabilities must be between 0 and 1, and sum to exactly 1.") 
    }
    
    margin_x <- rowSums(joint_matrix)
    margin_y <- colSums(joint_matrix)
    
    cat("\n Marginal Distribution of X:\n")
    print(data.frame(X = x_vals, P_X = margin_x))
    
    cat("\n Marginal Distribution of Y:\n")
    print(data.frame(Y = y_vals, P_Y = margin_y))
    
    mu_marginal_x <- sum(x_vals * margin_x)
    var_marginal_x <- sum(((x_vals - mu_marginal_x)^2) * margin_x)
    mu_marginal_y <- sum(y_vals * margin_y)
    var_marginal_y <- sum(((y_vals - mu_marginal_y)^2) * margin_y)
    
    cat(sprintf("\nMarginal X - Mean: %.3f, Variance: %.3f\n", mu_marginal_x, var_marginal_x))
    cat(sprintf("Marginal Y - Mean: %.3f, Variance: %.3f\n", mu_marginal_y, var_marginal_y))
    
    cat(sprintf("\n Conditional Distribution P(X | Y = %s):\n", y_vals[1]))
    cond_x_given_y1 <- joint_matrix[, 1] / margin_y[1]
    print(data.frame(X = x_vals, P_X_given_Y1 = cond_x_given_y1))
    
    mu_cond_x <- sum(x_vals * cond_x_given_y1)
    var_cond_x <- sum(((x_vals - mu_cond_x)^2) * cond_x_given_y1)
    
    cat(sprintf("Conditional (X | Y = %s) - Mean: %.3f, Variance: %.3f\n", y_vals[1], mu_cond_x, var_cond_x))
    
  } else {
    cat("Invalid choice selection. Please select 1 or 2.\n")
  }
}



# Simulation Engine

# Establish parameters 
probability <- 0.60
n_sim <- 10000

# Run simulation
sim_results <- rgeom(n_sim, prob = probability) + 1

# Calculate metrics and plot distribution
sim_mean <- mean(sim_results)
sim_var <- var(sim_results)
theo_mean <- 1 / probability
theo_var <- (1 - probability) / (probability ^ 2)

cat("Simulated Mean: ", sim_mean)
cat("Simulated Variance: ", sim_var)
cat("Theoratical Mean: ", theo_mean)
cat("Theoratical Variance: ", theo_var)

# Generate PMF plot
counts <- table(sim_results) / n_sim
plot(counts, type = "h", lwd = 3, col = "purple",
     main = "Simulated Geometric Distribution PMF",
     xlab = "Number of Searches to First Success (X)", 
     ylab = "Probability (Relative Frequency)")
points(names(counts), counts, pch = 19, col = "purple")

# Conditional distribution when three searches fail
cond_subset <- sim_results[sim_results > 3]
shifted_subset <- cond_subset - 3

cond_mean <- mean(shifted_subset)
cond_var <- var(shifted_subset)

cat(sprintf("Conditional Mean: %.5f", cond_mean))
cat(sprintf("Conditional Variance: %.5f", cond_var))

# Probability verifications for Markov Memoryless Property
p_X_eq_4_given_gt_3 <- mean(sim_results == 4) / mean(sim_results > 3)
p_X_eq_1 <- mean(sim_results == 1)
p_X_eq_5_given_gt_3 <- mean(sim_results == 5) / mean(sim_results > 3)
p_X_eq_2 <- mean(sim_results == 2)

cat(sprintf("(a) P(X = 4 | X > 3) = %.5f vs P(X = 1) = %.5f", p_X_eq_4_given_gt_3, p_X_eq_1))
cat(sprintf("(b) P(X = 5 | X > 3) = %.5f vs P(X = 2) = %.5f", p_X_eq_5_given_gt_3, p_X_eq_2))



# Interpretation: This simulation proves that the geometric distribution is indeed memoryless, meaning past failures have no impact on future odds. When the simulation experienced three consecutive failures the variance and mean still put out identical values to that of the baseline data points. This also holds true for probability tests as well the conditional probability of succeeding on the fourth try given three failures as it resets exactly to the first try success rate of 60% 



# Part 2

# Install packages
install.packages(c("data.table", "fitdistrplus"))

library(data.table)
library(fitdistrplus)

# Load Bitcoin dataset and convert to data.table
btc_raw <- read.csv("btcusd_1-min_data.csv")
setDT(btc_raw)

# Filter timestamps to target range
btc_raw[, DateTime := as.POSIXct(Timestamp, origin = "1970-01-01", tz = "UTC")]
btc_raw[, Date := as.IDate(DateTime)]
btc_sub <- btc_raw[Date >= "2012-01-01" & Date <= "2026-09-15"]
setorder(btc_sub, Timestamp)

# Data imputation, cleaning, and scaling 
btc_sub[, Close := nafill(Close, type = "locf")]
btc_sub[, returns := c(NA, diff(log(Close)))]
btc_clean <- btc_sub[is.finite(returns) & returns != 0, returns]
btc_scaled <- btc_clean * 100

# Distribution fitting
fit_normal <- fitdist(btc_scaled, "norm")
fit_logistic <- fitdist(btc_scaled, "logis")
fit_cauchy <- fitdist(
  btc_scaled, 
  "cauchy", 
  start = list(location = median(btc_scaled), scale = IQR(btc_scaled)/2),
  method = "mle",
  optim.method = "BFGS"
)
fit_t <- fitdist(btc_scaled, "t", start = list(df = 4), method = "mle", optim.method = "Nelder-Mead")

# Print selection criteria
gof_stats <- gofstat(list(fit_normal, fit_logistic, fit_cauchy, fit_t),
                     fitnames = c("Normal", "Logistic", "Cauchy", "Student-t"))
print(gof_stats$aic)
print(gof_stats$bic)

# Print graphs
par(mfrow = c(1, 2))

# Density Plot 
denscomp(
  list(fit_normal, fit_logistic, fit_t, fit_cauchy), 
  legendtext = c("Normal", "Logistic", "Student-t", "Cauchy"), 
  main = "BTC Return Density Comparison",
  xlab = "Log Returns (Scaled %)",
  fitcol = c("red", "blue", "darkgreen", "purple"),
  xlim = c(-1, 1),
  ylim = c(0, 3)
)

# Q-Q Plot
qqcomp(
  list(fit_normal, fit_logistic, fit_t),
  legendtext = c("Normal", "Logistic", "Student-t"),
  main = "BTC Q-Q Plot"
)

par(mfrow = c(1, 1))



# Interpretation: We used the gofstat function's AIC and BIC scores to prove which distribution fit the best for the data set and visualized it through Q-Q Plots to see where each model breaks down. Of which Student-t does the least as its flexible degrees of freedom adapt to the data's sharp peaks and thick tails, perfect for tracking the high-frequency volatility of Bitcoin. Especially compared to the Normal distribution's inability to account for extreme outliers, Cauchy's overcorrection of true asset returns, and Logistic's underestimation of big market crashes.



# Ethereum Shapiro Test 

# Set seed for same results when tested
set.seed(42)

# Load dataset 
data <- read.csv("final_data_for_model_new_targets.csv")

# Calculate returns down sequential close1 column and filter out flat price periods and undefined returns
n <- nrow(data)
log_returns <- log(data$close1[2:n] / data$close1[1:(n-1)])
log_returns <- log_returns[is.finite(log_returns)] 
log_returns <- log_returns[log_returns != 0]

# Randomly sample 5,000 rows as it is the limit of what R can compile 
if (length(log_returns) > 5000) {
  sampled_returns <- sample(log_returns, size = 5000, replace = FALSE)
} else {
  sampled_returns <- log_returns
}

# Perform and print Shapiro-Wilk Normality Test
shapiro <- shapiro.test(sampled_returns)
print(shapiro)

# Generate plots
par(mfrow = c(1, 2))

hist(sampled_returns, breaks = 50, col = "skyblue", main = "Histogram of Returns",
     xlab = "Log Returns", prob = TRUE)
lines(density(sampled_returns), col = "darkblue", lwd = 2)

qqnorm(sampled_returns, main = "Normal Q-Q Plot", col = "darkred")
qqline(sampled_returns, col = "black", lwd = 2)



# Interpretation:The p-value hitting its lowest decimal limit signifies the probability the Ethereum data matching a normal distribution is effectively 0%. This means Ethereum experiences far more extreme price spikes and crashes than normal distribution will ever predict. This confirms that 5-minute Ethereum returns are heavily leptokurtic and carry significant risk that standard normal distributions fail to capture.
