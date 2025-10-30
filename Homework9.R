##########################
# Group members: Keeley Kuru,
# Date: 10/30/25

# ===== Objective 1 ===== #

# 1A.

# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Set seed for reproducibility
set.seed(123)

# Constants
alpha <- 5
beta <- 8
n <- 100

# Generate x values
x <- runif(n, min = 0, max = 10)

# Create a function to generate y with a given sigma
generate_y <- function(sigma) {
  y <- alpha + beta*x + rnorm(n, mean = 0, sd = sigma)
  return(y)
}

# Generate y for three different sigma values
sigma_values <- c(1, 10, 25)

data <- data.frame(
  x = rep(x, times = length(sigma_values)),
  sigma = factor(rep(sigma_values, each = n)),
  y = unlist(lapply(sigma_values, generate_y))
)

head(data)

# 1B

ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ sigma, nrow = 1) +
  labs(title = "Effect of Increasing Observation Error on Linear Regression",
       x = "Predictor (x)",
       y = "Response (y)",
       subtitle = "Panels show σ = 1, 10, and 25") +
  theme_minimal()


# ===== Objective 2 ===== #

# 2A.

# Load library
library(ggplot2)

set.seed(123)

# Parameters
n_sims <- 100          # number of simulation repeats
p_values <- c(0.55, 0.6, 0.65)  # different degrees of unfairness
n_flips <- 1:20        # range of flips

# Function to simulate coin flips and test significance
simulate_coin <- function(n, p, n_sims = 100, alpha = 0.05) {
  significant_counts <- numeric(length(n))
  
  for (i in seq_along(n)) {
    sig <- 0
    for (sim in 1:n_sims) {
      flips <- rbinom(n[i], size = 1, prob = p)
      test <- prop.test(sum(flips), n[i], p = 0.5, alternative = "greater")
      if (test$p.value < alpha) {
        sig <- sig + 1
      }
    }
    significant_counts[i] <- sig / n_sims
  }
  
  return(data.frame(n_flips = n, power = significant_counts, p = p))
}

# Run simulation for all p values and combine
results <- do.call(rbind, lapply(p_values, function(p) simulate_coin(n_flips, p)))

head(results)

# 2B.

ggplot(results, aes(x = n_flips, y = power, color = factor(p))) +
  geom_line(linewidth = 1.2) +   
  geom_point(size = 2) +         
  labs(title = "Power to Detect an Unfair Coin",
       x = "Number of Coin Flips",
       y = "Probability of Significant Test (α < 0.05)",
       color = "p(heads)") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()


