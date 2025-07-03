# Load necessary packages
library(dplyr)
library(ggplot2)

# -----------------------------------------
# 1. Basic Probabilistic Events
# -----------------------------------------

# Define sample space and events
sample_space <- 1:6 # For a fair die roll
event_A <- c(1, 2, 3) # Event A: rolling 1, 2, or 3
event_B <- c(2, 4, 6) # Event B: rolling an even number

# Compute probabilities of events
P_A <- length(event_A) / length(sample_space) # Probability of A
P_B <- length(event_B) / length(sample_space) # Probability of B
P_A_and_B <- length(intersect(event_A, event_B)) / length(sample_space) # P(A and B)
P_A_or_B <- P_A + P_B - P_A_and_B # P(A or B)

# Conditional Probability P(A|B)
P_A_given_B <- P_A_and_B / P_B

# Complement of A
P_not_A <- 1 - P_A

cat("Probabilities: P(A) =", P_A, ", P(B) =", P_B, ", P(A and B) =", P_A_and_B, ", P(A or B) =", P_A_or_B, ", P(A|B) =", P_A_given_B, ", P(not A) =", P_not_A, "\n")


# -----------------------------------------
# 2. Nine Properties of Probabilities
# -----------------------------------------

# 1. Non-negativity: For any event E, P(E) ≥ 0
# Let's say we have a fair six-sided die. Each outcome has a non-negative probability.
sample_space <- 1:6 # Outcomes of a fair six-sided die roll
event_A <- c(1, 2)  # Event A is rolling a 1 or 2
P_A <- length(event_A) / length(sample_space)  # Probability of event A

# Check if P(A) is non-negative
cat("Non-negativity: P(A) =", P_A, "is non-negative (>= 0)\n")

# 2. Normalization: P(S) = 1, where S is the sample space
# The probability of the entire sample space should sum to 1
P_S <- sum(rep(1 / length(sample_space), length(sample_space)))
cat("Normalization: P(S) =", P_S, "is equal to 1\n")

# 3. Complementary Rule: P(A^c) = 1 - P(A), where A^c is the complement of A
# The probability of not rolling a 1 or 2
P_not_A <- 1 - P_A
cat("Complementary Rule: P(not A) =", P_not_A, "is equal to 1 - P(A)\n")

# 4. Additivity: For mutually exclusive events A and B, P(A ∪ B) = P(A) + P(B)
# Suppose event B is rolling a 3 or 4, and A and B are mutually exclusive
event_B <- c(3, 4)  # Event B
P_B <- length(event_B) / length(sample_space)  # Probability of event B
P_A_or_B <- P_A + P_B  # Since A and B are mutually exclusive, P(A ∪ B) = P(A) + P(B)
cat("Additivity: P(A or B) =", P_A_or_B, "is equal to P(A) + P(B) for mutually exclusive A and B\n")

# 5. Conditional Probability: P(A|B) = P(A ∩ B) / P(B), if P(B) > 0
# Suppose event C is rolling an even number (2, 4, or 6)
event_C <- c(2, 4, 6)
P_C <- length(event_C) / length(sample_space)
P_A_and_C <- length(intersect(event_A, event_C)) / length(sample_space)  # P(A ∩ C)



# -----------------------------------------
# 3. Discrete and Continuous Random Variables
# -----------------------------------------

# Discrete Random Variable (X = # heads in two coin flips)
X <- c(0, 1, 2) # Possible outcomes: 0 heads, 1 head, 2 heads
P_X <- c(0.25, 0.5, 0.25) # Probabilities of X

# Continuous Random Variable (Normal distribution)
continuous_rv <- rnorm(1000, mean = 0, sd = 1)

# Plot continuous distribution
hist(continuous_rv, probability = TRUE, main = "Continuous Random Variable (Normal)", col = "lightblue")


# -----------------------------------------
# 4. Probability Mass Function (PMF)
# -----------------------------------------

# PMF for discrete variable X
pmf_X <- data.frame(X, P_X)
print(pmf_X)

# Plot PMF
ggplot(pmf_X, aes(x = X, y = P_X)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  ggtitle("PMF of Discrete Random Variable X")


# -----------------------------------------
# 5. Probability Density Function (PDF)
# -----------------------------------------

# PDF for continuous random variable (Normal distribution)
x_vals <- seq(-4, 4, length.out = 100)
pdf_vals <- dnorm(x_vals, mean = 0, sd = 1)
plot(x_vals, pdf_vals, type = "l", main = "PDF of Standard Normal Distribution", col = "blue")


# -----------------------------------------
# 6. Cumulative Distribution Function (CDF)
# -----------------------------------------

# CDF for discrete X
cdf_X <- cumsum(P_X)
print(data.frame(X = X, CDF = cdf_X))

# CDF for continuous variable (Normal distribution)
cdf_vals <- pnorm(x_vals, mean = 0, sd = 1)
plot(x_vals, cdf_vals, type = "l", main = "CDF of Standard Normal Distribution", col = "red")


# -----------------------------------------
# 7. Expected Value, Variance, and Standard Deviation
# -----------------------------------------

# Discrete Expected Value (E[X])
expected_X <- sum(X * P_X)
cat("Expected value of discrete X:", expected_X, "\n")

# Discrete Variance and Standard Deviation
var_X <- sum((X - expected_X)^2 * P_X)
sd_X <- sqrt(var_X)
cat("Variance of discrete X:", var_X, ", SD of discrete X:", sd_X, "\n")

# Continuous Expected Value, Variance, and Standard Deviation (Normal distribution)
cat("Expected value of continuous (Normal):", mean(continuous_rv), "\n")
cat("Variance of continuous (Normal):", var(continuous_rv), "\n")


# -----------------------------------------
# 8. Discrete Distributions: Binomial, Poisson, Geometric
# -----------------------------------------

# Binomial Distribution (e.g., X ~ Binomial(n=10, p=0.5))
binom_dist <- dbinom(0:10, size = 10, prob = 0.5)
plot(0:10, binom_dist, type = "h", main = "Binomial Distribution (n=10, p=0.5)", col = "blue")

# Poisson Distribution (e.g., X ~ Poisson(lambda=3))
pois_dist <- dpois(0:10, lambda = 3)
plot(0:10, pois_dist, type = "h", main = "Poisson Distribution (lambda=3)", col = "green")

# Geometric Distribution (e.g., X ~ Geometric(p=0.5))
geom_dist <- dgeom(0:10, prob = 0.5)
plot(0:10, geom_dist, type = "h", main = "Geometric Distribution (p=0.5)", col = "purple")


# -----------------------------------------
# 9. Continuous Distributions: Normal, Exponential, Uniform
# -----------------------------------------

# Normal Distribution (X ~ Normal(0,1))
curve(dnorm(x, mean = 0, sd = 1), -4, 4, main = "Normal Distribution (0, 1)", col = "blue")

# Exponential Distribution (X ~ Exponential(rate=1))
curve(dexp(x, rate = 1), 0, 4, main = "Exponential Distribution (rate=1)", col = "red")

# Uniform Distribution (X ~ Uniform(0, 1))
curve(dunif(x, min = 0, max = 1), 0, 1, main = "Uniform Distribution (0,1)", col = "green")


# -----------------------------------------
# 10. Joint, Marginal, and Conditional Distributions
# -----------------------------------------

# Joint distribution example with two discrete variables
joint_probs <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
rownames(joint_probs) <- c("X=0", "X=1")
colnames(joint_probs) <- c("Y=0", "Y=1")

# Marginal distributions
marginal_X <- rowSums(joint_probs)
marginal_Y <- colSums(joint_probs)

# Conditional distribution P(Y=1 | X=1)
P_Y_given_X1 <- joint_probs[2, 2] / marginal_X[2]


# -----------------------------------------
# 11. Moment Generating Function (MGF)
# -----------------------------------------

# MGF of Binomial distribution with n=10 and p=0.5
binom_mgf <- function(t) (0.5 * exp(t) + 0.5)^10
curve(binom_mgf(x), -1, 1, main = "MGF of Binomial(n=10, p=0.5)", col = "orange")


# -----------------------------------------
# 12. Law of Large Numbers
# -----------------------------------------

# Demonstrate LLN with coin flips
set.seed(123)
n_flips <- 10000
flips <- sample(c(0, 1), size = n_flips, replace = TRUE)
cum_avg <- cumsum(flips) / (1:n_flips)
plot(cum_avg, type = "l", main = "Law of Large Numbers", xlab = "Number of Flips", ylab = "Cumulative Average")


# -----------------------------------------
# 13. Central Limit Theorem
# -----------------------------------------

# Simulate sample means from Uniform distribution
sample_means <- replicate(1000, mean(runif(30)))
hist(sample_means, probability = TRUE, main = "Central Limit Theorem", col = "lightgreen")


# -----------------------------------------
# 14. Statistical Inference (Confidence Interval for Mean)
# -----------------------------------------

# CI for mean of Normal distribution
sample_data <- rnorm(30, mean = 5, sd = 2)
ci <- t.test(sample_data)$conf.int
cat("95% Confidence Interval for mean:", ci, "\n")


# -----------------------------------------
# 15. Bayes Theorem
# -----------------------------------------

# Define prior, likelihood, and posterior
prior <- 0.01
likelihood <- 0.95
false_positive <- 0.05

# Compute posterior probability P(Disease | Positive Test)
posterior <- (likelihood * prior) / ((likelihood * prior) + (false_positive * (1 - prior)))
cat("Posterior Probability:", posterior, "\n")

