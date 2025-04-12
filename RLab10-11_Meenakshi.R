#1:
#sampling with and without replacement:

#a
X <- c(1,2,3,4,5,6,7,8,9)
print(sample(X, 4, replace=TRUE))
print(sample(X, 4, replace=FALSE))

#b

install.packages("gtools")
library(gtools)

x <- c("A","B","C","D")
n <- length(x)
p <- permutations(n,2,x,repeats.allowed = TRUE)
print(p)

c <- combinations(n,2,x)
print(c)

#2:
#distributions:

#binomial dist
#a
#‘d’<distn name> : probability density value is returned
n <- 10
p <- 0.4
q <- 1-p
m <- 3
pdf_bin <- dbinom(m,n,p)
print(pdf_bin)  #ans: 0.215

#verifying:
c <- choose(n,m)  #returns nCr
pr <- (p**m)*(q)**(n-m)

pdf_bin_manual <- c*pr
print(pdf_bin_manual) #ans: 0.215

#b
cpdf_bin <- pbinom(m,n,p)
print(cpdf_bin)  #ans: 0.38

#c
#‘q’<distn name> : returns the x value up to which the input cumulative probability represents
upper_lim <- qbinom(cpdf_bin,n,p)
print(upper_lim) #first verifying with p=0.4 done before.

cpdf2_bin <- pbinom(m,n,0.8)
print(cpdf2_bin)
upper_lim_0.8 <- qbinom(cpdf2_bin,n,0.8)
print(upper_lim_0.8) #ans: 3 itself

#d
five_rand <- rbinom(5,n,p)
print(five_rand)


#e
new_m <-0:n
new_pdf_bin1 <- dbinom(new_m,n,p)
new_pdf_bin2 <- dbinom(new_m,n,0.7)
plot(new_m, new_pdf_bin1,col='green',xlab='Successes',ylab='PMF',type = "b")
lines(new_m, new_pdf_bin2, type = "b", col = "brown", lwd = 2)


#f
sample_100 <- rbinom(100, n, p)    # 100 samples
sample_10000 <- rbinom(10000, n, p) # 10,000 samples

freq_100 <- table(sample_100)
freq_10000 <- table(sample_10000)

par(mfrow = c(2, 1)) 

# Bar plot for 100 samples
barplot(freq_100, col = "blue",
        xlab = "Number of Successes", ylab = "Frequency",
        main = "Binomial Distribution (100 Samples)")

# Bar plot for 10,000 samples
barplot(freq_10000, col = "red",
        xlab = "Number of Successes", ylab = "Frequency",
        main = "Binomial Distribution (10,000 Samples)")

#10,000 samples graph looks more like a bell-curve because as sample size increases it approaches normal distribution.

#hypergeometric dist
#a
N <- 100
K <- 70
n <- 12
k <- 0:n

probs <- dhyper(k,K,N-K,n)
barplot(probs, names.arg = k, col = "pink",
        xlab = "Number of Successes", ylab = "PMF",
        main = "Histogram of Hypergeometric Distribution",space=0)

#b
cum_prob_10 <- round(phyper(10, K, N - K, n), 3)
print(cum_prob_10)  # Prints the cumulative probability up to x = 10

#c
x_90 <- qhyper(0.9, K, N - K, n)
print(x_90)  # The smallest x where P(X ≤ x) ≥ 0.9

#d
sample_points <- rhyper(5, K, N - K, n)
formatted_samples <- formatC(sample_points, format = "fg", digits = 2)
print(formatted_samples)

#geometric distribution:
p1 <- 0.3
p2 <- 0.8

n <- 0:10

probs1 <- dgeom(n,p1)
probs2 <- dgeom(n,p2)


par(mfrow = c(1, 2))

plot(n, probs1, type = "l", col = "skyblue", lwd = 3,
     xlab = "Trial (m)", ylab = "PMF",
     main = "Geometric PMF (p=0.3)")
plot(n, probs2, type = "l", col = "purple", lwd = 3,
     xlab = "Trial (m)", ylab = "PMF",
     main = "Geometric PMF (p=0.8)")

#p = 0.3: The probabilities decrease more gradually because failures are more common.

#p = 0.8: The probabilities drop sharply since successes are much more frequent.

#Why Does This Happen?
# For small p (e.g., 0.3):
#   
#   Failures are more common, so the probability of the first success happening later is still significant.
# 
# For large p (e.g., 0.8):
#   
#   Success happens quickly, so the probability of needing many trials drops fast.

#negative binom
y <- 5
r <- 3
p <- 0.3

#a
prob_density <- dnbinom(y, r, p)
print(prob_density)

#b
cum_prob <- pnbinom(y, r,p)
print(cum_prob)

#c
median_y <- qnbinom(0.5, r, p)
print(median_y)

#d
random_samples <- rnbinom(4, r, p)
print(random_samples)


#e
r <- 10
p <- 0.3
y_vals <- 0:30  # Range of values for y
probabilities <- dnbinom(y_vals, r, p)

plot(y_vals, probabilities, type = "h", col = "blue",
     xlab = "Number of Failures (y)", ylab = "PMF",
     main = "Negative Binomial Distribution (r=10, p=0.3)")


#f
samples <- rnbinom(10000, size = r, prob = p)

hist(samples, breaks = 30, col = "lightblue", border = "black",
     main = "Histogram of Negative Binomial Samples",
     xlab = "Number of Failures (y)", probability = TRUE)


#Poisson
#a
lambda <- 10
m <- 7
poi_pmf <- dpois(m,lambda)

#b
poi_cpmf <- ppois(poi_pmf,lambda)
print(poi_cpmf)

#c
# Parameters
n <- 10000  # Number of trials
p <- 0.03   # Probability of success
lambda2 <- n * p  # Poisson parameter

# Binomial Distribution
x_binom <- 0:n
y_binom <- dbinom(x_binom, size = n, prob = p)

# Poisson Distribution
x_pois <- 0:n
y_pois <- dpois(x_pois, lambda2)

# Plot Binomial Distribution
par(mfrow = c(1, 2))  # Arrange plots in one row, two columns
plot(x_binom,y_binom, names.arg = x_binom, col = "black", border = NA,type="h",
        main = "Binomial Distribution", xlab = "k", ylab = "P(X=k)",xlim=c(200,400))

# Plot Poisson Distribution
lines(x_pois,y_pois, names.arg = x_pois, col = "violet", border = NA, type="h",
        main = paste("Poisson Distribution (λ=", lambda2, ")"), xlab = "k", ylab = "P(X=k)")


#d
quantile_val <- qpois(0.22,10)
print(quantile_val)

#e
random_samples <- rpois(10000,9)
hist(random_samples, breaks = 25, col = "coral", border = "black",
     main = "Histogram of Poisson Samples",
     xlab = "Number of Events", probability = TRUE)

#gaussian
#a
mu <- 12
sd <- 2
gaus_pdf <- dnorm(12,mu,sd)
print(gaus_pdf)

#b
Z <- 2.0
cumulative_prob <- pnorm(Z, mean = 0, sd = 1)  # Standard normal CDF
cumulative_prob_neg <- pnorm(-Z, mean = 0, sd = 1)
print(paste(cumulative_prob,(1-cumulative_prob_neg))) #they are equal

#c
x_vals <- seq(mu - 4 * sd, mu + 4 * sd, length.out = 100)
y_vals <- dnorm(x_vals, mu,sd)

plot(x_vals, y_vals, type = "l", col = "blue", lwd = 2, main = "Unit Normal Curve",
     xlab = "X", ylab = "Density")

text(mu, max(y_vals) * 0.8, labels = "Mean = 12, SD = 2", cex = 1, col = "black")

#d
quantile_75 <- qnorm(0.75, mu, sd)
print(quantile_75)

#e

normal_samples <- rnorm(10000, mean = 0, sd = 1)
hist(normal_samples, breaks = 50, probability = TRUE, col = "lightblue", border = "black",
     main = "Histogram of Standard Normal Samples", xlab = "Value", ylab = "Density")
lines(x_vals, y_vals, col = "red", lwd = 2)

#f
# Parameters
n <- 20
p <- 0.5
mu <- n * p
sigma <- sqrt(n * p * (1 - p))

# Generate Binomial Samples
m <- rbinom(10000, size = n, prob = p)

# Compute W = (m - np) / sqrt(np(1 - p))
W <- (m - mu) / sigma

# Plot Histogram
hist(W, breaks = 25, probability = TRUE, col = "yellow", border = "black",
     main = "Normalised Binomial Distribution", xlab = "W", ylab = "Density")

# Overlay Unit Normal Distribution
x_vals <- seq(-4, 4, length.out = 100)
y_vals <- dnorm(x_vals, mean = 0, sd = 1)
lines(x_vals, y_vals, col = "red", lwd = 2)

#g
# Poisson parameters
lambda_vals <- c(1, 10, 100, 1000)

par(mfrow = c(2, 2))

for (lambda in lambda_vals) {

  x_pois <- 0:(lambda * 3)
  y_pois <- dpois(x_pois, lambda)
  
  x_norm <- seq(min(x_pois), max(x_pois), length.out = 100)
  y_norm <- dnorm(x_norm, mean = lambda, sd = sqrt(lambda))
  
  plot(x_pois, y_pois, type = "h", col = "blue",
       main = paste("Poisson PDF (λ =", lambda, ")"),
       xlab = "m", ylab = "P(X=m)", ylim = c(0, max(y_pois, y_norm)))
  
  lines(x_norm, y_norm, col = "red", lwd = 2)
}

par(mfrow = c(1, 1))

# (h) Correlated Normal Distributions using MASS
library(MASS)
tolerance=0.5
xy <- mvrnorm(1000, mu=c(50,60), Sigma=matrix(c(4,tolerance,tolerance,9),2,2))
print("Variance-Covariance Matrix:")
print(var(xy))

# Extract x and y
x <- xy[,1]
y <- xy[,2]

# Scatter plot of x and y
plot(x, y, main="Scatter Plot of Correlated Variables", xlab="X", ylab="Y", col="blue")
print(paste("Var(X) =", var(x)))
print(paste("Var(Y) =", var(y)))

# Check independence: sum of variances vs variance of sum
var_sum <- var(x) + var(y)
var_combined <- var(x + y)
print(paste("Sum of Individual Variances =", round(var_sum, 4)))
print(paste("Variance of (X+Y) =", round(var_combined, 4)))
print(paste("Are they independent?", round(var_sum, 4) == round(var_combined, 4)))

# Compute covariance using correlation coefficient
cov_computed <- cor(x, y) * sqrt(var(x) * var(y))
cov_reported <- var(xy)[1,2]

print(paste("Computed Covariance =", round(cov_computed, 4)))
print(paste("Reported Covariance =", round(cov_reported, 4)))
print(paste("Do they match?", round(cov_computed, 4) == round(cov_reported, 4)))


# Ex 7 - uniform distributions

# (a) Generate 5 uniform random numbers between 0 and 1:
set.seed(123)  # For reproducibility
runif(5)

# (b) Generate 5 random samples from a uniform distribution between 50 and 100:
runif(5, min = 50, max = 100)

# (c) Generate 10,000 uniform deviates and plot a histogram with x-limits 1 and 2:
samples <- runif(10000)  # Uniform [0, 1]

# Plot histogram with x-limits from 1 to 2
hist(samples, breaks = 50, col = "skyblue", xlab = "Value", main = "Histogram of Uniform(0,1) Samples", xlim = c(0, 1))


# Ex 8

# (a) What is the probability density corresponding to x = 3 and λ = 2?
x <- 3
lambda <- 2
density <- dexp(x, rate = lambda)
print(paste("Density at x = 3 and λ = 2:", density))

# (b) What is the quantile value corresponding to cumulative probability 0.995?
p <- 0.995
quantile_val <- qexp(p, rate = lambda)
print(paste("Quantile at p = 0.995 and λ = 2:", quantile_val))

# (c) Plot the exponential CDFs for λ = 2, 10, 100 on the same graph:
x_vals <- seq(0, 2, length.out = 500)
plot(x_vals, pexp(x_vals, rate = 2), type = "l", col = "blue", lwd = 2,
     ylab = "Cumulative Probability", xlab = "x", main = "Exponential CDFs")
lines(x_vals, pexp(x_vals, rate = 10), col = "red", lwd = 2)
lines(x_vals, pexp(x_vals, rate = 100), col = "green", lwd = 2)
legend("bottomright", legend = c("λ = 2", "λ = 10", "λ = 100"),
       col = c("blue", "red", "green"), lwd = 2)

# (d) Compute and print 4 random deviates from exponential distribution with λ = 3:
set.seed(42)  # Optional, for reproducibility
random_vals <- rexp(4, rate = 3)
print("4 Random Exponential Deviates (λ = 3):")
print(random_vals)


# Ex 9 - Gamma distribution

# Set up 1x2 plotting grid
par(mfrow = c(1, 2))

# (a) Plot PDFs for alpha = 1, 2, 3, 4 with theta = 4
x_vals <- seq(0, 50, length.out = 1000)
theta <- 4
colors <- c("black", "blue", "red", "magenta")

plot(x_vals, dgamma(x_vals, shape = 1, scale = theta), type = "l", col = colors[1],
     main = "Varying α (θ = 4)", xlab = "x", ylab = "Density", ylim = c(0, 0.15))

for (a in 2:4) {
  lines(x_vals, dgamma(x_vals, shape = a, scale = theta), col = colors[a])
}

legend("topright", legend = paste("α =", 1:4), col = colors, lwd = 2,inset = 0.05)

# (a) Plot PDFs for theta = 1, 2, 3, 4 with alpha = 4
alpha <- 4
plot(x_vals, dgamma(x_vals, shape = alpha, scale = 1), type = "l", col = colors[1],
     main = "Varying θ (α = 4)", xlab = "x", ylab = "Density", ylim = c(0, 0.4))

for (t in 2:4) {
  lines(x_vals, dgamma(x_vals, shape = alpha, scale = t), col = colors[t])
}

legend("topright", legend = paste("θ =", 1:4), col = colors, lwd = 2)


# The Gamma distribution models the waiting time for α events to occur in a Poisson process.
# α (shape parameter):
# Controls how many events are required before stopping.
# More events = More spread out distribution.
# θ (scale parameter):
# Controls the time per event.
# Larger θ = Slower process = More spread out distribution.

# Reset layout
par(mfrow = c(1, 1))

# (b) Probability density at x = 6, α = 4, θ = 1
density_val <- dgamma(6, shape = 4, scale = 1)
print(paste("PDF at x = 6, α = 4, θ = 1:", round(density_val, 5)))

# (c) Cumulative probability up to x = 6
cum_prob <- pgamma(6, shape = 4, scale = 1)
print(paste("Cumulative probability up to x = 6:", round(cum_prob, 5)))

# (d) Quantile value for 0.95 cumulative probability
quantile_val <- qgamma(0.95, shape = 4, scale = 1)
print(paste("x value at 0.95 cumulative probability:", round(quantile_val, 5)))

# (e) 10,000 random deviates and histogram
set.seed(42)
gamma_sample <- rgamma(10000, shape = 4, scale = 1)
hist(gamma_sample, breaks = 30, main = "Histogram of Gamma(α=4, θ=1)", xlab = "Value", col = "skyblue", border = "white")


# Ex 10 - Chi square distribution 

# (a) Plot χ² distribution for df = 2, 3, 5, 10
x_vals <- seq(0, 30, length.out = 1000)
df_vals <- c(2, 3, 5, 10)
colors <- c("black", "blue", "red", "magenta")

plot(x_vals, dchisq(x_vals, df = 2), type = "l", col = colors[1],
     main = "Chi-square PDFs", xlab = "x", ylab = "Density", ylim = c(0, 0.3))

for (i in 2:4) {
  lines(x_vals, dchisq(x_vals, df = df_vals[i]), col = colors[i])
}

legend("topright", legend = paste("df =", df_vals), col = colors, lwd = 2)

# (b) Probability density at x = 6, df = 5
pdf_val <- dchisq(6, df = 5)
print(paste("PDF at x = 6, df = 5:", round(pdf_val, 5)))

# (c) Cumulative probability up to x = 6, df = 10
cum_val <- pchisq(6, df = 10)
print(paste("Cumulative probability up to x = 6, df = 10:", round(cum_val, 5)))

# (d) 85th quantile for df = 6
quantile_85 <- qchisq(0.85, df = 6)
print(paste("85th percentile for df = 6:", round(quantile_85, 5)))

# (e) Histogram of 10,000 random deviates, df = 6
set.seed(42)
chisq_sample <- rchisq(10000, df = 6)
hist(chisq_sample, breaks = 30, col = "red", border = "white", 
     main = "Chi-square r = 6", xlab = "Value")
text(20, 1000, "r = 6", col = "black", cex = 1.5)

# (f) Z² = ((x - μ)^2) / σ² and χ²(1) overlay
mu <- 2
sigma <- 1
x_vals <- seq(-5, 10, length.out = 1000)
z_sq_vals <- ((x_vals - mu)^2) / sigma^2
chisq_pdf <- dchisq(z_sq_vals, df = 1)

plot(z_sq_vals, chisq_pdf, type = "l", col = "darkgreen", lwd = 2,
     main = expression(paste("Chi-square PDF with ", df == 1)),
     xlab = expression(Z^2), ylab = "Density")


# Ex 11 - CLT
# Set seed for reproducibility
set.seed(42)

# (1) CLT with Uniform Distribution
# ----------------------------------

# Step (i): Generate 10,000 samples of size 5 from Uniform(0,10)
sample_means <- replicate(10000, mean(runif(5, min=0, max=10)))

# Step (ii): Plot histogram of sample means
hist(sample_means, breaks=30, prob=TRUE, col="lightblue", main="Histogram of Sample Means (Uniform Distribution)", xlab="Sample Mean", border="black")

# Compute mean and standard deviation of sample means
mean_sample <- mean(sample_means)
sd_sample <- sd(sample_means)
print(paste("Mean of sample means:", mean_sample))
print(paste("Standard deviation of sample means:", sd_sample))

# Step (iii): Generate normal PDF with calculated mean and SD
x_seq <- seq(0, 10, by=0.1)
normal_pdf <- dnorm(x_seq, mean=mean_sample, sd=sd_sample)

# Step (iv): Scale normal PDF to match histogram height
scaling_factor <- 10000 * 0.5  # Adjusted for histogram bin width
scaled_pdf <- normal_pdf * scaling_factor

# Step (v): Overlay normal curve
lines(x_seq, scaled_pdf, col="red", lwd=2)


# (2) CLT with Dice Rolls
# ------------------------

# Step (i): Single dice throw (10,000 rolls)
a <- sample(1:6, size=10000, replace=TRUE)
hist(a, breaks=6, prob=TRUE, col="lightblue", main="Single Dice Roll", xlab="Dice Value", border="black")

# Step (ii): Two dice (sum of two rolls)
b <- sample(1:6, size=10000, replace=TRUE) + sample(1:6, size=10000, replace=TRUE)
hist(b, breaks=11, prob=TRUE, col="lightblue", main="Sum of Two Dice Rolls", xlab="Sum", border="black")

# Step (iii): Three dice (sum of three rolls)
c <- sample(1:6, size=10000, replace=TRUE) + sample(1:6, size=10000, replace=TRUE) + sample(1:6, size=10000, replace=TRUE)
hist(c, breaks=16, prob=TRUE, col="lightblue", main="Sum of Three Dice Rolls", xlab="Sum", border="black")

# Step (iv): Five dice (sum of five rolls)
d <- sample(1:6, size=10000, replace=TRUE) + sample(1:6, size=10000, replace=TRUE) +
  sample(1:6, size=10000, replace=TRUE) + sample(1:6, size=10000, replace=TRUE) +
  sample(1:6, size=10000, replace=TRUE)

# Compute mean and standard deviation for normal curve overlay
mean_d <- mean(d)
sd_d <- sd(d)

# Generate normal PDF with calculated mean and SD
x_vals <- seq(min(d), max(d), length=100)
normal_curve <- dnorm(x_vals, mean=mean_d, sd=sd_d)

# Scale the normal PDF
scaled_curve <- normal_curve * 10000 * 0.5  # Adjusting for histogram scaling

# Plot histogram with normal curve overlay
hist(d, breaks=30, prob=TRUE, col="lightblue", main="Sum of Five Dice Rolls", xlab="Sum", border="black")
lines(x_vals, scaled_curve, col="red", lwd=2)


# Ex 12 - ROC

# Load required library
library(pROC)

# Read white wine dataset
wine_data <- read.csv("/home/ibab/R/Lab10/winequality-white.csv", sep = ";")

# Define threshold values and assign different colors
thresholds <- c(6, 7, 8, 9, 10)
colors <- c("black", "blue", "red", "magenta", "darkgreen")

# Initialize an empty list to store ROC curves
roc_list <- list()

# Create ROC curves for each threshold
for (i in 1:length(thresholds)) {
  t <- thresholds[i]
  
  # Convert quality score to binary: Good (1) if quality >= threshold
  wine_data$quality_binary <- ifelse(wine_data$quality >= t, 1, 0)
  
  # Calculate ROC curve using alcohol as predictor
  roc_list[[i]] <- roc(wine_data$quality_binary, wine_data$alcohol)
}

# Plot the first ROC curve to start the plot
plot.roc(roc_list[[1]], col = colors[1], 
         main = "ROC Curves for Various Quality Thresholds",
         legacy.axes = TRUE, ci = FALSE, print.auc = FALSE)

# Add the rest of the ROC curves
for (i in 2:length(roc_list)) {
  plot.roc(roc_list[[i]], col = colors[i], add = TRUE)
}

# Add legend
legend("bottomright", legend = paste("Threshold =", thresholds),
       col = colors, lwd = 2)












