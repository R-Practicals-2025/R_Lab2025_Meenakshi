xval <- seq(1,10,0.5)
yval <- 30*xval/(2+xval)
plot(xval,yval,pch=20,cex=2,col="pink")   #pch displays the point characters (shape)

#cex parameter for changing size of data point
#colors of the data points: col
#type parameter - for joining data points
#type="b" plots points and lines

#linetype and linewidth lty and lwd
#plot title and properties - col.main, font.main
#text(xcomp,ycomp,text,colours etc")

par(mfrow=c(1,2)) #1 row and 2 columns
x=seq(-pi,pi,0.1)
y=sin(x)
plot(x,y,type="l")
plot(x,y,pch=c(4,5,6),col=c('red','blue','violet','green'))

#legends
#abline - adding lines to the graph


#LAB9
#1
x <- 2
y <- 4
plot(x,y,pch=0,col='magenta')

#2
x=seq(-pi,pi,0.1)
y=sin(x)
z=cos(x)
plot(x,y,col='blue',pch=8,type="b",col.main="pink",font.main=1,main="Second question",xlab="-pi to pi",ylab="sin(x) and cos(x)")
lines(x,z,col="red",pch=4,type="b")

#3
# Define the number of programs and their frequencies
programs <- c(1, 2, 3, 4, 5, 6, 7, 8)
frequency <- c(62, 47, 39, 39, 58, 37, 4, 11)

# Convert to probability
total_families <- sum(frequency)
probability <- frequency / total_families

# Create the bar plot
bp <- barplot(probability, names.arg = programs, col = "gray90", border = "black",
              xlab = expression(italic(x) ~ "(number of assistance programs)"),
              ylab = "Probability",
              main = "", # No title in image
              ylim = c(0, 0.25), space = 0.5, axes=FALSE)

# Add custom X-axis with inward ticks
axis(1, at = bp, labels = programs, line = 0, col = "black", tck = 0.02)  # tck > 0 moves ticks inward

# Add Y-axis with inward ticks
axis(2, tck = 0.02)

abline(h=-0.0002,col="black",lty=1,lwd=1.8)

## Add grid lines
#grid(nx = NA, ny = NULL, lty = "dotted", col = "darkgray")

## Add frequency values above bars
#text(x = bp, y = frequency+2, labels = frequency, cex = 0.8)


# Exercise 4. Make a 2x3 grid of 6 graphs with the following specifications and their respective titles (keep the title short!)**
par(mfrow = c(2, 3))  # Set up a 2x3 grid for plots

# (i) x vs cos(x) with red color and lines**
x = seq(-pi, pi, 0.1)
y = cos(x)
plot(x, y, xlab = "x(pi values)", ylab = "cos(x)", type = "l", col = "red", main = "Cosine Function")

# (ii) x vs (x^2/3) + 4.2 with violet color, points and lines, linewidth 2 and linetype 1**
y = (x^2 / 3) + 4.2
plot(x, y, ylab = "(x^2/3) + 4.2", xlab = "x(pi values)", col = "violet", type = "b", lwd = 2, lty = 1, main = "Quadratic Function")

# Define a function to calculate binomial coefficients
binom_co <- function(n, m) {
  return(factorial(n) / (factorial(m) * factorial(n - m)))
}

# Define a function to calculate binomial distribution probabilities
bionom_dist <- function(n, p, m) {
  return(binom_co(n, m) * (p^m) * ((1 - p)^(n - m)))
}

# (iii) Histogram plot of binomial distribution for 12 trials and p=0.3**
n = 12
p = 0.3
m = 0:n
probabilities1 = sapply(m, function(m) bionom_dist(n, p, m))

# Generate a histogram by repeating each outcome according to its probability
hist(rep(m, times = round(probabilities1 * 1000)), 
     xlab = "Successes", 
     ylab = "Frequency", 
     main = "Binomial Distribution (n=12, p=0.3)")

# (iv) Histogram plot of binomial distribution for 12 trials and p=0.8**
p = 0.8
probabilities2 = sapply(m, function(m) bionom_dist(n, p, m))
hist(rep(m, times = round(probabilities2 * 1000)),
     xlab = "Successes",
     ylab = "Frequency",
     main = "Binomial Distribution (n=12, p=0.8)")

# (v) Histogram plot using type='h' option in plot() function for x sequence of 1 to 10 with 0.5 spacing and y function 50x/(x + 2) with colors blue and orange alternating between the points**
x = seq(1, 10, 0.5)
y = 50 * x / (x + 2)
plot(x, y, type = "h", col = c("blue", "orange"), lwd = 10, lend = 1, xlab = "x", ylab = "50*x/(x + 2)", main = "Histogram with Alternating Colors")

# (vi) x vs log(x) with orange color and 'step' linetype**
x = seq(1, 10, 0.5)
y = log(x)
plot(x, y, col = "orange", type = "s", xlab = "x", ylab = "log(x)", main = "Step Plot of Logarithm")

# Exercise 5. Write a script to recreate the plot in the slide with the plot title 'This is a graph'**
x <- 1:10
y <- c(2, 4.5, 7, 6, 5, 7, 10, 9, 8, 10)

# Plot performance over time with a dashed red line
plot(x, y, type = "l", col = "red", lty = "dashed",
     lwd = 2, xlab = "Time", ylab = "Performance", main = "This is a graph",
     col.main = "blue", font.main = 2, cex.main = 2, ylim = c(2, 10))

# Add a legend
legend("topleft", legend = "Per curve", col = "red", lty = 2, inset = 0.05, box.lty = 1)

# Identify odd x values and corresponding y values
odd_x = x[x %% 2 == 1]
odd_x <- c(odd_x, 10)
odd_y = y[x %% 2 == 1]
odd_y <- c(odd_y, 12)

# Add points and labels for odd x values
points(odd_x, odd_y, col = "red", pch = 16, cex = 1)
text(odd_x, odd_y, labels = odd_x, col = "red", pos = 3, cex = 1)


# Exercise 6. Plot a histogram representation of hypergeometric distribution with N=500, K=50 and n=30**
hypergeometric <- function(k, N, K, n) {
  return((choose(K, k) * choose(N - K, n - k)) / choose(N, n))
}

# Parameters for the hypergeometric distribution
N <- 500   # Total population size
K <- 50    # Number of successes in the population
n <- 30    # Sample size
x <- 0:min(K, n)  # Possible number of successes in the sample

# Calculate probabilities
probabilities <- sapply(x, function(k) hypergeometric(k, N, K, n))

# Handle potential NA values
valid_probs <- na.omit(probabilities)
valid_repeats <- pmax(round(valid_probs * 1000), 1)  # Ensure at least one repetition

# Plot histogram
hist(rep(x, times = valid_repeats),
     xlab = "Successes",
     ylab = "Frequency",
     main = "Hypergeometric Distribution (N=500, K=50, n=30)",
     col = "red", breaks = length(x))

# Exercise 7. Write few lines of script to explore what happens to a hypergeometric distribution when n is increased and gets closer to N. Show that it approaches the binomial distribution by plotting histograms of both hypergeometric and binomial on the same plot. Use a 3x3 grid of 9 graphs to show the convergence of hypergeometric to binomial distribution.**
binomial_prob <- function(k, n, p) {
  choose(n, k) * (p^k) * ((1 - p)^(n - k))
}

hypergeometric_prob <- function(k, N, K, n) {
  (choose(K, k) * choose(N - K, n - k)) / choose(N, n)
}

# Parameters
N <- 50    # Total population size
K <- 25    # Number of successes in the population
n_values <- seq(5, 45, length.out = 9)  # Increasing sample sizes

# Set up a 3x3 grid for plots
par(mfrow = c(3, 3))

# Loop through different sample sizes
for (n in n_values) {
  x <- 0:min(K, n)  # Possible values of k
  
  # Compute probabilities
  hyper_probs <- sapply(x, function(k) hypergeometric_prob(k, N, K, n))
  binom_probs <- sapply(x, function(k) binomial_prob(k, n, K / N))
  
  # Plot histogram
  plot(x, hyper_probs, type = "h", col = "blue", lwd = 2, ylim = c(0, max(c(hyper_probs, binom_probs))),
       xlab = "Successes", ylab = "Probability", main = paste("n =", n))
  lines(x, binom_probs, col = "red", type = "l")  
}

#Exercise 8. On the same plot, draw 3 Poisson distributions with λ values of 3,20,45 (Code the probability distribution function).**
poisson_prob <- function(k, lambda) {
  (lambda^k * exp(-lambda)) / factorial(k)
}

# Define λ values
lambda_values <- c(3, 20, 45)

# Define x values (support range)
x <- 0:80  # Covers a broad range for higher λ

# Plot Poisson distribution for λ = 3
plot(x, poisson_prob(x, lambda_values[1]), type = "h", col = "blue", lwd = 2, 
     xlab = "x", ylab = "Probability", main = "Poisson Distributions (λ = 3, 20, 45)",
     ylim = c(0, 0.2))  # Adjusted y-axis for visibility

# Add plots for λ = 20 and λ = 45
lines(x, poisson_prob(x, lambda_values[2]), type = "h", col = "red", lwd = 2)
lines(x, poisson_prob(x, lambda_values[3]), type = "h", col = "green", lwd = 2)

# Add legend
legend("topright", legend = c("λ = 3", "λ = 20", "λ = 45"), 
       col = c("blue", "red", "green"), lty = 1, lwd = 2)

#Exercise 9. Load the csv file for heights and weights of 25000 people and do the following:**
height_weight_data <- read.csv('/home/ibab/R/SOCR-HeightWeight.csv')

# Extract height and weight columns
height_data <- height_weight_data$Height.Inches.
weight_data <- height_weight_data$Weight.Pounds.

# Remove missing values
height_data <- height_data[!is.na(height_data)]
weight_data <- weight_data[!is.na(weight_data)]

# Compute mean and standard deviation for height
mean_height <- mean(height_data)
sd_height <- sd(height_data)

# Compute mean and standard deviation for weight
mean_weight <- mean(weight_data)
sd_weight <- sd(weight_data)

# Print statistics
cat("Mean Height:", mean_height, " SD Height:", sd_height, "\n")
cat("Mean Weight:", mean_weight, " SD Weight:", sd_weight, "\n")

# (i) Plot a histogram of the height variable and determine its mean and standard deviation**
hist(height_data, breaks = "Sturges", probability = TRUE, col = "lightblue",
     main = "Height Distribution", xlab = "Height (inches)", ylab = "Density")

#(ii) Plot a histogram of the weight variable and determine its mean and standard deviation**
hist(weight_data, breaks = "Sturges", probability = TRUE, col = "lightcoral",
     main = "Weight Distribution", xlab = "Weight (pounds)", ylab = "Density")

#(iii) Draw a Gaussian curve (recall the Gaussian PDF) with the above calculated mean and standard deviation for both height and weight variables as Z vs P(Z) (i.e., Z-transformed). Plot using either plot() function or curve() function.**
# Define a function to compute Z-scores
calculate_z_score <- function(value, mean_value, std_dev) {
  return((value - mean_value) / std_dev)
}

# Define a function for Gaussian PDF
gaussian_function <- function(z) {
  return((1 / sqrt(2 * pi)) * exp(-0.5 * z^2))
}

# Compute Z-scores for height and weight
z_scores_height <- calculate_z_score(height_data, mean_height, sd_height)
z_scores_weight <- calculate_z_score(weight_data, mean_weight, sd_weight)

# Sort Z-scores for smooth plotting
sorted_z_height <- sort(z_scores_height)
sorted_z_weight <- sort(z_scores_weight)

# Compute normal distribution PDF values
pdf_height <- gaussian_function(sorted_z_height)
pdf_weight <- gaussian_function(sorted_z_weight)

# Define y-axis limit
max_pdf_value <- max(pdf_height, pdf_weight)

# Plot Gaussian curves
plot(sorted_z_height, pdf_height, type = "l", col = "blue", lwd = 2, lty = 1,
     main = "Gaussian PDF for Height & Weight (Z-transformed)", 
     xlab = "Z-score", ylab = "Density", ylim = c(0, max_pdf_value))

lines(sorted_z_weight, pdf_weight, col = "red", lwd = 2, lty = 2)

# Add legend
legend("topright", legend = c("Height", "Weight"), col = c("blue", "red"), lwd = 2, lty = c(1, 2))

#(iv) What happens when you decrease the size of the bins in the histogram plots? Make a 1x3 grid of 3 plots that show the trend for decreasing bin sizes.**
par(mfrow = c(1, 3))  # Layout for 3 plots in a row

# Plot histograms with different bin sizes
hist(height_data, breaks = 10, col = "lightblue", main = "Bins = 10", xlab = "Height", ylab = "Frequency", border = "black")
hist(height_data, breaks = 30, col = "lightblue", main = "Bins = 30", xlab = "Height", ylab = "Frequency", border = "black")
hist(height_data, breaks = 50, col = "lightblue", main = "Bins = 50", xlab = "Height", ylab = "Frequency", border = "black")

par(mfrow = c(1, 1))  # Reset plot layout



#Exercise 10: Plot the PDF and CDF for the uniform distribution U(1,2). Find a way to shade the region under the PDF up to x = 1.5.**
# This exercise demonstrates plotting and shading for a uniform distribution.
#ex10
# Parameters for the uniform distribution
a <- 1; b <- 2  

# Define PDF and CDF functions for the uniform distribution
uniform_pdf <- function(x, a, b) ifelse(x >= a & x <= b, 1 / (b - a), 0)
uniform_cdf <- function(x, a, b) ifelse(x < a, 0, ifelse(x > b, 1, (x - a) / (b - a)))

# Generate values for plotting
x_vals <- seq(0.5, 2.5, length.out = 300)
pdf_vals <- uniform_pdf(x_vals, a, b)
cdf_vals <- uniform_cdf(x_vals, a, b)

# Plot PDF with shading
par(mfrow = c(1,2))  
plot(x_vals, pdf_vals, type = "l", col = "blue", lwd = 2, main = "PDF of U(1,2)", xlab = "x", ylab = "Density")
# Shade the region under the PDF up to x = 1.5
polygon(c(a, seq(a, 1.5, length.out = 100), 1.5), c(0, uniform_pdf(seq(a, 1.5, length.out = 100), a, b), 0), 
        col = rgb(1, 0, 0, 0.3), border = NA) 

# Plot CDF
plot(x_vals, cdf_vals, type = "l", col = "red", lwd = 2, main = "CDF of U(1,2)", xlab = "x", ylab = "Probability")
# Add horizontal grid lines
abline(h = 0:1, col = "black", lty = 2)

par(mfrow = c(1,1)) 

#Exercise 11: Plot the PDF and CDF for the exponential distribution with λ = 10. Shade the region under the PDF up to x = 2.8.**
# This exercise demonstrates plotting and shading for an exponential distribution.
#ex11
# Define PDF and CDF functions for the exponential distribution
exp_pdf <- function(x, lambda) {
  ifelse(x >= 0, lambda * exp(-lambda * x), 0)
}

exp_cdf <- function(x, lambda) {
  ifelse(x >= 0, 1 - exp(-lambda * x), 0)
}

# Parameters for the exponential distribution
lambda <- 10
x_vals <- seq(0, 5, length.out = 300)
pdf_vals <- exp_pdf(x_vals, lambda)
cdf_vals <- exp_cdf(x_vals, lambda)

# Plot PDF with darker shading
par(mfrow = c(1, 2))
plot(x_vals, pdf_vals, type = "l", col = "blue", lwd = 2,
     main = "Exponential PDF (λ=10)", xlab = "x", ylab = "Density")
# Shade the region under the PDF up to x = 2.8 with a darker red color
polygon(c(0, seq(0, 2.8, length.out = 100), 2.8), 
        c(0, exp_pdf(seq(0, 2.8, length.out = 100), lambda), 0), 
        col = rgb(0.6, 0, 0, 0.7), border = NA) # Darker red shade

# Plot CDF
plot(x_vals, cdf_vals, type = "l", col = "red", lwd = 2,
     main = "Exponential CDF (λ=10)", xlab = "x", ylab = "Probability")
par(mfrow = c(1,1))

#Exercise 12: Plot the PDF and CDF for the Gamma distribution with α = 5 and θ = 3.**
# This exercise demonstrates plotting for a Gamma distribution.
#ex12
# Define PDF and CDF functions for the Gamma distribution
gamma_pdf <- function(x, alpha, theta) {
  ifelse(x > 0, (x^(alpha-1) * exp(-x/theta)) / (theta^alpha * gamma(alpha)), 0)
}

gamma_cdf <- function(x, alpha, theta) {
  ifelse(x > 0, pgamma(x, shape = alpha, scale = theta), 0)  # pgamma() for incomplete gamma function
}

# Parameters for the Gamma distribution
alpha <- 5
theta <- 3
x_vals <- seq(0, 30, length.out = 300)
pdf_vals <- gamma_pdf(x_vals, alpha, theta)
cdf_vals <- gamma_cdf(x_vals, alpha, theta)

# Plot PDF
par(mfrow = c(1, 2))
plot(x_vals, pdf_vals, type = "l", col = "blue", lwd = 2,
     main = "Gamma PDF (α=5, θ=3)", xlab = "x", ylab = "Density")

# Plot CDF
plot(x_vals, cdf_vals, type = "l", col = "red", lwd = 2,
     main = "Gamma CDF (α=5, θ=3)", xlab = "x", ylab = "Probability")
par(mfrow = c(1,1))

# Exercise 13: Plot the PDF and CDF for the Chi-square distribution for 20 degrees of freedom. Shade the region under the PDF up to x = 1.0.**
# This exercise demonstrates plotting and shading for a Chi-square distribution.
#ex13
# Define PDF function for the Chi-square distribution
chi_square_pdf <- function(x, df) {
  ifelse(x > 0, (x^(df/2 - 1) * exp(-x/2)) / (2^(df/2) * gamma(df/2)), 0)
}

# Define CDF function for the Chi-square distribution
chi_square_cdf <- function(x, df) {
  pchisq(x, df)  # Built-in function for cumulative probability
}

# Parameters for the Chi-square distribution
df <- 20  # Degrees of freedom
x_vals <- seq(0, 50, length.out = 300)  # X values for plotting
pdf_vals <- chi_square_pdf(x_vals, df)  # PDF values
cdf_vals <- chi_square_cdf(x_vals, df)  # CDF values

# Plot PDF with solid dark red shading up to x = 1.0
par(mfrow = c(1, 2))  # Layout: 1 row, 2 columns
plot(x_vals, pdf_vals, type = "l", col = "blue", lwd = 2,
     main = "Chi-square PDF (df=20)", xlab = "x", ylab = "Density")

# Shade the region under the PDF up to x = 1.0 with solid dark red
shade_x <- seq(0, 1.0, length.out = 100)  # X values for shading
shade_y <- chi_square_pdf(shade_x, df)    # Corresponding PDF values
polygon(c(0, shade_x, 1.0), c(0, shade_y, 0), 
        col = "darkred", border = NA)  # SOLID dark red shading

# Plot CDF
plot(x_vals, cdf_vals, type = "l", col = "red", lwd = 2,
     main = "Chi-square CDF (df=20)", xlab = "x", ylab = "Probability")

par(mfrow = c(1, 1))  # Reset layout


















