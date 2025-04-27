#I
#(1)
means = c(20.34,19.49,25.68)
stderr = c(0.83,1.51,1.39)

#creating a barplot:
bargraph <- barplot(means,names.arg=c("A","B","C"),col="grey",ylim=c(0,max(means+stderr)+1),main="Errors")
#adding arrows:
arrows(x0=bargraph,y0=means+stderr,x1=bargraph,y1=means-stderr,angle=90,code=3,length=0.06,col='red')

#(2)
x = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
y = c(5, 9, 13, 17, 20, 24, 26, 27, 28, 27)
errors = c(0.5, 0.9, 1.4, 1.5, 2.0, 2.2, 2.3, 2.5, 2.9, 3.0)

plot(x, y, 
     pch = 19, 
     col = "coral",
     xlab = "concentration", 
     ylab = "optical activity", 
     main = "Error bars on data points", 
     ylim = c(min(y - errors), max(y + errors) + 1))

arrows(x0 = x, y0 = y + errors, 
       x1 = x, y1 = y - errors, 
       angle = 90, code = 3, length = 0.06, col = "violet")

#(3)

x <- c(10,20,30,40,50,60,70,80,90,100)
y <- c(95, 220, 279, 424, 499, 540, 720, 880, 950, 1200)
cov(x,y)
cor(x,y)
cor(longley) #multivariate data

#2
#(1) (a)
#if null hyp is more than or equal, we're looking at a right tailed dist
one_sample_Ztest <- function(x, sigma, muzero, alpha, nullhyp) {
  
  #initializing the variables:
  x_bar <- mean(x)
  n <- length(x)
  
  #computing the Z-value:
  z <- (x_bar - muzero) / (sigma / sqrt(n))
  
  #calculate p-value and draw conclusion:
  if (nullhyp == "equal") {
    p_value <- 2 * (1 - pnorm(abs(z))) #to account for areas on both the sides
    conclusion <- ifelse(p_value < alpha, "Reject H0", "Accept H0")
  } else if (nullhyp == "less_than_or_equal") {
    p_value <- 1 - pnorm(z)
    conclusion <- ifelse(p_value < alpha, "Reject H0", "Accept H0")
  } else if (nullhyp == "more_than_or_equal") {
    p_value <- pnorm(z)
    conclusion <- ifelse(p_value < alpha, "Reject H0", "Accept H0")
  } else {
    stop("Invalid null hypothesis. Use 'equal', 'less_than_or_equal', or 'more_than_or_equal'")
  }
  
  return(c(p_value = p_value, Z_value = z, Conclusion = conclusion))
}

#(b)
print(one_sample_Ztest(
  x = c(141.5, 152.3, 121.2, 123.0, 151.6, 124.8, 138.9,
        137.4, 145.6, 135.6, 135.4, 121.5), 
  sigma = 14.5, 
  muzero = 124.6, 
  alpha = 0.05, 
  nullhyp = 'equal'
))

#(2)
one_sample_t_test <- function(x, muzero, alpha, nullhyp) {
  
  n <- length(x)
  x_bar <- mean(x)
  s <- sd(x)
  dof <- n-1
  
  t_val = (x_bar - muzero) / (s/sqrt(n))
  
  if (nullhyp == 'equal') {
    p_val <- 2 * (1-pt(abs(t_val),dof))
    conclusion <- ifelse(p_val < alpha, "Reject H0", "Accept H0")
  } else if (nullhyp == "less_than_or_equal") {
    p_val <- (1 - pt(abs(t_val), dof))
    conclusion <- ifelse(p_val < alpha, "Reject H0", "Accept H0")
  } else if (nullhyp == "more_than_or_equal") {
    p_val <- pt(abs(t_val),dof)
    conclusion <- ifelse(p_val < alpha, "Reject H0", "Accept H0")
  } else {
    stop("Invalid null hypothesis. Use 'equal', 'less_than_or_equal', or 'more_than_or_equal'")
  }
  
  return(c(p_value = p_val, T_val=t_val, Conclusion = conclusion))
  
}

print(one_sample_t_test(x = c(96.0, 104.0, 99.1, 97.6, 99.4, 92.8, 105.6, 97.2,
                              96.8, 92.1, 100.6, 101.5, 100.7, 97.3, 99.6, 105.9), 100, 0.05, "equal"))


#(3)
# One-sample proportion test using binom.test (exact test)
binom_result <- binom.test(x = 710, n = 2600, p = 0.25, alternative = "greater")
print(binom_result)

# One-sample proportion test using prop.test (normal approximation)
prop_result <- prop.test(x = 710, n = 2600, p = 0.25, alternative = "greater", correct = TRUE)
print(prop_result)

#4
#(a)
one_sample_variance_test <- function(x, test_sigma, alpha, test_type = "two-sided") {
  # Sample size and sample variance
  n <- length(x)
  s2 <- var(x)
  
  # Hypothesized population variance
  sigma2 <- test_sigma^2
  
  # Compute the Chi-square test statistic
  chi2_stat <- (n - 1) * s2 / sigma2
  
  # Critical values for the Chi-square distribution
  if (test_type == "two-sided") {
    # Two-sided test: compare with both lower and upper quantiles
    lower_limit <- qchisq(alpha / 2, df = n - 1)
    upper_limit <- qchisq(1 - alpha / 2, df = n - 1)
    conclusion <- ifelse(chi2_stat < lower_limit | chi2_stat > upper_limit, "Reject H0", "Fail to Reject H0")
  } else if (test_type == "one-sided_lower") {
    # One-sided lower test: compare with the lower quantile
    lower_limit <- qchisq(alpha, df = n - 1)
    conclusion <- ifelse(chi2_stat < lower_limit, "Reject H0", "Fail to Reject H0")
  } else if (test_type == "one-sided_upper") {
    # One-sided upper test: compare with the upper quantile
    upper_limit <- qchisq(1 - alpha, df = n - 1)
    conclusion <- ifelse(chi2_stat > upper_limit, "Reject H0", "Fail to Reject H0")
  }
  
  # Return results
  result <- list(
    chi2_statistic = chi2_stat,
    p_value = 1 - pchisq(chi2_stat, df = n - 1),  # P-value for one-sided test
    lower_limit = if (test_type == "two-sided") lower_limit else NULL,
    upper_limit = if (test_type == "two-sided") upper_limit else NULL,
    conclusion = conclusion
  )
  
  return(result)
}

#(b)
# Given data
x <- c(142.8, 135.0, 157.5, 148.4, 135.9, 153.4, 149.0, 130.2,
       156.0, 189.7, 151.6, 156.5, 123.8, 152.9, 118.4, 145.8)


test_sigma <- 29
alpha <- 0.05


result_two_sided <- one_sample_variance_test(x, test_sigma, alpha, "two-sided")


result_one_sided_lower <- one_sample_variance_test(x, test_sigma, alpha, "one-sided_lower")


result_one_sided_upper <- one_sample_variance_test(x, test_sigma, alpha, "one-sided_upper")

print(result_two_sided)
print(result_one_sided_lower)
print(result_one_sided_upper)


#5
x <- c(176.9, 158.3, 152.1, 158.8, 172.4, 169.8, 159.7, 162.7,
       156.6, 174.5, 184.4, 165.2, 147.8, 177.8, 160.1, 161.5)

print(wilcox.test(x, mu = 160, alternative = "less", conf.int = TRUE, conf.level = 0.95))

#II
#(1)
#(a)
# Two sample Z test function for different types of alternatives
two_sample_Z_test <- function(x1, x2, sigma_x1, sigma_x2, alpha, null) {
  
  # Sample sizes
  n1 <- length(x1)
  n2 <- length(x2)
  
  # Sample means
  mean_x1 <- mean(x1)
  mean_x2 <- mean(x2)
  
  # Z-statistic calculation
  z_statistic <- (mean_x1 - mean_x2) / sqrt((sigma_x1^2 / n1) + (sigma_x2^2 / n2))
  
  # Determine critical value based on null hypothesis
  if (null == "greater") {
    z_critical <- qnorm(1 - alpha)  # Upper tail critical value for μ1 > μ2
  } else if (null == "less") {
    z_critical <- qnorm(alpha)  # Lower tail critical value for μ1 < μ2
  } else if (null == "equal") {
    z_critical <- qnorm(1 - alpha / 2)  # Two-tailed critical value for μ1 ≠ μ2
  } else {
    stop("Invalid null hypothesis specification. Choose 'greater', 'less', or 'equal'.")
  }
  
  # Calculate p-value based on alternative hypothesis
  if (null == "greater") {
    # Alternative hypothesis is "less" for the greater than null hypothesis
    p_value <- pnorm(z_statistic)  # For μ1 < μ2 (alternative hypothesis)
  } else if (null == "less") {
    p_value <- 1 - pnorm(z_statistic)  # For μ1 > μ2 (alternative hypothesis)
  } else if (null == "equal") {
    p_value <- 2 * min(pnorm(z_statistic), 1 - pnorm(z_statistic))  # Two-tailed
  }
  
  # Conclusion based on critical value and p-value
  if (null == "greater" && z_statistic < z_critical) {
    conclusion <- "Reject the null hypothesis: μ1 < μ2"
  } else if (null == "less" && z_statistic < -z_critical) {
    conclusion <- "Reject the null hypothesis: μ1 > μ2"
  } else if (null == "equal" && abs(z_statistic) > z_critical) {
    conclusion <- "Reject the null hypothesis: μ1 ≠ μ2"
  } else {
    conclusion <- "Fail to reject the null hypothesis"
  }
  
  # Return the results
  return(list(
    ZStatistic = z_statistic,
    PValue = p_value,
    ZCritical = z_critical,
    Conclusion = conclusion
  ))
}


#(b)
print(two_sample_Z_test(x1 <- c( 258.0, 271.5, 189.1, 216.5, 237.2, 222.0, 231.3, 181.7, 220.0, 179.3, 238.1, 217.7,
                                 246.2, 241.5, 233.8, 222.3, 199.2, 167.9, 216.2, 240.4, 235.3, 187.0, 233.7, 214.7,
                                 174.6, 246.3, 185.7, 207.0, 244.3, 237.7, 245.2, 228.3, 201.8, 218.3, 242.7, 213.8,
                                 231.9, 257.3, 208.4, 250.7, 198.3, 206.7, 259.7, 253.3, 200.3, 196.6, 210.6, 257.6,
                                 173.5, 267.5, 167.2, 227.1, 172.1, 197.6, 256.9, 203.7, 195.1, 237.4, 210.2, 208.8,
                                 218.0, 205.1, 241.1, 216.8, 223.6, 191.0, 225.9, 215.1, 233.1, 243.0),
                        
                        x2 <- c( 221.0, 213.0, 199.3, 211.2, 225.2, 229.1, 253.9, 194.6, 243.0, 221.9, 230.9, 221.1,
                                 206.7, 217.2, 215.8, 203.0, 234.0, 196.3, 235.8, 234.3, 244.7, 248.8, 200.5, 232.0,
                                 233.3, 220.6, 289.2, 244.9, 230.8, 182.9, 199.3, 263.2, 220.6, 266.7, 258.0, 243.9,
                                 178.1, 200.7, 270.2, 224.4, 222.4, 234.6, 296.7, 202.3, 277.9, 204.3, 221.1, 257.0,
                                 243.4, 239.4, 230.0, 263.5, 241.3, 216.6, 227.9, 230.1, 230.5, 188.6, 289.3, 234.4,
                                 267.5, 256.0, 246.5, 210.5, 270.6, 295.5, 195.8, 235.3, 245.4, 245.4), 24.6, 27.8, 0.05, "greater"))


#(2)
#(a)

Xvar <- c(4.95, 5.37, 4.70, 4.96, 4.72, 5.17, 5.28, 5.12, 5.26, 5.48)
Yvar <- c(4.65, 4.86, 4.57, 4.56, 4.96, 4.63, 5.04, 4.92, 5.37, 4.58, 4.26, 4.40)


welch_test <- t.test(Xvar, Yvar, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)


print(welch_test)

#(b)

data_before <- c(95, 106, 79, 71, 90, 79, 71, 77, 103, 103, 92, 63, 82, 76)
data_after <- c(97, 116, 82, 81, 82, 86, 107, 86, 94, 91, 85, 98, 91, 87)


paired_test <- t.test(data_before, data_after, alternative = "two.sided", paired = TRUE, conf.level = 0.95)


print(paired_test)

#(3)
#(a)

men_successes <- 520
men_trials <- 600
women_successes <- 550
women_trials <- 600

# Perform two-sample proportion test
proportion_test <- prop.test(c(men_successes, women_successes), c(men_trials, women_trials), 
                             alternative = "two.sided", correct = TRUE)

# Print the result of the test
print(proportion_test)

#(b)

data_matrix <- matrix(c(11, 42, 17, 39), nrow = 2, byrow = TRUE)

# Perform Fisher's Exact Test
fisher_test <- fisher.test(data_matrix, alternative = "two.sided", conf.int = TRUE, conf.level = 0.95)

# Print the result of the test
print(fisher_test)

#Question 4
#a)
# Function to perform two-sample variance test using F-distribution
two_sample_variance_test <- function(x, y, alpha) {
  # Calculate variances and sample sizes
  var_x <- var(x)
  var_y <- var(y)
  n_x <- length(x)
  n_y <- length(y)
  # Calculate F-statistic (larger variance / smaller variance)
  if (var_x > var_y) {
    F_stat <- var_x / var_y
    df1 <- n_x - 1
    df2 <- n_y - 1
  } else {
    F_stat <- var_y / var_x
    df1 <- n_y - 1
    df2 <- n_x - 1
  }
  # Calculate p-value from F-distribution
  p_value <- 2 * (1 - pf(F_stat, df1, df2))
  #Critical value for two-tailed test
  critical_upper <- qf(1 - alpha/2, df1, df2)
  critical_lower <- qf(alpha/2, df1, df2)
  # Conclusion
  if (F_stat < critical_lower || F_stat > critical_upper) {
    conclusion <- "Reject H0: Variances are significantly different."
  } else {
    conclusion <- "Fail to reject H0: No significant difference in variances."
  }
  # Output 
  print(paste("F-statistic:", round(F_stat, 4)))
  print(paste("Degrees of Freedom:", df1, "and", df2))
  print(paste("p-value:", round(p_value, 4)))
  print(paste("Conclusion:", conclusion))
}
#b)testing the function
x <- c(1067.7, 984.3, 998.8, 1025.9, 1060.9, 959.1, 1013.8, 1047.0, 987.8, 1051.0, 885.2, 1049.5, 1098.2, 1001.5, 1011.1, 991.6)
y <- c(957.6, 981.8, 1096.5, 984.4, 1074.3, 929.4, 1056.0, 1012.3, 1040.7, 1099.5, 1006.1, 1064.3, 865.6, 944.4, 1091.8, 952.1)
# Set alpha level
alpha <- 0.05
# Call the function
two_sample_variance_test(x, y, alpha)


#Question 5
# Function to perform Wilcoxon signed-rank test 
wilcoxon_conclusion <- function(before, after) {
  result <- wilcox.test(before, after,
                        paired = TRUE,
                        alternative = "greater",
                        conf.level = 0.95,
                        exact = FALSE)
  print(result)
  # Conclusion based on p-value
  if (result$p.value < 0.05) {
    conclusion <- "Reject the null hypothesis: Pre-therapy scores are significantly greater than Post-therapy scores."
  } else {
    conclusion <- "Fail to reject the null hypothesis: No significant evidence that Pre-therapy scores are greater than Post-therapy scores."
  }
  return(conclusion)
}
# Define the paired samples
Pre_therapy <- c(74, 72, 62, 58, 59, 65, 54, 63, 80, 66, 65, 64, 79, 60)
Post_therapy <- c(79, 55, 53, 53, 74, 55, 64, 55, 39, 44, 37, 68, 54, 54)
# Call the function 
wilcox_result <- wilcoxon_conclusion(Pre_therapy, Post_therapy)
print(wilcox_result)


#Question 6
# Define the function for Wilcoxon Rank Sum Test with conclusion
wilcoxon_rank_sum_conclusion <- function(group1, group2) {
  result <- wilcox.test(group1, group2,
                        alternative = "less",
                        conf.level = 0.95)
  print(result)
  # Conclusion based on p-value
  if (result$p.value < 0.05) {
    conclusion <- "Reject the null hypothesis: The placebo group has significantly lower values than the drug group."
  } else {
    conclusion <- "Fail to reject the null hypothesis: No significant evidence that the placebo group has lower values than the drug group."
  }
  return(conclusion)
}
# Sample data 
drug <- c(31.7, 75.0, 101.1, 60.5, 62.8, 59.3, 58.9, 91.3, 99.1, 52.0, 39.1)
placebo <- c(59.3, 72.7, 100.5, 64.7, 69.0, 72.7, 69.6, 97.4, 100.6, 65.1, 65.7)
# Call the function
wilcox_rs_result <- wilcoxon_rank_sum_conclusion(placebo, drug)
print(wilcox_rs_result)


#Question 7
# Define the function to perform Kruskal-Wallis test and give conclusion
kruskal_test_conclusion <- function(x, y) {
  result <- kruskal.test(x, y)
  print(result)
  # Conclusion based on p-value
  if (result$p.value < 0.05) {
    conclusion <- "Reject the null hypothesis: At least one group differs significantly from the others."
  } else {
    conclusion <- "Fail to reject the null hypothesis: No significant difference among the groups."
  }
  return(conclusion)
}
# Group data 
group1 <- c(220, 214, 203, 184, 186, 200, 165)
group2 <- c(262, 193, 225, 200, 164, 266, 179)
group3 <- c(272, 192, 190, 208, 231, 235, 141)
group4 <- c(190, 255, 247, 278, 230, 269, 289)
# Combine into a single vector
x <- c(group1, group2, group3, group4)
# Create factor labels for each group
y <- factor(rep(c("Group1", "Group2", "Group3", "Group4"), each = 7))
# Call the function and print the conclusion
kruskal_result <- kruskal_test_conclusion(x, y)
print(kruskal_result)


#Question 8
# Chi-square Goodness-of-Fit (GoF) Test Function
gof_test <- function(observed, expected, alpha = 0.05) {
  #Calculate the test statistic
  chi_sq_stat <- sum((observed - expected)^2 / expected)
  #Degrees of freedom = number of categories - 1
  df <- length(observed) - 1
  #Critical value from chi-square distribution
  critical_val <- qchisq(1 - alpha, df)
  #Compute the p-value
  p_val <- 1 - pchisq(chi_sq_stat, df)
  #Print the results
  cat("Chi-square statistic:", round(chi_sq_stat, 3), "\n")
  cat("Degrees of freedom:", df, "\n")
  cat("Critical value at alpha =", alpha, ":", round(critical_val, 3), "\n")
  cat("P-value:", round(p_val, 4), "\n")
  # Decision
  if (chi_sq_stat > critical_val) {
    cat("Result: Reject the null hypothesis.\n")
  } else {
    cat("Result: Do not reject the null hypothesis.\n")
  }
}
#testing the function
observed <- c(32, 82, 77, 49)
expected <- c(40, 80, 80, 40)
gof_test(observed, expected)



#####PART 4
#Question 1
#a) 
# Read the Titanic data
titanicData <- read.csv("/home/ibab/R/titanic.csv")
# View the first few rows
head(titanicData)
# Setting up a 3x1 plot grid for 3 classes
par(mfrow = c(3, 1))
# Plot histogram for 1st class
hist(titanicData$age[titanicData$passenger_class == '1st'],
     breaks = 40, main = "1st Class Passengers", 
     xlab = "Age", col = "lightblue")
#Plot histogram for 2nd class
hist(titanicData$age[titanicData$passenger_class == '2nd'],
     breaks = 40, main = "2nd Class Passengers",
     xlab = "Age", col="salmon")
#Plot histogram for 3rd class
hist(titanicData$age[titanicData$passenger_class == '3rd'],
     breaks = 40, main = "3rd Class Passengers",
     xlab = "Age", col="lightgreen") 
#all three have similar variances/spread(they are not that different)
#b)
# Loading required libraries
library(dplyr)
titanic_by_passenger_class<- group_by(titanicData,passenger_class)
summarise(titanic_by_passenger_class, group_mean=mean(age,na.rm=TRUE),
          group_sd=sd(age,na.rm=TRUE))
print("Conclusion: The standard deviations across the three classes are fairly similar.")
print("Thus, the assumption of equal variances is reasonably satisfied, and we can proceed with ANOVA.")
# (c) Fit ANOVA model and print conclusion
run_anova <- function(data) {
  lmresults <- lm(age ~ passenger_class, data = data)
  anova_result <- anova(lmresults)
  print(anova_result)
  
  p_val <- anova_result$`Pr(>F)`[1]
  print("[ANOVA Conclusion]:")
  if (!is.na(p_val) && p_val < 0.05) {
    print(paste(" The p-value is", round(p_val, 4), "< 0.05, so we reject H0."))
    print("At least one passenger class has a significantly different mean age.")
  } else {
    print(paste("The p-value is", round(p_val, 4), ">= 0.05, so we fail to reject H0."))
    print(" No significant difference in mean age among passenger classes.")
  }
  return(lmresults)
}
lm_model <- run_anova(titanicData)
# (d) Tukey HSD post-hoc test
tukey_result <- TukeyHSD(aov(lm_model))
print(tukey_result)
# (e) Kruskal-Wallis non-parametric test
run_kruskal_test <- function(data) {
  kw_result <- kruskal.test(age ~ passenger_class, data = data)
  print(kw_result)
  
  print("[Kruskal-Wallis Conclusion]:")
  if (kw_result$p.value < 0.05) {
    print(paste("p-value =", round(kw_result$p.value, 4), "< 0.05 → Reject H0."))
    print("At least one group differs significantly in age (non-parametric evidence).")
  } else {
    print(paste("p-value =", round(kw_result$p.value, 4), ">= 0.05 → Fail to reject H0."))
    print("→ No significant difference in age among classes (non-parametric test).")
  }
}
run_kruskal_test(titanicData)

#Question 2
# (a) Read the Cuckoo Egg Size Data
cuckooData <- read.csv("/home/ibab/R/cuckooeggs.csv")
# View the first few rows
head(cuckooData)

# Setting up a 3x1 plot grid (assuming 3 host species)
par(mfrow = c(3, 1))
# Plot histogram for each host species
hist(cuckooData$egg_length[cuckooData$host_species == 'Hedge Sparrow'],
     breaks = 20, main = "Dunnock Host", 
     xlab = "Cuckoo Egg Length", col = "lightgreen")
hist(cuckooData$egg_length[cuckooData$host_species == 'Meadow Pipit'],
     breaks = 20, main = "Meadow Pipit Host", 
     xlab = "Cuckoo Egg Length", col = "lightblue")
hist(cuckooData$egg_length[cuckooData$host_species == 'Pied Wagtail'],
     breaks = 20, main = "Pied Wagtail Host", 
     xlab = "Cuckoo Egg Length", col = "salmon")
hist(cuckooData$egg_length[cuckooData$host_species == 'Robin'],
     breaks = 20, main = "Dunnock Host", 
     xlab = "Cuckoo Egg Length", col = "lightgreen")
hist(cuckooData$egg_length[cuckooData$host_species == 'Tree Pipit'],
     breaks = 20, main = "Dunnock Host", 
     xlab = "Cuckoo Egg Length", col = "blue")
hist(cuckooData$egg_length[cuckooData$host_species == 'Wren'],
     breaks = 20, main = "Dunnock Host", 
     xlab = "Cuckoo Egg Length", col = "red")
#the spreads (variances) look reasonably similar, but we'll check using summary statistics next.

#b)
# Group by host species
cuckoo_by_host <- group_by(cuckooData, host_species)

# Summarize: mean and sd
summary_stats <- summarise(cuckoo_by_host,
                           group_mean = mean(egg_length, na.rm = TRUE),
                           group_sd = sd(egg_length, na.rm = TRUE))
print(summary_stats)
print("The standard deviations are similar, we can proceed with ANOVA testing.")
#c)
# Fit the ANOVA model
run_anova <- function(data) {
  # Perform ANOVA using lm (linear model)
  lm_result <- lm(egg_length ~ host_species, data = data)
  anova_result <- anova(lm_result)
  
  # Print the ANOVA result
  print(anova_result)
  
  # Extract p-value and make a conclusion
  p_val <- anova_result$`Pr(>F)`[1]
  
  print("[ANOVA Conclusion]:")
  if (!is.na(p_val) && p_val < 0.05) {
    print(paste(" The p-value is", round(p_val, 4), "< 0.05, so we reject H0."))
    print("At least one host species has a significantly different mean egg length.")
  } else {
    print(paste("The p-value is", round(p_val, 4), ">= 0.05, so we fail to reject H0."))
    print("No significant difference in egg length among host species.")
  }
  
  return(lm_result)
}

# Run the ANOVA
lm_model <- run_anova(cuckooData)
#d)
# Tukey-Kramer post-hoc test to determine which host species are significantly different
tukey_result <- TukeyHSD(aov(lm_model))
print(tukey_result)
# Conclusion based on Tukey-Kramer test
print("[Tukey-Kramer Conclusion]:")
# The comparisons are stored in tukey_result$host_species 
# Check if p-values are available and filter for significant differences (p.adj < 0.05)
# We extract the p-values from the result
significant_comparisons <- tukey_comparisons[tukey_comparisons[, "p adj"] < 0.05, ]
# Print significant comparisons if there are any
if (nrow(significant_comparisons) > 0) {
  print("There are significant differences in mean egg lengths between the following pairs of host species:")
  print(significant_comparisons)
} else {
  print("No significant differences were found between any pair of host species.")
}


#Question 3
#a)
# Load the data
malaria_maize_data <- read.csv("/home/ibab/R/malaria vs maize.csv")

# View the first few rows of the data
head(malaria_maize_data)

# Setting up a 3x1 plot grid for 3 levels of maize production
par(mfrow = c(3, 1))

# Plot histogram for low maize yield
hist(malaria_maize_data$incidence_rate[malaria_maize_data$maize_yield == 'Low'],
     breaks = 3, main = "Low Maize Yield", xlab = "Malaria Incidence (per 10,000)", col = "lightblue")

# Plot histogram for medium maize yield
hist(malaria_maize_data$incidence_rate[malaria_maize_data$maize_yield == 'Medium'],
     breaks = 3, main = "Medium Maize Yield", xlab = "Malaria Incidence (per 10,000)", col = "salmon")

# Plot histogram for high maize yield
hist(malaria_maize_data$incidence_rate[malaria_maize_data$maize_yield == 'High'],
     breaks = 3, main = "High Maize Yield", xlab = "Malaria Incidence (per 10,000)", col = "lightgreen")

#b)
# Group by maize yield level
maize_by_yield <- group_by(malaria_maize_data, maize_yield)

# Summarize: mean and standard deviation
summary_stats_maize <- summarise(maize_by_yield,
                                 group_mean = mean(incidence_rate_per_ten_thousand, na.rm = TRUE),
                                 group_sd = sd(incidence_rate_per_ten_thousand, na.rm = TRUE))
print(summary_stats_maize)

print("Based on the standard deviations, spreads seem quite different.")
print("This suggests violation of ANOVA's equal variance assumption.")

#c)
# (c) Log-transform the Malaria Incidence Rate and Redraw Histograms

# Create a new variable for the log-transformed incidence rate
malaria_maize_data$log_incidence <- log(malaria_maize_data$incidence_rate_per_ten_thousand)

# Set up a 3x1 plot grid again
par(mfrow = c(3, 1))

# Plot histograms for log-transformed data
hist(malaria_maize_data$log_incidence[malaria_maize_data$maize_yield == 'High'],
     breaks = 5, main = "High Maize Yield (Log Incidence)", 
     xlab = "Log Malaria Incidence Rate", col = "lightgreen")
hist(malaria_maize_data$log_incidence[malaria_maize_data$maize_yield == 'Medium'],
     breaks = 5, main = "Medium Maize Yield (Log Incidence)", 
     xlab = "Log Malaria Incidence Rate", col = "lightblue")
hist(malaria_maize_data$log_incidence[malaria_maize_data$maize_yield == 'Low'],
     breaks = 5, main = "Low Maize Yield (Log Incidence)", 
     xlab = "Log Malaria Incidence Rate", col = "salmon")
# the spreads after log transformation look more similar.
# Group by maize yield
maize_by_yield_log <- group_by(malaria_maize_data, maize_yield)

# Summarise mean and standard deviation for log-transformed data
summary_stats_maize_log <- summarise(maize_by_yield_log,
                                     group_mean_log = mean(log_incidence, na.rm = TRUE),
                                     group_sd_log = sd(log_incidence, na.rm = TRUE))
print(summary_stats_maize_log)

print("After log-transformation, standard deviations are more similar.")
print("The data now better meet ANOVA assumptions.")

#d)
# Define a function to run ANOVA
run_anova_maize <- function(data) {
  # Fit linear model
  lm_result_maize <- lm(log_incidence ~ maize_yield, data = data)
  anova_result_maize <- anova(lm_result_maize)
  
  # Print ANOVA table
  print(anova_result_maize)
  
  # Extract p-value and make conclusion
  p_val_maize <- anova_result_maize$`Pr(>F)`[1]
  
  print("[ANOVA Conclusion]:")
  if (!is.na(p_val_maize) && p_val_maize < 0.05) {
    print(paste("The p-value is", round(p_val_maize, 4), "< 0.05, so we reject H0."))
    print("There is a significant association between maize yield and malaria incidence.")
  } else {
    print(paste("The p-value is", round(p_val_maize, 4), ">= 0.05, so we fail to reject H0."))
    print("No significant association between maize yield and malaria incidence.")
  }
  
  return(lm_result_maize)
}
# Run the ANOVA
lm_model_maize <- run_anova_maize(malaria_maize_data)



#Question 4
#a)
# Load the data
circadian_data <- read.csv("/home/ibab/R/circadian mutant health.csv")

# Check the first few rows
head(circadian_data)

# Set up a 3x1 plot grid
par(mfrow = c(3, 1))

# Plot histograms for each genotype
hist(circadian_data$days_to_death[circadian_data$genotype == "tim01 (rescued)"],
     breaks = 5, main = "Normal Flies", 
     xlab = "Days to Death", col = "lightgreen")

hist(circadian_data$days_to_death[circadian_data$genotype == "tim01"],
     breaks = 5, main = "tim01 Mutant Flies", 
     xlab = "Days to Death", col = "lightblue")

hist(circadian_data$days_to_death[circadian_data$genotype == "wild type"],
     breaks = 5, main = "Heterozygous Flies", 
     xlab = "Days to Death", col = "salmon")
#b)
perform_kruskal_test <- function(data) {
  # Perform Kruskal-Wallis test
  kruskal_test_result <- kruskal.test(days_to_death ~ genotype, data = data)
  print(kruskal_test_result)
  # Extract p-value
  p_val <- kruskal_test_result$p.value
  # Print the conclusion based on p-value
  print("[Kruskal-Wallis Test Conclusion]:")
  if (p_val < 0.05) {
    print(paste("The p-value is", round(p_val, 4), "< 0.05, so we reject H0."))
    print("There is a significant difference in lifespan between the groups of flies.")
  } else {
    print(paste("The p-value is", round(p_val, 4), ">= 0.05, so we fail to reject H0."))
    print("There is no significant difference in lifespan between the groups of flies.")
  }
}
# Example usage
perform_kruskal_test(circadian_data)
