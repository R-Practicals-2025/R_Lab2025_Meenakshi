####LAB15
##LINEAR REGRESSION
#1)
x = c(0.5, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0)
y = c(0.87, 7.12, 14.01, 24.37, 39.058, 58.62, 83.92)
# Fit the third-degree polynomial
model <- lm(y ~ poly(x, 3, raw = TRUE))
# View the model summary
summary(model)
#2)
# Print the coefficient of the x term (linear term)
print(paste("Coefficient of x term:", coef(model)[2]))
#3)
# Print the coefficient of the x^2 term
print(paste("Coefficient of x^2 term:", coef(model)[3]))
#4)
# Print the coefficient of the x^3 term
print(paste("Coefficient of x^3 term:", coef(model)[4]))


####MULTIPLE LINEAR REGRESSION
# Load the dataset
library(datasets)
data(trees)
#i)
df <- trees  # Copy the dataset to a new dataframe
print(colnames(df)) 
# Convert Girth (inches to meters)
df$Girth <- df$Girth * 0.0254

# Convert Height (feet to meters)
df$Height <- df$Height * 0.3048

# Convert Volume (cubic feet to cubic meters)
df$Volume <- df$Volume * 0.028317

# View the updated dataframe
head(df)
#ii)
library(lattice)
splom(df, xlab = "Scatter Plot Matrix")
# Girth and Volume are strongly positively correlated.
# Height and Volume show a weak positive correlation.
# Girth and Height have little or no correlation.
#iii)
# Save Volume as the response vector y
y <- df$Volume
#iv)
# Initialize β0 (intercept) as 1 for each row
beta0 <- rep(1, nrow(df))
# View the first few rows to confirm
head(beta0)
#v)
# Create the X matrix
X <- cbind(beta0, df$Girth, df$Height)
# View the first few rows to check
head(X)
#vi)
# Solve for beta using matrix multiplication
model1 <- solve(t(X) %*% X) %*% t(X) %*% y
# Print the coefficients β0, β1, and β2
model1
#vii)
# Define new data for prediction
newdata <- data.frame(Girth = c(0.3, 0.4, 0.5), Height = c(20, 21, 22))
# Predict the volume using the model1 coefficients
predictions <- newdata$Girth * model1[2] + newdata$Height * model1[3] + model1[1]
# Print the predictions
predictions
#viii)
# Fit the linear regression model using lm()
model2 <- lm(Volume ~ Girth + Height, data = df)
# Display the model summary
summary(model2)
# Compare coefficients from lm() with those from solve()
model2$coefficients
model1 #both are matching
#ix)
# Define new data for prediction
newdata <- data.frame(Girth = c(0.3, 0.4, 0.5), Height = c(20, 21, 22))
# Predict the volume using the model2 (lm) object
predictions_lm <- predict(model2, newdata)
# Print the predictions
predictions_lm


####NON LINEAR REGRESSION
# Question 1 Enter the data sets
xdat = c(1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2,
         2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3.0)

ydat = c(-0.09, 0.59, 0.42, 2.03, 3.43, 1.84, 5.30, 4.40, 6.67, 7.40,
         7.34, 8.76, 10.25, 10.93, 13.78, 14.21, 17.82, 21.83, 23.04,
         24.25, 27.48)

#Question 2 Create a data frame
df = data.frame(xdat, ydat)
# Question 3 Fit the model y = a * (x^n) - simpelst form 
# Guess starting values: a = 1, n = 2 (since it looks like a curve )
model = nls(ydat ~ a * (xdat^n), data = df, start = list(a = 1, n = 2))
# Question 4 summary of the fit
summary(model)
#Question 5
#based on the summary there are 19 degrees of freedom
#this can also be inferred by  df = number of data points - number of parameters (here 2 parameters: a and n)
# df = 21 - 2 = 19
print(paste("Degrees of freedom:", "19"))
#Question 6
#Create a sequence from 1 to 3 with step 0.1
xseq = seq(1, 3, by = 0.1)
# Generate predicted values using the fitted model
# Extract coefficients
coef_a = coef(model)["a"]
coef_n = coef(model)["n"]

# Calculate fitted y values
yfit = coef_a * (xseq^coef_n)

# Plot original data points
plot(xdat, ydat, main = "Nonlinear Fit", xlab = "xdat", ylab = "ydat", pch = 19, col = "blue")

# Add fitted curve
lines(xseq, yfit, col = "red", lwd = 2)


####CLUSTERING METHODS
#Question 1
#i)
# Load the necessary packages
library(tidyverse)
library(dplyr)
library(RColorBrewer)
#ii)
# Read the data
spellman <- read.csv("spellman-wide.csv")
# Print the dimensions of the dataframe
dim(spellman)
#iii)
# Print the first 5 rows and first 8 columns
head(spellman[, 1:8], 5)
#iv)
# Correlation calculation and conversion to dissimilarity matrix
spellman_cor <- select(spellman, -time, -expt) %>%
  cor(use = "pairwise.complete.obs")
spellman_dist <- as.dist(1 - spellman_cor)
#v)
# Perform hierarchical clustering
spellman_tree <- hclust(spellman_dist, method = "complete")
# Plot the dendrogram
plot(spellman_tree)
#vi)
# Load dendextend package
library(dendextend)
# Create dendrogram and plot without labels
spellman_dend <- as.dendrogram(spellman_tree)
plot(spellman_dend, leaflab = 'none')
#vii)
# Cut the dendrogram into 4 clusters
clusters <- cutree(spellman_dend, k = 4)
# Print the cluster assignments
table(clusters)
print(clusters[1:6])
#viii)
# Color the branches with 4 clusters
plotc <- color_branches(spellman_tree, k = 4)
plot(plotc, leaflab = 'none')
# Repeat for 8 clusters and print frequency table
plotc_8 <- color_branches(spellman_tree, k = 8)
plot(plotc_8, leaflab = 'none')
table(cutree(spellman_tree, k = 8))
#ix)
# Create dataframe of gene names and cluster numbers
clusters_df <- data.frame(gene = names(clusters), cluster = clusters)
# Print the cluster for gene 'YALO22C'
print(clusters_df[clusters_df$gene == "YALO22C", ])
#x)
# Get all genes in the 3rd cluster
cluster3_genes <- filter(clusters_df, cluster == 3)$gene
print(cluster3_genes)
#xi)
# Reshape the data to long format
spellman_long <- gather(spellman, gene, expression, -expt, -time)
# Generate color scheme
color_scheme <- rev(brewer.pal(8, "RdBu"))
# Create heatmap for cluster 3 genes, for the "alpha" experiment
spellman_long %>%
  filter(gene %in% cluster3_genes & expt == "alpha") %>%
  ggplot(aes(x = time, y = gene)) +
  geom_tile(aes(fill = expression)) +
  scale_fill_gradientn(colors = color_scheme, limits = c(-2, 2)) +
  theme(axis.text.y = element_text(size = 6)) # Set size of y axis labels
#xii)
#a)
# Extract lower sub-trees
sub_trees <- cut(spellman_dend, h = 1.48)
#b)
# Extract and print 3rd sub-tree
cluster3_tree <- sub_trees$lower[[3]]
print(cluster3_tree)
#c)
# Plot the 3rd sub-tree
cluster3_tree %>%
  set("labels_cex", 0.45) %>%
  set("labels_col", "red") %>%
  plot(horiz = TRUE) # plot horizontally
#d)
# Load gplots package for heatmap
library(gplots)

# Filter data for "alpha" experiment
alpha_factor <- filter(spellman, expt == "alpha")
alpha_mtx <- as.matrix(dplyr::select(alpha_factor, -time, -expt))
row_names(alpha_mtx) <- alpha_factor$time
transposed_alpha_mtx <- t(alpha_mtx)
# Generate heatmap with dendrogram
heatmap.2(transposed_alpha_mtx,
          Rowv = cluster3_tree, # use the dendrogram previously calculated
          Colv = NULL, # don't mess with my columns
          dendrogram = "row", # only draw row dendrogram
          breaks = seq(-2, 2, length.out = 9), # set break points for colors
          col = color_scheme, # use previously defined colors
          trace = "none", density.info = "none", # remove distracting elements
          xlab = "Time (mins)")


#Question 2
#i)
# Load necessary packages
library(tidyverse)
library(ggplot2)
library(dplyr)
#ii)
# Read the data
listings <- read.csv("listings_airbnb.csv")
# Print the number of rows and column names
dim(listings)
names(listings)
#iii)
# Scatter plot of price vs number_of_reviews
ggplot(listings, aes(number_of_reviews, price, color = room_type, shape = room_type)) +
  geom_point(alpha = 0.25) +
  xlab("Number of reviews") +
  ylab("Price")
#iv)
# Normalize the price and number_of_reviews columns
listings[, c("price", "number_of_reviews")] <- scale(listings[, c("price", "number_of_reviews")])
#v)
# Create subset of data with price, minimum nights, and number of reviews
airbnb_2cols <- listings %>% select(price, minimum_nights, number_of_reviews)
print(airbnb_2cols)
#vi)
# Perform K-means clustering
set.seed(23)
km_out <- kmeans(na.omit(airbnb_2cols), centers = 3, nstart = 20)
print(km_out)
#vii)
# For loop to calculate WSS for different numbers of clusters
nclusters_max <- 10
wss <- numeric(nclusters_max)

for (k in 1:nclusters_max) {
  km_out <- kmeans(na.omit(airbnb_2cols), centers = k, nstart = 20)
  wss[k] <- km_out$tot.withinss
}

# Create a data frame for scree plot
wss_df <- tibble(clusters = 1:nclusters_max, wss = wss)

# Scree plot
scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')

# Display the scree plot with a dashed line for WSS
scree_plot + geom_hline(yintercept = wss[5], linetype = 'dashed', col = '#FF0000')
#viii)
# Perform K-means with 5 clusters
km_out <- kmeans(na.omit(airbnb_2cols), centers = 5, nstart = 20)
km_out$cluster_id <- factor(km_out$cluster)

# Plot the results
ggplot(km_out, aes(number_of_reviews, price, color = cluster_id)) +
  geom_point(alpha = 0.25) +
  xlab("Number of reviews") +
  ylab("Price")
#viii)
# Define the range of clusters to test
nclus_max <- 10
wss <- numeric(nclus_max)

# Calculate WSS for different numbers of clusters
for (i in 1:nclus_max) {
  km_out <- kmeans(na.omit(airbnb_2cols), centers = i, nstart = 20)
  wss[i] <- km_out$tot.withinss
}

# Create a tibble to store the number of clusters and corresponding WSS values
wss_df <- tibble(clusters = 1:nclus_max, wss = wss)

# Create the scree plot
scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')

# Show the scree plot with a horizontal line indicating the WSS for the optimal cluster (k=5)
scree_plot +
  geom_hline(
    yintercept = wss[5],  # WSS value at the optimal k (k=5)
    linetype = 'dashed',
    col = '#FF0000'
  )
#ix)
# Perform K-means clustering with k = 5
set.seed(23)
km_out_5 <- kmeans(na.omit(airbnb_2cols), centers = 5, nstart = 20)

# Add the cluster assignments to the dataset
airbnb_2cols$cluster_id <- factor(km_out_5$cluster)

# Plot the data points according to their cluster assignments
ggplot(airbnb_2cols, aes(number_of_reviews, price, color = cluster_id)) +
  geom_point(alpha = 0.25) +
  xlab("Number of reviews") +
  ylab("Price")






