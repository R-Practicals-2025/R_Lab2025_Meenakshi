data=read.csv("~/Desktop/biostat/BrainCancer.csv", header=TRUE)

#5
data$X <- as.factor(data$X)
data$sex <- as.factor(data$sex)
data$diagnosis <- as.factor(data$diagnosis)
data$loc <- as.factor(data$loc)
#data$ki <- as.factor(data$ki)
#data$gtv <- as.factor(data$gtv)
data$stereo <- as.factor(data$stereo)
data$status <- as.factor(data$status)
#data$time <- as.factor(data$time)

class(data$X)
class(data$stereo)

print(nlevels(data$sex))
print(levels(data$diagnosis))

#6
print(length(rownames(data)))

data$Temperature <- factor(gl(3, ceiling(nrow(data) / 3), nrow(data), labels = c("Hot", "Cold", "Lukewarm")))

print(tapply(data$gtv, data$ki, mean))

ki100_ind = which(data$ki==100)
ki_100 = data[ki100_ind,]
print(ki_100)

ki_100$ki <- as.factor(ki_100$ki)

#7
print(tapply(ki_100$gtv, ki_100$ki, mean))

print(tapply(data$gtv, data$ki, mean, trim=0.1))  #trim=removes extreme values on both end before calulating the mean

#8
pmin(data$gtv, data$ki, data$time)

pmax(data$gtv, data$ki, data$time)

#9
ranks <- rank(data$gtv)
sorted <- sort(data$gtv)
ordered <- order(data$gtv)
view <- data.frame(data$gtv, ranks, sorted, ordered)

print(view)

ordered_diagnosis <- data$diagnosis[ordered]

newdf = data.frame(sorted, ordered_diagnosis)


sortedsubgtv = sort(ki_100$gtv)

print(mean(sortedsubgtv, trim=0.2))
print(mean(sorted, trim=0.4))    #does not change much even with trim unlike the original dataset bnecause there's nothing to trim from.

write.csv(newdf, file = "lab4_ordered_data.csv", row.names = FALSE)

#How Each Works:

#rank()

#Assigns rank positions to elements in the original data.
#Ranks: [3, 1, 4, 2]
#20 is the smallest → Rank 1.
#30 is the second smallest → Rank 2.
#50 is the third smallest → Rank 3.
#80 is the largest → Rank 4.

#sort()

#Rearranges values in ascending order.
#Sorted: [20, 30, 50, 80]

#order()

#Returns the positions (indices) that would sort the data.
#Order: [2, 4, 1, 3]
#The 2nd element (20) is the smallest.
#The 4th element (30) is next.
#The 1st element (50) follows.
#The 3rd element (80) is the largest.

#10:
subset_data <- data[1:6, 3:8]
matrix_data <- as.matrix(subset_data)  #Convert dataframe to matrix
print(matrix_data)
print(class(matrix_data))
newcol <- data$ki + data$gtv + data$time  #Add 3 col
print(newcol
newcoladded <- data  #Copy original df
newcoladded$newcol <- newcol  # Add newcol as last col
print(colnames(newcoladded))  #Check column names to confirm
newcoladded2 <- cbind(data, newcol)  # Bind newcol as a new column   #its just another method to add
print(colnames(newcoladded2))
new_rows <- data[c(26, 35), ]  # Select rows 26 and 35
newdata <- rbind(data, new_rows)  # Append selected rows to original data
print(newdata)  #Print the updated dataframe
print(dim(newdata))  #Print dimensions to confirm

#11:
X <- matrix(c(1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0), nrow = 4)
print(X)
print(rownames((X)))
print(colnames(X))

rownames(X) <- rownames(X, do.NULL= FALSE, prefix = 'Trail.')
print(rownames(X))


drugs <- c('as-pirin','paracetamol','nurofen','hedex','placebo')
colnames(X) <- drugs
print(colnames(X))
print(X)


#12:
# calculation on rows/col of matrix:
print(mean(X[,5]))
print(var(X[4,]))
print(rowSums(X))  #3 method 1
print(colSums(X))

# method 2:

print(apply(X, 1, sum))     # ----->1-->rows
print(apply(X, 2, sum))     # second argu-->2--col
print(apply(X, 2, sqrt))    

print(apply(X, 1, function(X)X^2+X))    # 1 --->transpose

print(rowMeans(X))
print(colMeans(X))
print(apply(X, 1, mean))
print(apply(X,2,mean))

group = c("A", "B", "B", "A")
print(rowsum(X, group))   # different from rowSums

print(row(X))  
print(col(X))

print(group[row(X)])
print(col(X))
print(tapply(X, list(group[row(X)], col(X)), sum))

print(aggregate(X, 2, sample))
X <- rbind(X,apply(X,2,mean))
print(X)
X <- cbind(X,apply(1,var))
print(X)
headings <- c(paste("drug.",1:5,sep=""),"var")
dimnames(X) <- list(NULL,headings)
print(X)

#13
#sweep() function
#to perform sweep action
data=read.csv("~/Desktop/biostat/BrainCancer.csv", header=TRUE)
eg_sweep = data.frame(data$ki,data$gtv,data$time)
cols <- apply(eg_sweep,2,mean)
print(cols)

cols.means <- matrix(rep(cols,rep(dim(eg_sweep)[1],dim(eg_sweep)[2])),
                     nrow=dim(eg_sweep)[1])
print(cols.means)
eg_sweep_alt <- eg_sweep - cols.means
print("Method 1")
print(eg_sweep_alt)

eg_sweep_alt2 <- sweep(eg_sweep,2,cols)
print("Method 2")
print(eg_sweep_alt2)

#sapply() used for vectors
eg_sapply <- sapply(3:7,seq)
print(attributes(eg_sapply))


#14:
data <- read.table("pgfull.txt", header = TRUE)
species <- data[, 1:54]
max_indices <- max.col(species)
print(max_indices)
species_names <- names(species)[max_indices]
print(species_names)
species_freq <- table(species_names)
print(species_freq)
min_indices <- max.col(-species)  #Negating the values in 'species'
print(min_indices)
#alt method:
min_indices <- apply(species, 1, which.min)
print(min_indices)


#trying something out
vec <- c(10, 20, 30)
names(vec) <- c("X", "Y", "Z")
print(names(vec))
print(vec)

#15
apples <- c(4,4.5,4.2, 5.1,3.9)
oranges <- c(TRUE,TRUE,FALSE)
chalk <- c("limestone","marl","oolite","CaCO3")
cheese <- c(3.2-4.5i,12.8+2.2i)
items <- list(apples, oranges, chalk, cheese)
print(items)
#subscripts on lists have double square brackets
print(items[[3]])
print(items[[3]][3])

items[3] #this works
items[3][3] #this doesnt
print(names(items))
items <- list(first=apples,second=oranges,third=chalk,fourth=cheese)
print(items$fourth)
print(class(items))
print(lapply(items,length))
print(lapply(items,class))
print(lapply(items,mean))






















