install.packages("readxl")
library(readxl)
data2 = read_excel("/home/ibab/R/pone.0148733.s001.xlsx",1)
print(names(data2))

a <- 5
b <- 10
if(a<b) 
  print("a is less than b")

c <- 15
d <- 20

if(a>b)
{
  print("a is greater than b")
}
  
if(a>b)
{
  print("a>b")
  
}else if(b<c)
{
  print("b<c")
}

for(i in 1:5) print(i*2)
j=0
k=1
for(i in 1:5){
  j <- j+1
  k <- k+2
  print(paste(j,k))
}


fac1 <- function(x){
  f <- 1
  if(x<2) return(1)
  for(i in 2:x){
    f <- f*i
  }
  return(f)
}
print(fac1(4))
sapply(0.5,fac1)

fac2 <- function(x){
  f<-1
  t<-x
  while(t>1){
    f<-f*t
    t<-t-1
  }
  return(t)
}
print(fac2(5))

fac3 <- function(x){
  f<-1
  t<-x
  repeat{
    if(t<2) break
    f <- f*t
    t<-t-1
  }
return(t)
}
print(fac3(5))

pc <- proc.time()
result1 <- fac1(25)
proc.time()

x <- runif(10000000)
pc <- proc.time()
cmax <- x[1]
for(i in 10000000){
  if(x[i] > cmax) cmax <- x[i]
}
proc.time()-pc

setA <- c("a","b","c","d","e")
setB <- c("d","e","f","g")

union(setA, setB)
setA[setA%in%setB]

c(setdiff(setA,setB),intersect(setA,setB),setdiff(setB,setA))

#(1) Solving matrix equations and review of matrix operations.

# (i) Forming the matrices amat and amat2
amat <- matrix(seq(10, 120, by=10), nrow=3, ncol=4, byrow=TRUE)
print(amat)

amat2 <- matrix(seq(10, 120, by=10), nrow=3, ncol=4, byrow=FALSE)
print(amat2)

# (ii) Assigning row and column names
rownames(amat) <- c("R1", "R2", "R3")
colnames(amat) <- c("C1", "C2", "C3", "C4")
print(amat)

# (iii) Forming matrices A and B and performing operations
A <- matrix(c(2,5,7,3,1,8,9,10,1,12,5,10,4,17,15,11), nrow=4, ncol=4, byrow=TRUE)
B <- matrix(c(12,5,3,17,1,18,9,10,1,12,5,10,4,15,15,4), nrow=4, ncol=4, byrow=TRUE)
print(A)
print(B)

# Element-wise multiplication
C_elementwise <- A * B
print(C_elementwise)

# Matrix-matrix multiplication
C_matrixmult <- A %*% B
print(C_matrixmult)

# (iv) Outer and inner product of vectors X and Y
X <- c(5,6,8,9)
Y <- c(8,10,12,5)
outer_product <- outer(X, Y)
print(outer_product)

inner_product <- sum(X * Y)
print(inner_product)

# (v) Creating a diagonal matrix using X
diag_matrix <- diag(X)
print(diag_matrix)

# (vi) Printing diagonal elements of A
diag_elements <- diag(A)
print(diag_elements)

# (vii) Creating a 6x6 identity matrix
identity_matrix <- diag(6)
print(identity_matrix)

# (viii) Creating a 3x3 matrix A
A <- matrix(c(3,4,-2,4,-5,1,10,-6,5), nrow=3, ncol=3, byrow=TRUE)
print(A)

# (ix) Creating a 3x1 matrix B
B <- matrix(c(5,-3,13), nrow=3, ncol=1)
print(B)

# (x) Solving for X in AX = B
X <- solve(A, B)
print(X)
print(typeof(X))

# (xi) Finding the inverse of A and verifying
Ainv <- solve(A)
print(Ainv)
identity_check <- Ainv %*% A
print(identity_check)

# (xii) Finding eigenvalues and eigenvectors
eigen_results <- eigen(A)
print(eigen_results)

# Checking type of eigen_results
print(class(eigen_results))

# Matrix-vector multiplication with the second eigenvector
eigenvector2 <- eigen_results$vectors[,2]
matrix_vector_result <- A %*% eigenvector2
print(matrix_vector_result)


#5 subsets

# (i)
vec <- c(8,10,12,7,14,16,2,4,9,19,20,3,6)
# (a)
vec[vec > 12]
# (b) 
vec[vec > 10 & vec < 20]

# (ii) 
A <- c(2,7,29,32,41,11,15,NA,NA,55,32,NA,42,109)
# Remove NA values and keep values less than 100
A_filtered <- c()
for (i in A) {
  if (!i == NA && i < 100) {
    A_filtered <- c(A_filtered, i)
  }
}
print(A_filtered)

# (iii) 
A[is.na(A)] <- 0
print(A)

# (iv)
genes <- c("gene-1", "gene-2", "gene-3", "gene-4", "gene-5", "gene-6", "gene-7")
gender <- c("M", "M", "F", "M", "F", "F", "M")

# (v)
result1 <- c(12.3, 11.5, 13.6, 15.4, 9.4, 8.1, 10.0)
result2 <- c(22.1, 25.7, 32.5, 42.5, 12.6, 15.5, 17.6)
result3 <- c(15.5, 13.4, 11.5, 21.7, 14.5, 16.5, 12.1)
result4 <- c(14.4, 16.6, 45.0, 11.0, 9.7, 10.0, 12.5)
result5 <- c(12.2, 15.5, 17.4, 19.4, 10.2, 9.8, 9.0)
result6 <- c(13.3, 14.5, 21.6, 17.9, 15.6, 14.4, 12.0)
result7 <- c(11.0, 10.0, 12.2, 14.3, 23.3, 19.8, 13.4)

# (vi)
datframe <- data.frame(genes, gender, result1, result2, result3, result4, result5, result6, result7)

# (vii)
colnames(datframe) <- c("GeneName", "Gender", "expt1", "expt2", "expt3", "expt4", "expt5", "expt6", "expt7")

# (viii)
subset_expt2_gt20 <- subset(datframe, expt2 > 20)
print(subset_expt2_gt20)

# (ix)
subset_female <- subset(datframe, Gender == "F")
print(subset_female)

# (x)
subset_male_expt2_lt30 <- subset(datframe, Gender == "M" & expt2 < 30)
print(subset_male_expt2_lt30)

#6: if else

find_quadrant <- function(angle) {
  if (angle >= 0 & angle < 90) {
    print("First quadrant")
  } else if (angle >= 90 & angle < 180) {
    print("Second quadrant")
  } else if (angle >= 180 & angle < 270) {
    print("Third quadrant")
  } else if (angle >= 270 & angle < 360) {
    print("Fourth quadrant")
  } else {
    print("Invalid angle")
  }
}

sort_numbers <- function(a, b, c) {
  if (a >= b & a >= c) {
    if (b >= c) {
      print(c(a, b, c))
    } else {
      print(c(a, c, b))
    }
  } else if (b >= a & b >= c) {
    if (a >= c) {
      print(c(b, a, c))
    } else {
      print(c(b, c, a))
    }
  } else {
    if (a >= b) {
      print(c(c, a, b))
    } else {
      print(c(c, b, a))
    }
  }
}

calculate_ticket_price <- function(distance, age) {
  if (distance <= 100) {
    cost <- 100
  } else if (distance <= 1000) {
    cost <- 100 + (distance - 100) * 1.5
  } else {
    cost <- 100 + (900 * 1.5) + (distance - 1000) * 2
  }
  
  if (age > 60) {
    cost <- cost * 0.75  # 25% discount
  } else if (age < 6) {
    cost <- cost * 0.5   # 50% discount
  }
  
  print(paste("Ticket cost: Rs.", round(cost, 2)))
}


angle <- as.integer(readline("Enter angle: "))
find_quadrant(angle)

sort_numbers(12,10,4)

calculate_ticket_price(1300, 21)

#7:
replace_negatives <- function(vec) {
  vec[vec < 0] <- 0
  return(vec)
}

stirling_factorial <- function(n) {
  if (n <= 0) return(NA)
  sqrt_term <- sqrt(2 * pi * n)
  power_term <- (n / exp(1))^n
  correction <- 1 + 1 / (12 * n) + 1 / (288 * n^2) - 139 / (51840 * n^3) - 571 / (2488320 * n^4)
  return(sqrt_term * power_term * correction)
}

sum_digits <- function(num) {
 return(sum(as.integer(unlist(strsplit(as.character(num), "")))))
}

print(replace_negatives(1,2,3,-3,-42,1,-5,9,-8)

print(stirling_factorial(5))

print(sum_digits(1,2,3,4,5,6))


