#Lab2
#1
round(12.1343,digits=3)
round(123.12344,digits=3)
round(1234.12344,digits=3)
round(12345.12344,digits=3)
options(digits=15)
formatC(round(12345.12344,digits=3),format="f",digits=3)
print(1234.12344) #default print number is 7 significant digits, when we do options(digits=15, R  displays more significant digits, revealing the underlying floating-point approximation (1234.12344000000007))
print(1234.723,digits=3)
print(1234.223,digits=3)
print(34.223,digits=2)
print(1234.723,digits=5) #tried with a number 
round(123456788.123,digits=3)
print(round(123456788.123,digits=2),digits=20)
print(round(123456789.1234,digits=4),digits=20)
paste("Hello World")
paste("Hello","World")
paste(1:10)
paste(1:10)[4]
as.numeric(paste(1:10)) #convert strings as numbers
paste(c("Hello","World"),1:10,sep="-")
print(paste("Hello",1:10,sep="-"))


#2
#i
seq(0:10)
0:10
#ii
seq(15:5)
15:5
#iii
seq(0,1.5,0.1)
#iv
seq(6,4,-0.2)
#v
N <- c(55,76,92,103,84,88,121,91,65,77,99)
#vi
seq(from=0.04,by=0.01,length=11)
seq(0.04,by=0.01,along=N)
#vii
seq(from=0.04,to=0.14,along=N)
#viii
sequence(c(4,3,4,4,4,5))
#ix
rep(9,5)
rep(1:4,2)
rep(1:4,each=2)
rep(1:4,each=2,times=3)
rep(1:4,1:4)
#x
rep(1:4,c(4,1,4,2))
rep(c("cat","dog","goldfish","rat"), c(2,3,2,1,3))
#xi
seq(-1,1,by=0.1)
#xii
seq(-1,1,length=7)
#xiii
-1:1

#3
#i
3/0
#ii
exp(-Inf)
#iii
(0:3)**Inf
#iv
0/0
#v
Inf - Inf
#vi
Inf/Inf
#vii
is.finite(10) #true
#viii
is.infinite(Inf)
#ix
is.infinite(10)
#x
y<- c(4,NA,7)
y=="NA"
is.na(y)
#xi
y[!is.na(y)]
#xii
c1<- c(1,2,3,NA)
c2<- c(5,6,NA,8)
c3<- c(9,NA,11,12)
c4<- c(NA,14,15,16)
full.frame <- data.frame(c1,c2,c3,c4)
reduced.frame <- full.frame[! is.na(full.frame$c1),] #removes row containing NA from C1
#xiii
v <- c(1:6,NA,NA,9:12)
seq(along=v)[is.na(v)]
which(is.na(v))

#4
vec <- c(4,7,6,5,6,7)
#trying using scan:
vec2 <- scan()
vec2
vec1 <- c("Meena", 1, 2, 4 ,5)
class(vec)
class(vec1)
length(vec)
min(vec)
max(vec)
ind <- c(2,3,4)
vec1[ind]
vec3 <- c(3,1,5,3,8,9,0,2,3,4,5)
vec3[-1]
vec3 #it does not modify original vector
vec3[-length(vec3)]
trim <- function(x) {x <- sort(x); x[3:(length(x)-2)]} #saving sorted x to original x.
trim(vec3)
vec3[1:3]
vec3[seq(2,length(vec3),2)]
vec3[1:length(vec3)%%2==0]
which.max(vec3)
which.min(vec3)
cbind(1:10,10:1)
rbind(1:10,10:1)
X <- c(1:10)
X
Y <- c(1:10*5)
Y
X*Y
X+Y
X/Y
X^Y
log(X)
exp(Y)

#5
Y <- 1:24
dim(Y) <- c(2,12)
Y
X <- matrix(c(1,0,0,0,1,0,0,0),nrow=3)
print(X)
vector <- c(1,2,3,4,4,3,2,1)
V <- matrix(vector,byrow=T,nrow=2)
V
C <- matrix(vector,byrow=F,nrow=4)
C
dim(vector) <- c(4,2)
vector
is.matrix(vector)
min(vector)
max(vector)
sum(vector)
range(vector)
sort(vector)
A <- 1:50
dim(A) <- c(2, 5, 5)
A
matNA <- A
matNA[2,2,3] <- NA
matNA
matNA %*% A
Z <- X[1:4] %o% Y[1:3]
Z
YoX <- Y[1:3] %o% X[1:4]
print(YoX)
t(Z) 
t(YoX)
V %*% C
result <- sum(V*t(C))
print(result)
crossprod(X[1:4],Z)
print(diag(4))
print(class(X))

















