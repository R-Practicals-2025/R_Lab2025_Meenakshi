#Lab 1 3rd Jan 2025
#1.1
print(2.7/2)
#1.2
print(2.7%/%2)
#1.3
print(10+5i/2)
#1.4
print(round(2.5))
#1.5
print(round(-2.5))
#1.6
print(2%/%4-1)
#1.7
print(3*2**2)
#1.8
print(3**2*2)
#1.9
print(7%/%4)
#1.10
print(7%%4)
#1.11
print(-7%%4)
#1.12
print(trunc(5.7))
#1.13
print(trunc(-5.7))
print(-9%%4)

#2
fun <- function(x)floor(x+1)
fun(6.5)

#3
a <- 1
b <- 2
c <- 4
a && b
!a < b | c > b

#4
x<- c(5,3,7,8)   #4.1
is.integer(x)    #4.2
is.numeric(x)    #4.3
x<-integer(x)   #4.4
print(x)  #length error because x is a vector.
x<-as.integer(x)   #4.5
is.integer(x)
#double data type is now converted to integer and gives TRUE.

#5
x <- sqrt(2)    #5.1
x*x ==2
#FALSE, due to floating-point precision errors.
x*x - 2
all.equal(x*x,2)















