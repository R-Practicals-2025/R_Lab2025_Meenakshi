#question 1:

ops <- function(k,n){add <- k+n
add <- round(add, digits=2)       #rounding off the result to two decimal places.
sub <- k-n
sub <- round(sub, digits=2)
div <- k/n
div <- round(div, digits=2)
prod <- k%*%n
prod <- round(prod, digits=2)
result <- c(add,sub,div,prod)

return (result)
}

print(ops(4,6)) #example of working of the function.

#question 2:
root <- function(a,b,c){
x1 <- (((-b + (sqrt(b^2 -(4*a*c))))/2*a))
x1 <- round(x1, digits=2)
x2 <- (((-b - (sqrt(b^2 -(4*a*c))))/2*a))
x2 <- round(x2, digits=2)
result <- c(x1, x2)
return (result)
}

print(root(3,6,2))
print(root(1,-7,12))








