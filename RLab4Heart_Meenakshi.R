#ex1
data=read.csv("/home/ibab/Downloads/Heart.csv",header=TRUE)
#ex-2
print(data)
print(dim(data))
print(length(data))
print(colnames(data))
print(rownames(data))
print(head(data,30))
print(table(data$ChestPain))
print(table(data$Chol))
print(data$ChestPain[1:4])
print(levels(data$sex))
data$sex = factor(data$Sex,levels=c(0,1))
print(is.factor(data$Sex))
#ex3
print(mean(data$RestECG))
print(mean(data$Chol))
print(median(data$RestECG))
print(mode(data$RestECG))
print(which.max(data$RestECG))
print(sd(data$RestECG))
print(summary(data$RestECG))
hist(data$RestBP)
library(moments)
print(skewness(data$RestECG))
print(kurtosis(data$RestECG))
boxplot(data$RestECG)
boxplot(data$RestECG,xlabel="spread of gtv",ylabel="GTV",horizontal=TRUE,border=c("blue"),col=c
        ("yellow"))

boxplot(data$RestECG,range=0.1,xlabel="spread of gtv",ylabel="GTV",horizontal=FALSE,border=c("blue"),col=c
        ("yellow"))
boxplot(data$RestECG,range=0.2,xlabel="spread of gtv",ylabel="GTV",horizontal=FALSE,border=c("blue"),col=c
        ("yellow"))
boxplot(data$RestECG,range=0.05,xlabel="spread of gtv",ylabel="GTV",horizontal=FALSE,border=c("blue"),col=c
        ("yellow"))
boxplot(data$Chol)
boxplot(data$Fbs)
#RESTECG have broadest distribution
#ex4
filter1=subset(data,data$chol>20)
print(filter1)
print(dim(filter1))
filter2=unique(subset(data,data$chestpain=="typical"))
print(filter2)
filter3=data[c(1,3,8,9,13,14,18,21),]
print(filter3)
filter4_ind=which(data$sex=="0")
print(filter4_ind)
filter4=data[filter4_ind,]
print(filter4)
print(data$gtv*data$Chol/234)

write.csv(filter4,file="lab4 female heart.csv")
new_data<-data.frame(Chol=data$Chol ,RestBP=data$RestBP,newColumn=data$Chol*data$RestBP/234)
print(dim(new_data))
print(new_data)
