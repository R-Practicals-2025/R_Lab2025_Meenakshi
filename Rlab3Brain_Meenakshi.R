#reading the file
#1)
data=read.csv("~/Desktop/biostat/BrainCancer.csv", header=TRUE)
#2)
print(data)
print(dim(data))
print(length(data)) #cols
print(rownames(data))
print(colnames(data))
print(head(data, 30))
#print(unique(data))
#5 categorical variables are present in the dataset

print(table(data$diagnosis))   #frequency table
#sex, stereo, diagnosis, loc are the categorical variables
data$diagnosis <- as.factor(data$diagnosis)
data$sex <- as.factor(data$sex)
data$loc <- as.factor(data$loc)
data$stereo <- as.factor(data$stereo)
print(lapply(data[c("diagnosis", "sex", "loc", "stereo")], levels))

#3
print(mean(data$gtv))
print(mean(data$time))
print(median(data$gtv))  #since mean is greater than median, it is right skewed data.
print(which.max(data$gtv))
print(sd(data$gtv))
print(summary(data$gtv))
print(hist(data$gtv))
library(moments)
print(skewness(data$gtv))
print(kurtosis(data$gtv))
boxplot(data$gtv)
boxplot(range=0.1,data$gtv, xlabel="spread of GTV", ylabel="GTV", horizontal=FALSE, border = c('blue'), col=c('red'))
boxplot(range=0.2,data$gtv, xlabel="spread of GTV", ylabel="GTV", horizontal=FALSE, border = c('blue'), col=c('red'))
boxplot(range=0.05,data$gtv, xlabel="spread of GTV", ylabel="GTV", horizontal=FALSE, border = c('blue'), col=c('red'))
boxplot(data$time)
boxplot(data$ki)

#ranges 0.1 and 0.2 will show fewer outliers, and the time has the highest distribution.

fem_ind = which(data$sex=='FEMALE')
ind = data[fem_ind,]
print(ind)

data$new_col = (data$gtv*data$ki)/234
new_df = data[,c("gtv","ki","new_col")]
print(new_df)


write.csv(ind, file = "~/Desktop/biostat/lab4_female_BrainCancer.csv", row.names = FALSE) #ind df containing the rows named 'Female' was created before

