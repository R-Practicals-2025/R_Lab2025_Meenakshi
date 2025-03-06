install.packages("stringr")
library(stringr)

a <- "abc"
b <- "123"
print(is.numeric(a))
print(length(a))
print(nchar(a))
print(class(a))
print(is.factor(a))
print(letters)
print(LETTERS)
print(noquote(letters))
print(which(letters=="n"))
phrase <- "the quick brown fox, jumps over the lazy dog"
substr(phrase,1,1)

subs <- function(ph){
  for (i in 1:nchar(ph)) print(substr(ph,1,i))
}

print(subs("the quick brown fox jumps over the lazy dog"))

print(strsplit(phrase,split=" "))
print(strsplit(phrase,split=""))

print(table(lapply(strsplit(phrase,split=" "),nchar)))
print(lapply(strsplit(phrase,split=" "),nchar))

strsplit(phrase,split=NULL) #empty character different from null

func <- function(x) sapply(lapply(strsplit(x, split = ""), rev), paste, collapse = "")
print(func(phrase))

print(length(strsplit(phrase,split=" ")))
print(strsplit(phrase,split=" "))

vec1 <- strsplit(phrase,split=" ") 

print(length(strsplit(phrase,split=" ")[[1]]))

words <- 1+table(strsplit(phrase,NULL))[1]
print(words)

phrase <- "the quick brown fox, jumps over the lazy dog" #comma is considered as an extra character and not a separator
table(strsplit(phrase,split=NULL))

print(str(table(strsplit(phrase,split=NULL))))

first <- c(5,8,5,3,3,6,4,4,2,8,8,8,4,6)
second <- c(8,6,4,2)
match(first,second)

subjects <- c("A","B","G","M","N","S","T","V","Z")
suit_patients <- c("E","G","S","U","Z")
matched_sub <- match(subjects,suit_patients)

y <- c(1,2,-1,2,4,8,6,7,23,-9,10,-23)
z <- ifelse(y<0,-1,1)                  #if else
print(z)

x <- ifelse(y>median(y),"big","small")
print(median(y))
print(x)

drug <- c("conventional","new")
drug_suggested <- drug[ifelse(is.na(matched_sub),2,1)]

text <- c("arm","leg","head","foot","hand","hindleg","elbow")
gsub('h','H',text)
sub("o","O",text)

s <- "filename.doc"
strsplit(s,split=".")

s1 <- "filename.doc"
strsplit(s1,split="\\.")

#grep
print(grep("O{1}",text,value=T))
print(grep("ha{1}",text,value=T))

print(grep("[[:alnum:]]{4}",text,value=T))

#truncating a string
string2 <- "ABCDEFG"
trimmed <- strtrim(string2,4)
print(trimmed)

#lab exercises:

#excercise1
is_palindrome <- function(num) {
  num_str <- as.character(num)
  return(num_str == paste(rev(strsplit(num_str, "")[[1]]), collapse = ""))
}

print(is_palindrome(121))  
print(is_palindrome(123))  

#excercise2
s <- "seemerightnow"

substring1 <- substr(s, 1, 3)  
substring2 <- substr(s, 4, 5)  
substring3 <- substr(s, 6, 10) 

print(substring1) 
print(substring2)
print(substring3) 

#excercise3

content <- function(sequence) {
  count <- sum(strsplit(sequence, "")[[1]] %in% c("G", "C"))
  return(count / nchar(sequence))
  library(stringr)
  cou <-str_count("ATTGAGCGCGC",pattern="GC")
}

dna_seq <- "ATTGCGCATAGTCCGGG"
print(content(dna_seq))  
print(cou)
#excercise4

dna<- function(seq) {
  complement_map <- c(A="T", T="A", G="C", C="G")
  sequence <- sapply(strsplit(seq, "")[[1]], function(x) complement_map[x])
  return(paste(rev(sequence), collapse = "") == seq)
}

print(dna("TGGATCCA"))  

#excercise5
largest_words <- function(sentence) {
  words <- unlist(strsplit(sentence, "\\s+"))
  word_lengths <- nchar(words)
  
  max_length <- max(word_lengths)
  second_max_length <- max(word_lengths[word_lengths < max_length])
  
  largest_words <- words[word_lengths == max_length]
  second_largest_words <- words[word_lengths == second_max_length]
  
  return(list(Largest = largest_words, Second_Largest = second_largest_words))
}

sentence <- "She sells hundreds of sea oysters on the sea shore."
result <- largest_words(sentence)
print(result)


#excercise6
#Ex6-Load the data in ‘worldfloras.txt’ and do the following.
#(a) Create subsets of countries within the same continent and store the data (ie. the allied columns) as different dataframes.
# Load the data
world_data <- read.table("worldfloras.txt", header = TRUE, sep = "\t")
# Check the structure of the data
str(world_data)
# Create subsets for each continent (assuming the continent is named "Continent" and country is "Country")
asia_data <- subset(world_data, Continent == "Asia")
europe_data <- subset(world_data, Continent == "Europe")
africa_data <- subset(world_data, Continent == "Africa")
america_data <- subset(world_data, Continent == "America")
oceania_data <- subset(world_data, Continent == "Oceania")
# View a few rows of one of the dataframes as a sample
head(asia_data)


#(b) Make a boxplot of the distribution of floral count within each continent and print the statistical summary. What are the mean and standard deviation values? Also calculate and comment on the skewness and kurtosis parameters (interpret them)
# Boxplot for floral count distribution within each continent
boxplot(Flora ~ Continent, data = world_data, main = "Floral Count Distribution by Continent", xlab = "Continent", ylab = "Floral Count", col = "lightgreen", border = "black")
summary_stats <- aggregate(Flora ~ Continent, data = world_data, summary)
print(summary_stats)
mean_floral <- mean(world_data$Flora)
sd_floral <- sd(world_data$Flora)
print(mean_floral)
print(sd_floral)
library(e1071)
floral_skewness <- skewness(world_data$Flora)
floral_kurtosis <- kurtosis(world_data$Flora)
print(floral_skewness) #skewness is 3.835473 which indicates that the data has a long tail to the right
print(floral_kurtosis) #kurtosis is 19.1006 which indicates a more peaked distribution than a normal distribution (leptokurtic)


#(c) Make a boxplot and histogram plot of the population distribution within each con-tinent and print the statistical summary. Calculate and comment on the skewnessand kurtosis parameters (interpret them). Does this have any relation with the floral count data?
boxplot(Population ~ Continent, data = world_data, main = "Population Distribution by Continent", xlab = "Continent", ylab = "Population", col = "lightgreen", border = "black")
hist(world_data$Population, main = "Population Distribution", xlab = "Population", col = "purple", border = "black")
population_stats <- aggregate(Population ~ Continent, data = world_data, summary)
print(population_stats)
population_skewness <- skewness(world_data$Population)
population_kurtosis <- kurtosis(world_data$Population)
print(population_skewness)
print(population_kurtosis)
#Population distributions tend to be skewed right because of the small number of countries with extremely high populations.
# Population distributions can have heavy tails (high kurtosis), meaning a few countries with very large populations may significantly affect the distribution.

#(7) Read in the data from ‘HumanBones.txt’ and group the data into categories “Chest”,“Spine”,“Skull”, “Ear Bones”, “Arms” and “Legs”. The number in the brackets indicates the number of bones in that type. Create a dataframe with 3 columns- category, name of the bone and number of bones.
bones_data <- read.table("HumanBones.txt", header = FALSE,sep = "\t", stringsAsFactors = FALSE)
print(head(bones_data))

categories <- c()
bone_names <- c()
bone_numbers <- c()
current_category <- NULL
for (i in 1:nrow(bones_data)) {
  line <- bones_data$V1[i]
  if (!grepl("\\(", line)) {
    current_category <- line
  } else {
    bone_info <- strsplit(line, "\\(")[[1]]
    bone_name <- trimws(bone_info[1])  # Get the bone name
    bone_number <- gsub("[^0-9]", "", bone_info[2])  # Extract the number of bones
    categories <- c(categories, current_category)
    bone_names <- c(bone_names, bone_name)
    bone_numbers <- c(bone_numbers, as.numeric(bone_number))
  }
}
bones_info <- data.frame(category = categories, name_of_bone = bone_names, number_of_bones = bone_numbers, stringsAsFactors = FALSE)
head(bones_info)

#Ex8
category_bones_summary <- aggregate(number_of_bones ~ category, data = bones_info, sum)
max_category <- category_bones_summary[which.max(category_bones_summary$number_of_bones), ]
print(max_category$category) 
category_frequency <- table(bones_info$category)
print(category_frequency)
barplot(category_bones_summary$number_of_bones, names.arg = category_bones_summary$category, main = "Number of Bones by Category", xlab = "Category", ylab = "Number of Bones", col = "pink")

#Ex9
legs_data <- subset(bones_info, category == "Legs")
long_bones_legs <- subset(legs_data, nchar(name_of_bone) > 5)
print(long_bones_legs$name_of_bone)

#Ex10
bones_starting_M <- subset(bones_info, grepl("^M", name_of_bone))
bones_starting_M$name_of_bone <- gsub("a", "A", bones_starting_M$name_of_bone)
print(bones_starting_M$name_of_bone)

#Ex11
bones_ending_with_e <- subset(bones_info, grepl("e$", name_of_bone))
bones_ending_with_e$name_of_bone <- tolower(bones_ending_with_e$name_of_bone)
print(bones_ending_with_e$name_of_bone)

#Ex12
bones_with_two_o <- subset(bones_info, grepl("o.*o", name_of_bone))
print(bones_with_two_o$name_of_bone)
