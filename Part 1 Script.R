# Load Raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a survived variable to test set to allow for combining data sets
test.survived <- data.frame(survived = rep("None", nrow(test)), test[,])

# Combine Data Sets
data.combined <- rbind(train, test.survived)

# A Bit about R data types combined
str(data.combined)

data.combined$survived <- as.factor(data.combined$survived)
data.combined$pclass <- as.factor(data.combined$pclass)


# Take a look at gross survival rates
table(data.combined$survived)

#Spits out the Distribution across classes
table(data.combined$pclass)

#load ggplot2 package to use for visualizations
library(ggplot2)

# Hypothesis - Rich folks survived at a higher rate
train$Pclass <- as.factor(train$pclass)
ggplot(train, aes(x = pclass, fill = factor(survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")


# Examine the first few names in the training data set
head(as.character(train$name))

# How many unique names are there across both tain & test?
length(unique(as.character(data.combined)))

#Two Duplicate Names,take a closer look
#First, get the duplicate names and store them as a vector
dup.Names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])

# Next, take a look at the records in the combined data set
data.combined[which(data.combined$name %in% dup.Names),]

#what is up eith the "Miss. and Mr" thing?
library(stringr)

#is there any correlation with other variables?
misses <- data.combined[which(str_detect(data.combined$name, "Miss")),]
misses[1:5,]

#Hypothesis - Name titles correlation with age
mrses <- data.combined[which(str_detect(data.combined$name, "Mrs.")),]
mrses[1:5,]


#Check out the males to see if the pattern continues
males <- data.combined[which(train$sex == "male"),]
males[1:5,]

# expanding upon the relationship between "Survived" and "Pclass" by adding the new
# 'Title' variable data set anf then explore a potential 3-dimensional relationship


extractTitle <- function(name) {
  name <- as.character(name)
  
  if(length(grep("Miss", name)) > 0 ) {
    return("Miss")
  } else if (length(grep("Master.", name)) > 0) {
    return("Master")
  } else if (length(grep("Mrs.", name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles,extractTitle(data.combined[i,"name"]))
}
data.combined$title <- as.factor(titles)


#Since we only have survived lables for the reain set, only use the
# first 891 rows
ggplot(data.combined[1:891,], aes(x = title,fill = survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")


# Begining of part 2


# Distribution of male and females on the ship
table(data.combined$sex)


# 3 way relationship between survial rates of sex and class

ggplot(data.combined[1:891,], aes(x = title,fill = survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

# age and sex both seem fairly important 
# Distribution of age

summary(data.combined$age)
summary(data.combined[1:891,"age"])

# There are ALOT of missing values in this dataset for age!

ggplot(data.combined[1:891,], aes(age, fill = survived)) +
  facet_wrap(~sex + pclass) +
  geom_histogram(binwidth = 10) +
  ggtitle("Pclass") +
  xlab("age") +
  ylab("Total Count")


boys <- data.combined[which(data.combined$title == "Master"),]
summary(boys$age)

misses <- data.combined[which(data.combined$title == "Miss"),]
summary(misses$age)

ggplot(misses[misses$survived != "None",], aes(x = age, fill = survived)) +
  facet_wrap(~pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss' by pclass") +
  xlab("age") +
  ylab("Total Count")

misses.alone <- misses[which(misses$sibsp == 0 & misses$parch == 0),]
summary(misses.alone$age)
length(which(misses.alone$age <= 14.5))

length(unique(data.combined$sibsp))
data.combined$sibsp <- as.factor(data.combined$sibsp)


ggplot(data.combined[1:891,], aes(x = sibsp, fill = survived)) +
  stat_count(width = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


data.combined$parch <- as.factor(data.combined$parch)

ggplot(data.combined[1:891,], aes(x = parch, fill = survived)) +
  stat_count(width = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Trying some feature engineering 

temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch <- c(train$sibsp, test$parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)

#visualizing the data 

ggplot(data.combined[1:891,], aes(x = family.size, fill = survived)) +
  stat_count(width = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Family Size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
