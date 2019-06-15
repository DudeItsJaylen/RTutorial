# Load Raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a survived variable to test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

# Combine Data Sets
data.combined <- rbind(train, test.survived)

# A Bit about R data types combined
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)


# Take a look at gross survival rates
table(data.combined$Survived)

#Spits out the Distribution across classes
table(data.combined$Pclass)

#load ggplot2 package to use for visualizations
library(ggplot2)

# Hypothesis - Rich folks survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")


# Examine the first few names in the training data set
head(as.character(train$Name))

# How many unique names are there across both tain & test?
length(unique(as.character(data.combined)))

#Two Duplicate Names,take a closer look
#First, get the duplicate names and store them as a vector
dup.Names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

# Next, take a look at the records in the combined data set
data.combined[which(data.combined$Name %in% dup.Names),]

#what is up eith the "Miss. and Mr" thing?
library(stringr)

#is there any correlation with other variables?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss")),]
misses[1:5,]

#Hypothesis - Name titles correlation with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]


#Check out the males to see if the pattern continues
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]

# expanding upon the relationship between "Survived" and "Pclass" by adding the new
# 'Title' variable data set anf then explore a potential 3-dimensional relationship


extractTitle <- function(Name) {
  Name <- as.character(Name)
  
  if(length(grep("Miss", Name)) > 0 ) {
    return("Miss")
  } else if (length(grep("Master.", Name)) > 0) {
    return("Master")
  } else if (length(grep("Mrs.", Name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr.", Name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles,extractTitle(data.combined[i,"Name"]))
}
data.combined$title <- as.factor(titles)


#Since we only have survived lables for the reain set, only use the
# first 891 rows
ggplot(data.combined[1:891,], aes(x = title,fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")







