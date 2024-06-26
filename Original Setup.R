heart_og <- read.table("heart_disease3.txt")
heart <- heart_og

# Creating Categorical Variables
for (i in c(1:4, 6:9)){
  heart[,i] <- as.factor(heart[,i])
}

# Creating Ordered Factor for Education
for (i in c(11)){
  heart[,i] <- factor(heart[,i], ordered = TRUE)
}

# Excluding Categorical NA's
# Note: There are no continuous predictors with NA's in this dataset, 
# so mean imputation is actually not needed, just removal of rows with categorical NA's
heart_no_na <- na.omit(heart)
