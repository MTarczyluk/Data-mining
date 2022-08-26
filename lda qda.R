# Load the data sets
BankTest=read.csv("BankTest.csv", header = TRUE)
BankTrain=read.csv("BankTrain.csv", header = TRUE)

# Explore data sets
names(BankTest)
names(BankTrain)

dim(BankTest)
dim(BankTrain)

summary(BankTest)
summary(BankTrain)

pairs(BankTrain)

cor(BankTrain$x1, BankTrain$y)
cor(BankTrain[, -6])

# Perform linear discriminant analysis (LDA) 
library(MASS)
lda.fit <- lda(y ~ x1 + x3, data = BankTrain)
lda.fit
plot(lda.fit)

# Make predictions using testing data
lda.pred <- predict(lda.fit, BankTest)

# Compute confusion matrix
lda.class <- lda.pred$class
table(lda.class, BankTest$y)
mean(lda.class == BankTest$y) # Fraction of data for which prediction was correct
mean(lda.class != BankTest$y) # Test set error rate

# Perform quadratic discriminant analysis (QDA) 
qda.fit <- qda(y ~ x1 + x3, data = BankTrain)
qda.fit

# Make predictions using testing data
qda.pred <- predict(qda.fit, BankTest)

# Compute confusion matrix
qda.class <- qda.pred$class
table(qda.class, BankTest$y)
mean(qda.class == BankTest$y) # Fraction of data for which prediction was correct
mean(qda.class != BankTest$y) # Test set error rate

