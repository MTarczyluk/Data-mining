# Load the data sets
BankTest=read.csv("BankTest.csv", header = TRUE)
BankTrain=read.csv("BankTrain.csv", header = TRUE)

# Explor data sets
names(BankTest)
names(BankTrain)

dim(BankTest)
dim(BankTrain)

summary(BankTest)
summary(BankTrain)

pairs(BankTrain)

cor(BankTrain$x1, BankTrain$y)
cor(BankTrain[, -6])

# Perform logistic regression for training data
glm.fits <- glm(y ~ x1 + x3, data = BankTrain, family = binomial)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[, 4]

#Plot training data
group <- ifelse(BankTrain$y == 1, "Forged", "Genuine")
plot(BankTrain$x1, BankTrain$x3, pch= 10, cex=1, col=factor(group), main = "Bank Training Data", xlab = "x1", ylab = "x3")
legend(4, 15, legend=c("Forged", "Genuine"),
       col=c("black", "red"), pch = 10, cex=1,
       title="Legend", text.font=4,)


b = coef(glm.fits)
coef(glm.fits)
slope = -b[2]/b[3]
int = -b[1]/b[3]
abline(a=int, b=slope, col = 'black', lwd = 2)

# Make predictions using testing data
glm.probs=predict(glm.fits, BankTest, type="response")
glm.probs[1:10]

# Compute confusion matrix for probability = 0.5
glm.pred <- rep("0", 412)
glm.pred[glm.probs > .5] = "1"
table(glm.pred, BankTest$y)
mean(glm.pred == BankTest$y) # Fraction of data for which prediction was correct
mean(glm.pred != BankTest$y) # Test set error rate

# Compute confusion matrix for probability = 0.3
glm.pred <- rep("0", 412)
glm.pred[glm.probs > .3] = "1"
table(glm.pred, BankTest$y)
mean(glm.pred == BankTest$y) # Fraction of data for which prediction was correct
mean(glm.pred != BankTest$y) # Test set error rate

# Compute confusion matrix for probability = 0.6
glm.pred <- rep("0", 412)
glm.pred[glm.probs > .6] = "1"
table(glm.pred, BankTest$y)
mean(glm.pred == BankTest$y) # Fraction of data for which prediction was correct
mean(glm.pred != BankTest$y) # Test set error rate

