AutoTrain=read.csv("AutoTrain.csv", header=T)
head(AutoTrain)

## STAT318/462 kNN regression function

kNN <- function(k,x.train,y.train,x.pred) {
  # 
  ## This is kNN regression function for problems with
  ## 1 predictor
  #
  ## INPUTS
  #
  # k       = number of observations in nieghbourhood 
  # x.train = vector of training predictor values
  # y.train = vector of training response values
  # x.pred  = vector of predictor inputs with unknown
  #           response values 
  #
  ## OUTPUT
  #
  # y.pred  = predicted response values for x.pred
  
  ## Initialize:
  n.pred <- length(x.pred);		y.pred <- numeric(n.pred)
  
  ## Main Loop
  for (i in 1:n.pred){
    d <- abs(x.train - x.pred[i])
    dstar = d[order(d)[k]]
    y.pred[i] <- mean(y.train[d <= dstar])		
  }
  ## Return the vector of predictions
  invisible(y.pred)
}


# kNN Training data 

# k = 2
plot(AutoTrain$horsepower, AutoTrain$mpg)
kNN.vals = kNN(2, AutoTrain$horsepower, AutoTrain$mpg, AutoTrain$horsepower)
points(AutoTest$horsepower, kNN.vals, pch=15, col="blue", cex=0.6)
MSE.Tr = mean((AutoTrain$mpg - kNN.vals)^2)
MSE.Tr

# k = 5
plot(AutoTrain$horsepower, AutoTrain$mpg)
kNN.vals = kNN(5, AutoTrain$horsepower, AutoTrain$mpg, AutoTrain$horsepower)
points(AutoTest$horsepower, kNN.vals, pch=15, col="blue", cex=0.6)
MSE.Tr = mean((AutoTrain$mpg - kNN.vals)^2)
MSE.Tr

# k = 10
plot(AutoTrain$horsepower, AutoTrain$mpg)
kNN.vals = kNN(10, AutoTrain$horsepower, AutoTrain$mpg, AutoTrain$horsepower)
points(AutoTest$horsepower, kNN.vals, pch=15, col="blue", cex=0.6)
MSE.Tr = mean((AutoTrain$mpg - kNN.vals)^2)
MSE.Tr

# k = 20
plot(AutoTrain$horsepower, AutoTrain$mpg)
kNN.vals = kNN(20, AutoTrain$horsepower, AutoTrain$mpg, AutoTrain$horsepower)
points(AutoTest$horsepower, kNN.vals, pch=15, col="blue", cex=0.6)
MSE.Tr = mean((AutoTrain$mpg - kNN.vals)^2)
MSE.Tr

# k = 30
plot(AutoTrain$horsepower, AutoTrain$mpg)
kNN.vals = kNN(30, AutoTrain$horsepower, AutoTrain$mpg, AutoTrain$horsepower)
points(AutoTest$horsepower, kNN.vals, pch=15, col="blue", cex=0.6)
MSE.Tr = mean((AutoTrain$mpg - kNN.vals)^2)
MSE.Tr

# k = 50
plot(AutoTrain$horsepower, AutoTrain$mpg)
kNN.vals = kNN(50, AutoTrain$horsepower, AutoTrain$mpg, AutoTrain$horsepower)
points(AutoTest$horsepower, kNN.vals, pch=15, col="blue", cex=0.6)
MSE.Tr = mean((AutoTrain$mpg - kNN.vals)^2)
MSE.Tr

# k = 100
plot(AutoTrain$horsepower, AutoTrain$mpg)
kNN.vals = kNN(100, AutoTrain$horsepower, AutoTrain$mpg, AutoTrain$horsepower)
points(AutoTest$horsepower, kNN.vals, pch=15, col="blue", cex=0.6)
MSE.Tr = mean((AutoTrain$mpg - kNN.vals)^2)
MSE.Tr


# kNN Testing data

# k = 2
plot(AutoTrain$horsepower, AutoTrain$mpg)
kNN.vals = kNN(2, AutoTrain$horsepower, AutoTrain$mpg, AutoTest$horsepower)
points(AutoTest$horsepower, kNN.vals, pch=15, col="blue", cex=0.6)
MSE.Te = mean((AutoTest$mpg - kNN.vals)^2)
MSE.Te

# k = 5
plot(AutoTrain$horsepower, AutoTrain$mpg)
kNN.vals = kNN(5, AutoTrain$horsepower, AutoTrain$mpg, AutoTest$horsepower)
points(AutoTest$horsepower, kNN.vals, pch=15, col="blue", cex=0.6)
MSE.Te = mean((AutoTest$mpg - kNN.vals)^2)
MSE.Te

# k = 10
plot(AutoTrain$horsepower, AutoTrain$mpg)
kNN.vals = kNN(10, AutoTrain$horsepower, AutoTrain$mpg, AutoTest$horsepower)
points(AutoTest$horsepower, kNN.vals, pch=15, col="blue", cex=0.6)
MSE.Te = mean((AutoTest$mpg - kNN.vals)^2)
MSE.Te

# k = 20
plot(AutoTrain$horsepower, AutoTrain$mpg)
kNN.vals = kNN(20, AutoTrain$horsepower, AutoTrain$mpg, AutoTest$horsepower)
points(AutoTest$horsepower, kNN.vals, pch=15, col="blue", cex=0.6)
MSE.Te = mean((AutoTest$mpg - kNN.vals)^2)
MSE.Te

# k = 30
plot(AutoTrain$horsepower, AutoTrain$mpg)
kNN.vals = kNN(30, AutoTrain$horsepower, AutoTrain$mpg, AutoTest$horsepower)
points(AutoTest$horsepower, kNN.vals, pch=15, col="blue", cex=0.6)
MSE.Te = mean((AutoTest$mpg - kNN.vals)^2)
MSE.Te

# k = 50
plot(AutoTrain$horsepower, AutoTrain$mpg)
kNN.vals = kNN(50, AutoTrain$horsepower, AutoTrain$mpg, AutoTest$horsepower)
points(AutoTest$horsepower, kNN.vals, pch=15, col="blue", cex=0.6)
MSE.Te = mean((AutoTest$mpg - kNN.vals)^2)
MSE.Te

# k = 100
plot(AutoTrain$horsepower, AutoTrain$mpg)
kNN.vals = kNN(100, AutoTrain$horsepower, AutoTrain$mpg, AutoTest$horsepower)
points(AutoTest$horsepower, kNN.vals, pch=15, col="blue", cex=0.6)
MSE.Te = mean((AutoTest$mpg - kNN.vals)^2)
MSE.Te

# best KNN
plot(AutoTrain$horsepower, AutoTrain$mpg, pch=15, col="black", cex=0.6, xlab = "horsepower", ylab = "mpg", main = "Auto data set")
kNN.vals = kNN(20, AutoTrain$horsepower, AutoTrain$mpg, AutoTest$horsepower)
points(AutoTest$horsepower, kNN.vals, pch=15, col="blue", cex=0.6)
points(AutoTest$horsepower, AutoTest$mpg, pch=15, col="red", cex=0.6)
legend(200, 40, legend=c("training data", "testing data", "kNN model"),
       col=c("black", "red", "blue"), pch = 15, cex=0.6,
       title="Legend", text.font=4,)

