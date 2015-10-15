library("nnet")

#ir <- rbind(iris3[,,1],iris3[,,2],iris3[,,3])
#iris without the label
irisunlab <- iris[,1:4] 

#build the label list: iris has 3 classes with 50 samples in each class
targets <- class.ind( c(rep("s", 50), rep("c", 50), rep("v", 50)) )

#split train and test data equaly for each class
sampTrain <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))

#train part
trainData <- irisunlab[sampTrain,]
trainLab <- targets[sampTrain,]

#test part
testData <- irisunlab[-sampTrain,] #with the '-', it reverses the selection
testLab <- targets[-sampTrain,]

#train the MLP

# a MLP without hidden units
#ir1 <- nnet(trainData, trainLab, skip = TRUE, size = 0, rang = 0.1,  maxit = 200)
# a MLP with 2 hidden unit 
ir1 <- nnet(trainData, trainLab, size = 2, rang = 0.1,  maxit = 2000)
# a MLP with 4 hidden unit 
#ir1 <- nnet(trainData, trainLab, size = 4, rang = 0.1,  maxit = 2000,abstol=1.0e-12,reltol=1.0e-12)

# a MLP with 2 hidden unit with weight decay (penalize hight weight, so better generalization)
#ir1 <- nnet(trainData, trainLab, size = 2, decay = 1e-4,rang = 0.1,  maxit = 200)

#try a recognition
cat("predict class for 1 sample:\n")
cat(predict(ir1, c(7,3,6,2)), "\n")


test.cl <- function(true, pred) {
  true <- max.col(true)
  cres <- max.col(pred)
  return (table(true, cres))
}

test.reco <- function(true, pred) {
  true <- max.col(true)
  cres <- max.col(pred)
  return (sum(true == cres) )
}

#recognize test part
recoLabel <- predict(ir1, testData)
recoLabelTrain <- predict(ir1, trainData)

#compute reco rate and confution matrix
cat("reco rate Train:",test.reco(trainLab,recoLabelTrain ),"\n")
cat("reco rate Test:",test.reco(testLab,recoLabel ),"\n")
print(test.cl(testLab,recoLabel ))

