library("nnet")

# remember to change your working directory :  setwd("...path...")

#load data sets
digits <- read.delim("Digits_Train_7x20.data", header=F)
digits <- digits[1:1300,]
# classes are the first column
digitCl = digits[,1]
digitsTest <- read.delim("Digits_Test_7x20.data", header=F)
digitClTest = digitsTest[,1]
#full version:
 #digitFt = digits[,3:141]
#or reduced features version:
f <- 0:19 * 7 +3
digitFt = digits[,f]
digitFtTest = digitsTest[,f]

#reduce number of classes:
select <- ((digitCl == 1) | (digitCl == 2)| (digitCl == 3)| (digitCl == 4)|
           (digitCl == 5))
selectTest <- ((digitClTest == 1) | (digitClTest == 2)| (digitClTest == 3)|
               (digitClTest == 4)| (digitClTest == 5))
digitCl <- digitCl[select]
digitFt <- digitFt[select,]
digitClTest <- digitClTest[selectTest]
digitFtTest <- digitFtTest[selectTest,]

#build the target vectors [0,..,0,1,0,..0]
digitTarg <- class.ind(digitCl)
digitTargTest <- class.ind(digitClTest)

#usefull functions to compute results:
# computes confusion matrix
test.cl <- function(true, pred) {
  true <- max.col(true)
  res <- max.col(pred)
  return (table(true, res))
}

# computes recognition rates
test.reco <- function(true, pred) {
  true <- max.col(true)
  res <- max.col(pred)
  return (as.numeric(sum(true == res)))
}

#compute the MSE
test.mse <- function(true, pred) {
  diff <- true - pred
  sqred <- diff * diff
  return (sum(sqred) / length(sqred))
}


# print(test.cl(digitTarg, predict(nndigit,digitFt)))
# cat("Reco rate (train) = ",test.reco(digitTarg, predict(nndigit,digitFt)),"/",dim(digitTarg)[1],"\n")
# cat("MSE  (train)      = ",test.mse(digitTarg, predict(nndigit,digitFt)),"\n")
# cat("Reco rate (Test)  = ",test.reco(digitTargTest, predict(nndigit,digitFtTest)),"/",dim(digitTargTest)[1],"\n")


###### Proposal for the first question :



learnVal <- function (dataFt, dataTarg, trainId, valId, nbN, maxIt, nbLoop){
  #init a new random MLP
  new_nn <- nnet(dataFt[trainId,], dataTarg[trainId,], size=nbN, maxit=0,
                 decay=1e-4,rang = 1, trace=FALSE)
  best_nn = new_nn
  curr_w <- new_nn$wts
  # compute initial rates / mse and save them
  currTrRate <- test.reco(dataTarg[trainId,], predict(new_nn, dataFt[trainId,]))
  currTrRateVal <- test.reco(dataTarg[valId,], predict(new_nn, dataFt[valId,]))
  currTrMSE <- test.mse(dataTarg[trainId,], predict(new_nn, dataFt[trainId,]))
  currTrMSEVal <- test.mse(dataTarg[valId,], predict(new_nn, dataFt[valId,]))
  scoresT <- c(currTrRate/length(trainId))
  scoresV <- c(currTrRateVal/length(valId))
  mseT <- c(currTrMSE)
  mseV <- c(currTrMSEVal)
  iterations <- c(0)
  bestRate <- currTrRateVal 
  bestIt <- 0
  cat("Starting Reco rate = ",currTrRate,"\n")
  for(i in 1:nbLoop){
    cat("\r", i," / ", nbLoop)
    #continue the training
    new_nn <- nnet(dataFt[trainId,], dataTarg[trainId,], size=nbN, maxit=maxIt, decay=1e-4,rang = 1, Wts=curr_w, trace=FALSE)
    curr_w <- new_nn$wts
    #compute the rates/MSE
    currTrRate <- test.reco(dataTarg[trainId,], predict(new_nn, dataFt[trainId,]))
    currTrRateVal <- test.reco(dataTarg[valId,], predict(new_nn, dataFt[valId,]))
    currTrMSE <- test.mse(dataTarg[trainId,], predict(new_nn, dataFt[trainId,]))
    currTrMSEVal <- test.mse(dataTarg[valId,], predict(new_nn, dataFt[valId,]))
    
    #save values to plot
    scoresT <- c(scoresT,currTrRate/length(trainId))
    scoresV <- c(scoresV,currTrRateVal/length(valId))
    mseT <- c(mseT,currTrMSE)
    mseV <- c(mseV,currTrMSEVal)
    iterations <- c(iterations, i * maxIt)
    #save if best
    if(currTrRateVal > bestRate){
      bestRate <- currTrRateVal
      best_nn <- new_nn
      bestIt <- maxIt * i
    }
  }
  cat("\n")
  return (list(nn = best_nn, nbIt=bestIt, scoreTrain = scoresT,
               scoreVal = scoresV, it=iterations, mseT=mseT,mseV=mseV))
}


learnCross  <- function(dataFt, dataTarg, nbN, maxIt, nbLoop, fold, sizeOfFold) {
   scoresT <- c()
   scoresV <- c()
   msesT <- c()
   msesV <- c()
   for(i in 1:fold) {
      # take a random training set
      valId <- sample(dim(dataFt)[1], sizeOfFold)
      trainId <- -valId
      res <- learnVal(dataFt, dataTarg, trainId, valId, nbN, maxIt, nbLoop)
      scoresT <- c(scoresT, res$scoreTrain[res$nbIt/maxIt])
      scoresV <- c(scoresV, res$scoreVal[res$nbIt/maxIt])
      msesT <- c(msesT, res$mseT[res$nbIt/maxIt])
      msesV <- c(msesV, res$msesV[res$nbIt/maxIt])
   }
   scoresT <- mean(scoresT)
   scoresV <- mean(scoresV)
   msesT <- mean(msesT)
   msesV <- mean(msesV)


   return (list(meanScoreT=scoresT, meanScoreV=scoresV,
                meanMseT=msesT, meanMseV=msesV))
}

#res <-  learnVal(digitFt, digitTarg, 1:400, 401:646, 20, 100, 20)
#nndigit <- res$nn

#par(fg = "black")
#plot(res$it, res$scoreTrain, type = "l")
#par(fg = "red")
#lines(res$it, res$scoreVal, type = "l")
#
#par(fg = "black")
#plot(res$it, res$mseT, type = "l")
#par(fg = "red")
#lines(res$it, res$mseV, type = "l")

# cross validation
meansT <- c()
meansV <- c()
tmp <- 15:25
for( i in tmp) {
   cat(" ########################### ", i, " #######################\n")
   res <- learnCross(digitFt, digitTarg, i, 40, 20, 2, 300)
   meansT <- c(meansT, res$meanScoreT)
   meansV <- c(meansV, res$meanScoreV)
}
par(fg = "black")
plot(tmp, meansV, type = "l")
par(fg = "red")
plot(tmp, meansT, type = "l")
