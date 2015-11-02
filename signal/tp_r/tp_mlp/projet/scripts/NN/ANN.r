source("../tools/usefullTools.r")
library("nnet")

# {{{ SIMU_SYMBOL
stroke <-function(x0=-1,y0=-1,x1=1,y1=1,N=10)
{
   strk <- matrix(c(seq(x0,x1,length=N),seq(y0,y1,length=N)),ncol=2)
   return(strk)
}

simu_symbol <- function()
{
   digit_1 <- rbind(stroke(-0.3,0.5,0.3,1.0,10),stroke(0.3,0.9,0.3,-1.0,20))
   dimnames(digit_1) <- list(num=1:nrow(digit_1),point=c("x","y"))
   plot(digit_1,type="l",col="red",xlim=c(-1,1),ylim=c(-1,1))
   points(digit_1)

   digit_4 <- rbind(stroke(0.2,1.0,-0.8,-0.3,10),stroke(-0.85,-0.32,0.5,-0.1,10),stroke(0.3,0.1,0.2,-1.0,10))
   dimnames(digit_4) <- list(num=1:nrow(digit_4),point=c("x","y"))
   plot(digit_4,type="l",col="red",xlim=c(-1,1),ylim=c(-1,1))
   points(digit_4)

   digit_6 <- rbind(stroke(0.5,1,-0.5,1,10),stroke(-0.5,1,-0.5,-1,10),
                    stroke(-0.5,-1,0.5,-1,10), stroke(0.5,-1,0.5,0,10),
                    stroke(0.5,0,-0.5,0,10))
   dimnames(digit_6) <- list(num=1:nrow(digit_6),point=c("x","y"))
   plot(digit_6,type="l",col="red",xlim=c(-1,1),ylim=c(-1,1))
   points(digit_6)
   return(list(d1=digit_1,d4=digit_4, d6=digit_6))
}
# }}}

# {{{ COMPUTE_SYMBOL
compute_symbol <- function (trace,nr=5,nc=3)
{
   LUT <- matrix(1:(nr*nc),nrow=nr,ncol=nc,byrow=T)
   NB <- length(trace[,"x"])
   Ix <- pmax(pmin(1+floor((trace[,"x"]-(-1))*nc/2),rep(nc,NB)),rep(1,NB))
   Iy <- pmax(pmin(1+floor((trace[,"y"]-(-1))*nr/2),rep(nr,NB)),rep(1,NB))
   return(LUT[matrix(c(Iy, Ix),ncol=2)])
}
# }}}

# {{{ COMPUTE_SYMBOL_DIR
compute_symbol_dir <- function (trace,nangle=8)
{
   NB <- length(trace[,"x"])
   delta <- trace
   delta[1:(NB-1),] <- delta[2:NB,]
   delta <- delta - trace
   delta[NB,] <- delta[NB-1,]
   angle <- atan2(delta[,"y"],delta[,"x"]) + pi/nangle
   angle[angle < 0] <- angle[angle < 0] + 2*pi
   angle <- pmin(1 + floor(angle*nangle/(2*pi)),nangle)
   return(angle)
}
# }}}

# {{{ CONSTRUCT_IMG
constructImg <- function(seq, nr=5, nc=3) {
   lut <- t(matrix(0, nrow=nr, ncol=nc, byrow=T))
   for(i in seq) {
      lut[i] = lut[i]+1 
   }
   lut <- apply(lut, 1, rev)
   return(lut)
}
# }}}

# {{{ PRETTY_PRINT_IMG
prettyPrintImg <- function(imgData) {
   for (k in 1:dim(imgData)[1]) {
      for(x in 1:dim(imgData)[2]) {
         if (imgData[k,x] > 0) {
            cat("\033[31;1m",imgData[k,x])
         }
         else {
            cat("\033[00;0m", imgData[k,x])
         }
      }
      cat("\n\033[00;0m")
   }
   cat("\n\n")
}
# }}}

# {{{ VISUALIZE_DATA
visualizeData <- function(t, nr=5, nc=3) {
   for(i in 1:dim(t)[1]) {
      img <- constructImg(t[i,], nr, nc)
      prettyPrintImg(img)
      density <- createDensity(img)
      cat(density$nNon0PerRows)
      zeroLeft <- createSoundLeft(img)
      cat("\n", zeroLeft$nZeroLeft)
      zeroRight <- createSoundRight(img)
      cat("\n", zeroRight$nZeroRight)
      zeroTop <- createSoundTop(img)
      cat("\n", zeroTop$nZeroTop)
      zeroBottom <- createSoundBottom(img)
      cat("\n", zeroBottom$nZeroBottom)
      cat("\n\n")
   }
}
# }}}

############## FEATURES ################
# {{{ CREATE_DENSITY
createDensity <- function(img) {
   # binarise the img
   binarised <- apply(img, 2, function(x) { ifelse(x != 0, 1, 0) } )
   # make a vector with the sum of each binarised line
   nbNon0PerRows <- matrix(rowSums(binarised), ncol=1)
   # sum all 
   bigSum <- sum(nbNon0PerRows)
   # normalize
   tmp <- unlist(Map(function(x) { x/bigSum }, nbNon0PerRows))
   return(tmp)
}
# }}}

# {{{ CREATE_SOUND_LEFT
createSoundLeft <- function(img) {
   # binarise the img
   binarised <- apply(img, 2, function(x) { ifelse(x != 0, 1, 0) } )
   # make an empty vector
   tmp <- c()
   for (i in 1:dim(binarised)[1]) {
      # init count at 0
      nb <- 0
      # init past to False
      past <- F
      for (j in 1:length(binarised[1,])) {
         if (binarised[i,j] == 0 && !past) {
            nb <- nb+1
         }
         else {
            past <- T
         }
      }
      tmp <- c(tmp, nb)
   }
   # sum the vector 
   bigSum <- sum(tmp)
   # normalize
   tmp <- unlist(Map(function(x) { x/ifelse(bigSum == 0, 1, bigSum) }, tmp))
   return(tmp)
}
# }}}

# {{{ CREATE_SOUND_RIGHT
createSoundRight <- function(img) {
   # binarise the img
   binarised <- apply(img, 2, function(x) { ifelse(x != 0, 1, 0) } )
   # make an empty vector
   tmp <- c()
   for (i in 1:dim(binarised)[1]) {
      # init count at 0
      nb <- 0
      for (j in 1:length(binarised[1,])) {
         if (binarised[i,j] == 0) {
            nb <- nb+1
         }
         else {
            nb <- 0
         }
      }
      tmp <- c(tmp, nb)
   }
   # sum the vector 
   bigSum <- sum(tmp)
   # normalize
   tmp <- unlist(Map(function(x) { x/ifelse(bigSum == 0, 1, bigSum) }, tmp))
   return(tmp)
}
# }}}

# {{{ CREATE_SOUND_TOP
createSoundTop <- function(img) {
   # binarise the img
   binarised <- apply(img, 2, function(x) { ifelse(x != 0, 1, 0) } )
   # make an empty vector
   tmp <- c()
   for (i in 1:dim(binarised)[2]) {
      # init count at 0
      nb <- 0
      past <- F
      for (j in 1:dim(binarised)[1]) {
         if (binarised[j,i] == 0 && !past) {
            nb <- nb+1
         }
         else {
            past <- T
         }
      }
      tmp <- c(tmp, nb)
   }
   # sum the vector 
   bigSum <- sum(tmp)
   # normalize
   tmp <- unlist(Map(function(x) { x/ifelse(bigSum == 0, 1, bigSum) }, tmp))
   return(tmp)
}
# }}}

# {{{ CREATE_SOUND_BOTTOM
createSoundBottom <- function(img) {
   # binarise the img
   binarised <- apply(img, 2, function(x) { ifelse(x != 0, 1, 0) } )
   # make an empty vector
   tmp <- c()
   for (i in 1:dim(binarised)[2]) {
      # init count at 0
      nb <- 0
      for (j in 1:dim(binarised)[1]) {
         if (binarised[j,i] == 0) {
            nb <- nb+1
         }
         else {
            nb <- 0
         }
      }
      tmp <- c(tmp, nb)
   }
   # sum the vector 
   bigSum <- sum(tmp)
   # normalize
   tmp <- unlist(Map(function(x) { x/ifelse(bigSum == 0, 1, bigSum) }, tmp))
   return(tmp)
}
# }}}

# {{{ CREATE_MEAN_R
createMeanR <- function(img) {
   # make an empty vector
   tmp <- c()
   for (i in 1:dim(img)[1]) {
      # compute the mean
      tmp <- c(tmp, mean(img[i,]))
   }
   # sum the vector 
   bigSum <- sum(tmp)
   # normalize
   tmp <- unlist(Map(function(x) { x/ifelse(bigSum == 0, 1, bigSum) }, tmp))
   return(tmp)
}
# }}}

# {{{ CREATE_MEAN_C
createMeanC <- function(img) {
   # make an empty vector
   tmp <- c()
   for (i in 1:dim(img)[2]) {
      # compute the mean
      tmp <- c(tmp, mean(img[,i]))
   }
   # sum the vector 
   bigSum <- sum(tmp)
   # normalize
   tmp <- unlist(Map(function(x) { x/ifelse(bigSum == 0, 1, bigSum) }, tmp))
   return(tmp)
}
# }}}

# {{{ CREATE_FEATURES(TAB, NR, NC, D, SL, SR, ST, SB, MR, MC)
createFeatures <- function(tab, nr=5, nc=3, d=T, sl=T, sr=T, st=T, sb=T, mr=T, mc=T) {
   res <- c()
   len <- 0
   for(i in 1:dim(tab)[1]) {
      img <- constructImg(tab[i,], nr, nc)

      df <- c()
      if (d) { df <- createDensity(img) }

      l <- c() 
      if (sl) { l <- createSoundLeft(img) }

      r <- c()  
      if (sr) { r <- createSoundRight(img) }

      t <- c()  
      if (st) { t <- createSoundTop(img) }

      b <- c()  
      if (sb) { b <- createSoundBottom(img) }
      
      mR <- c()
      if (mr) { mR <- createMeanR(img) }

      mC <- c()
      if (mc) { mC <- createMeanC(img) }

      concat <- c(df,l,r,t,b,mR,mC)
      len <- length(concat)

      res <- c(res, concat)
   }
   return(matrix(res, ncol=len, byrow=T))
}
# }}}

# {{{ USEFULL_FUNCTIONS_TO_COMPUTE_RESULTS
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
# }}}

# {{{ LEARN_VAL(DATAFT, DATATARG, TRAINID, VALID, NBN, MAXIT, NBLOOP)
learn.val <- function (dataFt, dataTarg, trainId, valId, nbN, maxIt, nbLoop){
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
# }}}

# {{{ LEARN_CROSS(DATAFT, DATATARG, NBN, MAXIT, NBLOOP, FOLD, SIZEOFFOLD)
learn.cross  <- function(dataFt, dataTarg, nbN, maxIt, nbLoop, fold, sizeOfFold) {
   scoresT <- c()
   scoresV <- c()
   msesT <- c()
   msesV <- c()
   for(i in 1:fold) {
      # take a random training set
      valId <- sample(dim(dataFt)[1], sizeOfFold)
      trainId <- -valId
      res <- learn.val(dataFt, dataTarg, trainId, valId, nbN, maxIt, nbLoop)
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
# }}}

# {{{ LOAD_DATAS(NR, NC, PRINTDATA)
loadDatas <- function(nr=5, nc=3, printdata=F, d=T, sl=T, sr=T, st=T, sb=T, mr=T, mc=T) {
   table0 <- Load_Obs(paste("../../data/Data",nr,"X",nc,"/Train_compute_symbol_",nr,"_",nc,"Digit0.txt", sep=""))
   table1 <- Load_Obs(paste("../../data/Data",nr,"X",nc,"/Train_compute_symbol_",nr,"_",nc,"Digit1.txt", sep=""))
   table2 <- Load_Obs(paste("../../data/Data",nr,"X",nc,"/Train_compute_symbol_",nr,"_",nc,"Digit2.txt", sep=""))
   table3 <- Load_Obs(paste("../../data/Data",nr,"X",nc,"/Train_compute_symbol_",nr,"_",nc,"Digit3.txt", sep=""))
   table4 <- Load_Obs(paste("../../data/Data",nr,"X",nc,"/Train_compute_symbol_",nr,"_",nc,"Digit4.txt", sep=""))
   table5 <- Load_Obs(paste("../../data/Data",nr,"X",nc,"/Train_compute_symbol_",nr,"_",nc,"Digit5.txt", sep=""))
   table6 <- Load_Obs(paste("../../data/Data",nr,"X",nc,"/Train_compute_symbol_",nr,"_",nc,"Digit6.txt", sep=""))
   table7 <- Load_Obs(paste("../../data/Data",nr,"X",nc,"/Train_compute_symbol_",nr,"_",nc,"Digit7.txt", sep=""))
   table8 <- Load_Obs(paste("../../data/Data",nr,"X",nc,"/Train_compute_symbol_",nr,"_",nc,"Digit8.txt", sep=""))
   table9 <- Load_Obs(paste("../../data/Data",nr,"X",nc,"/Train_compute_symbol_",nr,"_",nc,"Digit9.txt", sep=""))

   if (printdata) {
      visualizeData(table0, nr, nc)
      visualizeData(table1, nr, nc)
      visualizeData(table2, nr, nc)
      visualizeData(table3, nr, nc)
      visualizeData(table4, nr, nc)
      visualizeData(table5, nr, nc)
      visualizeData(table6, nr, nc)
      visualizeData(table7, nr, nc)
      visualizeData(table8, nr, nc)
      visualizeData(table9, nr, nc)
   }

   f0 <- createFeatures(table0,nr=nr, nc=nc, d=d, sl=sl, sr=sr, st=st, sb=sb, mr=mr, mc=mc)
   f1 <- createFeatures(table1,nr=nr, nc=nc, d=d, sl=sl, sr=sr, st=st, sb=sb, mr=mr, mc=mc)
   f2 <- createFeatures(table2,nr=nr, nc=nc, d=d, sl=sl, sr=sr, st=st, sb=sb, mr=mr, mc=mc)
   f3 <- createFeatures(table3,nr=nr, nc=nc, d=d, sl=sl, sr=sr, st=st, sb=sb, mr=mr, mc=mc)
   f4 <- createFeatures(table4,nr=nr, nc=nc, d=d, sl=sl, sr=sr, st=st, sb=sb, mr=mr, mc=mc)
   f5 <- createFeatures(table5,nr=nr, nc=nc, d=d, sl=sl, sr=sr, st=st, sb=sb, mr=mr, mc=mc)
   f6 <- createFeatures(table6,nr=nr, nc=nc, d=d, sl=sl, sr=sr, st=st, sb=sb, mr=mr, mc=mc)
   f7 <- createFeatures(table7,nr=nr, nc=nc, d=d, sl=sl, sr=sr, st=st, sb=sb, mr=mr, mc=mc)
   f8 <- createFeatures(table8,nr=nr, nc=nc, d=d, sl=sl, sr=sr, st=st, sb=sb, mr=mr, mc=mc)
   f9 <- createFeatures(table9,nr=nr, nc=nc, d=d, sl=sl, sr=sr, st=st, sb=sb, mr=mr, mc=mc)

   targs0 <- class.ind(matrix(rep(c(0,1,2,3,4,5,6,7,8,9), dim(table0)[1]),
                              ncol=10, byrow=T))[1:dim(table0)[1],]

   targs1 <- class.ind(matrix(rep(c(0,1,2,3,4,5,6,7,8,9), dim(table1)[1]),
                              ncol=10, byrow=T))[(1+dim(table1)[1]):(2*dim(table1)[1]),]

   targs2 <- class.ind(matrix(rep(c(0,1,2,3,4,5,6,7,8,9), dim(table2)[1])
                              , ncol=10, byrow=T))[(1+2*dim(table2)[1]):(3*dim(table2)[1]),]

   targs3 <- class.ind(matrix(rep(c(0,1,2,3,4,5,6,7,8,9), dim(table3)[1])
                              , ncol=10, byrow=T))[(1+3*dim(table3)[1]):(4*dim(table3)[1]),]

   targs4 <- class.ind(matrix(rep(c(0,1,2,3,4,5,6,7,8,9), dim(table4)[1])
                              , ncol=10, byrow=T))[(1+4*dim(table4)[1]):(5*dim(table4)[1]),]

   targs5 <- class.ind(matrix(rep(c(0,1,2,3,4,5,6,7,8,9), dim(table5)[1])
                              , ncol=10, byrow=T))[(1+5*dim(table5)[1]):(6*dim(table5)[1]),]

   targs6 <- class.ind(matrix(rep(c(0,1,2,3,4,5,6,7,8,9), dim(table6)[1])
                              , ncol=10, byrow=T))[(1+6*dim(table6)[1]):(7*dim(table6)[1]),]

   targs7 <- class.ind(matrix(rep(c(0,1,2,3,4,5,6,7,8,9), dim(table7)[1])
                              , ncol=10, byrow=T))[(1+7*dim(table7)[1]):(8*dim(table7)[1]),]

   targs8 <- class.ind(matrix(rep(c(0,1,2,3,4,5,6,7,8,9), dim(table8)[1])
                              , ncol=10, byrow=T))[(1+8*dim(table8)[1]):(9*dim(table8)[1]),]

   targs9 <- class.ind(matrix(rep(c(0,1,2,3,4,5,6,7,8,9), dim(table9)[1])
                              , ncol=10, byrow=T))[(1+9*dim(table9)[1]):(10*dim(table9)[1]),]
  
   allset <- rbind(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9)
   alltargs <- rbind(targs0, targs1, targs2, targs3, targs4, targs5, targs6, targs7, targs8, targs9)

   return(list(allDataSet=allset, allTargSet=alltargs))
}
# }}}

#sim <- simu_symbol()
#test <- compute_symbol(sim$d6, 30, 20)
#test
#
#lut <- constructImg(test, 30, 20)
#prettyPrintImg(lut)
#
#test <- matrix(test, nrow=1)
#features <- createFeatures(test, 30, 20)
#features



#testdir <- compute_symbol_dir(sim$d1)
#testdir



sets <- loadDatas(nr=7, nc=5)
dataSets <- sets$allDataSet
targSets <- sets$allTargSet

dim(dataSets)
dim(targSets)

trainId <- sort(sample(dim(dataSets)[1], 2500))
validId <- -trainId

res <- learn.val(dataSets, targSets, trainId, validId, 15, 100, 20)
nndigit <- res$nn

test0 <- Load_Obs("../../data/Data5X3/Test_compute_symbol_5_3Digit0.txt")
test1 <- Load_Obs("../../data/Data5X3/Test_compute_symbol_5_3Digit1.txt")
test2 <- Load_Obs("../../data/Data5X3/Test_compute_symbol_5_3Digit2.txt")
test3 <- Load_Obs("../../data/Data5X3/Test_compute_symbol_5_3Digit3.txt")
test4 <- Load_Obs("../../data/Data5X3/Test_compute_symbol_5_3Digit4.txt")
test5 <- Load_Obs("../../data/Data5X3/Test_compute_symbol_5_3Digit5.txt")
test6 <- Load_Obs("../../data/Data5X3/Test_compute_symbol_5_3Digit6.txt")
test7 <- Load_Obs("../../data/Data5X3/Test_compute_symbol_5_3Digit7.txt")
test8 <- Load_Obs("../../data/Data5X3/Test_compute_symbol_5_3Digit8.txt")
test9 <- Load_Obs("../../data/Data5X3/Test_compute_symbol_5_3Digit9.txt")

# cross validation
meansT <- c()
meansV <- c()
tmp <- 10:18
for( i in tmp) {
   cat(" ########################### ", i, " #######################\n")
   res <- learn.cross(dataSets, targSets, i, 40, 20, 15, 724)
   meansT <- c(meansT, res$meanScoreT)
   meansV <- c(meansV, res$meanScoreV)
}
par(fg = "black")
plot(tmp, meansV, type = "l")
par(fg = "red")
plot(tmp, meansT, type = "l")

#for(i in 1:10) {
#   predict(nndigit, table[i,])
#}
#par(fg = "black")
#plot(res$it, res$scoreTrain, type = "l")
#par(fg = "red")
#lines(res$it, res$scoreVal, type = "l")
#
#par(fg = "black")
#plot(res$it, res$mseT, type = "l")
#par(fg = "red")
#lines(res$it, res$mseV, type = "l")



