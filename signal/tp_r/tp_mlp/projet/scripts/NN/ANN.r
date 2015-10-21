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
constuctImg <- function(seq, nr=5, nc=3) {
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
      img <- constuctImg(t[i,], nr, nc)
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

# {{{ CREATE_FEATURES
createFeatures <- function(tab, nr=5, nc=3, d=T, sl=T, sr=T, st=T, sb=T) {
   res <- c()
   len <- 0
   for(i in 1:dim(tab)[1]) {
      img <- constuctImg(tab[i,], nr, nc)

      df <- c()
      if (d) { df <- createDensity(img) }

      l <- c() 
      if (sl) { l <- createSoundLeft(img) }

      r <- c()  
      if (sr) { r <- createSoundRight(img) }

      t <- c()  
      if (st) { t <- createSoundTop(img) }

      b <- c(sb)  
      if (sb) { b <- createSoundBottom(img) }

      concat <- c(df,l,r,t,b)
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

# {{{ LEARN_VAL(NR, NC, TRAINID, VALID, NBN, MAXIT, NBLOOP)
learn.val <- function (nr, nc, trainId, valId, nbN, maxIt, nbLoop){
   table0 <- Load_Obs(paste("../../data/Data",
                            nr,
                            "X",
                            nc,
                            "/Train_compute_symbol_",
                            nr,
                            "_",
                            nc,
                            "Digit0.txt", sep=""))
   dataTarg <- class.ind(matrix(rep(c(0,1,2,3,4,5,6,7,8,9), dim(table0)[1]),
                                ncol=10, 
                                byrow=T))
   dataFt <- createFeatures(table0)
   #init a new random MLP
   new_nn <- nnet(dataFt[trainId,], dataTarg[trainId,], size=nbN, maxit=0,
                  decay=1e-4,rang = 1)
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
   for(k in 1:9) {
      table <- Load_Obs(paste("../../data/Data",
                              nr,
                              "X",
                              nc,
                              "/Train_compute_symbol_",
                              nr,
                              "_",
                              nc,
                              "Digit",
                              k,
                              ".txt", sep=""))
      dataFt <- createFeatures(table)
      cat("Starting Reco rate = ",currTrRate,"\n")
      for(i in 1:nbLoop){
         cat("\r", i," / ", nbLoop)
         #continue the training
         new_nn <- nnet(dataFt[trainId,], dataTarg[(k+1)*trainId,], size=nbN, maxit=maxIt, decay=1e-4,rang = 1, Wts=curr_w)
         curr_w <- new_nn$wts
         #compute the rates/MSE
         currTrRate <- test.reco(dataTarg[(k+1)*trainId,], predict(new_nn, dataFt[trainId,]))
         currTrRateVal <- test.reco(dataTarg[(k+1)*valId,], predict(new_nn, dataFt[valId,]))
         currTrMSE <- test.mse(dataTarg[(k+1)*trainId,], predict(new_nn, dataFt[trainId,]))
         currTrMSEVal <- test.mse(dataTarg[(k+1)*valId,], predict(new_nn, dataFt[valId,]))

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
   }
   cat("\n")
   return (list(nn = best_nn, nbIt=bestIt, scoreTrain = scoresT,
                scoreVal = scoresV, it=iterations, mseT=mseT,mseV=mseV))
}
# }}}



#sim <- simu_symbol()
#test <- compute_symbol(sim$d6, 7, 5)
#test
#
#lut <- constuctImg(test, 7, 5)
#prettyPrintImg(lut)
#
#features <- createFeatures(lut)$nNon0PerRows
#features



#testdir <- compute_symbol_dir(sim$d1)
#testdir

table <- Load_Obs("../../data/Data5X3/Test_compute_symbol_5_3Digit4.txt")
#visualizeData(table0, 7, 5)


#dataTarg <- class.ind(matrix(rep(c(0), dim(table0)[1]), ncol=1))
#dataFt <- createFeatures(table0)
res <- learn.val(5, 3, 1:140, 141:180, 25, 100, 20)

nndigit <- res$nn
for(i in 1:10) {
   predict(nndigit, table[i,])
}
#par(fg = "black")
#plot(res$it, res$scoreTrain, type = "l")
#par(fg = "red")
#lines(res$it, res$scoreVal, type = "l")
#
#par(fg = "black")
#plot(res$it, res$mseT, type = "l")
#par(fg = "red")
#lines(res$it, res$mseV, type = "l")

#table1 <- Load_Obs("../../data/Data7X5/Test_compute_symbol_7_5Digit1.txt")
#visualizeData(table1, 7, 5)
#table2 <- Load_Obs("../../data/Data7X5/Test_compute_symbol_7_5Digit2.txt")
#visualizeData(table2, 7, 5)
#table3 <- Load_Obs("../../data/Data7X5/Test_compute_symbol_7_5Digit3.txt")
#visualizeData(table3, 7, 5)
#table4 <- Load_Obs("../../data/Data7X5/Test_compute_symbol_7_5Digit4.txt")
#visualizeData(table4, 7, 5)
#table5 <- Load_Obs("../../data/Data7X5/Test_compute_symbol_7_5Digit5.txt")
#visualizeData(table5, 7, 5)
#table6 <- Load_Obs("../../data/Data7X5/Test_compute_symbol_7_5Digit6.txt")
#visualizeData(table6, 7, 5)
#table7 <- Load_Obs("../../data/Data7X5/Test_compute_symbol_7_5Digit7.txt")
#visualizeData(table7, 7, 5)
#table8 <- Load_Obs("../../data/Data7X5/Test_compute_symbol_7_5Digit8.txt")
#visualizeData(table8, 7, 5)
#table9 <- Load_Obs("../../data/Data7X5/Test_compute_symbol_7_5Digit9.txt")
#visualizeData(table9, 7, 5)

