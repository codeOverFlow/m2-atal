source("usefullTools.r")

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
rotate <- function(m) t(apply(m, 2, rev))

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
visualizeData <- function(t) {
   for(i in 1:dim(t)[1]) {
      img <- constuctImg(t[i,])
      prettyPrintImg(img)
      density <- createDensity(img)
      cat(density$nNon0PerRows)
   }
}
# }}}

# {{{ CREATE_DENSITY
createDensity <- function(img) {
   binarised <- apply(img, 2, function(x) { ifelse(x != 0, 1, 0) } )
   nbNon0PerRows <- matrix(rowSums(binarised), ncol=1)
      
   for(i in 1:dim(img)[1]) {
      if (i == 1) {
         tmp <- matrix(0, dim(img)[1], ncol=1)
      }
      tmp <- tmp + nbNon0PerRows
   }
   bigSum <- sum(tmp)
   tmp <- unlist(Map(function(x) { x/bigSum }, tmp))
   return(list(nNon0PerRows=tmp))
}
# }}}

# {{{ CREATE_SOUND_LEFT
createSoundLeft <- function(img) {
   past <- F
   binarised <- apply(img, 2, function(x) { ifelse(x != 0, 1, 0) } )
   nb0Left <- matrix(rowSums(), ncol=1)
      
   for(i in 1:dim(img)[1]) {
      if (i == 1) {
         tmp <- matrix(0, dim(img)[1], ncol=1)
      }
      tmp <- tmp + nbNon0PerRows
   }
   bigSum <- sum(tmp)
   tmp <- unlist(Map(function(x) { x/bigSum }, tmp))
   return(list(nNon0PerRows=tmp))
}
# }}}

sim <- simu_symbol()
test <- compute_symbol(sim$d6, 7, 5)
test

lut <- constuctImg(test, 7, 5)
prettyPrintImg(lut)
#
#features <- createFeatures(lut)$nNon0PerRows
#features

#testdir <- compute_symbol_dir(sim$d1)
#testdir

#table0 <- Load_Obs("../data/Data5X3/Test_compute_symbol_5_3Digit0.txt")
#visualizeData(table0)
#table1 <- Load_Obs("../data/Data5X3/Test_compute_symbol_5_3Digit1.txt")
#visualizeData(table1)
#table2 <- Load_Obs("../data/Data5X3/Test_compute_symbol_5_3Digit2.txt")
#visualizeData(table2)
#table3 <- Load_Obs("../data/Data5X3/Test_compute_symbol_5_3Digit3.txt")
#visualizeData(table3)
#table4 <- Load_Obs("../data/Data5X3/Test_compute_symbol_5_3Digit4.txt")
#visualizeData(table4)
#table5 <- Load_Obs("../data/Data5X3/Test_compute_symbol_5_3Digit5.txt")
#visualizeData(table5)
#table6 <- Load_Obs("../data/Data5X3/Test_compute_symbol_5_3Digit6.txt")
#visualizeData(table6)
#table7 <- Load_Obs("../data/Data5X3/Test_compute_symbol_5_3Digit7.txt")
#visualizeData(table7)
#table8 <- Load_Obs("../data/Data5X3/Test_compute_symbol_5_3Digit8.txt")
#visualizeData(table8)
#table9 <- Load_Obs("../data/Data5X3/Test_compute_symbol_5_3Digit9.txt")
#visualizeData(table9)

