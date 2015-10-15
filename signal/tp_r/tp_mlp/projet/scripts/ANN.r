source("usefullTools.r")

stroke <-function(x0=-1,y0=-1,x1=1,y1=1,N=10)
{
   strk <- matrix(c(seq(x0,x1,length=N),seq(y0,y1,length=N)),ncol=2)
   return(strk)
}

# {{{ SIMU_SYMBOL
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

rotate <- function(m) t(apply(m, 2, rev))

constuctImg <- function(seq, nr=5, nc=3) {
   lut <- t(matrix(0, nrow=nr, ncol=nc, byrow=T))
   for(i in seq) {
      lut[i] = lut[i]+1 
   }
   lut <- t(apply(rotate(rotate(t(lut))), 1, rev))
   return(lut)
}

#sim <- simu_symbol()
#test <- compute_symbol(sim$d6, 7, 5)
#test

#lut <- constuctImg(test, 7, 5)
#lut

#testdir <- compute_symbol_dir(sim$d1)
#testdir

table <- Load_Obs("../data/Data7X5/Test_compute_symbol_7_5Digit6.txt")
for(i in 1:dim(table)[1]) {
   img <- constuctImg(table[i,], 7, 5)
   for (k in 1:dim(img)[1]) {
      for(x in 1:dim(img)[2]) {
         cat(img[k][x])
      }
      cat("\n")
   }
}
