#Nu : number of neurons
#Nex : number of sample in training
#Comp : complexity of the target function
library("nnet")

demoBV <- function(Nu,Nex,Comp = 1){
  v = 1:Nex / Nex 
  v <- matrix(v,Nex,1)
  f <- function(x){abs(2* sin(x * Comp*pi))}
  o <- f(v)
  snn <- nnet(v,o,skip=(Nu==0),size=Nu, maxit=1000,abstol=1.0e-12,rang = 1,linout = T)
  plot(v,o,col='black')
  toPlotX <- matrix(1:100 /100,100,1)
  toPlotY <- predict(snn,toPlotX)
  lines(toPlotX,f(toPlotX), type='l',col='black')
  lines(toPlotX,toPlotY, type='l',col='red')
  
  cat("error to target=",mean((toPlotY - f(toPlotX))^2), "\n")
  cat("error to samples=",mean(snn$residuals^2), "\n")
  
  return (snn)
}

demoBV(2,10,2)
demoBV(30, 100,8)
# demoBV(20,10,2)
# demoBV(20,100,2)
# demoBV(20,10,4)
# demoBV(20,100,4)
