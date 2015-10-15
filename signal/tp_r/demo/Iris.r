par(ask=F,col='black')
# load iris dataset 
data(iris)
# plot the full dataset
plot(iris$Petal.Length, iris$Petal.Width, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)])

# use only a sub set : 2 classes, sample from setosa and versicolor
subiris <- iris[1:100,]
plot(subiris$Sepal.Length, subiris$Sepal.Width, pch=21, bg=c("red","green3","blue")[unclass(subiris$Species)],asp = 1)

drawSolution <- function(w){
  atx <- 5.5 #x position of the arrow
  abline(coef = c(-w[3]/w[2],-w[1]/w[2]))
  p <- matrix(c(atx,(-atx*w[1]-w[3])/w[2]))
  arrows(p[1],p[2],p[1]+w[1],p[2]+w[2])
}


#produce an extended feature vector
extendedX <- function(x){
  matrix(c(x,c(1.0)),max(NCOL(x),NROW(x))+1,1)
}

# the classification rule
classif <- function(ex,w){
  if(t(w) %*% ex > 0){
    1
  }else{
    -1
  }
}


computeErrorRate <- function(w){
  #apply classification to all samples
  res <- apply(subiris[1:2], 1, function (x) {classif(w,extendedX(x))})
  #compute true labels (ground-truth)
  gt <- c(-1,1,0)[unclass(subiris$Species)]
  #compare and sum
  return (sum(res != gt))
}


# Part 1 : manual search of a solution

# choose a solution vector 

cat("input the 3 values for w : v(x) = w[1]*x[1]+w[2]*x[2]+w[3]")
#w = matrix(c(1,-1,-2.3),3,1) #in the code
w = matrix(c(-1,0.5,4),3,1) #in the code
#w = matrix(scan(n=3),3,1) #inline

# classification test
#print(classif(w,extendedX(c(6.0,4.0))))

drawSolution(w)

cat("best error manually found : ", computeErrorRate(w),"\n")


# Part 2 : automatic search of a solution


randomlearning2D<- function(){
  bestError <- 1000;
  for(i in 1:1000){
    #wtry <- c(1, runif(1, -10.0, 10.0),runif(1, -10.0, 10.0))
    wtry <- runif(3, -1.0, 1.0)
    e <- computeErrorRate(wtry)
    if(e < bestError){
      wbest <- wtry
      bestError <- e
      cat("  current best =",bestError,"\n")
    }
  }
  return (wbest)
}

w <- randomlearning2D()

par(col='red')
drawSolution(w)
cat("best error randomly found : ", computeErrorRate(w), " with :\nw=",w)


