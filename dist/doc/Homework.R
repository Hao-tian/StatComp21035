## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
text <- 'Welcome to statistical computing!'
text

## -----------------------------------------------------------------------------
set.seed(1)
x <- rnorm(30,1,1)
y <- rnorm(30,1,0.5)
plot(x,y,main = "Scatter plot of X and Y")

## -----------------------------------------------------------------------------
state.x77.df <- data.frame(state.x77)
names(state.x77.df) <- c("Popul", "Income", "Illit", "LifeExp",
"Murder", "HSGrad", "Frost", "Area")
state.x77.df

## -----------------------------------------------------------------------------
state.pca <- princomp(state.x77.df, cor=T)
summary(state.pca,loadings=F)

## -----------------------------------------------------------------------------
# sigma = 1
set.seed(1)
sigma <- 1
n <- 1000
u <- runif(n)
x <- (-2*sigma^2*log(1-u))^0.5
hist(x, prob = TRUE, main = expression(sigma==1))
y <- seq(0,5,0.01)
lines(y,y/sigma^2*exp(1)^(-y^2/(2*sigma^2)))

## -----------------------------------------------------------------------------
# sigma = 2
sigma <- 2
x <- (-2*sigma^2*log(1-u))^0.5
hist(x, prob = TRUE, main = expression(sigma==2))
y <- seq(0,8,0.01)
lines(y,y/sigma^2*exp(1)^(-y^2/(2*sigma^2)))

## -----------------------------------------------------------------------------
# sigma = 4
sigma <- 4
x <- (-2*sigma^2*log(1-u))^0.5
hist(x, prob = TRUE, main = expression(sigma==4))
y <- seq(0,15,0.01)
lines(y,y/sigma^2*exp(1)^(-y^2/(2*sigma^2)))

## -----------------------------------------------------------------------------
# p_1 = 0.75
set.seed(2)
n <- 1000
x1 <- rnorm(n,0,1)
x2 <- rnorm(n,3,1)
p1 <- 0.75
p <- sample(c(1,0),n,TRUE,c(p1,1-p1))
z <- p*x1 + (1-p)*x2
hist(z, prob = TRUE, main = expression(p_1==0.75))

## -----------------------------------------------------------------------------
# p_1 = 0.5
set.seed(2)
p1 <- 0.5
p <- sample(c(1,0),n,TRUE,c(p1,1-p1))
z <- p*x1 + (1-p)*x2
hist(z, prob = TRUE, main = expression(p_1==0.5))

## -----------------------------------------------------------------------------
# p_1 = 0.6
set.seed(2)
p1 <- 0.6
p <- sample(c(1,0),n,TRUE,c(p1,1-p1))
z <- p*x1 + (1-p)*x2
hist(z, prob = TRUE, main = expression(p_1==0.6))

## -----------------------------------------------------------------------------
# p_1 = 0.65
set.seed(2)
p1 <- 0.65
p <- sample(c(1,0),n,TRUE,c(p1,1-p1))
z <- p*x1 + (1-p)*x2
hist(z, prob = TRUE, main = expression(p_1==0.65))

## -----------------------------------------------------------------------------
# lambda = 2, r = 4, beta = 3
set.seed(3)
n <- 1000
lambda <- 2
r <- 4
beta <- 3
N <- vector(length = n)
for (i in 1:n) {
  N[i] <- rpois(1,lambda*i)
}
Y <- rgamma(max(N),r,beta)
X <- vector(length = n)
for (i in 1:n) {
  X[i] <- sum(Y[1:N[i]])
}
hist(x, prob = TRUE, main = 'lambda = 2, r = 4, beta = 3')

## -----------------------------------------------------------------------------
E <- mean(Y)
E2 <- mean(Y^2)

## -----------------------------------------------------------------------------
lambda*10*E # estimate mean
lambda*10*E2 # estimate variance

## -----------------------------------------------------------------------------
lambda <- 4
r <- 8
beta <- 6
N <- vector(length = n)
for (i in 1:n) {
  N[i] <- rpois(1,lambda*i)
}
Y <- rgamma(max(N),r,beta)

## -----------------------------------------------------------------------------
E <- mean(Y)
E2 <- mean(Y^2)

## -----------------------------------------------------------------------------
lambda*10*E # estimate mean
lambda*10*E2 # estimate variance

## -----------------------------------------------------------------------------
lambda <- 4
r <- 2
beta <- 4
N <- vector(length = n)
for (i in 1:n) {
  N[i] <- rpois(1,lambda*i)
}
Y <- rgamma(max(N),r,beta)

## -----------------------------------------------------------------------------
E <- mean(Y)
E2 <- mean(Y^2)

## -----------------------------------------------------------------------------
lambda*10*E # estimate mean
lambda*10*E2 # estimate variance

## -----------------------------------------------------------------------------
set.seed(1)
pdf <- function(x){x^2*(1-x)^2/beta(3,3)}
cdf <- function(x){
  U <- runif(1e4, 0, x)
  return(x*mean(pdf(U)))
}

## -----------------------------------------------------------------------------
x <- seq(0.1,0.9,0.1)
MC <- vector(length = length(x))
for (i in 1:length(x)) {
  MC[i] <- cdf(x[i])
}
B <- pbeta(x,3,3)
print(round(rbind(x, MC, B), 9))
# MC is estimate, B is real

## -----------------------------------------------------------------------------
MC.Phi <- function(x, R = 10000, antithetic = TRUE) {
  u <- runif(R/2)
  if (!antithetic) v <- runif(R/2) else
    v <- 1 - u
  u <- c(u, v)
  cdf <- numeric(length(x))
  for (i in 1:length(x)) {
    g <- x[i] *u*x[i]* exp(-(u * x[i])^2 / 2)
    cdf[i] <- mean(g) + 0.5
  }
  cdf
}

## -----------------------------------------------------------------------------
set.seed(1)
x <- seq(.1, 2.5, length=100)
MC1 <- MC.Phi(x, anti = FALSE)
MC2 <- MC.Phi(x)
(var(MC2)-var(MC1))/var(MC1)

## -----------------------------------------------------------------------------
set.seed(1)
U <- runif(1e4)
X <- -log(1-U)
Y <- X^2/(2*3.14)^0.5 * exp(X-X^2/2)
mean(Y)

## -----------------------------------------------------------------------------
set.seed(4)
X <- rchisq(20,2)
t.test(X,mu=2,conf.level = 0.95)

## -----------------------------------------------------------------------------
Y <- rchisq(1e4,2)
Z <- c()
for (i in 1:1e4) {
  Z[i] <- (Y[i]> 1.250399&&Y[i]<3.304838)
}
mean(Z) #coverage probability

## -----------------------------------------------------------------------------
set.seed(1)
X <- rnorm(20)
t.test(X,mu=0,conf.level = 0.95)

## -----------------------------------------------------------------------------
set.seed(4)
Y <- rnorm(1e4)
Z <- c()
for (i in 1:1e4) {
  Z[i] <- (Y[i]> -0.2368920&&Y[i]<0.6179398)
} 
mean(Z) #coverage probability

## -----------------------------------------------------------------------------
p <- vector(length = 1e4)
for (i in 1:1e4) {
  X <- rchisq(10,1)
  a <- t.test(X,mu = 1 ,conf.level = 0.9) #alpha = 0.1
  p[i] <- a$p.value
}
mean(p<=0.1)

## -----------------------------------------------------------------------------
p <- vector(length = 1e4)
for (i in 1:1e4) {
  X <- runif(10,0,2)
  a <- t.test(X,mu = 1,conf.level = 0.9)
  p[i] <- a$p.value
}
mean(p<=0.1)

## -----------------------------------------------------------------------------
p <- vector(length = 1e4)
for (i in 1:1e4) {
  X <- rexp(10,1)
  a <- t.test(X,mu = 1,conf.level = 0.9)
  p[i] <- a$p.value
}
mean(p<=0.1)

## -----------------------------------------------------------------------------
X <- c(rep(1,6510),rep(0,3490))
Y <- c(rep(1,6760),rep(0,3240))
t.test(X,Y)

## -----------------------------------------------------------------------------
d <- 2
n <- c(10, 20, 30 ,50, 100, 500) #sample sizes
cv <- qchisq(.95, d*(d+1)*(d+2)/6)*6/n #crit. values for each n

## -----------------------------------------------------------------------------
b1d <- function(x) {
#computes the multivariate skewness statistic
  n <- nrow(x)
  x.bar <- colMeans(x)
  x.bar.matrix <- matrix(rep(x.bar,n),ncol = n,nrow = d)
  x.bar.matrix <- t(x.bar.matrix)
  sigma.hat <- cov(x)
  b <- ((x-x.bar.matrix) %*% solve(sigma.hat) %*% t(x-x.bar.matrix))^3
  b <- mean(b)
  return(b)
}

## -----------------------------------------------------------------------------
library(MASS)
x.gener <- function(d,n){
#x generation process
  mean <- rep(0,d)
  sigma <- matrix(0,nrow = d,ncol = d)
  for (i in 1:d) {
    sigma[i,i] <- 1
  }
  x <- mvrnorm(n, mean, sigma)
}

## -----------------------------------------------------------------------------
#p.reject <- numeric(length(n)) #to store sim. results
#m <- 10000

#for (i in 1:length(n)) {
#  sktests <- numeric(m) #test decisions
#  for (j in 1:m) {
#    x <- x.gener(d,n[i])
#    #test decision is 1 (reject) or 0
#    sktests[j] <- as.integer(abs(b1d(x)) >= cv[i] )
#  }
#  p.reject[i] <- mean(sktests) #proportion rejected
#}
#p.reject
#corresponding to n = 10, 20, 30, 50, 100, 500

## -----------------------------------------------------------------------------
#set.seed(1)
#alpha <- .1
#n <- 500
#d <- 3
#m <- 2500
#epsilon <- c(seq(0, .15, .01), seq(.15, 1, .05))
#N <- length(epsilon)
#pwr <- numeric(N)

##critical value for the skewness test
#cv <- cv <- qchisq(.95, d*(d+1)*(d+2)/6)*6/n
#for (j in 1:N) { #for each epsilon
#  e <- epsilon[j]
#  sktests <- numeric(m)
#  for (i in 1:m) { #for each replicate
#    sigma1 <- diag(rep(1,d))
#    sigma2 <- diag(rep(10,d))
#    mean <- rep(0,d)
#    x1 <- mvrnorm(n, mean, sigma1)
#    x2 <- mvrnorm(n, mean, sigma2)
#    p <- sample(c(1,0),replace = TRUE, size = n*d,prob = c(1-e,e))
#    x <- x1*p+x2*(1-p)
#    sktests[i] <- as.integer(abs(b1d(x)) >= cv)
#  } 
#  pwr[j] <- mean(sktests)
#}

##plot power vs epsilon
#plot(epsilon, pwr, type = "b",
#xlab = bquote(epsilon), ylim = c(0,0.8))
##abline(h = .1, lty = 3)
#se <- sqrt(pwr * (1-pwr) / m) #add standard errors
#lines(epsilon, pwr+se, lty = 3)
#lines(epsilon, pwr-se, lty = 3)

## -----------------------------------------------------------------------------
library(bootstrap)
sigma.hat <- cov(scor)
lambda.hat <- eigen(sigma.hat)$values
theta.hat <- lambda.hat[1]/sum(lambda.hat)
theta.hat

## -----------------------------------------------------------------------------
theta.jack <- numeric(nrow(scor))
for (i in 1:nrow(scor)) {
  data <- scor[-i,]
  sigma <- cov(data)
  lambda <- eigen(sigma)$values
  theta.jack[i] <- lambda[1]/sum(lambda)
}
bias <- (nrow(scor)-1)*(mean(theta.jack)-theta.hat)
bias

## -----------------------------------------------------------------------------
se <- sqrt((nrow(scor)-1)*mean((theta.jack-theta.hat)^2))
se

## -----------------------------------------------------------------------------
library(boot)
set.seed(1)
theta <- function(data,indices){
  d <- data[indices,]
  sigma.hat <- cov(d)
  lambda.hat <- eigen(sigma.hat)$values
  theta.hat <- lambda.hat[1]/sum(lambda.hat)
  return(theta.hat)
}
result <- boot(data = scor, statistic = theta, R=1000)
boot.ci(result,conf = 0.95,type = "bca")

## -----------------------------------------------------------------------------
sk <- function(data,indices) {
#computes the sample skewness coeff.
  x <- data[indices]
  xbar <- mean(x)
  m3 <- mean((x - xbar)^3)
  m2 <- mean((x - xbar)^2)
  return( m3 / m2^1.5 )
}
getbc <- function(x){
  result <- boot(data = x,statistic = sk,R=1000)
  bc <- boot.ci(result,conf = 0.95,type = c("norm","basic","perc"))
  norm.ci <- c(bc$normal[2:3])
  basic.ci <- c(bc$basic[4:5])
  percent.ci <- c(bc$percent[4:5])
  matri <- as.matrix(rbind(rbind(norm.ci, basic.ci),percent.ci))
  return(matri)
}

## -----------------------------------------------------------------------------
##Monte Carlo progress
#norm.cr <- numeric(1000)
#basic.cr <- numeric(1000)
#percent.cr <- numeric(1000)
#for (i in 1:1000) {
#  x <- rnorm(100)
#  ci <- getbc(x)
#  norm.cr[i] <- as.integer(0 < ci[1,2]&0 > ci[1,1])
#  basic.cr[i] <- as.integer(0 < ci[2,2]&0 > ci[2,1])
#  percent.cr[i] <- as.integer(0 < ci[3,2]&0 > ci[3,1])
#}
#norm.cr <- mean(norm.cr)
#norm.cr #coverage probabilities of the standard normal bootstrap confidence interval

## -----------------------------------------------------------------------------
#basic.cr <- mean(basic.cr)
#basic.cr #coverage probabilities of the basic bootstrap confidence interval

## -----------------------------------------------------------------------------
#percent.cr <- mean(percent.cr)
#percent.cr #coverage probabilities of the percentile confidence interval

## -----------------------------------------------------------------------------
#Monte Carlo progress
#norm.cr <- numeric(1000)
#basic.cr <- numeric(1000)
#percent.cr <- numeric(1000)
#for (i in 1:1000) {
#  x <- rchisq(100,df=5)
#  ci <- getbc(x)
#  norm.cr[i] <- as.integer(sqrt(8/5) < ci[1,2]& sqrt(8/5) > ci[1,1])
#  basic.cr[i] <- as.integer(sqrt(8/5) < ci[2,2]& sqrt(8/5) > ci[2,1])
#  percent.cr[i] <- as.integer(sqrt(8/5) < ci[3,2]& sqrt(8/5) > ci[3,1])
#}
#norm.cr <- mean(norm.cr)
#norm.cr #coverage probabilities of the standard normal bootstrap confidence interval

## -----------------------------------------------------------------------------
#basic.cr <- mean(basic.cr)
#basic.cr #coverage probabilities of the basic bootstrap confidence interval

## -----------------------------------------------------------------------------
#percent.cr <- mean(percent.cr)
#percent.cr #coverage probabilities of the percentile confidence interval

## -----------------------------------------------------------------------------
x <- rnorm(100)
y <- rnorm(100)
cor(x,y,method = "spearman")

## -----------------------------------------------------------------------------
cor.test(x,y)

## -----------------------------------------------------------------------------
# NN method
library(RANN)
library(boot)
Tn <- function(z, ix, sizes,k) {
    n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
    if(is.vector(z)) z <- data.frame(z,0)
    z <- z[ix, ]
    NN <- nn2(data=z, k=k+1) 
    block1 <- NN$nn.idx[1:n1,-1]
    block2 <- NN$nn.idx[(n1+1):n,-1]
    i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5)
    (i1 + i2) / (k * n)
}

## -----------------------------------------------------------------------------
#pow.nn <- numeric(length = 200)
#for (i in 1:200) {# monte carlo
#  
#  x <- rnorm(30,0,1)
#  y <- rnorm(30,0,2)
#  
#  z <- c(x, y)
#  N <- c(length(x), length(y))
#  
#  boot.obj <- boot(data = z, statistic = Tn, R = 999,
#    sim = "permutation", sizes = N,k=3)
#  ts <- c(boot.obj$t0,boot.obj$t)
#  p.value <- mean(ts>=ts[1])
#  pow.nn[i] <- as.integer(p.value<0.05)
#}
#pow.nn <- mean(pow.nn)
#pow.nn #power of NN test

## -----------------------------------------------------------------------------
# energy test
#library(energy)
#pow.energy <- numeric(length = 200)
#for (i in 1:200) {
#  x <- rnorm(30,0,1)
#  y <- rnorm(30,0,2)
  
#  z <- c(x,y)
#  N <- c(length(x), length(y))
  
#  boot.obs <- eqdist.etest(z, sizes=N, R=999)
#  p.value <- boot.obs$p.value
  
#  pow.energy[i] <- as.integer(p.value<0.05)
#}
#pow.energy <- mean(pow.energy)
#pow.energy # power of energy test

## -----------------------------------------------------------------------------
# ball method
#library(Ball)
#pow.ball <- numeric(length = 200)
#for (i in 1:200) {
#  x <- rnorm(30,0,1)
#  y <- rnorm(30,0,2)
  
#  z <- c(x,y)
#  N <- c(length(x), length(y))
  
#  p.value = bd.test(x = x, y = y, num.permutations=999)$p.value
  
#  pow.ball[i] <- as.integer(as.numeric(p.value)<0.05)
#}
#pow.ball <- mean(pow.ball)
#pow.ball # power of ball method


## -----------------------------------------------------------------------------
#data.frame(pow.nn,pow.energy,pow.ball)

## -----------------------------------------------------------------------------
# NN method
#pow.nn <- numeric(length = 200)
#for (i in 1:200) {# monte carlo
#  x <- rnorm(30,0,1)
#  y <- rnorm(30,0.5,2)
  
#  z <- c(x, y)
#  N <- c(length(x), length(y))
  
#  boot.obj <- boot(data = z, statistic = Tn, R = 999,
#    sim = "permutation", sizes = N,k=3)
#  ts <- c(boot.obj$t0,boot.obj$t)
#  p.value <- mean(ts>=ts[1])
#  pow.nn[i] <- as.integer(p.value<0.05)
#}
#pow.nn <- mean(pow.nn)
#pow.nn #power of NN test

## -----------------------------------------------------------------------------
# energy test
#pow.energy <- numeric(length = 200)
#for (i in 1:200) {
#  x <- rnorm(30,0,1)
#  y <- rnorm(30,0.5,2)
  
#  z <- c(x,y)
#  N <- c(length(x), length(y))
  
#  boot.obs <- eqdist.etest(z, sizes=N, R=999)
#  p.value <- boot.obs$p.value
  
#  pow.energy[i] <- as.integer(p.value<0.05)
#}
#pow.energy <- mean(pow.energy)
#pow.energy # power of energy test

## -----------------------------------------------------------------------------
# ball method
#pow.ball <- numeric(length = 200)
#for (i in 1:200) {
#  x <- rnorm(30,0,1)
#  y <- rnorm(30,0.5,2)
  
#  z <- c(x,y)
#  N <- c(length(x), length(y))
  
#  p.value = bd.test(x = x, y = y, num.permutations=999)$p.value
  
#  pow.ball[i] <- as.integer(as.numeric(p.value)<0.05)
#}
#pow.ball <- mean(pow.ball)
#pow.ball # power of ball method


## -----------------------------------------------------------------------------
#data.frame(pow.nn,pow.energy,pow.ball)

## -----------------------------------------------------------------------------
# NN method
#pow.nn <- numeric(length = 200)
#for (i in 1:200) {# monte carlo
#  x <- rt(30,df=1)
#  y1 <- rnorm(30,0,1)
#  y2 <- rnorm(30,0,2)
#  p <- sample(c(0,1),size = 1)
#  y <- p*y1+(1-p)*y2
#  
#  z <- c(x, y)
#  N <- c(length(x), length(y))
#  
#  boot.obj <- boot(data = z, statistic = Tn, R = 999,
#    sim = "permutation", sizes = N,k=3)
#  ts <- c(boot.obj$t0,boot.obj$t)
#  p.value <- mean(ts>=ts[1])
#  pow.nn[i] <- as.integer(p.value<0.05)
#}
#pow.nn <- mean(pow.nn)
#pow.nn #power of NN test

## -----------------------------------------------------------------------------
# energy test
#pow.energy <- numeric(length = 200)
#for (i in 1:1000) {
#  x <- rt(30,df=1)
#  y1 <- rnorm(30,0,1)
#  y2 <- rnorm(30,0,2)
#  p <- sample(c(0,1),size = 1)
#  y <- p*y1+(1-p)*y2
#  
#  z <- c(x,y)
#  N <- c(length(x), length(y))
#  
#  boot.obs <- eqdist.etest(z, sizes=N, R=999)
#  p.value <- boot.obs$p.value
#  
#  pow.energy[i] <- as.integer(p.value<0.05)
#}
#pow.energy <- mean(pow.energy)
#pow.energy # power of energy test

## -----------------------------------------------------------------------------
# ball method
#pow.ball <- numeric(length = 200)
#for (i in 1:1000) {
#  x <- rt(30,df=1)
#  y1 <- rnorm(30,0,1)
#  y2 <- rnorm(30,0,2)
#  p <- sample(c(0,1),size = 1)
#  y <- p*y1+(1-p)*y2
#  
#  z <- c(x,y)
#  N <- c(length(x), length(y))
#  
#  p.value = bd.test(x = x, y = y, num.permutations=999)$p.value
#  
#  pow.ball[i] <- as.integer(as.numeric(p.value)<0.05)
#}
#pow.ball <- mean(pow.ball)
#pow.ball # power of ball method


## -----------------------------------------------------------------------------
#data.frame(pow.nn,pow.energy,pow.ball)

## -----------------------------------------------------------------------------
# NN method
#pow.nn <- numeric(length = 200)
#for (i in 1:200) {# monte carlo
#  x <- rnorm(10,0,1)
#  y <- rnorm(50,1,2)
#  
#  z <- c(x, y)
#  N <- c(length(x), length(y))
#  
#  boot.obj <- boot(data = z, statistic = Tn, R = 999,
#    sim = "permutation", sizes = N,k=3)
#  ts <- c(boot.obj$t0,boot.obj$t)
#  p.value <- mean(ts>=ts[1])
#  pow.nn[i] <- as.integer(p.value<0.05)
#}
#pow.nn <- mean(pow.nn)
#pow.nn #power of NN test

## -----------------------------------------------------------------------------
# energy test
#pow.energy <- numeric(length = 200)
#for (i in 1:200) {
#  x <- rnorm(10,0,1)
#  y <- rnorm(50,1,2)
#  
#  z <- c(x,y)
#  N <- c(length(x), length(y))
#  
#  boot.obs <- eqdist.etest(z, sizes=N, R=999)
#  p.value <- boot.obs$p.value
#  
#  pow.energy[i] <- as.integer(p.value<0.05)
#}
#pow.energy <- mean(pow.energy)
#pow.energy # power of energy test

## -----------------------------------------------------------------------------
# ball method
#pow.ball <- numeric(length = 200)
#for (i in 1:200) {
#  x <- rnorm(10,0,1)
#  y <- rnorm(50,1,2)
#  
#  z <- c(x,y)
#  N <- c(length(x), length(y))
#  
#  p.value = bd.test(x = x, y = y, num.permutations=999)$p.value
#  
#  pow.ball[i] <- as.integer(as.numeric(p.value)<0.05)
#}
#pow.ball <- mean(pow.ball)
#pow.ball # power of ball method


## -----------------------------------------------------------------------------
#data.frame(pow.nn,pow.energy,pow.ball)

## -----------------------------------------------------------------------------
options (warn = -1)
f <- function(x){ # target pdf
  return(1/(pi*(1+x^2)))
}

g <- function(a,b){#g(a|b), proposal pdf
  return(dnorm(a,b))
}

set.seed(2)
X <- numeric(length = 10000)
X[1] <- 0

for (i in 2:16000) {
  Y <- rnorm(1,X[i-1])
  U <- runif(1)
  alpha <- f(Y)*g(X[i-1],Y)/(f(X[i-1])*g(Y,X[i-1]))
  if(U<=alpha)X[i] <- Y else X[i] <- X[i-1]
}
X <- X[-(1:1000)]

## -----------------------------------------------------------------------------
df <- data.frame(qt=qt((1:9)/10,df=1),X=quantile(X, probs = (1:9)/10))
rownames(df) <- c("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9")
df

## -----------------------------------------------------------------------------
n <- 5
a <- 2
b <- 3

## -----------------------------------------------------------------------------
X <- numeric(length = 1000)
Y <- numeric(length = 1000)
X[1] <- 1
Y[1] <- 1

for (i in 2:1000) {
  Y[i] <- rbeta(1,X[i-1]+a,n-X[i-1]+b)
  X[i] <- rbinom(1,n,Y[i])
}

## -----------------------------------------------------------------------------
data.frame(X=X[701:710],Y=Y[701:710])

## -----------------------------------------------------------------------------
# k chains
# n units each chain

getX <- function(n){
  k <- 100
  X <- matrix(nrow = n,ncol = k) # one colomn is one chain
  for (i in 1:k) {
    X[1,i] <- rnorm(1)
    for (j in 2:n) {
      Y <- rnorm(1,X[j-1,i])
      U <- runif(1)
      alpha <- f(Y)*g(X[j-1,i],Y)/(f(X[j-1,i])*g(Y,X[j-1,i]))
      if(U<=alpha)X[j,i] <- Y else X[j,i] <- X[j-1,i] 
    }
  }
  return(X)
}

## -----------------------------------------------------------------------------
# phi_in is X[,i]
# phi_i,j is X[j,i]

getR.hat <- function(n){
  k <- 100
  X <- getX(n)
  
  Xbar <- mean(X)
  Bn <- 0
  for (i in 1:k) {
    Bn <- Bn + (mean(X[,i])-Xbar)^2
  }
  Bn <- n*Bn/(k-1)
  
  Wn <- 0
  for (i in 1:k) {
    for (j in 1:n) {
      Wn <- Wn + (X[j,i]-mean(X[,i]))^2
    }
  }
  Wn <- Wn/n/k
  
  var.hat <- (n-1)*Wn/n + Bn/n
  R.hat <- var.hat/Wn
  return(R.hat)
}

## -----------------------------------------------------------------------------
getR.hat(100)

## -----------------------------------------------------------------------------
k <- 10   # k chains
n <- 100 # n units each chain

n. <- 5
a <- 2
b <- 3

## -----------------------------------------------------------------------------
X <- matrix(nrow = n,ncol = k) # one colomn is one chain
Y <- matrix(nrow = n,ncol = k) # one colomn is one chain
X[1] <- 1
Y[1] <- 1

for (j in 1:k) {
  X[1,j] <- 1
  Y[1,j] <- 1
  
  for (i in 2:n) {
    Y[i,j] <- rbeta(1,X[i-1,j]+a,n.-X[i-1,j]+b)
    X[i,j] <- rbinom(1,n.,Y[i,j])
  }
}


## -----------------------------------------------------------------------------
Xbar <- mean(X)
Bn.X <- 0
for (i in 1:k) {
  Bn.X <- Bn.X + (mean(X[,i])-Xbar)^2
}
Bn.X <- n*Bn.X/(k-1)

## -----------------------------------------------------------------------------
Wn.X <- 0
for (i in 1:k) {
  for (j in 1:n) {
    Wn.X <- Wn.X + (X[j,i]-mean(X[,i]))^2
  }
}
Wn.X <- Wn.X/n/k

## -----------------------------------------------------------------------------
var.hat.X <- (n-1)*Wn.X/n + Bn.X/n
R.hat.X <- var.hat.X/Wn.X
R.hat.X

## -----------------------------------------------------------------------------
options (warn = -1)
Ybar <- mean(Y)
Bn.Y <- 0
for (i in 1:k) {
  Bn.Y <- Bn.Y + (mean(Y[,i])-Ybar)^2
}
Bn.Y <- n*Bn.Y/(k-1)

## -----------------------------------------------------------------------------
Wn.Y <- 0
for (i in 1:k) {
  for (j in 1:n) {
    Wn.Y <- Wn.Y + (Y[j,i]-mean(Y[,i]))^2
  }
}
Wn.Y <- Wn.Y/n/k

## -----------------------------------------------------------------------------
var.hat.Y <- (n-1)*Wn.Y/n + Bn.Y/n
R.hat.Y <- var.hat.Y/Wn.Y
R.hat.Y

## -----------------------------------------------------------------------------
fk <- function(k,d,a){
  
  return(   (-1)^k * exp( log(sum(a^2)^(k+1))+lgamma((d+1)/2)+lgamma(k+3/2) - lgamma(k+1)-log(2^k)-log(2*k+1)-log(2*k+2)-lgamma(k+d/2+1) )    )
  
  
}

## -----------------------------------------------------------------------------
fk(10,5,rep(1,5))

## -----------------------------------------------------------------------------
fk(50,10,rep(1,10))

## -----------------------------------------------------------------------------
fk(100,20,rep(1,20))

## -----------------------------------------------------------------------------
fk(200,30,rep(1,30))

## -----------------------------------------------------------------------------
f <- function(d,a){
  
  index <- 1
  while (fk(index,d,a>1e-16)) {
    index <- index + 1
  }
  
  sum <- 0
  for (i in 1:index) {
    sum <- sum + fk(i,d,a)
  }
  return(sum)
}

## -----------------------------------------------------------------------------
f(d=2,a=c(1,2))

## -----------------------------------------------------------------------------
f1 <- function(u,a){
  (1+u^2/(k-1))^(-k/2)
}
f2 <- function(u,a){
  (1+u^2/k)^(-(k+1)/2)
}
  
f <- function(a){
  2*exp(lgamma(k/2)-log(pi*(k-1))/2-lgamma((k-1)/2))*integrate(f1,lower = 0,upper = (a^2*(k-1)/(k-a^2))^(1/2),a=a)$value  -  2*exp( lgamma((k+1)/2)-log(pi*k)/2-lgamma(k/2)*integrate(f2,lower = 0,upper = (a^2*k/(k+1-a^2))^(1/2),a=a)$value  )
  
}

## -----------------------------------------------------------------------------
k_value <- c(4:25,100,500,1000)
A <- c()
for (i in 1:length(k_value)) {
  k <- k_value[i]
  res <- uniroot(f,c(0,2))
  A[i] <- unlist(res)[1]
}
data.frame(k=k_value,A_k=A)

## -----------------------------------------------------------------------------
# 11.4
f <- function(a){
  pt((a^2*(k-1)/(k-a^2))^(1/2),df=k-1) - pt((a^2*k/(k+1-a^2))^(1/2),df=k)
}



## -----------------------------------------------------------------------------
#k_value <- c(4:25,100,500,1000)
#A <- c()
#for (i in 1:length(k_value)) {
#  k <- k_value[i]
#  res <- uniroot(f,c(0,k^(1/2)))
#  A[i] <- unlist(res)[1]
#}
#data.frame(k=k_value,A_k=A)

## -----------------------------------------------------------------------------
k <- 9
res <- uniroot(f,c(0,k^(1/2)))
unlist(res)

## -----------------------------------------------------------------------------
y <- c(0.54,0.48,0.33,0.43,0.91,0.21,0.85)
x <- c(1,1,1)

## -----------------------------------------------------------------------------
lambda <- mean(c(x,y))
lambda_new <- lambda+1
while (abs(lambda-lambda_new)>1e-16) {
  lambda_new <- lambda
  # E step
  x <- rep(1+lambda_new,3)
  
  # M step
  lambda <- mean(c(x,y))
  
}
lambda

## -----------------------------------------------------------------------------
mean(y)

## -----------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared

## -----------------------------------------------------------------------------
# 3.
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)
f <- function(formula){
  mod <- lm(formula,mtcars)
  return(rsq(mod))
}
lapply(formulas, f)

## -----------------------------------------------------------------------------
# 4.
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})


rsq <- function(data){
  mod <- lm(mpg~disp,data=data)
  return(summary(mod)$r.squared)
}

## -----------------------------------------------------------------------------
# lapply
lapply(bootstraps, rsq)

## -----------------------------------------------------------------------------
# for loop
for (i in 1:10) {
  data <- as.data.frame(bootstraps[i])
  print(rsq(data))
}

## -----------------------------------------------------------------------------
x <- data.frame(x1=rnorm(10),x2=rchisq(10,2),x3=runif(10))
x # dataframe

## -----------------------------------------------------------------------------
round(vapply(x,sd,FUN.VALUE = c(sd= 1)),3)

## -----------------------------------------------------------------------------
x <- data.frame(x1=letters[1:10],x2=rnorm(10),x3=runif(10),x4=letters[11:20])
x # dataframe

## -----------------------------------------------------------------------------
round(vapply(x[which(vapply(x, is.numeric, FUN.VALUE = 1)==1)], sd, FUN.VALUE = c(sd=1)),3)

## -----------------------------------------------------------------------------
# mcsapply
library(parallel)
mcsapply <- function(X, FUN, ..., simplify = TRUE,USE.NAMES = TRUE, mc.cores=2){
  
  cl <- makeCluster(cores)
  res <- parSapply(cl,X,FUN,...,simplify = simplify, USE.NAMES = USE.NAMES)
  stopCluster(cl)
  
  return(res)
}

## -----------------------------------------------------------------------------
# gibbs in rcpp
library(Rcpp)
cppFunction('
  
NumericMatrix gibbs_cpp(int n, int a, int b){
  NumericMatrix df(10000,2);
  NumericVector X(10000);
  NumericVector Y(10000);
  NumericVector temp(1);

  X[0]=1;
  Y[0]=1;

  for (int i=1;i<10000;i++){
    temp=rbeta(1,X[i-1]+a,n-X[i-1]+b);
    Y[i]=temp[0];
    temp=rbinom(1,n,Y[i]);
    X[i]=temp[0];
  }

  df(_,0)=X;
  df(_,1)=Y;
  
  return df;
}

')

## -----------------------------------------------------------------------------
data_cpp <- gibbs_cpp(5,2,3)
# 1st column is X, 2nd column is Y

## -----------------------------------------------------------------------------
# gibbs in r
gibbs_r <- function(n,a,b){
  X <- numeric(length = 10000)
  Y <- numeric(length = 10000)
  X[1] <- 1
  Y[1] <- 1

  for (i in 2:10000) {
    Y[i] <- rbeta(1,X[i-1]+a,n-X[i-1]+b)
    X[i] <- rbinom(1,n,Y[i])
  }
  
  return(data.frame(X=X,Y=Y))
}

## -----------------------------------------------------------------------------
data_r <- gibbs_r(5,2,3)

## -----------------------------------------------------------------------------
qqplot(x=data_cpp[,1],y=data_r$X,xlab = 'X from cpp',ylab = 'X from R')

## -----------------------------------------------------------------------------
qqplot(x=data_cpp[,2],y=data_r$Y,xlab = 'Y from cpp',ylab = 'Y from R')

## -----------------------------------------------------------------------------
library(microbenchmark)
ts <- microbenchmark(gibbsR=gibbs_r(5,2,3),gibbsC=gibbs_cpp(5,2,3))
summary(ts)[,c(1,3,5,6)]

