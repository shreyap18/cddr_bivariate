est.fun1 <- function(X,e,n)
{
  K <- L <- matrix(NA,nrow = n,ncol = n)
  I <- matrix(1,nrow=n,ncol=1)
  H <- diag(1,nrow = n,ncol = n)-I %*% t(I)/n

  L <- exp(-(e%*%t(I) - I%*%t(e))^2)
  K <- exp(-(X%*%t(I) - I%*%t(X))^2)

  sum(diag(K%*%H%*%L%*%H))/n^2
}

##xony
do.one.xony.seperate <- function(seed.id,dataset,theta)
{
  ## randomization
  set.seed(seed.id+2333)
  ###
  
  # splines
  # xiony <- formula("sleep ~ splines::bs(mbdi, knots = c(12, 16))")
  
  # not splines
  xiony <- formula("sleep~mbdi")
  fitxiony <- lm(xiony, data=dataset)
  betai <- coef(fitxiony)
  e.yi <- residuals(fitxiony)
  e.centered <- e.yi - mean(e.yi)

  n <- nrow(dataset)
  bindex1 <- sample(1:n,n,replace = TRUE)
  bindex2 <- sample(1:n,n,replace = TRUE)
  xi <- data.frame(dataset[,"mbdi"])
  colnames(xi) <- "mbdi"

  X.star <- data.frame(xi[bindex2,])
  colnames(X.star) <- "mbdi"
  newdata <- data.frame(X.star)
  eta.star <- as.matrix(e.centered[bindex1])

  Y.star <- predict(fitxiony,newdata = newdata) + eta.star
  fit.star <- lm(Y.star~.,data = newdata)
  e.star <- residuals(fit.star)

  thetai <- est.fun1(X.star[,1],e.star,n)
  thetahat.star <- thetai

  return(list(thetahat.star,thetahat.star >= theta))
}

##yonx
do.one.yonx.seperate <- function(seed.id,dataset,theta)
{
  ## randomization
  set.seed(seed.id+2333)
  ###
  
  # splines
  # yonxi <- formula("mbdi ~ splines::bs(sleep, knots = c(0.8))")
  
  # not splines
  yonxi <- formula("mbdi~sleep")
  xi <- data.frame(dataset[,"sleep"])
  colnames(xi) <- "sleep"
  fityonxi <- lm(yonxi, data=dataset)
  e.yi <- residuals(fityonxi)
  e.centered <- e.yi - mean(e.yi)

  n <- nrow(dataset)
  bindex1 <- sample(1:n,n,replace = TRUE)
  bindex2 <- sample(1:n,n,replace = TRUE)

  X.star <- data.frame(xi[bindex2,])
  colnames(X.star) <- "sleep"
  newdata <- data.frame(X.star)
  eta.star <- as.matrix(e.centered[bindex1])

  Y.star <- predict(fityonxi,newdata = newdata) + eta.star
  fit.star <- lm(Y.star~.,data = newdata)
  e.star <- residuals(fit.star)

  thetahat.star <- est.fun1(X.star[,1],e.star,n)

  return(list(thetahat.star,thetahat.star >= theta))
}