pointest <- function(arg, samplesize, main_dir, subdata_all)
{
  ## randomization
  seed <- seed.vec[arg]
  # num <- nums[arg]
  # current_dir <- file.path(main_dir,paste0("dataset_",num,"_",samplesize))
  ###
  current_dir <- file.path(main_dir,paste0("dataset_",seed,"_",samplesize))
  dir.create(current_dir)
  xony_dir <- file.path(current_dir,"xony")
  yonx_dir <- file.path(current_dir,"yonx")
  
  dir.create(xony_dir)
  dir.create(yonx_dir)
  
  # set.seed(seed)
  
  ## sample w replacement
  ## this is the regular way of sampling
  index <- sample(1:nrow(subdata_all),samplesize, replace = T)
  
  ## sample w replacement
  ## this is when we want a stratified sample per dose group
  # groups <- unique(subdata_all$sleep)
  # index <- unlist(lapply(groups, function(group) {
  #   group_indices <- which(subdata_all$sleep == group)
  #   sample(group_indices, samplesize/length(groups), replace = TRUE)
  # }))
  # 
  randomdataset1 <- subdata_all[index,]
  
  write.csv(randomdataset1,file.path(current_dir,"dataset.csv"))

  ############################################################yonx############################################################
  ####################combined
  est.fun <- function(X,e,n)
  {
    K <- L <- matrix(NA,nrow = n,ncol = n)
    I <- matrix(1,nrow=n,ncol=1)
    H <- diag(1,nrow = n,ncol = n)-I %*% t(I)/n
    
    L <- exp(-(e%*%t(I) - I%*%t(e))^2)
    K <- exp(-(X%*%t(I) - I%*%t(X))^2)
    
    sum(diag(K%*%H%*%L%*%H))/n^2
  }
  
  yonx <- formula("mbdi~sleep")
  # ## for anm for bmd data
  # yonx <- formula("mbdi~splines::bs(sleep, knots = c(12, 16))")
  X <- randomdataset1[,"sleep"]
  fityonx <- lm(yonx, data=randomdataset1)
  beta <- coef(fityonx)
  e.y <- residuals(fityonx)
  theta <- est.fun(X,e.y,nrow(randomdataset1))

  write.csv(theta,file.path(yonx_dir,"thetahat.csv"))
  ####################
  
  ############################################################xony######################################
  xiony <- formula("sleep~mbdi")
  # ## for anm for bmd data
  # xiony <- formula("sleep~splines::bs(mbdi, knots = c(0.8))")
  fitxiony <- lm(xiony, data=randomdataset1)
  betai <- coef(fitxiony)
  e.x <- residuals(fitxiony)
  
  x_theta <- est.fun(e.x,randomdataset1[,"mbdi"], nrow(randomdataset1))
  
  write.csv(x_theta,file.path(xony_dir,"thetahat.csv"))
}
