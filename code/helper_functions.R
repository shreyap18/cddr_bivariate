source("pointest.R")
source("boot.R")

verify_setup <- function(i, sampsize, main_dir, subdata_all)
{
  pointest(i, sampsize, main_dir, subdata_all)
}

do_setup <- function(mBDI, file, samplesizes, nsubsamp = 100){
  subdata_all <- mBDI
  seed.vec <- seq(1, nsubsamp)
  trials <- 1:nsubsamp
  for (sampsize in samplesizes) {
    sampsize <- sampsize
    main_dir <- getwd()
    main_dir <- file.path(main_dir,paste0(file))
    main_dir <- file.path(main_dir,paste0("samplesize",sampsize))
    dir.create(main_dir)
    lapply(trials, verify_setup, sampsize = sampsize, main_dir = main_dir, subdata_all = subdata_all)
  }
}

verify_boot <- function(i, j, nsubsamp = 100)
{
  do_one(i, j, sampsize, file, nsubsamp = nsubsamp)
}

do_one <- function(i, j, sampsize, file, nsubsamp = 100)
{
  sum_pvals <- 0
  seed.vec <- seq(1, nsubsamp)
  seed <- seed.vec[i]
  main_dir <- getwd()
  main_dir <- file.path(main_dir, paste0(file))
  main_dir <- file.path(main_dir,paste0("samplesize",sampsize))
  current_dir <- file.path(main_dir,paste0("dataset_",seed,"_",sampsize))
  xony_dir <- file.path(current_dir,"xony")
  yonx_dir <- file.path(current_dir,"yonx")
  dataset <- read.csv(paste0(current_dir,"/dataset.csv"),header = T)[,-1]
  
  theta_xony <- read.csv(paste0(xony_dir,"/thetahat.csv"),header = T)[,-1]
  theta_yonx <- read.csv(paste0(yonx_dir,"/thetahat.csv"),header = T)[,-1]
  
  seed.id <- j
  res_xony <- do.one.xony.seperate(seed.id,dataset,theta_xony)
  sum_pvals <- sum_pvals + res_xony[[2]]
  
  write.csv(res_xony[[1]],paste0(xony_dir,"/btheta",seed.id,".csv"))
  write.csv(res_xony[[2]],paste0(xony_dir,"/bp",seed.id,".csv"))
  
  res_yonx <- do.one.yonx.seperate(seed.id,dataset,theta_yonx)
  sum_pvals <- sum_pvals + res_yonx[[2]]
  
  write.csv(res_yonx[[1]],paste0(yonx_dir,"/btheta",seed.id,".csv"))
  write.csv(res_yonx[[2]],paste0(yonx_dir,"/bp",seed.id,".csv"))
  return(sum_pvals)
}

do_bootstrap <- function(file, sampsize, nsubsamp = 100){
  nboot <- 100
  seed.vec <- seq(1, nsubsamp)
  main_dir <- getwd()
  main_dir <- file.path(main_dir,paste0(file))
  i <- 1
  for (i in 1:nsubsamp) {
    i <- i
    trials <- 1:nboot
    all <- lapply(trials,verify_boot,i=i, nsubsamp = nsubsamp)
    # all <- mclapply(trials,verify_boot,i=i,seed=seed,mc.cores=detectCores())
  }
  
}

get_pvals <- function(num_b, samplesize, seed.vec, nboot, nsubsamp, file){
  p_vals_xy <- rep(NA, nsubsamp)
  p_vals_yx <- rep(NA, nsubsamp)
  main_dir <- getwd()
  main_dir <- file.path(main_dir, paste0(file))
  main_dir <- file.path(main_dir,paste0("samplesize",sampsize))
  for (s in 1:nsubsamp){
    seed <- seed.vec[s]
    p_xony <- rep(NA, nsubsamp)
    p_yonx <- rep(NA, nsubsamp)
    for (b in 1:nboot){
      current_dir <- file.path(main_dir,paste0("dataset_",seed,"_",sampsize))
      xony_dir <- file.path(current_dir,"xony")
      yonx_dir <- file.path(current_dir,"yonx")
      xony_bp <- paste0(xony_dir,"/bp",b,".csv")
      yonx_bp <- paste0(yonx_dir,"/bp",b,".csv")
      tryCatch({
        bp_xony <- read.csv(xony_bp, header = TRUE)[, -1]
        bp_yonx <- read.csv(yonx_bp, header = TRUE)[, -1]
        p_xony[b] <- bp_xony
        p_yonx[b] <- bp_yonx
      }, error = function(e) {
        # Handle the error (e.g., print a message)
        cat("Error reading CSV file:", e$message, "\n")
        # You may choose to set p_xony[b] and p_yonx[b] to a default value or NA
        p_xony[b] <- NA
        p_yonx[b] <- NA
        nboot = nboot-1
      })
    }
    p_val_xony <- sum(p_xony, na.rm=TRUE)/nboot
    p_val_yonx <- sum(p_yonx, na.rm=TRUE)/nboot
    p_vals_xy[s] <- p_val_xony
    p_vals_yx[s] <- p_val_yonx
  }
  dfx <- as.data.frame(p_vals_xy)
  dfy <- as.data.frame(p_vals_yx)
  main_dir <- getwd()
  main_dir <- file.path(main_dir, paste0(file))
  if (num_b == -1){
    write.csv(dfx,file.path(main_dir,sprintf("p_valsxy_my_kiss_drc_samp%d.csv", samplesize)))
    write.csv(dfy,file.path(main_dir,sprintf("p_valsyx_my_kiss_drc_samp%d.csv", samplesize)))
  }
  else{
    filexy_name = paste0("p_valsxy_my_kiss_drc_samp", sampsize, "_b", num_b, ".csv")
    write.csv(dfx,file.path(main_dir,filexy_name))
    fileyx_name = paste0("p_valsyx_my_kiss_drc_samp", sampsize, "_b", num_b, ".csv")
    write.csv(dfy,file.path(main_dir,fileyx_name))
  }
}

get_cddr <- function(num_b, sampsize, nsubsamp, file, alpha = 0.05){
  main_dir <- getwd()
  main_dir <- file.path(main_dir, paste0(file))
  if (num_b == -1){
    dfyx <- read.csv(file.path(main_dir,sprintf("p_valsyx_my_kiss_drc_samp%d.csv", sampsize)))[,-1]
    dfxy <- read.csv(file.path(main_dir,sprintf("p_valsxy_my_kiss_drc_samp%d.csv", sampsize)))[,-1]
  }
  else{
    fileyx_name = paste0("p_valsyx_my_kiss_drc_samp", sampsize, "_b", num_b, ".csv")
    dfyx <- read.csv(file.path(main_dir, fileyx_name))[,-1]
    filexy_name = paste0("p_valsxy_my_kiss_drc_samp", sampsize, "_b", num_b, ".csv")
    dfxy <- read.csv(file.path(main_dir, filexy_name))[,-1]
  }
  reject_yonx_and_xony <- 0
  noreject_yonx_and_xony <- 0
  reject_yonx_noreject_xony <- 0
  reject_xony_noreject_yonx <- 0
  for (s in 1:nsubsamp){
    p_val_xony <- dfxy[s]
    p_val_yonx <- dfyx[s]
    if ((p_val_xony <= alpha) & (p_val_yonx <= alpha)){
      reject_yonx_and_xony <- reject_yonx_and_xony + 1
    }
    if ((p_val_xony > alpha) & (p_val_yonx > alpha)){
      noreject_yonx_and_xony <- noreject_yonx_and_xony + 1
    }
    if ((p_val_xony > alpha) & (p_val_yonx <= alpha)){
      reject_yonx_noreject_xony <- reject_yonx_noreject_xony + 1
    }
    if ((p_val_xony <= alpha) & (p_val_yonx > alpha)){
      reject_xony_noreject_yonx <- reject_xony_noreject_yonx + 1
    }
  }
  df_cddr <- data.frame(c(sampsize, reject_yonx_and_xony/nsubsamp, noreject_yonx_and_xony/nsubsamp, reject_yonx_noreject_xony/nsubsamp, reject_xony_noreject_yonx/nsubsamp))
  if (num_b == -1){
    write.csv(df_cddr,file.path(main_dir,sprintf("cddr_my_kiss_drc_samp%d.csv", sampsize)))
  }
  else{
    file_name = paste0("cddr_my_kiss_drc_samp", sampsize, "_b", num_b, ".csv")
    write.csv(df_cddr,file.path(main_dir,file_name))
  }
  return(df_cddr)
}

## Rewriting the LiNGAM package
BaseLiNGAM <- R6::R6Class("LiNGAM",
                          public = list(
                            #' @field random_state (integer) Random seed
                            #' @field causal_order (numeric vector) Causal order of variables
                            #' @field adjacency_matrix (numeric matrix) Estimated adjacency matrix
                            #' @field intercept (numeric vector) Estimated intercept term
                            #' @field lasso_engine (character) "lars" or "glmnet"
                            random_state = NULL,
                            causal_order = NULL,
                            adjacency_matrix = NULL,
                            intercept = NULL,
                            lasso_engine = NULL,
                            
                            
                            #' @description create lingam object
                            #' @param random_state (integer) Random seed
                            #' @param lasso_engine (character) "lars" or "glmnet"
                            initialize = function(random_state = NULL, lasso_engine = "glmnet") {
                              self$random_state <- random_state
                              self$lasso_engine <- lasso_engine
                            },
                            
                            #' @description subclasses should implement this method
                            #' @param X (numeric matrix or data.frame) data matrix
                            fit = function(X) {
                            },
                            
                            #' @description estimate adjacency matrix based on causal order
                            #' @param X (numeric matrix or data.frame) data matrix
                            estimate_adjacency_matrix = function(X) {
                              # A: intercept
                              # B: adjacency matrix
                              A <- rep(NA, ncol(X))
                              B <- matrix(0, nrow = ncol(X), ncol = ncol(X))
                              A[self$causal_order[1]] <- mean(X[, self$causal_order[1]])
                              for (i in 2:length(self$causal_order)) {
                                res <- self$predict_adaptive_lasso(X, self$causal_order[1:(i - 1)], self$causal_order[i])
                                A[self$causal_order[i]] <- res$intercept
                                B[self$causal_order[i], self$causal_order[1:(i - 1)]] <- res$coef
                              }
                              self$intercept <- A
                              self$adjacency_matrix <- B
                            },
                            
                            #' @description fit adaptice lasso
                            #' @param X (numeric matrix or data.frame) data matrix
                            #' @param predictors (numeric vector) index of explanatory variables
                            #' @param target (integer) index of target variable
                            #' @param gamma (numeric) data x will be weighted like x^(gamma) for adaptive lasso
                            #' @return coef_ (numeric vector) estimated coefficients
                            predict_adaptive_lasso = function(X, predictors, target, gamma = 1) {
                              
                              # 1st stage (OLS to determine weights)
                              fml <- as.formula(paste0(names(X)[target], "~."))
                              X_ <- X[names(X)[c(target, predictors)]]
                              lr <- lm(fml, data = X_)
                              weight <- abs(lr$coefficients[-1])^(gamma)
                              
                              # 2nd stage
                              x <- as.matrix(X_[, -1, drop = FALSE])
                              y <- as.matrix(X_[, 1], drop = FALSE)
                              
                              if (self$lasso_engine == "lars") {
                                # adaptive lasso by lars()
                                # use coefs which minimizes bic
                                x <- t(t(x) * weight)
                                reg <- lars::lars(x = x, y = y, type = "stepwise")
                                bic <- log(nrow(x)) * reg$df + nrow(x) * log(reg$RSS/nrow(x))
                                
                                # lars does not explicitly return intercept term...
                                idx <- which.min(bic)
                                coef_ <- matrix(coef(reg), ncol = ncol(x))[idx ,]
                                coef_ <- coef_ * weight
                                eval(parse(text = paste0("intercept <- predict(reg, s=", idx, ", data.frame(", paste0(names(coef_), "=", 0, collapse = ","), "))$fit")))
                                
                              } else if (self$lasso_engine == "glmnet") {
                                # adaptive lasso by glmnet()
                                # glmnet() can not handle x with single column
                                
                                if (ncol(x) > 1) {
                                  # specify lambda sequence (default did not search small lambdas)
                                  lambda_seq <- exp(seq(2, -7, length.out = 80))
                                  reg <- glmnet::cv.glmnet(x = x, y = y, penalty.factor = 1 / weight, type.measure = "mse", lambda = lambda_seq, relax = FALSE)
                                  
                                  # use 1se rule
                                  idx_best  <- which(reg$lambda == reg$lambda.1se)
                                  coef_     <- reg$glmnet.fit$beta[, idx_best]
                                  coef_     <- matrix(coef_, ncol = ncol(x))
                                  intercept <- reg$glmnet.fit$a0[idx_best]
                                } else {
                                  coef_ <- matrix(lr$coefficients[-1], ncol = ncol(x))
                                  intercept <- lr$coefficients[1]
                                }
                              }
                              
                              # return coefs and intercept
                              res <- list()
                              res$coef <- coef_
                              res$intercept <- intercept
                              return(res)
                            }
                          )
)
DirectLiNGAM <- R6::R6Class("DirectLiNGAM", inherit = BaseLiNGAM,
                            public = list(
                              #' @description fit DirectLiNGAM
                              #' @param X (numeric matrix or data.frame) data matrix to fit
                              fit = function(X) {
                                self$estimate_causal_order(X)
                                self$estimate_adjacency_matrix(X)
                              },
                              
                              #' @description search causal ordering
                              #' @param X (numerical matrix or data.frame) data matrix
                              estimate_causal_order = function(X) {
                                num_feat <- ncol(X)
                                
                                # Causal Discovery
                                U <- c(1:num_feat)
                                K <- c()
                                X_ <- X
                                
                                for (f in 1:num_feat) {
                                  m <- self$search_exogenous_variable(X_, U)
                                  for (i in U) {
                                    if (i != m) {
                                      X_[, i] <- self$residual(X_[, i], X_[, m])
                                    }
                                  }
                                  K <- c(K, m)
                                  U <- U[U != m]
                                }
                                self$causal_order <- K
                              },
                              
                              #' @description search exogenous variable
                              #' @param X (numerical matrix or data.frame) data matrix
                              #' @param U (numeric vector) index of each columns
                              #' @return index of estimated exogenous variable
                              search_exogenous_variable = function(X, U) {
                                Uc <- U
                                Vj <- NULL
                                
                                M_list <- c()
                                for (i in Uc) {
                                  M <- 0
                                  for (j in U) {
                                    if (i != j) {
                                      xi_std = (X[, i] - mean(X[, i])) / sd(X[, i])
                                      xj_std = (X[, j] - mean(X[, j])) / sd(X[, j])
                                      
                                      # did not work with ifelse...
                                      if ((i %in% Vj) & (j %in% Uc)) {
                                        ri_j <- xi_std
                                      } else {
                                        ri_j <- self$residual(xi_std, xj_std)
                                      }
                                      
                                      if ((j %in% Vj) & (i %in% Uc)) {
                                        rj_i <- xj_std
                                      } else {
                                        rj_i <- self$residual(xj_std, xi_std)
                                      }
                                      M <- M + self$diff_hs(xi_std, xj_std, ri_j, rj_i)**2
                                    }
                                  }
                                  M_list <- c(M_list, M)
                                }
                                return(Uc[which.min(M_list)])
                              },
                              #' @description search causal ordering
                              #' @param X (numerical matrix or data.frame) data matrix
                              estimate_causal_order_mi = function(X) {
                                num_feat <- ncol(X)
                                
                                # Causal Discovery
                                U <- c(1:num_feat)
                                K <- c()
                                X_ <- X
                                
                                for (f in 1:num_feat) {
                                  m <- self$search_exogenous_variable_mi(X_, U)
                                  for (i in U) {
                                    if (i != m) {
                                      X_[, i] <- self$residual(X_[, i], X_[, m])
                                    }
                                  }
                                  K <- c(K, m)
                                  U <- U[U != m]
                                }
                                self$causal_order <- K
                              },
                              
                              #' @description search exogenous variable
                              #' @param X (numerical matrix or data.frame) data matrix
                              #' @param U (numeric vector) index of each columns
                              #' @return index of estimated exogenous variable
                              search_exogenous_variable_mi = function(X, U) {
                                Uc <- U
                                Vj <- NULL
                                
                                M_list <- c()
                                for (i in Uc) {
                                  M <- 0
                                  for (j in U) {
                                    if (i != j) {
                                      xi_std = (X[, i] - mean(X[, i])) / sd(X[, i])
                                      xj_std = (X[, j] - mean(X[, j])) / sd(X[, j])
                                      
                                      # did not work with ifelse...
                                      if ((i %in% Vj) & (j %in% Uc)) {
                                        ri_j <- xi_std
                                      } else {
                                        ri_j <- self$residual(xi_std, xj_std)
                                      }
                                      
                                      if ((j %in% Vj) & (i %in% Uc)) {
                                        rj_i <- xj_std
                                      } else {
                                        rj_i <- self$residual(xj_std, xi_std)
                                      }
                                      M <- M + min(c(0, self$diff_mutual_info(xi_std, xj_std, ri_j, rj_i)))**2
                                    }
                                  }
                                  M_list <- c(M_list, -M)
                                }
                                return(Uc[which.max(M_list)])
                              },
                              
                              #' @description residual when xi is regressed on xj
                              #' @param xi (numeric vector) target variable
                              #' @param xj (numeric vector) explanatory variable
                              #' @return resid (numeric vector) calculated residual
                              residual = function(xi, xj) {
                                resid <- xi - (cov(xi, xj) / var(xj)) * xj
                                return(resid)
                              },
                              
                              #' @description calculate the H-S Independence Criterion
                              #' @param X standardized xi matrix
                              #' @param e residuals
                              #' @param n number of rows
                              #' @return scalar value of H-S Independence Criterion
                              est.fun = function(X,e,n)
                              {
                                K <- L <- matrix(NA,nrow = n,ncol = n)
                                I <- matrix(1,nrow=n,ncol=1)
                                H <- diag(1,nrow = n,ncol = n)-I %*% t(I)/n
                                
                                L <- exp(-(e%*%t(I) - I%*%t(e))^2)
                                K <- exp(-(X%*%t(I) - I%*%t(X))^2)
                                
                                return(sum(diag(K%*%H%*%L%*%H))/n^2)
                              },
                              
                              #' @description calculate the H-S Independence Criterion
                              #' @param xi_std (numeric vector) standardized xi
                              #' @param xj_std (numeric vector) standardized xj
                              #' @param ri_j (numeric vector) resid of xi_std regressed on xj_std
                              #' @param rj_i (numeric vector) resid of xj_std regressed on xi_std
                              #' @return scalar value of the H-S Independence Criterions
                              diff_hs = function(xi_std, xj_std, ri_j, rj_i) {
                                data <- data.frame(x=xi_std, y=xj_std)
                                X <- data$x
                                fityonx <- lm(y~x, data=data)
                                beta <- coef(fityonx)
                                e.y <- residuals(fityonx)
                                theta <- self$est.fun(X,e.y,nrow(data))
                                return(theta)
                              },
                              
                              #' @description calculate the difference of the mutual information
                              #' @param xi_std (numeric vector) standardized xi
                              #' @param xj_std (numeric vector) standardized xj
                              #' @param ri_j (numeric vector) resid of xi_std regressed on xj_std
                              #' @param rj_i (numeric vector) resid of xj_std regressed on xi_std
                              #' @return scalar value of the difference of mutual information
                              diff_mutual_info = function(xi_std, xj_std, ri_j, rj_i) {
                                term1 <- self$entropy(xj_std) + self$entropy(ri_j / sd(ri_j))
                                term2 <- self$entropy(xi_std) + self$entropy(rj_i / sd(rj_i))
                                return(term1 - term2)
                              },
                              
                              #' @description calculate entropy using maximum entropy approximation
                              #' @param u (numeric vector) vector to calculate entropy
                              #' @return scalar value of entropy
                              entropy = function(u) {
                                k1 <- 79.047
                                k2 <- 7.4129
                                gamma <- 0.37457
                                
                                term1 <- (1 + log(2 * pi)) / 2
                                term2 <- -k1 * (mean(log(cosh(u))) - gamma)^2
                                term3 <- -k2 * (mean(u * exp((-u^2) / 2)))^2
                                return(term1 + term2 + term3)
                              }
                            ),
)

## getting lingam results
get_lingam <- function(sampsize, seed.vec, nsubsamp, file){
  main_dir <- getwd()
  main_dir <- file.path(main_dir, paste0(file))
  main_dir <- file.path(main_dir,paste0("samplesize",sampsize))
  total_right <- 0
  total_left <- 0
  for (s in 1:nsubsamp){
    seed <- seed.vec[s]
    current_dir <- file.path(main_dir,paste0("dataset_",seed,"_",sampsize))
    data <- read.csv(paste0(current_dir,"/dataset",".csv"))[,-1]
    df <- data.frame(sleep = data$sleep, depr = data$mbdi)
    mdl <- DirectLiNGAM$new()
    mdl$fit(df)
    X <- as.matrix(df)
    i <- 1
    j <- 2
    xi_std = (X[, i] - mean(X[, i])) / sd(X[, i])
    xj_std = (X[, j] - mean(X[, j])) / sd(X[, j])
    
    ri_j <- mdl$residual(xi_std, xj_std)
    rj_i <- mdl$residual(xj_std, xi_std)
    
    order <- mdl$causal_order
    if (sum(order == c(1,2)) == 2){
      total_right <- total_right + 1
    }
    if (sum(order == c(2,1)) == 2){
      total_left <- total_left + 1
    }
  }
  cddr_right <- total_right/nsubsamp
  cddr_left <- total_left/nsubsamp
  df_cddr <- data.frame(c(sampsize, cddr_right, cddr_left))
  main_dir <- getwd()
  main_dir <- file.path(main_dir, paste0(file))
  write.csv(df_cddr,file.path(main_dir,sprintf("cddr_my_kiss_drc_lingam_samp%d.csv", sampsize)))
  return(df_cddr)
}

## combining the cddr_test and lingam values per samplesize
combine_cddr_samp <- function(num_b, samplesizes, file, names_col, name_file, name_out, test = T){
  cddr <- data.frame(matrix(ncol = length(names_col), nrow = 0))
  colnames(cddr) <- names_col
  cddr_samp <- NA
  for (sampsize in samplesizes){
    main_dir <- getwd()
    main_dir <- file.path(main_dir,paste0(file))
    if (num_b == -1){
      cddr_samp <- read.csv(paste0(main_dir,"/",name_file, sprintf("%d.csv", sampsize)),header = T)[,-1]
    } else{
      cddr_samp <- read.csv(paste0(main_dir,"/",name_file, sampsize, "_b", num_b, ".csv"),header = T)[,-1]
    }
    cddr <- rbind(cddr, cddr_samp)
  }
  colnames(cddr) <- names_col
  if (num_b == -1){
    write.csv(cddr,file.path(main_dir,sprintf("%s.csv", name_out)))
  }
  else{
    file_name = paste0(name_out, "_b", num_b, ".csv")
    write.csv(cddr,file.path(main_dir,file_name))
  }
  main_dir <- getwd()
  main_dir <- file.path(main_dir,paste0(file))
  files_to_delete <- list.files(main_dir, pattern = name_file)
  # Check if any files match the pattern
  if (length(files_to_delete) > 0) {
    # Loop through the files and remove them
    for (f in files_to_delete) {
      print(f)
      file.remove(file.path(main_dir, f))
    }
  } else {
    print("No files found matching the pattern.")
  }
  return(cddr)
}
