
fit_resid_yonx <- function(data) {
  model <- lm(formula("y~x"), data = data)
  resids <- residuals(model)
  model_obs <- list(model = model, residuals = resids)
  return(model_obs)
}

fit_resid_xony <- function(data) {
  model <- lm(formula("x~y"), data = data)
  resids <- residuals(model)
  model_obs <- list(model = model, residuals = resids)
  return(model_obs)
}

predict_yonx <- function(model, data) {
  preds <- predict(model,newdata = data)
  return(preds)
}

predict_xony <- function(model, data) {
  preds <- predict(model,newdata = data)
  return(preds)
}

run_causal_method <- function(data){
  p <- rbinom(1, 1, p = 0.4)
  if (p == 0){
    return(c(1,2))
  }else{
    return(c(2,1))
  }
}

data <- data.frame(x=rnorm(20), y = rnorm(20))
newdata <- data.frame(x=rnorm(50))
a <- fit_resid_xiony(data)
b <- predict_xiony(a$model, newdata)
