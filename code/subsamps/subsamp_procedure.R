library(parallel)
library(dplyr)
library(ReIns)
library(mixtools)
library(extraDistr)

source("helper_functions.R")

nsubsamp <- 500
nboot <- 100
seed.vec <- seq(1, nsubsamp)
trials <- 1:nsubsamp
samplesizes <- c(seq(20, 70, 10), seq(80, 120, 20))
file <- "subsample_ci/sims_p125/s500"

args=(commandArgs(TRUE))
num_b <- as.numeric(args[1])
sampsize <- as.numeric(args[2])
data_sim <- NA

# file <- paste0(file, sprintf("/data_sim_%d", num_b))
# dataset creation if at the beginning sample size
if (sampsize == samplesizes[1]){
  rate = 1.2802
  N = 10000
  X = rtexp(N, rate, endpoint = 3) + 1
  mu1 <- -0.06
  sigma1 <- 0.48
  mu2 <- -0.59
  sigma2 <- 0.16
  mu3 <- 0.65
  sigma3 <- 0.68
  lams <- c(0.12, 0.71, 0.17)
  props <- sample(c(0, 1, 2), N, replace = T, prob = lams)
  g1 <- rnorm(N, mu1, sigma1)
  g2 <- rnorm(N, mu2, sigma2)
  g3 <- rnorm(N, mu3, sigma3)
  eta3 = ifelse(props == 0, g1,
                ifelse(props == 1, g2, g3))
  beta = 2
  a = 2.5
  X_shift <- X-a
  b = 1.25
  y125 = (sign(X_shift)*abs(X_shift)^b*beta) + eta3
  data_sim = data.frame(sleep = X, depr = y125)
  colnames(data_sim) <- c("sleep","mbdi")
  main_dir <- getwd()
  file <- paste0(file, sprintf("/data_sim_%d", num_b))
  main_dir <- file.path(main_dir,paste0(file))
  dir.create(main_dir)
  write.csv(data_sim,file.path(main_dir,sprintf("dataset_sim_%d.csv", num_b)))
} else{
  main_dir <- getwd()
  file <- paste0(file, sprintf("/data_sim_%d", num_b))
  main_dir <- file.path(main_dir,paste0(file))
  data_sim <- read.csv(file.path(main_dir,sprintf("dataset_sim_%d.csv", num_b)))[,-1]
}

## do setup
main_dir <- file.path(main_dir,paste0("samplesize",sampsize))
dir.create(main_dir)
lapply(trials, verify_setup, sampsize = sampsize, main_dir = main_dir, subdata_all = data_sim)

## bootstrapping
main_dir <- getwd()
main_dir <- file.path(main_dir,paste0(file))
do_bootstrap(file, sampsize, nsubsamp)

# get p-values
get_pvals(num_b, sampsize, seed.vec, nboot, nsubsamp, file)

## get cddr for test-based method
df_cddr <- get_cddr(num_b, sampsize, nsubsamp, file)

## delete files except cddr files
main_dir <- getwd()
main_dir <- file.path(main_dir,paste0(file))
main_dir <- file.path(main_dir,paste0("samplesize",sampsize))
unlink(main_dir, recursive = TRUE)

## combine the results if it is the last samples size
if (sampsize == tail(samplesizes, 1)){
  colname_test <- c("samplesizes", "yonx_and_xony_reject", "yonx_and_xony_noreject", "yonx_reject_xony_noreject", "xony_reject_yonx_noreject")
  name_file_test <- "cddr_p125_samp"
  name_out_test <- "cddr_test_p125"
  df_cddr_test <- combine_cddr_samp(num_b, samplesizes, file, colname_test, name_file_test, name_out_test)
  
  ## for the pvals
  colname_pvalxy <- 1:nsubsamp
  name_file_pxy <- "p_valsxy_p125_samp"
  name_out_pxy <- "p_valsxy_p125"
  df_pvalxy <- combine_cddr_samp(num_b, samplesizes, file, colname_pvalxy, name_file_pxy, name_out_pxy)
  
  colname_pvalyx <- 1:nsubsamp
  name_file_pyx <- "p_valsyx_p125_samp"
  name_out_pyx <- "p_valsyx_p125"
  df_pvalyx <- combine_cddr_samp(num_b, samplesizes, file, colname_pvalyx, name_file_pyx, name_out_pyx)
}

