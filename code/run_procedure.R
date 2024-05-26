library(parallel)
library(dplyr)
library(ggplot2)

source("helper_functions.R")

## get the samplesize
args=(commandArgs(TRUE))
sampsize <- as.numeric(args[1])
file <- "real_data/drc/mykiss/strat"
nsubsamp <- 100
nboot <- 100
seed.vec <- seq(1, nsubsamp)
trials <- 1:nsubsamp

## setup
data <- read.csv("mykiss_drc.csv")
colnames(data) <- c("id", "sleep","mbdi")
main_dir <- getwd()
main_dir <- file.path(main_dir,paste0(file))
main_dir <- file.path(main_dir,paste0("samplesize",sampsize))
dir.create(main_dir)
lapply(trials, verify_setup, sampsize = sampsize, main_dir = main_dir, subdata_all = data)

## bootstrapping
main_dir <- getwd()
main_dir <- file.path(main_dir,paste0(file))
do_bootstrap(file, sampsize, nsubsamp)

# get p-values
get_pvals(-1, sampsize, seed.vec, nboot, nsubsamp, file)

## get cddr for test-based method
df_cddr <- get_cddr(-1, sampsize, nsubsamp, file)

## get cddr for lingam
df_cddr_lingam <- get_lingam(sampsize, seed.vec, nsubsamp, file)

## delete files except cddr files
main_dir <- getwd()
main_dir <- file.path(main_dir,paste0(file))
main_dir <- file.path(main_dir,paste0("samplesize",sampsize))
unlink(main_dir, recursive = TRUE)

## delete the out files
