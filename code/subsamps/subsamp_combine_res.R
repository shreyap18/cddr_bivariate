library(dplyr)

## number of replications (default 100)
R = 100
file <- "subsample_ci/sims_p125/s500"
names_col <- c("samplesizes", "yonx_and_xony_reject", "yonx_and_xony_noreject", "yonx_reject_xony_noreject", "xony_reject_yonx_noreject")
cddr <- data.frame(matrix(ncol = length(names_col), nrow = 0))
colnames(cddr) <- names_col

for (r in 1:R){
  main_dir <- getwd()
  file_name <- paste0(file, "/data_sim_", r, "/cddr_test_p125_b", r, ".csv")
  cddr_subsamp <- read.csv(file.path(main_dir, file_name))[,-1]
  cddr <- rbind(cddr, cddr_subsamp)
}

file_name <- paste0(file, "/cddr_test_s500.csv")
write.csv(cddr,file.path(main_dir,file_name))
