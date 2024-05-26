library(dplyr)
source("helper_functions.R")

file <- "real_data/drc/mykiss/strat"
samplesizes <- c(seq(20, 70, 10), seq(80, 180, 20), seq(200, 1650, 50), c(1699))


num_b = -1
nsubsamp = 100
colname_test <- c("samplesizes", "yonx_and_xony_reject", "yonx_and_xony_noreject", "yonx_reject_xony_noreject", "xony_reject_yonx_noreject")
colname_lingam <- c("samplesizes", "order_right", "order_left")
name_file_test <- "cddr_my_kiss_drc_samp"
name_file_lingam <- "cddr_my_kiss_drc_lingam_samp"
name_out_test <- "cddr_test_my_kiss_drc"
name_out_lingam <- "cddr_lingam_my_kiss_drc"

df_cddr_test <- combine_cddr_samp(-1, samplesizes, file, colname_test, name_file_test, name_out_test)
df_cddr_lingam <- combine_cddr_samp(-1, samplesizes, file, colname_lingam, name_file_lingam, name_out_lingam, FALSE)

## for the pvals
colname_pvalxy <- 1:nsubsamp
name_file_pxy <- "p_valsxy_my_kiss_drc_samp"
name_out_pxy <- "p_valsxy_my_kiss_drc"
df_pvalxy <- combine_cddr_samp(-1, samplesizes, file, colname_pvalxy, name_file_pxy, name_out_pxy)

colname_pvalyx <- 1:nsubsamp
name_file_pyx <- "p_valsyx_my_kiss_drc_samp"
name_out_pyx <- "p_valsyx_my_kiss_drc"
df_pvalyx <- combine_cddr_samp(-1, samplesizes, file, colname_pvalyx, name_file_pyx, name_out_pyx)




