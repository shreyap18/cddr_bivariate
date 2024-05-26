library(ggplot2)
library(tidyverse)
library(patchwork)

## get the bootstrap distributions and CI's
main_dir <- "~/Documents/cddr_paper/code/subsamps"
tddr_p125_s100 <- read.csv(file.path(main_dir,"cddr_test_s100.csv"))[,-1]

tddr_p125_s100 <- tddr_p125_s100 %>%
  mutate_if(~is.character(.) | is.factor(.), as.numeric)

p11_boot_s100 <- tddr_p125_s100[,c("samplesizes", "yonx_and_xony_reject")]
p00_boot_s100 <- tddr_p125_s100[,c("samplesizes", "yonx_and_xony_noreject")]
p01_boot_s100 <- tddr_p125_s100[,c("samplesizes", "yonx_reject_xony_noreject")]
p10_boot_s100 <- tddr_p125_s100[,c("samplesizes", "xony_reject_yonx_noreject")]

# getting the bootstrap confidence intervals
get_ci_boot <- function(data, ci_level = 0.95){
  ci_df <- data %>%
    group_by(samplesizes) %>%
    # summarise_all(list(mean_true = ~mean(.), var_true = ~(mean(.)*(1-mean(.)))))
    summarise_all(list(mean_true = ~mean(.), var_true = ~var(.), ci_low_true = ~quantile(., (1 - ci_level) / 2), ci_high_true = ~quantile(., 1 - (1 - ci_level) / 2)))
  return(ci_df)
}
library(purrr)

calculate_diffs <- function(data, sample_size_column, outcome_column, dataset_name, S = 100, ci_level = 0.95) {
  # Calculate differences for each sample size
  differences <- map_dfr(unique(data[[sample_size_column]]), function(size) {
    z_value <- qnorm((1 + ci_level) / 2)
    data_subset <- data[data[[sample_size_column]] == size, ]
    true_sd <- sd(data_subset[[outcome_column]])
    lower_true <- quantile(data_subset[[outcome_column]], (1 - ci_level) / 2)
    upper_true <- quantile(data_subset[[outcome_column]], 1-(1 - ci_level) / 2)
    data_means <- data_subset[[outcome_column]]
    sd_hat <- sqrt((data_means * (1 - data_means)) / S)
    lower_bounds <- data_means - z_value*sd_hat
    upper_bounds <- data_means + z_value *sd_hat
    mean_diff <- mean(sd_hat - true_sd)
    mean_diff_lower <- mean(lower_bounds - lower_true)
    mean_diff_upper <- mean(upper_bounds - upper_true)
    tibble(samplesize = size, diff = mean_diff, diff_lower = mean_diff_lower, diff_upper = mean_diff_upper, sd_true = true_sd, name = dataset_name)
  })
  
  return(differences)
}

p11_sd_bias_s100 <- calculate_diffs(p11_boot_s100, "samplesizes", "yonx_and_xony_reject", "p11", S=100)
p00_sd_bias_s100 <- calculate_diffs(p00_boot_s100, "samplesizes", "yonx_and_xony_noreject", "p00", S=100)
p01_sd_bias_s100 <- calculate_diffs(p01_boot_s100, "samplesizes", "yonx_reject_xony_noreject", "p01", S=100)
p10_sd_bias_s100 <- calculate_diffs(p10_boot_s100, "samplesizes", "xony_reject_yonx_noreject", "p10", S=100)

combined_diff_s100 <- bind_rows(p11_sd_bias_s100, p00_sd_bias_s100, p01_sd_bias_s100, p10_sd_bias_s100)


diffs_p10_s100 = combined_diff_s100[combined_diff_s100$name == "p10",]

bias_plot_s100 <- ggplot(diffs_p10_s100) +
  geom_point(aes(x = samplesize, y = diff, color = "Bias of Standard Errors")) +
  geom_line(aes(x = samplesize, y = diff, color = "Bias of Standard Errors")) +
  geom_point(aes(x = samplesize, y = diff_lower, color = "Bias of Lower CI")) +
  geom_line(aes(x = samplesize, y = diff_lower, color = "Bias of Lower CI")) +
  geom_point(aes(x = samplesize, y = diff_upper, color = "Bias of Upper CI")) +
  geom_line(aes(x = samplesize, y = diff_upper, color = "Bias of Upper CI")) +
  ylim(-0.5, 0.5) +
  theme_bw() +
  labs(x = "Samplesize", y = "Biases", title = "Estimate of Biases in Normal Approximation \nvs Sample Size (S=100)") +
  scale_color_manual(values = c("Bias of Standard Errors" = "purple", "Bias of Lower CI" = "blue", "Bias of Upper CI" = "darkgreen")) +
  guides(color = guide_legend(title = "Biases"))

true_std_s100 <- ggplot(diffs_p10_s100, aes(x = samplesize, y = sd_true)) +
  geom_point() +
  geom_line() +
  ylim(-0.1, 0.5) +
  theme_bw() +
  labs(x = "Samplesize", y = "Estimate of True Variability", title = "Estimate of True Variability \nvs Sample Size (S=100)")

## s = 500
tddr_p125_s500 <- read.csv(file.path(main_dir,"cddr_test_s500.csv"))[,-1]

tddr_p125_s500 <- tddr_p125_s500 %>%
  mutate_if(~is.character(.) | is.factor(.), as.numeric)

p11_boot_s500 <- tddr_p125_s500[,c("samplesizes", "yonx_and_xony_reject")]
p00_boot_s500 <- tddr_p125_s500[,c("samplesizes", "yonx_and_xony_noreject")]
p01_boot_s500 <- tddr_p125_s500[,c("samplesizes", "yonx_reject_xony_noreject")]
p10_boot_s500 <- tddr_p125_s500[,c("samplesizes", "xony_reject_yonx_noreject")]

p11_sd_bias_s500 <- calculate_diffs(p11_boot_s500, "samplesizes", "yonx_and_xony_reject", "p11", S=500)
p00_sd_bias_s500 <- calculate_diffs(p00_boot_s500, "samplesizes", "yonx_and_xony_noreject", "p00", S=500)
p01_sd_bias_s500 <- calculate_diffs(p01_boot_s500, "samplesizes", "yonx_reject_xony_noreject", "p01", S=500)
p10_sd_bias_s500 <- calculate_diffs(p10_boot_s500, "samplesizes", "xony_reject_yonx_noreject", "p10", S=500)

combined_diff_s500 <- bind_rows(p11_sd_bias_s500, p00_sd_bias_s500, p01_sd_bias_s500, p10_sd_bias_s500)
diffs_p10_s500 = combined_diff_s500[combined_diff_s500$name == "p10",]

bias_plot_s500 <- ggplot(diffs_p10_s500) +
  geom_point(aes(x = samplesize, y = diff, color = "Bias of Standard Errors")) +
  geom_line(aes(x = samplesize, y = diff, color = "Bias of Standard Errors")) +
  geom_point(aes(x = samplesize, y = diff_lower, color = "Bias of Lower CI")) +
  geom_line(aes(x = samplesize, y = diff_lower, color = "Bias of Lower CI")) +
  geom_point(aes(x = samplesize, y = diff_upper, color = "Bias of Upper CI")) +
  geom_line(aes(x = samplesize, y = diff_upper, color = "Bias of Upper CI")) +
  ylim(-0.5, 0.5) +
  theme_bw() +
  labs(x = "Samplesize", y = "Biases", title = "Estimate of Biases in Normal Approximation \nvs Sample Size (S=500)") +
  scale_color_manual(values = c("Bias of Standard Errors" = "purple", "Bias of Lower CI" = "blue", "Bias of Upper CI" = "darkgreen")) +
  guides(color = guide_legend(title = "Biases"))

true_std_s500 <- ggplot(diffs_p10_s500, aes(x = samplesize, y = sd_true)) +
  geom_point() +
  geom_line() +
  ylim(-0.1, 0.5) +
  theme_bw() +
  labs(x = "Samplesize", y = "Estimate of True Variability", title = "Estimate of True Variability \nvs Sample Size (S=500)")



clt_sim_plots <- bias_plot_s100 + true_std_s100 + bias_plot_s500 + true_std_s500
clt_sim_plots 

ggsave(path = main_dir, "clt_sim_plots.png")
