library(dplyr)
library(ggplot2)
library(patchwork)

calc_ci <- function(data, outcome_column, dataset_name, S = 100, ci_level = 0.95) {
  z_value <- qnorm((1 + ci_level) / 2)
  data_means <- data[[outcome_column]]
  sd_hat <- sqrt((data_means * (1 - data_means)) / S)
  lower_bounds <- data_means - z_value*sd_hat
  upper_bounds <- data_means + z_value *sd_hat
  data[sprintf("%s_ci_lower", dataset_name)] = lower_bounds
  data[sprintf("%s_ci_upper", dataset_name)] = upper_bounds
  return(data)
}

## linearity assumption violated cases
main_dir <- "~/Documents/cddr_paper/simulations/results"
### test-based approach
cddr_test_p1 <- read.csv(file.path(main_dir,"cddr_test_p1.csv"))
cddr_test_p125 <- read.csv(file.path(main_dir,"cddr_test_p125.csv"))
cddr_test_p3 <- read.csv(file.path(main_dir,"cddr_test_p3.csv"))
### lingam
cddr_lingam_p1 <- read.csv(file.path(main_dir,"cddr_lingam_p1.csv"))
cddr_lingam_p125 <- read.csv(file.path(main_dir,"cddr_lingam_p125.csv"))
cddr_lingam_p3 <- read.csv(file.path(main_dir,"cddr_lingam_p3.csv"))

## gaussianity assumption violated cases
### test-based approach
cddr_test_e1 <- read.csv(file.path(main_dir,"cddr_test_e1.csv"))
cddr_test_e2 <- read.csv(file.path(main_dir,"cddr_test_e2.csv"))
cddr_test_e3 <- read.csv(file.path(main_dir,"cddr_test_e3.csv"))
### lingam
cddr_lingam_e1 <- read.csv(file.path(main_dir,"cddr_lingam_e1.csv"))
cddr_lingam_e2 <- read.csv(file.path(main_dir,"cddr_lingam_e2.csv"))
cddr_lingam_e3 <- read.csv(file.path(main_dir,"cddr_lingam_e3.csv"))

## adding ci
cddr_test_p1 <- calc_ci(cddr_test_p1, "yonx_and_xony_reject", "p11")
cddr_test_p1 <- calc_ci(cddr_test_p1, "yonx_and_xony_noreject", "p00")
cddr_test_p1 <- calc_ci(cddr_test_p1, "yonx_reject_xony_noreject", "p01")
cddr_test_p1 <- calc_ci(cddr_test_p1, "xony_reject_yonx_noreject", "p10")

cddr_test_p125 <- calc_ci(cddr_test_p125, "yonx_and_xony_reject", "p11")
cddr_test_p125 <- calc_ci(cddr_test_p125, "yonx_and_xony_noreject", "p00")
cddr_test_p125 <- calc_ci(cddr_test_p125, "yonx_reject_xony_noreject", "p01")
cddr_test_p125 <- calc_ci(cddr_test_p125, "xony_reject_yonx_noreject", "p10")

cddr_test_p3 <- calc_ci(cddr_test_p3, "yonx_and_xony_reject", "p11")
cddr_test_p3 <- calc_ci(cddr_test_p3, "yonx_and_xony_noreject", "p00")
cddr_test_p3 <- calc_ci(cddr_test_p3, "yonx_reject_xony_noreject", "p01")
cddr_test_p3 <- calc_ci(cddr_test_p3, "xony_reject_yonx_noreject", "p10")

cddr_lingam_p1 <- calc_ci(cddr_lingam_p1, "order_right", "d1")
cddr_lingam_p1 <- calc_ci(cddr_lingam_p1, "order_left", "d2")

cddr_lingam_p125 <- calc_ci(cddr_lingam_p125, "order_right", "d1")
cddr_lingam_p125 <- calc_ci(cddr_lingam_p125, "order_left", "d2")

cddr_lingam_p3 <- calc_ci(cddr_lingam_p3, "order_right", "d1")
cddr_lingam_p3 <- calc_ci(cddr_lingam_p3, "order_left", "d2")

cddr_test_e1 <- calc_ci(cddr_test_e1, "yonx_and_xony_reject", "p11")
cddr_test_e1 <- calc_ci(cddr_test_e1, "yonx_and_xony_noreject", "p00")
cddr_test_e1 <- calc_ci(cddr_test_e1, "yonx_reject_xony_noreject", "p01")
cddr_test_e1 <- calc_ci(cddr_test_e1, "xony_reject_yonx_noreject", "p10")

cddr_test_e2 <- calc_ci(cddr_test_e2, "yonx_and_xony_reject", "p11")
cddr_test_e2 <- calc_ci(cddr_test_e2, "yonx_and_xony_noreject", "p00")
cddr_test_e2 <- calc_ci(cddr_test_e2, "yonx_reject_xony_noreject", "p01")
cddr_test_e2 <- calc_ci(cddr_test_e2, "xony_reject_yonx_noreject", "p10")

cddr_test_e3 <- calc_ci(cddr_test_e3, "yonx_and_xony_reject", "p11")
cddr_test_e3 <- calc_ci(cddr_test_e3, "yonx_and_xony_noreject", "p00")
cddr_test_e3 <- calc_ci(cddr_test_e3, "yonx_reject_xony_noreject", "p01")
cddr_test_e3 <- calc_ci(cddr_test_e3, "xony_reject_yonx_noreject", "p10")

cddr_lingam_e1 <- calc_ci(cddr_lingam_e1, "order_right", "d1")
cddr_lingam_e1 <- calc_ci(cddr_lingam_e1, "order_left", "d2")

cddr_lingam_e2 <- calc_ci(cddr_lingam_e2, "order_right", "d1")
cddr_lingam_e2 <- calc_ci(cddr_lingam_e2, "order_left", "d2")

cddr_lingam_e3 <- calc_ci(cddr_lingam_e3, "order_right", "d1")
cddr_lingam_e3 <- calc_ci(cddr_lingam_e3, "order_left", "d2")

## plots
colors <- c("both reject" = "#8046A0", "reject only X -> Y" = "#6699CC", "reject only Y -> X" = "#CC6600", "fail to reject both" = "#007A5E")
cddr_test_p1_plot <- ggplot(cddr_test_p1) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_reject, color = "both reject")) +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_noreject, color = "fail to reject both")) +
  geom_line(aes(x=samplesizes, y=yonx_reject_xony_noreject, color = "reject only X -> Y")) +
  geom_line(aes(x=samplesizes, y=xony_reject_yonx_noreject, color = "reject only Y -> X")) +
  geom_ribbon(aes(x = samplesizes, ymin = p11_ci_lower, ymax = p11_ci_upper), alpha = 0.3, fill = "#8046A0") +
  geom_ribbon(aes(x = samplesizes, ymin = p00_ci_lower, ymax = p00_ci_upper), alpha = 0.3, fill = "#007A5E") +
  geom_ribbon(aes(x = samplesizes, ymin = p01_ci_lower, ymax = p01_ci_upper), alpha = 0.3, fill = "#6699CC") +
  geom_ribbon(aes(x = samplesizes, ymin = p10_ci_lower, ymax = p10_ci_upper), alpha = 0.3, fill = "#CC6600") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Test-based Approach CDDR\nDiagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)


cddr_test_p125_plot <- ggplot(cddr_test_p125) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_reject, color = "both reject")) +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_noreject, color = "fail to reject both")) +
  geom_line(aes(x=samplesizes, y=yonx_reject_xony_noreject, color = "reject only X -> Y")) +
  geom_line(aes(x=samplesizes, y=xony_reject_yonx_noreject, color = "reject only Y -> X")) +
  geom_ribbon(aes(x = samplesizes, ymin = p11_ci_lower, ymax = p11_ci_upper), alpha = 0.3, fill = "#8046A0") +
  geom_ribbon(aes(x = samplesizes, ymin = p00_ci_lower, ymax = p00_ci_upper), alpha = 0.3, fill = "#007A5E") +
  geom_ribbon(aes(x = samplesizes, ymin = p01_ci_lower, ymax = p01_ci_upper), alpha = 0.3, fill = "#6699CC") +
  geom_ribbon(aes(x = samplesizes, ymin = p10_ci_lower, ymax = p10_ci_upper), alpha = 0.3, fill = "#CC6600") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Test-based Approach CDDR\nDiagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

cddr_test_p3_plot <- ggplot(cddr_test_p3) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_reject, color = "both reject")) +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_noreject, color = "fail to reject both")) +
  geom_line(aes(x=samplesizes, y=yonx_reject_xony_noreject, color = "reject only X -> Y")) +
  geom_line(aes(x=samplesizes, y=xony_reject_yonx_noreject, color = "reject only Y -> X")) +
  geom_errorbar(aes(x = samplesizes, ymin = p11_ci_lower, ymax = p11_ci_upper), width = 0.1, color="#8046A0", alpha = 0.5) +
  geom_ribbon(aes(x = samplesizes, ymin = p11_ci_lower, ymax = p11_ci_upper), alpha = 0.5, fill = "#8046A0") +
  geom_errorbar(aes(x = samplesizes, ymin = p00_ci_lower, ymax = p00_ci_upper), width = 0.1, color="#8046A0", alpha = 0.5) +
  geom_ribbon(aes(x = samplesizes, ymin = p00_ci_lower, ymax = p00_ci_upper), alpha = 0.5, fill = "#007A5E") +
  geom_errorbar(aes(x = samplesizes, ymin = p01_ci_lower, ymax = p01_ci_upper), width = 0.1, color="#8046A0", alpha = 0.5) +
  geom_ribbon(aes(x = samplesizes, ymin = p01_ci_lower, ymax = p01_ci_upper), alpha = 0.5, fill = "#6699CC") +
  geom_errorbar(aes(x = samplesizes, ymin = p10_ci_lower, ymax = p10_ci_upper), width = 0.1, color="#8046A0", alpha = 0.5) +
  geom_ribbon(aes(x = samplesizes, ymin = p10_ci_lower, ymax = p10_ci_upper), alpha = 0.5, fill = "#CC6600") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Test-based Approach CDDR\nDiagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

colors <- c("Choose X->Y" = "#CC6600", "Choose Y->X" = "#6699CC")

cddr_lingam_p1_plot <- ggplot(cddr_lingam_p1) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=order_right, color = "Choose X->Y")) +
  geom_ribbon(aes(x = samplesizes, ymin = d1_ci_lower, ymax = d1_ci_upper), alpha = 0.4, fill = "#CC6600") +
  geom_line(aes(x=samplesizes, y=order_left, color = "Choose Y->X")) +
  geom_ribbon(aes(x = samplesizes, ymin = d2_ci_lower, ymax = d2_ci_upper), alpha = 0.4, fill = "#6699CC") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Lingam CDDR Diagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

cddr_lingam_p125_plot <- ggplot(cddr_lingam_p125) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=order_right, color = "Choose X->Y")) +
  geom_ribbon(aes(x = samplesizes, ymin = d1_ci_lower, ymax = d1_ci_upper), alpha = 0.4, fill = "#CC6600") +
  geom_line(aes(x=samplesizes, y=order_left, color = "Choose Y->X")) +
  geom_ribbon(aes(x = samplesizes, ymin = d2_ci_lower, ymax = d2_ci_upper), alpha = 0.4, fill = "#6699CC") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Lingam CDDR Diagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

cddr_lingam_p3_plot <- ggplot(cddr_lingam_p3) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=order_right, color = "Choose X->Y")) +
  geom_line(aes(x=samplesizes, y=order_left, color = "Choose Y->X")) +
  geom_ribbon(aes(x = samplesizes, ymin = d1_ci_lower, ymax = d1_ci_upper), alpha = 0.4, fill = "#CC6600") +
  geom_ribbon(aes(x = samplesizes, ymin = d2_ci_lower, ymax = d2_ci_upper), alpha = 0.4, fill = "#6699CC") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Lingam CDDR Diagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

linear_lingam_plot <- cddr_lingam_p1_plot + cddr_lingam_p125_plot + cddr_lingam_p3_plot +
  plot_annotation(title = 'Lingam') +
  plot_layout(guides = "collect") & theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
linear_test_plot <- cddr_test_p1_plot + cddr_test_p125_plot + cddr_test_p3_plot + 
  plot_annotation(title = 'test-based approach') +
  plot_layout(guides = "collect") & theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
combined_linear <- (cddr_lingam_p1_plot + cddr_lingam_p125_plot + cddr_lingam_p3_plot) / (cddr_test_p1_plot + cddr_test_p125_plot + cddr_test_p3_plot) +
  plot_annotation(title = '') +
  plot_layout(guides = "collect") & theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
combined_linear
grid::grid.draw(grid::textGrob(ylab, x = 0.02, rot = 90))
grid::grid.draw(grid::textGrob(xlab, x = 0.53, y = 0.03))

ggsave(path = main_dir, "linearity_plots.png")

## Gaussian Plots
colors <- c("both reject" = "#8046A0", "reject only X -> Y" = "#6699CC", "reject only Y -> X" = "#CC6600", "fail to reject both" = "#007A5E")
cddr_test_e1_plot <- ggplot(cddr_test_e1) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_reject, color = "both reject")) +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_noreject, color = "fail to reject both")) +
  geom_line(aes(x=samplesizes, y=yonx_reject_xony_noreject, color = "reject only X -> Y")) +
  geom_line(aes(x=samplesizes, y=xony_reject_yonx_noreject, color = "reject only Y -> X")) +
  geom_ribbon(aes(x = samplesizes, ymin = p11_ci_lower, ymax = p11_ci_upper), alpha = 0.3, fill = "#8046A0") +
  geom_ribbon(aes(x = samplesizes, ymin = p00_ci_lower, ymax = p00_ci_upper), alpha = 0.3, fill = "#007A5E") +
  geom_ribbon(aes(x = samplesizes, ymin = p01_ci_lower, ymax = p01_ci_upper), alpha = 0.3, fill = "#6699CC") +
  geom_ribbon(aes(x = samplesizes, ymin = p10_ci_lower, ymax = p10_ci_upper), alpha = 0.3, fill = "#CC6600") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Test-based Approach CDDR\nDiagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

cddr_test_e2_plot <- ggplot(cddr_test_e2) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_reject, color = "both reject")) +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_noreject, color = "fail to reject both")) +
  geom_line(aes(x=samplesizes, y=yonx_reject_xony_noreject, color = "reject only X -> Y")) +
  geom_line(aes(x=samplesizes, y=xony_reject_yonx_noreject, color = "reject only Y -> X")) +
  geom_ribbon(aes(x = samplesizes, ymin = p11_ci_lower, ymax = p11_ci_upper), alpha = 0.3, fill = "#8046A0") +
  geom_ribbon(aes(x = samplesizes, ymin = p00_ci_lower, ymax = p00_ci_upper), alpha = 0.3, fill = "#007A5E") +
  geom_ribbon(aes(x = samplesizes, ymin = p01_ci_lower, ymax = p01_ci_upper), alpha = 0.3, fill = "#6699CC") +
  geom_ribbon(aes(x = samplesizes, ymin = p10_ci_lower, ymax = p10_ci_upper), alpha = 0.3, fill = "#CC6600") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Test-based Approach CDDR\nDiagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

cddr_test_e3_plot <- ggplot(cddr_test_e3) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_reject, color = "both reject")) +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_noreject, color = "fail to reject both")) +
  geom_line(aes(x=samplesizes, y=yonx_reject_xony_noreject, color = "reject only X -> Y")) +
  geom_line(aes(x=samplesizes, y=xony_reject_yonx_noreject, color = "reject only Y -> X")) +
  geom_ribbon(aes(x = samplesizes, ymin = p11_ci_lower, ymax = p11_ci_upper), alpha = 0.3, fill = "#8046A0") +
  geom_ribbon(aes(x = samplesizes, ymin = p00_ci_lower, ymax = p00_ci_upper), alpha = 0.3, fill = "#007A5E") +
  geom_ribbon(aes(x = samplesizes, ymin = p01_ci_lower, ymax = p01_ci_upper), alpha = 0.3, fill = "#6699CC") +
  geom_ribbon(aes(x = samplesizes, ymin = p10_ci_lower, ymax = p10_ci_upper), alpha = 0.3, fill = "#CC6600") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Test-based Approach CDDR\nDiagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

colors <- c("Choose X->Y" = "#CC6600", "Choose Y->X" = "#6699CC")
cddr_lingam_e1_plot <- ggplot(cddr_lingam_e1) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=order_right, color = "Choose X->Y")) +
  geom_line(aes(x=samplesizes, y=order_left, color = "Choose Y->X")) +
  geom_ribbon(aes(x = samplesizes, ymin = d1_ci_lower, ymax = d1_ci_upper), alpha = 0.4, fill = "#CC6600") +
  geom_ribbon(aes(x = samplesizes, ymin = d2_ci_lower, ymax = d2_ci_upper), alpha = 0.4, fill = "#6699CC") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Lingam CDDR Diagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

cddr_lingam_e2_plot <- ggplot(cddr_lingam_e2) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=order_right, color = "Choose X->Y")) +
  geom_line(aes(x=samplesizes, y=order_left, color = "Choose Y->X")) +
  geom_ribbon(aes(x = samplesizes, ymin = d1_ci_lower, ymax = d1_ci_upper), alpha = 0.4, fill = "#CC6600") +
  geom_ribbon(aes(x = samplesizes, ymin = d2_ci_lower, ymax = d2_ci_upper), alpha = 0.4, fill = "#6699CC") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Lingam CDDR Diagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

cddr_lingam_e3_plot <- ggplot(cddr_lingam_e3) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=order_right, color = "Choose X->Y")) +
  geom_line(aes(x=samplesizes, y=order_left, color = "Choose Y->X")) +
  geom_ribbon(aes(x = samplesizes, ymin = d1_ci_lower, ymax = d1_ci_upper), alpha = 0.4, fill = "#CC6600") +
  geom_ribbon(aes(x = samplesizes, ymin = d2_ci_lower, ymax = d2_ci_upper), alpha = 0.4, fill = "#6699CC") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Lingam CDDR Diagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

gauss_test_plot <- cddr_test_e3_plot + cddr_test_e2_plot + cddr_test_e1_plot + 
  plot_annotation(title = 'Test-based approach') +
  plot_layout(guides = "collect") & theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
gauss_lingam_plot <- cddr_lingam_e3_plot + cddr_lingam_e2_plot + cddr_lingam_e1_plot +
  plot_annotation(title = 'Lingam') +
  plot_layout(guides = "collect") & theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
combined_gauss <-  (cddr_lingam_e3_plot + cddr_lingam_e2_plot + cddr_lingam_e1_plot) / (cddr_test_e3_plot + cddr_test_e2_plot + cddr_test_e1_plot)+
  plot_annotation(title = '') +
  plot_layout(guides = "collect") & theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ylab <- "True Direction Detection Rate"
xlab <- "Sample Sizes"
combined_gauss 
grid::grid.draw(grid::textGrob(ylab, x = 0.02, rot = 90))
grid::grid.draw(grid::textGrob(xlab, x = 0.53, y = 0.03))

ggsave(path = main_dir, "gauss_plot.png")

## real data
## the population and calorie data

data_dir <- "~/Documents/cddr_paper/real_data/data/"
main_dir <- "~/Documents/cddr_paper/real_data/results/"
cal_data <- read.csv(paste0(data_dir, "pop_cal.csv"))

### test-based approach
cddr_test_cal <- read.csv(file.path(main_dir,"cddr_test_pop_cal.csv"))
### lingam
cddr_lingam_cal <- read.csv(file.path(main_dir,"cddr_lingam_pop_cal.csv"))

cddr_test_cal <- calc_ci(cddr_test_cal, "yonx_and_xony_reject", "p11")
cddr_test_cal <- calc_ci(cddr_test_cal, "yonx_and_xony_noreject", "p00")
cddr_test_cal <- calc_ci(cddr_test_cal, "yonx_reject_xony_noreject", "p01")
cddr_test_cal <- calc_ci(cddr_test_cal, "xony_reject_yonx_noreject", "p10")

cddr_lingam_cal <- calc_ci(cddr_lingam_cal, "order_right", "d1")
cddr_lingam_cal <- calc_ci(cddr_lingam_cal, "order_left", "d2")



# plotting lingam results for cal
colors <- c("Choose X -> Y" = "#CC6600", "Choose Y -> X" = "#6699CC")
lin_cal_plot <- ggplot(cddr_lingam_cal) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=order_right, color = "Choose X -> Y")) +
  geom_line(aes(x=samplesizes, y=order_left, color = "Choose Y -> X")) +
  geom_ribbon(aes(x = samplesizes, ymin = d1_ci_lower, ymax = d1_ci_upper), alpha = 0.4, fill = "#CC6600") +
  geom_ribbon(aes(x = samplesizes, ymin = d2_ci_lower, ymax = d2_ci_upper), alpha = 0.4, fill = "#6699CC") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Lingam CDDR Diagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors) +
  theme(axis.title=element_text(size=10))

# plotting test-based approach results for cal
colors <- c("both reject" = "#8046A0", "reject only X -> Y" = "#6699CC", "reject only Y -> X" = "#CC6600", "fail to reject both" = "#007A5E")
test_cal_plot <- ggplot(cddr_test_cal) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_reject, color = "both reject")) +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_noreject, color = "fail to reject both")) +
  geom_line(aes(x=samplesizes, y=yonx_reject_xony_noreject, color = "reject only X -> Y")) +
  geom_line(aes(x=samplesizes, y=xony_reject_yonx_noreject, color = "reject only Y -> X")) +
  geom_ribbon(aes(x = samplesizes, ymin = p11_ci_lower, ymax = p11_ci_upper), alpha = 0.3, fill = "#8046A0") +
  geom_ribbon(aes(x = samplesizes, ymin = p00_ci_lower, ymax = p00_ci_upper), alpha = 0.3, fill = "#007A5E") +
  geom_ribbon(aes(x = samplesizes, ymin = p01_ci_lower, ymax = p01_ci_upper), alpha = 0.3, fill = "#6699CC") +
  geom_ribbon(aes(x = samplesizes, ymin = p10_ci_lower, ymax = p10_ci_upper), alpha = 0.3, fill = "#CC6600") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Test-based Approach CDDR\nDiagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors) +
  theme(axis.title=element_text(size=10))

cal_data_plot <- ggplot(cal_data, aes(x=pop, y=food)) +
  geom_point() +
  theme_bw() + 
  labs(
    x = "X: Rate of Change in Population",
    y = "Y: Rate of Change in\nTotal Dietary Consumption"
  ) +
  theme(axis.title=element_text(size=10))


combined_cal <- cal_data_plot + lin_cal_plot + test_cal_plot +
  plot_annotation(title = 'Population and Food Data Results') +
  plot_layout(guides = "collect") & theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
combined_cal
## if want to separately save results
# ggsave(path = main_dir, "combined_cal_plots.png")

## DRC my_kiss
my_kiss_drc_data <- read.csv(paste0(data_dir, "mykiss_drc.csv"))

### test-based approach
cddr_test_my_kiss_drc <- read.csv(file.path(main_dir,"cddr_test_my_kiss_drc.csv"))
### lingam
cddr_lingam_my_kiss_drc <- read.csv(file.path(main_dir,"cddr_lingam_my_kiss_drc.csv"))

cddr_test_my_kiss_drc <- calc_ci(cddr_test_my_kiss_drc, "yonx_and_xony_reject", "p11")
cddr_test_my_kiss_drc <- calc_ci(cddr_test_my_kiss_drc, "yonx_and_xony_noreject", "p00")
cddr_test_my_kiss_drc <- calc_ci(cddr_test_my_kiss_drc, "yonx_reject_xony_noreject", "p01")
cddr_test_my_kiss_drc <- calc_ci(cddr_test_my_kiss_drc, "xony_reject_yonx_noreject", "p10")

cddr_lingam_my_kiss_drc <- calc_ci(cddr_lingam_my_kiss_drc, "order_right", "d1")
cddr_lingam_my_kiss_drc <- calc_ci(cddr_lingam_my_kiss_drc, "order_left", "d2")



# plotting lingam results for my_kiss_drc
colors <- c("Choose X -> Y" = "#CC6600", "Choose Y -> X" = "#6699CC")
lin_my_kiss_drc_plot <- ggplot(cddr_lingam_my_kiss_drc) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=order_right, color = "Choose X -> Y")) +
  geom_line(aes(x=samplesizes, y=order_left, color = "Choose Y -> X")) +
  geom_ribbon(aes(x = samplesizes, ymin = d1_ci_lower, ymax = d1_ci_upper), alpha = 0.4, fill = "#CC6600") +
  geom_ribbon(aes(x = samplesizes, ymin = d2_ci_lower, ymax = d2_ci_upper), alpha = 0.4, fill = "#6699CC") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Lingam CDDR Diagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors) +
  theme(axis.title=element_text(size=10))

# plotting test-based approach results for my_kiss_drc
colors <- c("both reject" = "#8046A0", "reject only X -> Y" = "#6699CC", "reject only Y -> X" = "#CC6600", "fail to reject both" = "#007A5E")
test_my_kiss_drc_plot <- ggplot(cddr_test_my_kiss_drc) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_reject, color = "both reject")) +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_noreject, color = "fail to reject both")) +
  geom_line(aes(x=samplesizes, y=yonx_reject_xony_noreject, color = "reject only X -> Y")) +
  geom_line(aes(x=samplesizes, y=xony_reject_yonx_noreject, color = "reject only Y -> X")) +
  geom_ribbon(aes(x = samplesizes, ymin = p11_ci_lower, ymax = p11_ci_upper), alpha = 0.3, fill = "#8046A0") +
  geom_ribbon(aes(x = samplesizes, ymin = p00_ci_lower, ymax = p00_ci_upper), alpha = 0.3, fill = "#007A5E") +
  geom_ribbon(aes(x = samplesizes, ymin = p01_ci_lower, ymax = p01_ci_upper), alpha = 0.3, fill = "#6699CC") +
  geom_ribbon(aes(x = samplesizes, ymin = p10_ci_lower, ymax = p10_ci_upper), alpha = 0.3, fill = "#CC6600") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Test-based Approach CDDR\nDiagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors) +
  theme(axis.title=element_text(size=10))

my_kiss_drc_data_plot <- ggplot(my_kiss_drc_data, aes(x=conc_new, y=weight)) +
  geom_point() +
  theme_bw() + 
  labs(
    x = "X: Dose Concentration (Transformed)",
    y = "Y: Wet Weight"
  ) +
  theme(axis.title=element_text(size=10))


combined_my_kiss <- my_kiss_drc_data_plot + lin_my_kiss_drc_plot + test_my_kiss_drc_plot +
  plot_annotation(title = 'Fish Dose Response Plot Results') +
  plot_layout(guides = "collect") & theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
combined_my_kiss
## if want to separately save results
# ggsave(path = main_dir, "combined_my_kiss_plots.png")

## Plots for Ozone and Temperature 3
temp_ozone3_data <- read.csv(paste0(data_dir, "temp_ozone3.csv"))

### test-based approach
cddr_test_temp_ozone3 <- read.csv(file.path(main_dir,"cddr_test_temp_ozone3.csv"))
### lingam
cddr_lingam_temp_ozone3 <- read.csv(file.path(main_dir,"cddr_lingam_temp_ozone3.csv"))

cddr_test_temp_ozone3 <- calc_ci(cddr_test_temp_ozone3, "yonx_and_xony_reject", "p11")
cddr_test_temp_ozone3 <- calc_ci(cddr_test_temp_ozone3, "yonx_and_xony_noreject", "p00")
cddr_test_temp_ozone3 <- calc_ci(cddr_test_temp_ozone3, "yonx_reject_xony_noreject", "p01")
cddr_test_temp_ozone3 <- calc_ci(cddr_test_temp_ozone3, "xony_reject_yonx_noreject", "p10")

cddr_lingam_temp_ozone3 <- calc_ci(cddr_lingam_temp_ozone3, "order_right", "d1")
cddr_lingam_temp_ozone3 <- calc_ci(cddr_lingam_temp_ozone3, "order_left", "d2")



# plotting lingam results for temp_ozone3
colors <- c("Choose Y -> X" = "#6699CC", "Choose X -> Y" = "#CC6600")
lin_temp_ozone3_plot <- ggplot(cddr_lingam_temp_ozone3) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=order_right, color = "Choose Y -> X")) +
  geom_line(aes(x=samplesizes, y=order_left, color = "Choose X -> Y")) +
  geom_ribbon(aes(x = samplesizes, ymin = d1_ci_lower, ymax = d1_ci_upper), alpha = 0.4, fill = "#6699CC") +
  geom_ribbon(aes(x = samplesizes, ymin = d2_ci_lower, ymax = d2_ci_upper), alpha = 0.4, fill = "#CC6600") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Lingam CDDR Diagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors) +
  theme(axis.title=element_text(size=10))

# plotting test-based approach results for temp_ozone3
colors <- c("both reject" = "#8046A0", "reject only Y -> X" = "#CC6600", "reject only X -> Y" = "#6699CC", "fail to reject both" = "#007A5E")
test_temp_ozone3_plot <- ggplot(cddr_test_temp_ozone3) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_reject, color = "both reject")) +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_noreject, color = "fail to reject both")) +
  geom_line(aes(x=samplesizes, y=yonx_reject_xony_noreject, color = "reject only Y -> X")) +
  geom_line(aes(x=samplesizes, y=xony_reject_yonx_noreject, color = "reject only X -> Y")) +
  geom_ribbon(aes(x = samplesizes, ymin = p11_ci_lower, ymax = p11_ci_upper), alpha = 0.3, fill = "#8046A0") +
  geom_ribbon(aes(x = samplesizes, ymin = p00_ci_lower, ymax = p00_ci_upper), alpha = 0.3, fill = "#007A5E") +
  geom_ribbon(aes(x = samplesizes, ymin = p01_ci_lower, ymax = p01_ci_upper), alpha = 0.3, fill = "#CC6600") +
  geom_ribbon(aes(x = samplesizes, ymin = p10_ci_lower, ymax = p10_ci_upper), alpha = 0.3, fill = "#6699CC") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Test-based Approach CDDR\nDiagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors) +
  theme(axis.title=element_text(size=10))

temp_ozone3_data_plot <- ggplot(temp_ozone3_data, aes(x=ozone, y=temp)) +
  geom_point() +
  theme_bw() +
  labs(
    x = "X: Lower Atmosphere Ozone Concentration",
    y = "Y: Temperature"
  ) +
  theme(axis.title=element_text(size=10))


combined_ozone3 <- temp_ozone3_data_plot + lin_temp_ozone3_plot + test_temp_ozone3_plot +
  plot_annotation(title = 'Ozone and Temperature Results') +
  plot_layout(guides = "collect") & theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
combined_ozone3

## Plots for Sleep and Depression Data
### test-based approach
cddr_test_mbdi <- read.csv(file.path(main_dir,"cddr_test_mbdi_real.csv"))
### lingam
cddr_lingam_mbdi <- read.csv(file.path(main_dir,"cddr_lingam_mbdi_real.csv"))

cddr_test_mbdi <- calc_ci(cddr_test_mbdi, "yonx_and_xony_reject", "p11")
cddr_test_mbdi <- calc_ci(cddr_test_mbdi, "yonx_and_xony_noreject", "p00")
cddr_test_mbdi <- calc_ci(cddr_test_mbdi, "yonx_reject_xony_noreject", "p01")
cddr_test_mbdi <- calc_ci(cddr_test_mbdi, "xony_reject_yonx_noreject", "p10")

cddr_lingam_mbdi <- calc_ci(cddr_lingam_mbdi, "order_right", "d1")
cddr_lingam_mbdi <- calc_ci(cddr_lingam_mbdi, "order_left", "d2")


mBDI <- read.table(file.path(data_dir,"mBDIdataset.dat"), sep="\t")
colnames(mBDI) <- c("id","sex","age","sleep","mbdi")

colors <- c("Choose X -> Y" = "#CC6600", "Choose Y -> X" = "#6699CC")
# plotting lingam results for sleep data
lin_sleep_plot <- ggplot(cddr_lingam_mbdi) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=order_right, color = "Choose X -> Y")) +
  geom_line(aes(x=samplesizes, y=order_left, color = "Choose Y -> X")) +
  geom_ribbon(aes(x = samplesizes, ymin = d1_ci_lower, ymax = d1_ci_upper), alpha = 0.4, fill = "#CC6600") +
  geom_ribbon(aes(x = samplesizes, ymin = d2_ci_lower, ymax = d2_ci_upper), alpha = 0.4, fill = "#6699CC") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Lingam CDDR Diagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors) +
  theme(axis.title=element_text(size=10))


# plotting test-based approach results for sleep data
colors <- c("both reject" = "#8046A0", "reject only X -> Y" = "#6699CC", "reject only Y -> X" = "#CC6600", "fail to reject both" = "#007A5E")
test_sleep_plot <- ggplot(cddr_test_mbdi) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_reject, color = "both reject")) +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_noreject, color = "fail to reject both")) +
  geom_line(aes(x=samplesizes, y=yonx_reject_xony_noreject, color = "reject only X -> Y")) +
  geom_line(aes(x=samplesizes, y=xony_reject_yonx_noreject, color = "reject only Y -> X")) +
  geom_ribbon(aes(x = samplesizes, ymin = p11_ci_lower, ymax = p11_ci_upper), alpha = 0.3, fill = "#8046A0") +
  geom_ribbon(aes(x = samplesizes, ymin = p00_ci_lower, ymax = p00_ci_upper), alpha = 0.3, fill = "#007A5E") +
  geom_ribbon(aes(x = samplesizes, ymin = p01_ci_lower, ymax = p01_ci_upper), alpha = 0.3, fill = "#6699CC") +
  geom_ribbon(aes(x = samplesizes, ymin = p10_ci_lower, ymax = p10_ci_upper), alpha = 0.3, fill = "#CC6600") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Test-based Approach CDDR\nDiagnostic Colors") +
  ylim(-0.05,1.05) +
  theme(axis.title=element_text(size=10)) + 
  scale_color_manual(values = colors)

sleep_data_plot <- ggplot(mBDI, aes(x=sleep, y=mbdi)) +
  geom_point() +
  theme_bw() + 
  labs(
    x = "X: Sleep Problems",
    y = "Y: Depression Symptoms"
  ) +
  theme(axis.title=element_text(size=10))

combined_sleep <- sleep_data_plot + lin_sleep_plot + test_sleep_plot +
  plot_annotation(title = 'Sleep and Depression Data Results') +
  plot_layout(guides = "collect") & theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

combined_sleep

## if want to separately save results
# ggsave(path = main_dir, "sleep_res_plots.png")

theme_settings <- theme(axis.title = element_text(size = 9)) +
  theme(legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10))

real_data_plots <- (temp_ozone3_data_plot + lin_temp_ozone3_plot + test_temp_ozone3_plot) / (cal_data_plot + lin_cal_plot + test_cal_plot) / (my_kiss_drc_data_plot + lin_my_kiss_drc_plot + test_my_kiss_drc_plot)  / (sleep_data_plot + lin_sleep_plot + test_sleep_plot) +
  plot_layout(guides = "collect") &
  theme_settings +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

real_data_plots
ggsave(path = main_dir, "real_data_plots.png")


## Supplement Stuff

# n = 400
## linearity assumption violated cases
main_dir <- "~/Documents/cddr_paper/simulations/small_samples/results"
## linearity assumption violated cases
### test-based approach
cddr_test_p1_small <- read.csv(file.path(main_dir,"cddr_test_y1_n400.csv"))
cddr_test_p125_small <- read.csv(file.path(main_dir,"cddr_test_y125_n400.csv"))
cddr_test_p3_small <- read.csv(file.path(main_dir,"cddr_test_y3_n400.csv"))
### lingam
cddr_lingam_p1_small <- read.csv(file.path(main_dir,"cddr_lingam_y1_n400.csv"))
cddr_lingam_p125_small <- read.csv(file.path(main_dir,"cddr_lingam_y125_n400.csv"))
cddr_lingam_p3_small <- read.csv(file.path(main_dir,"cddr_lingam_y3_n400.csv"))

## gaussianity assumption violated cases
### test-based approach
cddr_test_e1_small <- read.csv(file.path(main_dir,"cddr_test_e1_n400.csv"))
cddr_test_e2_small <- read.csv(file.path(main_dir,"cddr_test_e2_n400.csv"))
cddr_test_e3_small <- read.csv(file.path(main_dir,"cddr_test_e3_n400.csv"))
### lingam
cddr_lingam_e1_small <- read.csv(file.path(main_dir,"cddr_lingam_e1_n400.csv"))
cddr_lingam_e2_small <- read.csv(file.path(main_dir,"cddr_lingam_e2_n400.csv"))
cddr_lingam_e3_small <- read.csv(file.path(main_dir,"cddr_lingam_e3_n400.csv"))

## adding ci
cddr_test_p1_small <- calc_ci(cddr_test_p1_small, "yonx_and_xony_reject", "p11")
cddr_test_p1_small <- calc_ci(cddr_test_p1_small, "yonx_and_xony_noreject", "p00")
cddr_test_p1_small <- calc_ci(cddr_test_p1_small, "yonx_reject_xony_noreject", "p01")
cddr_test_p1_small <- calc_ci(cddr_test_p1_small, "xony_reject_yonx_noreject", "p10")

cddr_test_p125_small <- calc_ci(cddr_test_p125_small, "yonx_and_xony_reject", "p11")
cddr_test_p125_small <- calc_ci(cddr_test_p125_small, "yonx_and_xony_noreject", "p00")
cddr_test_p125_small <- calc_ci(cddr_test_p125_small, "yonx_reject_xony_noreject", "p01")
cddr_test_p125_small <- calc_ci(cddr_test_p125_small, "xony_reject_yonx_noreject", "p10")

cddr_test_p3_small <- calc_ci(cddr_test_p3_small, "yonx_and_xony_reject", "p11")
cddr_test_p3_small <- calc_ci(cddr_test_p3_small, "yonx_and_xony_noreject", "p00")
cddr_test_p3_small <- calc_ci(cddr_test_p3_small, "yonx_reject_xony_noreject", "p01")
cddr_test_p3_small <- calc_ci(cddr_test_p3_small, "xony_reject_yonx_noreject", "p10")

cddr_lingam_p1_small <- calc_ci(cddr_lingam_p1_small, "order_right", "d1")
cddr_lingam_p1_small <- calc_ci(cddr_lingam_p1_small, "order_left", "d2")

cddr_lingam_p125_small <- calc_ci(cddr_lingam_p125_small, "order_right", "d1")
cddr_lingam_p125_small <- calc_ci(cddr_lingam_p125_small, "order_left", "d2")

cddr_lingam_p3_small <- calc_ci(cddr_lingam_p3_small, "order_right", "d1")
cddr_lingam_p3_small <- calc_ci(cddr_lingam_p3_small, "order_left", "d2")

cddr_test_e1_small <- calc_ci(cddr_test_e1_small, "yonx_and_xony_reject", "p11")
cddr_test_e1_small <- calc_ci(cddr_test_e1_small, "yonx_and_xony_noreject", "p00")
cddr_test_e1_small <- calc_ci(cddr_test_e1_small, "yonx_reject_xony_noreject", "p01")
cddr_test_e1_small <- calc_ci(cddr_test_e1_small, "xony_reject_yonx_noreject", "p10")

cddr_test_e2_small <- calc_ci(cddr_test_e2_small, "yonx_and_xony_reject", "p11")
cddr_test_e2_small <- calc_ci(cddr_test_e2_small, "yonx_and_xony_noreject", "p00")
cddr_test_e2_small <- calc_ci(cddr_test_e2_small, "yonx_reject_xony_noreject", "p01")
cddr_test_e2_small <- calc_ci(cddr_test_e2_small, "xony_reject_yonx_noreject", "p10")

cddr_test_e3_small <- calc_ci(cddr_test_e3_small, "yonx_and_xony_reject", "p11")
cddr_test_e3_small <- calc_ci(cddr_test_e3_small, "yonx_and_xony_noreject", "p00")
cddr_test_e3_small <- calc_ci(cddr_test_e3_small, "yonx_reject_xony_noreject", "p01")
cddr_test_e3_small <- calc_ci(cddr_test_e3_small, "xony_reject_yonx_noreject", "p10")

cddr_lingam_e1_small <- calc_ci(cddr_lingam_e1_small, "order_right", "d1")
cddr_lingam_e1_small <- calc_ci(cddr_lingam_e1_small, "order_left", "d2")

cddr_lingam_e2_small <- calc_ci(cddr_lingam_e2_small, "order_right", "d1")
cddr_lingam_e2_small <- calc_ci(cddr_lingam_e2_small, "order_left", "d2")

cddr_lingam_e3_small <- calc_ci(cddr_lingam_e3_small, "order_right", "d1")
cddr_lingam_e3_small <- calc_ci(cddr_lingam_e3_small, "order_left", "d2")

colors <- c("both reject" = "#8046A0", "reject only X -> Y" = "#6699CC", "reject only Y -> X" = "#CC6600", "fail to reject both" = "#007A5E")
cddr_test_p1_plot_small <- ggplot(cddr_test_p1_small) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_reject, color = "both reject")) +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_noreject, color = "fail to reject both")) +
  geom_line(aes(x=samplesizes, y=yonx_reject_xony_noreject, color = "reject only X -> Y")) +
  geom_line(aes(x=samplesizes, y=xony_reject_yonx_noreject, color = "reject only Y -> X")) +
  geom_ribbon(aes(x = samplesizes, ymin = p11_ci_lower, ymax = p11_ci_upper), alpha = 0.3, fill = "#8046A0") +
  geom_ribbon(aes(x = samplesizes, ymin = p00_ci_lower, ymax = p00_ci_upper), alpha = 0.3, fill = "#007A5E") +
  geom_ribbon(aes(x = samplesizes, ymin = p01_ci_lower, ymax = p01_ci_upper), alpha = 0.3, fill = "#6699CC") +
  geom_ribbon(aes(x = samplesizes, ymin = p10_ci_lower, ymax = p10_ci_upper), alpha = 0.3, fill = "#CC6600") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Test-based Approach CDDR\nDiagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)


cddr_test_p125_plot_small <- ggplot(cddr_test_p125_small) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_reject, color = "both reject")) +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_noreject, color = "fail to reject both")) +
  geom_line(aes(x=samplesizes, y=yonx_reject_xony_noreject, color = "reject only X -> Y")) +
  geom_line(aes(x=samplesizes, y=xony_reject_yonx_noreject, color = "reject only Y -> X")) +
  geom_ribbon(aes(x = samplesizes, ymin = p11_ci_lower, ymax = p11_ci_upper), alpha = 0.3, fill = "#8046A0") +
  geom_ribbon(aes(x = samplesizes, ymin = p00_ci_lower, ymax = p00_ci_upper), alpha = 0.3, fill = "#007A5E") +
  geom_ribbon(aes(x = samplesizes, ymin = p01_ci_lower, ymax = p01_ci_upper), alpha = 0.3, fill = "#6699CC") +
  geom_ribbon(aes(x = samplesizes, ymin = p10_ci_lower, ymax = p10_ci_upper), alpha = 0.3, fill = "#CC6600") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Test-based Approach CDDR\nDiagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

cddr_test_p3_plot_small <- ggplot(cddr_test_p3_small) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_reject, color = "both reject")) +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_noreject, color = "fail to reject both")) +
  geom_line(aes(x=samplesizes, y=yonx_reject_xony_noreject, color = "reject only X -> Y")) +
  geom_line(aes(x=samplesizes, y=xony_reject_yonx_noreject, color = "reject only Y -> X")) +
  geom_errorbar(aes(x = samplesizes, ymin = p11_ci_lower, ymax = p11_ci_upper), width = 0.1, color="#8046A0", alpha = 0.5) +
  geom_ribbon(aes(x = samplesizes, ymin = p11_ci_lower, ymax = p11_ci_upper), alpha = 0.5, fill = "#8046A0") +
  geom_errorbar(aes(x = samplesizes, ymin = p00_ci_lower, ymax = p00_ci_upper), width = 0.1, color="#8046A0", alpha = 0.5) +
  geom_ribbon(aes(x = samplesizes, ymin = p00_ci_lower, ymax = p00_ci_upper), alpha = 0.5, fill = "#007A5E") +
  geom_errorbar(aes(x = samplesizes, ymin = p01_ci_lower, ymax = p01_ci_upper), width = 0.1, color="#8046A0", alpha = 0.5) +
  geom_ribbon(aes(x = samplesizes, ymin = p01_ci_lower, ymax = p01_ci_upper), alpha = 0.5, fill = "#6699CC") +
  geom_errorbar(aes(x = samplesizes, ymin = p10_ci_lower, ymax = p10_ci_upper), width = 0.1, color="#8046A0", alpha = 0.5) +
  geom_ribbon(aes(x = samplesizes, ymin = p10_ci_lower, ymax = p10_ci_upper), alpha = 0.5, fill = "#CC6600") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Test-based Approach CDDR\nDiagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

colors <- c("Choose X->Y" = "#CC6600", "Choose Y->X" = "#6699CC")

cddr_lingam_p1_plot_small <- ggplot(cddr_lingam_p1_small) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=order_right, color = "Choose X->Y")) +
  geom_ribbon(aes(x = samplesizes, ymin = d1_ci_lower, ymax = d1_ci_upper), alpha = 0.4, fill = "#CC6600") +
  geom_line(aes(x=samplesizes, y=order_left, color = "Choose Y->X")) +
  geom_ribbon(aes(x = samplesizes, ymin = d2_ci_lower, ymax = d2_ci_upper), alpha = 0.4, fill = "#6699CC") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Lingam CDDR Diagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

cddr_lingam_p125_plot_small <- ggplot(cddr_lingam_p125_small) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=order_right, color = "Choose X->Y")) +
  geom_ribbon(aes(x = samplesizes, ymin = d1_ci_lower, ymax = d1_ci_upper), alpha = 0.4, fill = "#CC6600") +
  geom_line(aes(x=samplesizes, y=order_left, color = "Choose Y->X")) +
  geom_ribbon(aes(x = samplesizes, ymin = d2_ci_lower, ymax = d2_ci_upper), alpha = 0.4, fill = "#6699CC") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Lingam CDDR Diagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

cddr_lingam_p3_plot_small <- ggplot(cddr_lingam_p3_small) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=order_right, color = "Choose X->Y")) +
  geom_line(aes(x=samplesizes, y=order_left, color = "Choose Y->X")) +
  geom_ribbon(aes(x = samplesizes, ymin = d1_ci_lower, ymax = d1_ci_upper), alpha = 0.4, fill = "#CC6600") +
  geom_ribbon(aes(x = samplesizes, ymin = d2_ci_lower, ymax = d2_ci_upper), alpha = 0.4, fill = "#6699CC") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Lingam CDDR Diagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

linear_lingam_plot_small <- cddr_lingam_p1_plot_small + cddr_lingam_p125_plot_small + cddr_lingam_p3_plot_small +
  plot_annotation(title = 'Lingam') +
  plot_layout(guides = "collect") & theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
linear_test_plot_small <- cddr_test_p1_plot_small + cddr_test_p125_plot_small + cddr_test_p3_plot_small + 
  plot_annotation(title = 'test-based approach') +
  plot_layout(guides = "collect") & theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
combined_linear_small <- (cddr_lingam_p1_plot_small + cddr_lingam_p125_plot_small + cddr_lingam_p3_plot_small) / (cddr_test_p1_plot_small + cddr_test_p125_plot_small + cddr_test_p3_plot_small) +
  plot_annotation(title = '') +
  plot_layout(guides = "collect") & theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
combined_linear_small
grid::grid.draw(grid::textGrob(ylab, x = 0.02, rot = 90))
grid::grid.draw(grid::textGrob(xlab, x = 0.53, y = 0.03))

ggsave(path = main_dir, "linearity_plots_n400.png")

## Gaussian Plots
colors <- c("both reject" = "#8046A0", "reject only X -> Y" = "#6699CC", "reject only Y -> X" = "#CC6600", "fail to reject both" = "#007A5E")
cddr_test_e1_plot_small <- ggplot(cddr_test_e1_small) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_reject, color = "both reject")) +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_noreject, color = "fail to reject both")) +
  geom_line(aes(x=samplesizes, y=yonx_reject_xony_noreject, color = "reject only X -> Y")) +
  geom_line(aes(x=samplesizes, y=xony_reject_yonx_noreject, color = "reject only Y -> X")) +
  geom_ribbon(aes(x = samplesizes, ymin = p11_ci_lower, ymax = p11_ci_upper), alpha = 0.3, fill = "#8046A0") +
  geom_ribbon(aes(x = samplesizes, ymin = p00_ci_lower, ymax = p00_ci_upper), alpha = 0.3, fill = "#007A5E") +
  geom_ribbon(aes(x = samplesizes, ymin = p01_ci_lower, ymax = p01_ci_upper), alpha = 0.3, fill = "#6699CC") +
  geom_ribbon(aes(x = samplesizes, ymin = p10_ci_lower, ymax = p10_ci_upper), alpha = 0.3, fill = "#CC6600") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Test-based Approach CDDR\nDiagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

cddr_test_e2_plot_small <- ggplot(cddr_test_e2_small) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_reject, color = "both reject")) +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_noreject, color = "fail to reject both")) +
  geom_line(aes(x=samplesizes, y=yonx_reject_xony_noreject, color = "reject only X -> Y")) +
  geom_line(aes(x=samplesizes, y=xony_reject_yonx_noreject, color = "reject only Y -> X")) +
  geom_ribbon(aes(x = samplesizes, ymin = p11_ci_lower, ymax = p11_ci_upper), alpha = 0.3, fill = "#8046A0") +
  geom_ribbon(aes(x = samplesizes, ymin = p00_ci_lower, ymax = p00_ci_upper), alpha = 0.3, fill = "#007A5E") +
  geom_ribbon(aes(x = samplesizes, ymin = p01_ci_lower, ymax = p01_ci_upper), alpha = 0.3, fill = "#6699CC") +
  geom_ribbon(aes(x = samplesizes, ymin = p10_ci_lower, ymax = p10_ci_upper), alpha = 0.3, fill = "#CC6600") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Test-based Approach CDDR\nDiagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

cddr_test_e3_plot_small <- ggplot(cddr_test_e3_small) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_reject, color = "both reject")) +
  geom_line(aes(x=samplesizes, y=yonx_and_xony_noreject, color = "fail to reject both")) +
  geom_line(aes(x=samplesizes, y=yonx_reject_xony_noreject, color = "reject only X -> Y")) +
  geom_line(aes(x=samplesizes, y=xony_reject_yonx_noreject, color = "reject only Y -> X")) +
  geom_ribbon(aes(x = samplesizes, ymin = p11_ci_lower, ymax = p11_ci_upper), alpha = 0.3, fill = "#8046A0") +
  geom_ribbon(aes(x = samplesizes, ymin = p00_ci_lower, ymax = p00_ci_upper), alpha = 0.3, fill = "#007A5E") +
  geom_ribbon(aes(x = samplesizes, ymin = p01_ci_lower, ymax = p01_ci_upper), alpha = 0.3, fill = "#6699CC") +
  geom_ribbon(aes(x = samplesizes, ymin = p10_ci_lower, ymax = p10_ci_upper), alpha = 0.3, fill = "#CC6600") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Test-based Approach CDDR\nDiagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

colors <- c("Choose X->Y" = "#CC6600", "Choose Y->X" = "#6699CC")
cddr_lingam_e1_plot_small <- ggplot(cddr_lingam_e1_small) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=order_right, color = "Choose X->Y")) +
  geom_line(aes(x=samplesizes, y=order_left, color = "Choose Y->X")) +
  geom_ribbon(aes(x = samplesizes, ymin = d1_ci_lower, ymax = d1_ci_upper), alpha = 0.4, fill = "#CC6600") +
  geom_ribbon(aes(x = samplesizes, ymin = d2_ci_lower, ymax = d2_ci_upper), alpha = 0.4, fill = "#6699CC") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Lingam CDDR Diagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

cddr_lingam_e2_plot_small <- ggplot(cddr_lingam_e2_small) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=order_right, color = "Choose X->Y")) +
  geom_line(aes(x=samplesizes, y=order_left, color = "Choose Y->X")) +
  geom_ribbon(aes(x = samplesizes, ymin = d1_ci_lower, ymax = d1_ci_upper), alpha = 0.4, fill = "#CC6600") +
  geom_ribbon(aes(x = samplesizes, ymin = d2_ci_lower, ymax = d2_ci_upper), alpha = 0.4, fill = "#6699CC") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Lingam CDDR Diagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

cddr_lingam_e3_plot_small <- ggplot(cddr_lingam_e3_small) +
  theme_bw() +
  geom_line(aes(x=samplesizes, y=order_right, color = "Choose X->Y")) +
  geom_line(aes(x=samplesizes, y=order_left, color = "Choose Y->X")) +
  geom_ribbon(aes(x = samplesizes, ymin = d1_ci_lower, ymax = d1_ci_upper), alpha = 0.4, fill = "#CC6600") +
  geom_ribbon(aes(x = samplesizes, ymin = d2_ci_lower, ymax = d2_ci_upper), alpha = 0.4, fill = "#6699CC") +
  labs(
    x = "Subsample Size",
    y = "Causal Outcome Rate",
    color = "Lingam CDDR Diagnostic Colors") +
  ylim(-0.05,1.05) +
  scale_color_manual(values = colors)

gauss_test_plot_small <- cddr_test_e3_plot_small + cddr_test_e2_plot_small + cddr_test_e1_plot_small + 
  plot_annotation(title = 'test-based approach') +
  plot_layout(guides = "collect") & theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
gauss_lingam_plot_small <- cddr_lingam_e3_plot_small + cddr_lingam_e2_plot_small + cddr_lingam_e1_plot_small +
  plot_annotation(title = 'Lingam') +
  plot_layout(guides = "collect") & theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
combined_gauss_small <- (cddr_lingam_e3_plot_small + cddr_lingam_e2_plot_small + cddr_lingam_e1_plot_small) / (cddr_test_e3_plot_small + cddr_test_e2_plot_small + cddr_test_e1_plot_small) +
  plot_annotation(title = '') +
  plot_layout(guides = "collect") & theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ylab <- "True Direction Detection Rate"
xlab <- "Sample Sizes"
combined_gauss_small 
grid::grid.draw(grid::textGrob(ylab, x = 0.02, rot = 90))
grid::grid.draw(grid::textGrob(xlab, x = 0.53, y = 0.03))

ggsave(path = main_dir, "gauss_plot_n400.png")