rm(list = ls())
library(mixtools)
library(splines)
library(ISLR)
library(lspline)
library(ggplot2)
library(mclust)
library(ReIns)
library(rlingam)
library(extraDistr)
library(patchwork)

#####
# different simulation setup (using polynomial models)
#####

directory <- "~/Documents/cddr_paper/simulations/data/"
set.seed(361)

## simulation linearity violation data
N = 10000
rate = 1.2802
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
b = 1
y1 = (sign(X_shift)*abs(X_shift)^b*beta) + eta3
yl1 = (sign(X_shift)*abs(X_shift)^b*beta)
data_sim1 = data.frame(sleep = X, depr = y1)
colnames(data_sim1) <- c("sleep","mbdi")

ggplot(data_sim1) +
  geom_point(aes(x=X, y=y1)) +
  theme_bw() +
  labs(x = "X",
       y = "Y^b",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  theme(text = element_text(size = 20))

b = 1.25
y125 = (sign(X_shift)*abs(X_shift)^b*beta) + eta3
yl125 = (sign(X_shift)*abs(X_shift)^b*beta)
data_sim125 = data.frame(sleep = X, depr = y125)
colnames(data_sim125) <- c("sleep","mbdi")

ggplot(data_sim125) +
  geom_point(aes(x=X, y=y125)) +
  theme_bw() +
  labs(x = "X",
       y = "Y^b",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  theme(text = element_text(size = 20))

b = 3
y3 = (sign(X_shift)*abs(X_shift)^b*beta) + eta3
yl3 = (sign(X_shift)*abs(X_shift)^b*beta)
data_sim3 = data.frame(sleep = X, depr = y3)
colnames(data_sim3) <- c("sleep","mbdi")

ggplot(data_sim3) +
  geom_point(aes(x=X, y=y3)) +
  theme_bw() +
  labs(x = "X",
       y = "Y^b",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  theme(text = element_text(size = 20))

data_sims_poly = data.frame(X, y125, y1, yl1, yl125, y3, yl3)
colors <- c("Polynomial = 1" = "#666666", "Polynomial = 1.25" = "cyan", "Polynomial = 3" = "blue")

lin_sets <- ggplot(data_sims_poly) +
  geom_point(aes(x=X, y=y1), color = "navy") +
  geom_line(aes(x=X, y=yl1, color = "Polynomial = 1")) +
  geom_point(aes(x=X, y=y125), color = "darkgreen") +
  geom_line(aes(x=X, y=yl125, color = "Polynomial = 1.25")) +
  geom_point(aes(x=X, y=y3), color = "lightblue") +
  geom_line(aes(x=X, y=yl3, color = "Polynomial = 3")) +
  theme_bw() +
  labs(x = "X",
       y = "Y (Differing Polynomial Powers)",
       color = "Linearity Settings") +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.8, 0.15),
        text = element_text(size = 18))

lin_sets
main_dir <- "~/Documents/cddr_paper/simulations/data/eda/"
ggsave(path = main_dir,"lin_sets.png")

write.csv(data_sim1,file.path(directory,"datasims_poly_y1.csv"))
write.csv(data_sim125,file.path(directory,"datasims_poly_y125.csv"))
write.csv(data_sim3,file.path(directory,"datasims_poly_y3.csv"))

## non-guassianity assump violated

set.seed(361)
N = 10000
X = rnorm(N)

## gaussian
mu1 <- 0
sigma1 <- 0.6
eta1 <- rnorm(N, mu1, sigma1)

## 2 mixture gaussian
lambda2 <- c(0.57, 0.43)
mu21 <- -1
mu22 <- 0.25
sigma21 <- 0.5
sigma22 <- 0.6
props <- sample(c(0, 1), N, replace = T, prob = lambda2)
g1 <- rnorm(N, mu21, sigma21)
g2 <- rnorm(N, mu22, sigma22)
eta2 = ifelse(props == 0, g1, g2)

# ## 3 mixtures gaussian
lambda3 <- c(0.4, 0.2, 0.4)
mu31 <- -2
mu32 <- 0
mu33 <- 2
sigma31 <- 0.5
sigma32 <- 1
sigma33 <- 3
props <- sample(c(0, 1, 2), N, replace = T, prob = lambda3)
g1 <- rnorm(N, mu31, sigma31)
g2 <- rnorm(N, mu32, sigma32)
g3 <- rnorm(N, mu33, sigma33)
eta3 = ifelse(props == 0, g1,
              ifelse(props == 1, g2, g3))


beta = 2
b = 1/3
a = 2.5
X_shift <- X-a
b = 1
ye1 = (sign(X_shift)*abs(X_shift)^b*beta) + eta1
ye2 = (sign(X_shift)*abs(X_shift)^b*beta) + eta2
ye3 = (sign(X_shift)*abs(X_shift)^b*beta) + eta3

data_sims_poly = data.frame(X, ye1, ye2, ye3, eta1, eta2, eta3)
colors <- c("Normal" = "black", "GMM(k=3)" = "brown", "GMM(k=2)" = "purple")
ggplot(data_sims_poly) +
  geom_point(aes(x=X, y=ye1, color = "Normal")) +
  geom_point(aes(x=X, y=ye2, color = "GMM(k=2)")) +
  geom_point(aes(x=X, y=ye2, color = "GMM(k=3)")) +
  theme_bw() +
  labs(x = "X",
       y = "Y with different error distributions",
       color = "Error Distributions") +
  scale_color_manual(values = colors) + 
  theme(text = element_text(size = 20))

ggsave(path = main_dir,"gauss_sims_data.png")

colors <- c("X Linearity Simulations" = "#666666", "X non-Gaussianity Simuations" = "#000080")
X_exp = rtexp(N, rate, endpoint = 3) + 1
data_sims_x <- data.frame(X_exp = X_exp, X_guass = X)
sims_xdata <- ggplot(data_sims_x, aes(x = value)) + 
  geom_density(aes(x=X_guass, color = "X non-Gaussianity Simuations")) +
  geom_density(aes(x=X_exp, color = "X Linearity Simulations")) +
  theme_bw() +
  labs(x = "X",
       color = "X Distributions") +
  scale_color_manual(values = colors) + 
  theme(legend.position = c(0.3, 0.88),
        text = element_text(size = 18))

sims_xdata
ggsave(path = main_dir,"sims_xdata.png")

colors <- c("Normal" = "#666666", "GMM(k=3)" = "#DAA520", "GMM(k=2)" = "#000080")
gauss_sims_errors <- ggplot(data_sims_poly, aes(x = value)) + 
  geom_density(aes(x=eta1, color = "Normal")) +
  geom_density(aes(x=eta2, color = "GMM(k=2)")) +
  geom_density(aes(x=eta3, color = "GMM(k=3)")) +
  theme_bw() +
  labs(x = "",
       color = "Error Distributions") +
  scale_color_manual(values = colors) + 
  theme(legend.position = c(0.8, 0.85),
        text = element_text(size = 18))

gauss_sims_errors

ggsave(path = main_dir,"gauss_sims_errors.png")


data_sim_g1 = data.frame(sleep = X, depr = ye1)
write.csv(data_sim_g1,file.path(directory,"datasims_ye1.csv"))

data_sim_g2 = data.frame(sleep = X, depr = ye2)
write.csv(data_sim_g2,file.path(directory,"datasims_ye2.csv"))

data_sim_g3 = data.frame(sleep = X, depr = ye3)
write.csv(data_sim_g3,file.path(directory,"datasims_ye3.csv"))



## small sample size
## linearity sims with small sample size

set.seed(361)
N = 400
rate = 1.2802
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
b = 1
y1 = (sign(X_shift)*abs(X_shift)^b*beta) + eta3
yl1 = (sign(X_shift)*abs(X_shift)^b*beta)
data_sim1 = data.frame(sleep = X, depr = y1)
colnames(data_sim1) <- c("sleep","mbdi")

ggplot(data_sim1) +
  geom_point(aes(x=X, y=y1)) +
  theme_bw() +
  labs(x = "X",
       y = "Y^b",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  theme(text = element_text(size = 20))

b = 1.25
y125 = (sign(X_shift)*abs(X_shift)^b*beta) + eta3
yl125 = (sign(X_shift)*abs(X_shift)^b*beta)
data_sim125 = data.frame(sleep = X, depr = y125)
colnames(data_sim125) <- c("sleep","mbdi")

ggplot(data_sim125) +
  geom_point(aes(x=X, y=y125)) +
  theme_bw() +
  labs(x = "X",
       y = "Y^b",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  theme(text = element_text(size = 20))

b = 3 
y3 = (sign(X_shift)*abs(X_shift)^b*beta) + eta3
yl3 = (sign(X_shift)*abs(X_shift)^b*beta)
data_sim3 = data.frame(sleep = X, depr = y3)
colnames(data_sim3) <- c("sleep","mbdi")

ggplot(data_sim3) +
  geom_point(aes(x=X, y=y3)) +
  theme_bw() +
  labs(x = "X",
       y = "Y^b",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  theme(text = element_text(size = 20))

data_sims_poly = data.frame(X, y125, y1, yl1, yl125, y3, yl3)
colors <- c("y^1.25" = "blue", "y^1.25 line" = "brown", "y^1" = "black", "y^1 line" = "red", "y^3" = "purple", "y^3 line" = "orange")

ggplot(data_sims_poly) +
  geom_point(aes(x=X, y=y1, color = "y^1")) +
  geom_line(aes(x=X, y=yl1, color = "y^1 line")) +
  geom_point(aes(x=X, y=y125, color = "y^1.25")) +
  geom_line(aes(x=X, y=yl125, color = "y^1.25 line")) +
  geom_point(aes(x=X, y=y3, color = "y^3")) +
  geom_line(aes(x=X, y=yl3, color = "y^3 line")) +
  theme_bw() +
  labs(x = "X",
       y = "Y^b",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  theme(text = element_text(size = 20))

main_dir <- "~/Documents/cddr_paper/simulations/small_samples/data/eda/"
ggsave(path = main_dir,"lin_sets_n400.png")

write.csv(data_sim1,file.path(directory,"datasims_poly_y1_n400.csv"))
write.csv(data_sim125,file.path(directory,"datasims_poly_y125_n400.csv"))
write.csv(data_sim3,file.path(directory,"datasims_poly_y3_n400.csv"))

## non-guassianity assump violated
set.seed(361)
N = 400
X = rnorm(N)

## gaussian
mu1 <- 0
sigma1 <- 0.6
eta1 <- rnorm(N, mu1, sigma1)

## 2 mixture gaussian
lambda2 <- c(0.57, 0.43)
mu21 <- -1
mu22 <- 0.25
sigma21 <- 0.5
sigma22 <- 0.6
props <- sample(c(0, 1), N, replace = T, prob = lambda2)
g1 <- rnorm(N, mu21, sigma21)
g2 <- rnorm(N, mu22, sigma22)
eta2 = ifelse(props == 0, g1, g2)

# ## 3 mixtures gaussian
lambda3 <- c(0.4, 0.2, 0.4)
mu31 <- -2
mu32 <- 0
mu33 <- 2
sigma31 <- 0.5
sigma32 <- 1
sigma33 <- 3
props <- sample(c(0, 1, 2), N, replace = T, prob = lambda3)
g1 <- rnorm(N, mu31, sigma31)
g2 <- rnorm(N, mu32, sigma32)
g3 <- rnorm(N, mu33, sigma33)
eta3 = ifelse(props == 0, g1,
              ifelse(props == 1, g2, g3))


beta = 2
b = 1/3
a = 2.5
X_shift <- X-a
b = 1
ye1 = (sign(X_shift)*abs(X_shift)^b*beta) + eta1
ye2 = (sign(X_shift)*abs(X_shift)^b*beta) + eta2
ye3 = (sign(X_shift)*abs(X_shift)^b*beta) + eta3

data_sims_poly = data.frame(X, ye1, ye2, ye3, eta1, eta2, eta3)
colors <- c("g1" = "black", "g2" = "purple", "g3" = "brown")
ggplot(data_sims_poly) +
  geom_point(aes(x=X, y=ye1, color = "g1")) +
  geom_point(aes(x=X, y=ye2, color = "g2")) +
  geom_point(aes(x=X, y=ye2, color = "g3")) +
  theme_bw() +
  labs(x = "X",
       y = "Y with different guassianity",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  theme(text = element_text(size = 20))

ggsave(path = main_dir,"gauss_sims_data_n400.png")

colors <- c("X Linearity" = "black", "X Guassianity" = "purple")
X_exp = rtexp(N, rate, endpoint = 3) + 1
data_sims_x <- data.frame(X_exp = X_exp, X_guass = X)
ggplot(data_sims_x, aes(x = value)) + 
  geom_density(aes(x=X_guass, color = "X Guassianity")) +
  geom_density(aes(x=X_exp, color = "X Linearity")) +
  theme_bw() +
  labs(x = "X",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  theme(text = element_text(size = 20))

ggsave(path = main_dir,"sims_xdata_n400.png")

colors <- c("g1" = "black", "g2" = "purple", "g3" = "brown")
ggplot(data_sims_poly, aes(x = value)) + 
  geom_density(aes(x=eta1, color = "g1")) +
  geom_density(aes(x=eta2, color = "g2")) +
  geom_density(aes(x=eta3, color = "g3")) +
  theme_bw() +
  labs(x = "Error Funtions",
       color = "Legend") +
  scale_color_manual(values = colors) + 
  theme(text = element_text(size = 20))

ggsave(path = main_dir,"gauss_sims_errors_n400.png")


data_sim_g1 = data.frame(sleep = X, depr = ye1)
main_dir <- "~/Documents/Elena_Research/W2022/code_cluster"
write.csv(data_sim_g1,file.path(main_dir,"datasims_ye1_n400.csv"))

data_sim_g2 = data.frame(sleep = X, depr = ye2)
main_dir <- "~/Documents/Elena_Research/W2022/code_cluster"
write.csv(data_sim_g2,file.path(main_dir,"datasims_ye2_n400.csv"))

data_sim_g3 = data.frame(sleep = X, depr = ye3)
main_dir <- "~/Documents/Elena_Research/W2022/code_cluster"
write.csv(data_sim_g3,file.path(main_dir,"datasims_ye3_n400.csv"))

## real data analysis

## ozone and temp 3
directory <- "~/Documents/cddr_paper/real_data/data/pairs_orig_data/"
temp_ozone3 <- read.table(paste0(directory, "pair0051.txt"), sep="\t")
colnames(temp_ozone3) <- c("ozone", "temp")

ggplot(temp_ozone3, aes(x=ozone, y=temp)) +
  geom_point() +
  theme_bw()

data_dir <- "~/Documents/cddr_paper/real_data/data"
write.csv(temp_ozone3,file.path(data_dir,"temp_ozone3.csv"))

mdl <- DirectLiNGAM$new()
mdl$fit(temp_ozone3)
print(mdl$causal_order)
print(mdl$adjacency_matrix)
plot_adjacency_mat(mdl$adjacency_matrix, node_labels = names(temp_ozone3))

## population and food consumption dataset 
directory <- "~/Documents/cddr_paper/real_data/data/pairs_orig_data/"
pop_cal <- read.table(paste0(directory, "pair0076.txt"), sep="\t")
colnames(pop_cal) <- c("pop", "NA", "food")
pop_cal = subset(pop_cal, select = c(pop,food) )

ggplot(pop_cal, aes(x=pop, y=food)) +
  geom_point() +
  theme_bw()
mdl <- DirectLiNGAM$new()
mdl$fit(pop_cal)
print(mdl$causal_order)
print(mdl$adjacency_matrix)
plot_adjacency_mat(mdl$adjacency_matrix, node_labels = names(pop_cal))

write.csv(pop_cal,file.path(data_dir,"pop_cal.csv"))

## fish dose response data

library("drc")
mykiss_data <- O.mykiss %>% drop_na()
O.mykiss.m1 <- drm(weight ~ conc, data = mykiss_data, fct = EXD.2(), na.action = na.omit)
modelFit(O.mykiss.m1)
summary(O.mykiss.m1)
### fitting known model and transforming data based on this model
plot(O.mykiss.m1, type = "all", xlim = c(0, 500), ylim = c(0,4), xlab = "Concentration (mg/l)", ylab = "Weight (g)", broken = TRUE)
mykiss_data$conc_new <- fitted(O.mykiss.m1)
plot(mykiss_data$conc_new, mykiss_data$weight)
mykiss_data = subset(mykiss_data, select = c(conc_new,weight) )
mdl <- DirectLiNGAM$new()
mdl$fit(mykiss_data)
mdl$causal_order
write.csv(mykiss_data,file.path(data_dir,"mykiss_drc.csv"))

## sleep and depression
mBDI <- read.table(paste0(data_dir, "/mBDIdataset.dat"), sep="\t")
colnames(mBDI) <- c("id","sex","age","sleep","mbdi")
subdata_mBDI <- mBDI[,c("sleep","mbdi")]

mdl2 <- DirectLiNGAM$new()
mdl2$fit(subdata_mBDI)
mdl2$causal_order

write.csv(subdata_mBDI,file.path(data_dir,"mbdi.csv"))

