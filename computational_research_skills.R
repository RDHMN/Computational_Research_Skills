##### load data #####
data <- read.csv('/Users/raouldohmen/Documents/UM   (2023-2024)/Time Series Econometrics/TSE_R/ScanRecords.csv')
library(dplyr)
library(stats)
library(fitdistrplus)

# create subsets for different patient types
t1_data <- subset(data, PatientType == 'Type 1')
t2_data <- subset(data, PatientType == "Type 2")

# convert dates to date type
t1_data$Date <- as.Date(t1_data$Date)
t2_data$Date <- as.Date(t2_data$Date)

# retrieve interarival times for t1 and t2
#scaling 0-9
time1=(t1_data$Time)-8
time2=(t2_data$Time)-8
#differencing (0 is first iteration)
interArr1=diff(c(0,time1))
interArr2=diff(c(0,time2))
# correct negative values by +9
interArr1[which(interArr1<0)]=interArr1[which(interArr1<0)]+9
interArr2[which(interArr2<0)]=interArr2[which(interArr2<0)]+9

plot(density(interArr1))
plot(density(interArr2))

# density of duration for type 1 and type 2
plot(density(t2_data$Duration), ylim=c(0, 4.5))
lines(density(t1_data$Duration), col='red')

# exploring the data
nrow(t1_data)       # 379 type 1 patients
nrow(t2_data)       # 239 type 2 patients


######## general bootstrap functions #####
bootstrap <- function(data, num_samples, statistic_func){
  boot_stats <- numeric(num_samples)

  for(i in 1:num_samples){
    sampled_data <- sample(data, replace = TRUE)
    boot_stats[i] <- statistic_func(sampled_data)
  }
  return(boot_stats)
}

bootstrap_gamma <- function(data, num_samples){
  boot_params <- matrix(NA, nrow = num_samples, ncol = 2)
  
  for(i in 1:num_samples){
    sampled_data <- sample(data, replace = TRUE)
    fit_params <- fit_gamma(sampled_data)
    boot_params[i, ] <- fit_params
  }
  return(boot_params)
}

bootstrap_normal <- function(data, num_samples) {
  boot_params <- matrix(NA, nrow = num_samples, ncol = 2)
  
  for (i in 1:num_samples) {
    sampled_data <- sample(data, replace = TRUE)
    fit_params <- fit_normal(sampled_data)
    boot_params[i, ] <- fit_params
  }
  colnames(boot_params) <- c("mean", "sd")
  return(boot_params)
}

bootstrap_weibull <- function(data, num_samples) {
  boot_params <- matrix(NA, nrow = num_samples, ncol = 2)
  
  for (i in 1:num_samples) {
    sampled_data <- sample(data, replace = TRUE)
    fit_params <- fit_weibull(sampled_data)
    boot_params[i, ] <- fit_params
  }
  colnames(boot_params) <- c("shape", "scale")
  return(boot_params)
}
####### Type 1 duration / normal dist. #####
# extract parameters of Type 1 duration normal dist.
t1_data$Duration          # bootstrap data
num_samples <- 10000      # bootstrap samples

t1_dur_means <- bootstrap(t1_data$Duration, num_samples, mean)
t1_dur_vars <- bootstrap(t1_data$Duration, num_samples, var)

hist(t1_dur_means,type='l', breaks=30)
hist(t1_dur_vars,type='l', breaks=30)

# take mean of estimated bootstrap parameters
t1_dur_mean <- mean(t1_dur_means)
t1_dur_var <- mean(t1_dur_vars)

# calculate confidence intervals
ci_dur_mean_1 <- quantile(t1_dur_means, c(0.025, 0.975))
ci_dur_var_1 <-  quantile(t1_dur_vars, c(0.025, 0.975))


# plot densities against eachother
x_values <- seq(t1_dur_mean-0.30, t1_dur_mean+0.30, length.out = num_samples)
dur_density <- dnorm(x_values, mean = t1_dur_mean, sd = sqrt(t1_dur_var))
emp_density <- density(t1_data$Duration)
# Normalize
dur_density_norm <- dur_density / sum(dur_density * diff(x_values)[1])
emp_density_norm <- emp_density$y / sum(emp_density$y * diff(emp_density$x)[1])
# plot
plot(x_values, dur_density_norm, type = 'l', ylab = 'Density', xlab = 'Duration', lwd=2)
lines(emp_density, col = 'red', lwd=2)



####### Type 1 arrival times / exp dist.. ####
# extract parameters of T1 interarrival times
interArr1              # Bootstrap data
num_samples <- 10000      # Bootstrap samples

negative_values <- interArr1[interArr1 < 0]
print(negative_values)

# Function to fit exponential distribution and extract parameter
fit_exponential <- function(data) {
  rate <- 1 / mean(data)  # Using the sample mean as the rate parameter for exponential distribution
  return(rate)
}

intArrival_boot1 <- bootstrap_dist(interArr1, num_samples, fitting_func = fit_exponential)

# Plot histogram of the empirical data
plot(density(interArr1,
             main = "Histogram with Bootstrapped Exponential Distribution"), ylim=c(0, 2), lwd=2)
# Overlay the PDF of bootstrapped exponential distribution
curve(dexp(x, rate = mean(intArrival_boot1)), col = "red", lwd = 2, add = TRUE)

# calculate conf. int
ci_intArrival_1 <- quantile(intArrival_boot1, c(0.025, 0.975))


####### Type 2 patient duration #######
num_samples <- 10000
plot(t2_data$Duration,type='l') # Duration

# Function to fit gamma distribution and extract parameters
fit_gamma <- function(data, indices) {
  sample_data <- data[indices]
  params <- fitdistr(sample_data, "gamma")$estimate
  return(params)
}

norm_params_1 <- bootstrap_normal(t2_data$Duration, num_samples)
mean_boot_1 <- mean(norm_params_1[,1])
var_boot_1 <- mean(norm_params_1[,2])


gamma_params <- bootstrap_gamma(t2_data$Duration, num_samples)
shape_boot <- mean(gamma_params[, 1])
rate_boot <- mean(gamma_params[, 2])


plot(density(t2_data$Duration, probability = TRUE, col = "lightblue", main = "Gamma Distribution Fit"), ylim=c(0, 2.5), lwd=2)
# plot normal 
curve(dnorm(x, mean = mean_boot_1, sd = var_boot_1), col = "red", lwd = 2, add = TRUE)
# plot gamma
curve(dgamma(x, shape = shape_boot, rate = rate_boot), add = TRUE, col = "blue", lwd = 2)
# Add legend
legend("topright", legend = c("Data", "Fitted Normal","Fitted Gamma"), col = c("black", "red", "blue"), lwd = 2)


# bootstrap mean and variance
t2_dur_means <- bootstrap(t2_data$Duration, num_samples, mean)
t2_dur_vars <- bootstrap(t2_data$Duration, num_samples, var)

hist(t2_dur_means,type='l', breaks=100)
hist(t2_dur_vars,type='l', breaks=100)

t2_dur_mean <- mean(t2_dur_means)
t2_dur_var <- mean(t2_dur_vars)
# generate conf. ints
ci_dur_mean_2 <- quantile(t2_dur_means, c(0.025, 0.975))
ci_dur_var_2 <-  quantile(t2_dur_vars, c(0.025, 0.975))

####### Type 2 arrival times #######
num_samples <- 10000
interArr2   # data 

# Fit several common distributions
fit_norm <- fitdist(interArr2, "norm")
fit_gamma <- fitdist(interArr2, "gamma")
fit_exp <- fitdist(interArr2, "exp")
fit_weibull <- fitdist(interArr2, "weibull")

summary(fit_norm)
summary(fit_gamma)
summary(fit_exp)
summary(fit_weibull)

# Function to fit normal distribution and extract parameters
fit_normal <- function(data) {
  fit_params <- fitdist(data, "norm")$estimate
  return(fit_params)
}

# Function to fit weibull distribution and extract parameters
fit_weibull <- function(data, indices) {
  sample_data <- data[indices]
  params <- fitdistr(sample_data, "weibull")$estimate
  return(params)
}

norm_params <- bootstrap_normal(interArr2, num_samples)
mean_boot_n <- mean(norm_params[,1])
var_boot_n <- mean(norm_params [,2])

confInt_mean_n <- quantile(norm_params[1], c(0.025, 0.975))
confInt_var_n <- quantile(norm_params[2], c(0.025, 0.975))

weibull_params <- bootstrap_weibull(interArr2, num_samples)
shape_boot_w <- mean(weibull_params[,1])
scale_boot_w <- mean(weibull_params [,2])

confInt_shape_w <- quantile(weibull_params[1], c(0.025, 0.975))
confInt_scale_w <- quantile(weibull_params[2], c(0.025, 0.975))

# Plot histogram of the empirical data
plot(density(interArr2), lwd=2)
# Overlay the PDF of fitted normal distribution
curve(dnorm(x, mean = mean_boot_n, sd = var_boot_n), col = "red", lwd = 2, add = TRUE)
# Overlay the PDF of fitted normal distribution
curve(dweibull(x, shape = shape_boot_w, scale = scale_boot_w), col = "blue", lwd = 2, add = TRUE)
legend("topright", legend = c("Data", "Fitted Normal", "Fitted Weibull"), col = c("black", "red", "blue"), lwd = 2)
