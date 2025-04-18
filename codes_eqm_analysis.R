library(tidyverse)
# install.packages("truncnorm")
library(truncnorm)

phi <- function(u0, a, ybar){
  beta = 1
  tau2 = 1
  sigma2 = 1
  n = 100
  num = a*n*ybar/sigma2 + u0/(tau2*(1 - a)^beta)
  denom = a*n/sigma2 + 1/(tau2*(1 - a)^(2*beta))
  return(num/denom)
}
g <- function(theta, a, e_noise){
  ynot = 10
  mnot = 2
  res = ynot - mnot/(theta + 1)*a + e_noise
  return(res)
}
u <- function(theta){
  u0 <- 5
  return(u0*theta)
}

################################################################################
##################################### pooling ##################################
################################################################################

ast_values <- seq(0, 1, length.out = 100)
theta_smpls = seq(0, 1, 0.01)

# beta prior:
a = 5
b = 1
set.seed(42)
n_smpls = 1000000
smpls = rbeta(n_smpls, a, b)
res1 = rep(0, length(ast_values))
for (i in 1:length(ast_values)){ # alpha
  print(i)
  temp_res = 0
  for (j in theta_smpls){ # types
    ub1 = phi(mean(u(smpls)), ast_values[i], g(j, ast_values[i], -0.1))
    ub2 = phi(mean(u(smpls)), ast_values[i], g(j, ast_values[i], 0.1))
    temp_f1 <- function(a){
      -phi(0, a, g(j, a, -0.1))
    }
    lb1 = -optimize(temp_f1, interval = c(0, 1))$objective
    temp_f2 <- function(a){
      -phi(0, a, g(j, a, 0.1))
    }
    lb2 = -optimize(temp_f2, interval = c(0, 1))$objective
    temp_res = temp_res + max(0, lb1 - ub1) + max(0, lb2 - ub2)
  }
  res1[i] = temp_res
}
res1
ast_values[9]
ast_values[34]
plot(ast_values, res1, type = "l", col = "blue",
     xlab = "α*", ylab = "",
     main = "Plot of Differences under Beta(5, 1) prior")
abline(v = 0.08080808, lty = "dashed")
abline(v = 0.3333333, lty = "dashed")
data.frame(ast_values = ast_values, res = res1) %>%
  ggplot(aes(x = ast_values, y = res)) +
  geom_line(color = "black", linewidth = 1) +
  annotate("rect", xmin = 0.08080808, xmax = 0.3333333, 
           ymin = -Inf, ymax = Inf, fill = "lightgreen", alpha = 0.5) +
  labs(x = "α*", y = "", title = "Plot of Differences under Beta(5, 1) prior",
       subtitle = "Pooling Equilibrium For [0.08, 0.33]") +
  theme_bw()


# beta prior:
a = 1
b = 5
set.seed(42)
n_smpls = 1000000
smpls = rbeta(n_smpls, a, b)
res2 = rep(0, length(ast_values))
for (i in 1:length(ast_values)){ # alpha
  print(i)
  temp_res = 0
  for (j in theta_smpls){ # types
    ub1 = phi(mean(u(smpls)), ast_values[i], g(j, ast_values[i], -0.1))
    ub2 = phi(mean(u(smpls)), ast_values[i], g(j, ast_values[i], 0.1))
    temp_f1 <- function(a){
      -phi(0, a, g(j, a, -0.1))
    }
    lb1 = -optimize(temp_f1, interval = c(0, 1))$objective
    temp_f2 <- function(a){
      -phi(0, a, g(j, a, 0.1))
    }
    lb2 = -optimize(temp_f2, interval = c(0, 1))$objective
    temp_res = temp_res + max(0, lb1 - ub1) + max(0, lb2 - ub2)
  }
  res2[i] = temp_res
}
res2
ast_values[18]
ast_values[26]
plot(ast_values, res2, type = "l", col = "blue",
     xlab = "α*", ylab = "",
     main = "Plot of Differences under Beta(1, 5) prior")
abline(v = 0.1717172, lty = "dashed")
abline(v = 0.2525253, lty = "dashed")
data.frame(ast_values = ast_values, res = res2) %>%
  ggplot(aes(x = ast_values, y = res)) +
  geom_line(color = "black", linewidth = 1) +
  annotate("rect", xmin = 0.1717172, xmax = 0.2525253, 
           ymin = -Inf, ymax = Inf, fill = "lightgreen", alpha = 0.5) +
  labs(x = "α*", y = "", title = "Plot of Differences under Beta(1, 5) prior",
       subtitle = "Pooling Equilibrium For [0.17, 0.25]") +
  theme_bw()



# unif prior:
# monte carlo integration
set.seed(42)
n_smpls = 1000000
smpls = runif(n_smpls)
res3 = rep(0, length(ast_values))
for (i in 1:length(ast_values)){ # alpha
  print(i)
  temp_res = 0
  for (j in theta_smpls){ # types
    ub1 = phi(mean(u(smpls)), ast_values[i], g(j, ast_values[i], -0.1))
    ub2 = phi(mean(u(smpls)), ast_values[i], g(j, ast_values[i], 0.1))
    temp_f1 <- function(a){
      -phi(0, a, g(j, a, -0.1))
    }
    lb1 = -optimize(temp_f1, interval = c(0, 1))$objective
    temp_f2 <- function(a){
      -phi(0, a, g(j, a, 0.1))
    }
    lb2 = -optimize(temp_f2, interval = c(0, 1))$objective
    temp_res = temp_res + max(0, lb1 - ub1) + max(0, lb2 - ub2)
  }
  res3[i] = temp_res
}
res3
ast_values[13]
ast_values[31]
plot(ast_values, res3, type = "l", col = "blue",
     xlab = "α*", ylab = "",
     main = "Plot of Differences under U[0, 1] prior")
abline(v = 0.1212121, lty = "dashed")
abline(v = 0.3030303, lty = "dashed")
data.frame(ast_values = ast_values, res = res3) %>%
  ggplot(aes(x = ast_values, y = res)) +
  geom_line(color = "black", linewidth = 1) +
  annotate("rect", xmin = 0.1212121, xmax = 0.3030303, 
           ymin = -Inf, ymax = Inf, fill = "lightgreen", alpha = 0.5) +
  labs(x = "α*", y = "", title = "Plot of Differences under U[0, 1] prior",
       subtitle = "Pooling Equilibrium For [0.12, 0.30]") +
  theme_bw()


df1 = data.frame(ast_values = ast_values, res = res1)
df1$Prior = "Beta(5, 1)"
df2 = data.frame(ast_values = ast_values, res = res2)
df2$Prior = "Beta(1, 5)"
df3 = data.frame(ast_values = ast_values, res = res3)
df3$Prior = "U(0, 1)"
rbind(df1, df2, df3) %>%
  ggplot(aes(x = ast_values, y = res, color = Prior)) +
  geom_line() +
  annotate("rect", xmin = 0.08080808, xmax = 0.3333333, 
           ymin = 0, ymax = Inf, fill = "green", alpha = 0.3) +
  annotate("rect", xmin = 0.1212121, xmax = 0.3030303, 
           ymin = 0, ymax = Inf, fill = "blue", alpha = 0.3) +
  annotate("rect", xmin = 0.1717172, xmax = 0.2525253, 
           ymin = 0, ymax = Inf, fill = "red", alpha = 0.3) +
  labs(x = "α*", y = "", title = "Plot of Differences under Various Priors",
       subtitle = "Pooling Equilibrium") +
  theme_bw() +
  scale_y_sqrt()

# Alt legend:
df1 = data.frame(ast_values = ast_values, res = res1)
df1$Prior = "0.83"
df2 = data.frame(ast_values = ast_values, res = res2)
df2$Prior = "0.17"
df3 = data.frame(ast_values = ast_values, res = res3)
df3$Prior = "0.5"
rbind(df1, df2, df3) %>%
  ggplot(aes(x = ast_values, y = res, color = Prior)) +
  geom_line() +
  annotate("rect", xmin = 0.08080808, xmax = 0.3333333, 
           ymin = 0, ymax = Inf, fill = "green", alpha = 0.3) +
  annotate("rect", xmin = 0.1212121, xmax = 0.3030303, 
           ymin = 0, ymax = Inf, fill = "blue", alpha = 0.3) +
  annotate("rect", xmin = 0.1717172, xmax = 0.2525253, 
           ymin = 0, ymax = Inf, fill = "red", alpha = 0.3) +
  labs(x = "α*", y = "", title = "Plot of Differences under Various Priors",
       subtitle = "Pooling Equilibrium", color = "Prior Mean",
       caption = "Higher Prior Mean shows higher presence of High Type Sellers") +
  theme_bw() +
  scale_y_sqrt() +
  scale_color_manual(values = c("0.83" = "green", "0.17" = "red", "0.5" = "blue"))



################################################################################
#################################### separating ################################
################################################################################




phi <- function(alpha, offset, m, u) {
  (100 * alpha * (10 - m * alpha + offset) + u / (1 - alpha)) /
    (100 * alpha + 1 / ((1 - alpha)^2))
}

calc_max_util <- function(func) {
  res = optimize(func, interval = c(0, 1), maximum = TRUE)$objective
  print(res)
  return(res)
}

# Compute deviations for both high and low types
calculate_deviations <- function(alpha_values, type = "high") {
  deviations <- rep(0, length(alpha_values))
  for (i in seq_along(alpha_values)) {
    alpha_star <- alpha_values[i]
    temp_res <- 0
    
    # Accumulate deviations based on type
    if (type == "high") {
      # Upper bound with offset -0.1 and +0.1
      ub1 <- phi(alpha_star, -0.1, 2, 1)
      ub2 <- phi(alpha_star, 0.1, 2, 1)
      
      # Lower bound calculations for deviation
      lb1 <- calc_max_util(function(a) phi(a, -0.1, 2, 0))
      lb2 <- calc_max_util(function(a) phi(a, 0.1, 2, 0))
      
      temp_res <- temp_res + max(0, lb1 - ub1) + max(0, lb2 - ub2)
    } else if (type == "low") {
      
      # Upper bound with offset -0.1 and +0.1
      ub1 <- phi(alpha_star, -0.1, 4, 1)
      ub2 <- phi(alpha_star, 0.1, 4, 1)
      
      # Lower bound calculations for deviation
      lb1 <- calc_max_util(function(a) phi(a, -0.1, 4, 0))
      lb2 <- calc_max_util(function(a) phi(a, 0.1, 4, 0))
      
      temp_res <- temp_res + max(0, ub1 - lb1) + max(0, ub2 - lb2)
    }
    deviations[i] <- temp_res
  }
  return(deviations)
}

alpha_values <- seq(0, 1, length.out = 100)
high_deviations <- calculate_deviations(alpha_values, type = "high")
alpha_values[14]
alpha_values[26]
data.frame(ast_values = alpha_values, res = high_deviations) %>%
  ggplot(aes(x = ast_values, y = res)) +
  geom_line(color = "black", linewidth = 1) +
  annotate("rect", xmin = 0.1313131, xmax = 0.2525253, 
           ymin = -Inf, ymax = Inf, fill = "lightgreen", alpha = 0.5) +
  labs(x = "α*", y = "", title = "Plot of Differences for High Type",
       subtitle = "High Type No Deviation For [0.13, 0.25]") +
  theme_bw()


# Calculate deviations for low type
low_deviations <- calculate_deviations(alpha_values, type = "low")
alpha_values[10]
alpha_values[20]
data.frame(ast_values = alpha_values, res = low_deviations) %>%
  ggplot(aes(x = ast_values, y = res)) +
  geom_line(color = "black", linewidth = 1) +
  annotate("rect", xmin = 0, xmax = 0.09090909, 
           ymin = -Inf, ymax = Inf, fill = "lightgreen", alpha = 0.5) +
  annotate("rect", xmin = 0.1919192, xmax = 1, 
           ymin = -Inf, ymax = Inf, fill = "lightgreen", alpha = 0.5) +
  labs(x = "α*", y = "", title = "Plot of Differences for Low Type",
       subtitle = "Low Type No Deviation For [0, 0.09] and [0.19, 1]") +
  theme_bw()



df1 = data.frame(ast_values = alpha_values, res = high_deviations)
df1$Type = "High"
df2 = data.frame(ast_values = alpha_values, res = low_deviations)
df2$Type = "Low"
rbind(df1, df2) %>%
  ggplot(aes(x = ast_values, y = res, color = Type)) +
  geom_line() +
  annotate("rect", xmin = 0, xmax = 0.09090909, 
           ymin = 0, ymax = Inf, fill = "blue", alpha = 0.3) +
  annotate("rect", xmin = 0.1919192, xmax = 1, 
           ymin = 0, ymax = Inf, fill = "blue", alpha = 0.3) +
  annotate("rect", xmin = 0.1313131, xmax = 0.2525253, 
           ymin = 0, ymax = Inf, fill = "red", alpha = 0.3) +
  labs(x = "α*", y = "", title = "Plot of Differences under Various Priors",
       subtitle = "Pooling Equilibrium") +
  theme_bw() +
  scale_y_sqrt()

