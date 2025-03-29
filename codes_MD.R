library(MASS)
library(tidyverse)
library(latex2exp)

# Parameters (adjust as needed)
mu0 <- 5    # Prior mean of buyer's valuation
tau_02 <- 1 # Prior var of buyer's valuation
phi_02 <- 1 # conditional var for updated valuation
theta <- 0 # Utility Function risk Aversion
diff <- 2 # ratio between mean of vi^* and v_i, (v_i* mean = diff * v_i)
n_samples <- 1000  # Number of Monte Carlo samples
k <- 11 # number of bins
p1_vec_st = seq(0, mu0 + sqrt(tau_02), length.out = k)


U <- function(a, v){
  (a*v)^(1 - theta)/(1 - theta)
}

# Can memoize?
optimize_p2 <- function(a, vi) {
  if (a == 1){return(0)} # If already see data, can't charge
  mu <- vi
  sigma <- sqrt(phi_02)
  # Objective function to maximize a * P(f(X) >= a)
  objective_function <- function(p2) {
    t_p2 = (p2 / (1 - a))
    prob = 1 - pnorm(t_p2, mean = mu, sd = sigma)
    res = p2*prob
    res = ifelse(res == 0, -Inf, res)
    return(res) 
  }
  # range 0 to mu + 3*sd, other wise prob shrinks to 0
  result <- optimize(objective_function, interval = c(0, mu + 3*sigma), maximum = TRUE)
  return(result$maximum)
}

get_p1 <- function(p1_vec, a) {
  n <- length(p1_vec)
  index <- a * (k - 1) + 1
  lower <- floor(index)
  upper <- ceiling(index)
  if (lower == upper) {
    return(p1_vec[lower]) 
  }
  # Linear interpolation
  weight <- index - lower
  res = (1 - weight) * p1_vec[lower] + weight * p1_vec[upper]
  return(res)
}

buyers_first_choice_a_seller_fxd <- function(p1_vec, s_i0, tau1 = tau_02, tau2 = phi_02) {
  objective_function <- function(alpha, tp = F) {
    p1_val = get_p1(p1_vec, alpha)
    term1 <- ifelse(alpha * s_i0 >= p1_val, (alpha * s_i0 - p1_val), 0)
    if (alpha == 1){return(term1)}
    p2_val = optimize_p2(alpha, s_i0)
    z_c <- (p2_val - (1 - alpha)*s_i0) / ((1 - alpha)^2 * tau2) # maybe use different variance heere?
    phi_zc <- dnorm(z_c)
    Phi_zc <- pnorm(z_c) 
    term2 <- ((1 - alpha) * s_i0 - p2_val) * (1 - Phi_zc)
    term3 <- (1 - alpha) * tau1 * alpha * phi_zc
    if (tp){
      print(term1)
      print(term2 + term3)
    }
    return(term1 + term2 + term3)
  }
  result <- optimize(
    objective_function, 
    interval = c(0, 1), 
    maximum = TRUE
  )
  objective_function(result$maximum, tp = T)
  print(paste("alpha: ", result$maximum))
  return(result$maximum)
}

vi = 2
tau_02 = 2
phi_02 = 2
p1_vec_st = seq(0, vi, length.out = k)
buyers_first_choice_a_seller_fxd(p1_vec_st, vi)

v_i_vals <- seq(1, 5, by = 0.1) 
results <- data.frame(v_i = numeric(), a_act = numeric())  # Empty dataframe to store results
for (v_i in v_i_vals) {
  p1_vec_st = seq(0, v_i - 0.1, length.out = k)
  tau_02 = 1
  phi_02 = 2
  a_act <- buyers_first_choice_a_seller_fxd(p1_vec_st, v_i)
  results <- rbind(results, data.frame(v_i = v_i, a_act = a_act))
}
ggplot(results, aes(x = v_i, y = a_act)) +
  geom_line() +
  labs(title = "Line Chart of Optimal α",
       x = "s_i0", y = "α", fill = "Optimal α") +
  theme_minimal() 


v_i_vals <- seq(1, 5, by = 0.1) 
var_vit_vals <- seq(1, 5, by = 0.1)
results <- data.frame(v_i = numeric(), a_act = numeric(), var_vit_vals = numeric()) 
for (tau1 in var_vit_vals){
  for (v_i in v_i_vals) {
    p1_vec_st = seq(0, v_i - 0.1, length.out = k)
    phi_02 = 2
    a_act <- buyers_first_choice_a_seller_fxd(p1_vec_st, v_i, tau1 = tau1)
    results <- rbind(results, data.frame(v_i = v_i, a_act = a_act, var_vit_vals = tau1))
  }
}

p1 = ggplot(results, aes(x = v_i, y = var_vit_vals, fill = a_act)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = TeX("Heatmap of Optimal $\\alpha$"),
       x = TeX("$\\s_{i0} = E[v_i^*]$"), 
       y = TeX("$\\sigma^2_{v_i^*}$ = Var$(v_i^*)$"), 
       fill = TeX("Optimal $\\alpha$")) +
  theme_minimal() +
  theme(legend.position = "bottom")
p2 = results %>% filter(var_vit_vals <= 3) %>%
  ggplot(aes(x = v_i, y = a_act, group = var_vit_vals, 
                    color = var_vit_vals)) +
  geom_line() +
  labs(title = "Line Chart for Optimal α",
       x = TeX("$\\s_{i0} = E[v_i^*]$"), y = TeX("$\\alpha$"), 
       color = TeX("$\\sigma^2_{v_i^*}$ = Var$(v_i^*)")) +
  theme_minimal() +
  scale_color_viridis_c() +
  theme(legend.position = "bottom")
gridExtra::grid.arrange(p2, p1, ncol = 2)










alphas <- seq(0, 0.7, by = 0.05)
results_p2 <- data.frame(alphas = numeric(), v_i_inferred = numeric(),  p2 = numeric())  
for (a in alphas) {
  v_i_inferred = results %>% filter(var_vit_vals == 1) %>%
    mutate(dif_val = abs(a_act - a)) %>%
    slice_min(dif_val) %>% pull(v_i)
  p2 = optimize_p2(a, v_i_inferred)
  results_p2 <- rbind(results_p2, data.frame(alphas = a, 
                                             v_i_inferred = v_i_inferred, p2 = p2))
}
results_p2 %>% 
  pivot_longer(2:3, names_to = "Type", values_to = "val") %>%
  ggplot(aes(x = alphas, y = val, color = Type)) +
  geom_line() +
  scale_y_continuous(
    name = TeX("Inferred $\\s_{i0} = E[v_i^*]$"),
    sec.axis = sec_axis(~ ., name = TeX("$p_2$")) # Adjust transformation if scaling is needed
  ) +
  labs(title = "Dual Axis Line Chart",
       x = expression(alpha)) +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red")
  )

