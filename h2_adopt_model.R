# -------------------------------------------------------------------
# FILENAME: model.R
# AUTHOR:   Brett Devine
# EMAIL:    brett.r.devine@gmail.com
# PROJECT: Convergent Aeronautical Solutions discovery team
# -------------------------------------------------------------------
# This script establishes the functions and objects utilized in the
# economics model of H2 technology adoption in the aviation industry.

# Required libraries
library(tidyverse)
library(rootSolve)

# -------------------------
# Airline Related Functions
# -------------------------

h2_demand <- function(y, s, model_params) {
  # Returns the airline's demand for liquid hydrogen based flight
  # The price of hydrogen, p_h, is relative to kerosene p_h* = p_h / p_k
  # model_params must be a single row vector.
  p_h = model_params[1, "p_h"]
  rho = model_params[1, "rho"]
  inner = ((1/p_h)^(rho/rho-1)) * (s^(rho/(rho-1))) + s
  demand = y * (inner)^(-1/rho)
  return(demand)
}

k_demand <- function(y, s, model_params) {
  # Returns the airline's demand for kerosene / jetfuel based flight.
  # The price of hydrogen, p_h, is relative to kerosene p_h* = p_h / p_k
  # model_params must be a single row vector.
  p_h = model_params[1, "p_h"]
  rho = model_params[1, "rho"]
  inner = (p_h^(rho/(rho-1))) * (s^(-1/(rho-1))) + 1
  demand = y * (inner)^(-1/rho)
  return(demand)
}

alpha <- function(s, model_params) {
  # Returns variable alpha from the paper.
  # The proportion of flight units demanded
  # from hydrogen sources. The output y cancels
  # out and is therefore nullified by y = 1.
  # model_params must be a single row vector
  hyd_D = h2_demand(1, s, model_params)
  ker_D = k_demand(1, s, model_params)
  alpha = hyd_D / (hyd_D + ker_D)
  return(alpha)
}

alpha_vec <- function(s, model_params) {
  n <- nrow(model_params)
  alpha_path <- c()
  for (i in 1:n) {
    alpha_path[i] <- alpha(s[i, 1], model_params[i, ])
  }
  return(alpha_path)
}


# --------------------------
# Airport Related Functions
# --------------------------

adoption_cutoff <- function(s, model_params) {
  x <- model_params[1, "x"]
  f_h <- model_params[1, "f_h"]
  delta <- model_params[1, "delta"]
  gamma <- model_params[1, "gamma"]

  (x * (delta - gamma)) / (f_h * alpha(s, model_params) * s)
}

adoption_cutoff_vec <- function(s, model_params) {
  n <- nrow(s)
  cutoff_path <- c()
  for (i in 1:n) {
    cutoff_path[i] <- adoption_cutoff(s[i, 1], model_params[i, ])
  }
  return(cutoff_path)
}

adoption_curve <- function(s, model_params, cdf = pexp) {
  #x <- model_params[1, "x"]
  #delta <- model_params[1, "delta"]
  #gamma <- model_params[1, "gamma"]
  #f_h <- model_params[1, "f_h"]

  #Pr(Qi > cutoff) = 1 - Pr(Qi <= cutoff) = 1 - G(cutoff)
  q_star <- adoption_cutoff(s, model_params)
  if (is.numeric(q_star)) {
    ac <- 1 - cdf(q_star)
  } else {
    ac <- 1 - cdf(q_star[, 1])
    #exp(-1 * x * (delta - gamma) / (f_h * alpha(s, model_params) * s))
  }
  return(ac)
}


# ----------------------------------
# Nash Equilbrium Related Functions
# ----------------------------------
nash_equilibria <- function(model_params) {
  f <- function(s) {
      s - adoption_curve(s, model_params)
  }
  uniroot.all(f, c(0, 1))
}

nash_equilibria_vec <- function(model_params) {
    num <- nrow(model_params)
    ne_df <- data.frame("low" = 0, "tipp" = 0, "high" = 0)
    if (num > 1) {
      for (i in 1:num) {
        f <- function(s) {
            s - adoption_curve(s, model_params[i, ])
          }
        ne_df[i, ] <- uniroot.all(f, c(0, 1))
      }
    } else {
    f <- function(s) { s - adoption_curve(s, model_params) }
    ne_df[1, ] <- uniroot.all(f, c(0, 1))
    }
    return(ne_df)
}

tipping_point <- function(model_params) {
    ne <- nash_equilibria(model_params)
    if (nrow(ne) > 1) {
      return(ne[1, 2])
    } else {
      return(NULL)
    }
}

# --------------------------------------
# Parameter Evolution Related Functions
# --------------------------------------
parameter_paths <- function(init_params, growth_rates, periods = 50) {
  df <- init_params
  for (t in 1:(periods - 1)) {
    df[t + 1, "p_h"] <- df[t, "p_h"] * (1 + growth_rates[1, "p_h"])
    df[t + 1, "f_h"] <- df[t, "f_h"] * (1 + growth_rates[1, "f_h"])
    df[t + 1, "x"] <- df[t, "x"] * (1 + growth_rates[1, "x"])
    df[t + 1, "gamma"] <- df[t, "gamma"] * (1 + growth_rates[1, "gamma"])
    df[t + 1, "delta"] <- df[t, "delta"] * (1 + growth_rates[1, "delta"])
    df[t + 1, "rho"] <- df[t, "rho"] * (1 + growth_rates[1, "rho"])
  }
  return(df)
}

# ------------------------------------------
# Adoption Dynamics
# ------------------------------------------
adoption_dynamics <- function(model_params, s_inc, s0 = 0) {
  n <- nrow(model_params)
  s_path <- c(s0)
  s_inc_path <- c(0)
  for (i in 2:n) {
    next_s <- adoption_curve(s_path[i - 1], model_params[i, ])
    if (next_s > s_path[i - 1]) {
      s_path[i] <- next_s
      s_inc_path[i] <- 0
    } else {
      s_path[i] <- s_path[i - 1] + s_inc
      s_inc_path[i] <- 1
    }
  }
  return(data.frame("s" = s_path, "s_inc" = s_inc_path))
}