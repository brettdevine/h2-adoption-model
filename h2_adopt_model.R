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

alpha <- function(s, model_params) {
    # Parameter alpha from paper.
    # The proportion of passenger miles attributed
    # to Hydrogen-fueled aircraft.
    p_h <- model_params[1, "p_h"]
    (s / (p_h + s^2))^2
}


# --------------------------
# Airport Related Functions
# --------------------------
adoption_curve <- function(s, model_params) {
  x <- model_params[1, "x"]
  delta <- model_params[1, "delta"]
  gamma <- model_params[1, "gamma"]
  f_h <- model_params[1, "f_h"]
  exp(-1 * x * (delta - gamma) / (f_h * alpha(s, model_params) * s))
}


# ----------------------------------
# Nash Equilbrium Related Functions
# ----------------------------------
nash_equilibria <- function(model_params) {
  f <- function(s) {s - adoption_curve(s, model_params)}
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

# -----------------------------------
# Parameter Evolution Related Functions
# -----------------------------------
parameter_paths <- function(init_params, growth_rates, periods = 50) {
  df <- init_params
  for (t in 1:(periods - 1)) {
    df[t + 1, "p_h"] <- df[t, "p_h"] * (1 + growth_rates[1, "p_h"])
    df[t + 1, "f_h"] <- df[t, "f_h"] * (1 + growth_rates[1, "f_h"])
    df[t + 1, "x"] <- df[t, "x"] * (1 + growth_rates[1, "x"])
    df[t + 1, "gamma"] <- df[t, "gamma"] * (1 + growth_rates[1, "gamma"])
    df[t + 1, "delta"] <- df[t, "delta"] * (1 + growth_rates[1, "delta"])
  }
  return(df)
}
