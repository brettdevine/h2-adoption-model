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
    p_h <- model_params[["p_h"]]
    (s / (p_h + s^2))^2
}


# --------------------------
# Airport Related Functions
# --------------------------
adoption_curve <- function(s, model_params) {
  x <- model_params[["x"]]
  delta <- model_params[["delta"]]
  gamma <- model_params[["gamma"]]
  f_h <- model_params[["f_h"]]
  exp(-1 * x * (delta - gamma) / (f_h * alpha(s, model_params) * s))
}




# ----------------------------------
# Nash Equilbrium Related Functions
# ----------------------------------
nash_equilibria <- function(s, model_params) {
    f <- function(s){ s - adoption_curve(s, model_params) }
    uniroot.all(f, c(0, 1))
}