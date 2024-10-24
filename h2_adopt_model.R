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
  #' @description Returns the airline's demand for liquid hydrogen based flight
  #' The price of hydrogen, p_h, is relative to kerosene p_h* = p_h / p_k
  #' 
  #' @param y a number for the airline production output (market demand)
  #' @param s a number [0,1] for the proportion of airports adopting LH2
  #' @param model_params a single-row dataframe of calibrated model 
  #' parameters.
  #' 
  #' @returns a number for the quantity of hydrogen demanded.
  
  p_h = model_params[1, "p_h"]
  rho = model_params[1, "rho"]
  inner = ((1/p_h)^(rho/rho-1)) * (s^(rho/(rho-1))) + s
  demand = y * (inner)^(-1/rho)
  return(demand)
}

k_demand <- function(y, s, model_params) {
  #' @description Calculates the airline's demand for kerosene based flight.
  #' The price of hydrogen, p_h, is relative to kerosene p_h* = p_h / p_k
  #' 
  #' @param y a number for the airline's production output (market demand)
  #' @param s a number [0,1] for the proportion of airports adopting LH2
  #' @param model_params a single-row dataframe of calibrated model 
  #' parameters.
  #' 
  #' @returns a number for the quantity of kerosene/jetfuel demanded.

  p_h = model_params[1, "p_h"]
  rho = model_params[1, "rho"]
  inner = (p_h^(rho/(rho-1))) * (s^(-1/(rho-1))) + 1
  demand = y * (inner)^(-1/rho)
  return(demand)
}

alpha <- function(s, model_params) {
  #' Calculates the proportion of flight-energy demanded
  #' in the form of hydrogen by the airline industry.
  #' 
  #' @description The airline industry's demand for jetfuel
  #' and hydyrogen is represented by the single production 
  #' function and the corresponding conditional input demand
  #' functions. This function first calculates the industry's
  #' quantity demanded for hydrogen and kerosene fuels and
  #' determines what percent of total flight-energy is demanded
  #' in the form of hydrogen.
  #' 
  #' @param s a number [0,1] for the proportion of airports adopting LH2
  #' @param model_params a single-row dataframe of calibrated 
  #' model parameters.
  #' 
  #' @returns a number for hydrogen's proportion of 
  #' flight-energy / -miles, etc.
  
  hyd_D = h2_demand(1, s, model_params)
  ker_D = k_demand(1, s, model_params)
  alpha = hyd_D / (hyd_D + ker_D)
  return(alpha)
}

alpha_vec <- function(s, model_params) {
  #' Calculates the hydrogen's proportion of flight-energy
  #' /miles for several sets of parameter values.
  #' 
  #' @description Calculates hydrogen's proportion of
  #' airline industry input energy demand for each set
  #' of model parameters in the dataframe.
  #' 
  #' @param s a number [0,1] for the proportion of airports
  #' adopting LH2
  #' @param model_params a dataframe of calibrated model 
  #' parameters. One column for each parameter and each
  #' row represents a specific parameter combination.
  #' 
  #' @returns a numeric vector of proportions.
  
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

adoption_benefit <- function(q, s, model_params) {
  #' Calculates the benefits (revenues) from airport
  #' LH2 services.
  #' 
  #' @description This function returns the present
  #' value of future benefits accruing to an airport
  #' that adopts and invests in LH2 infrastructure.
  #' 
  #' @param q a number representing the passenger-mile
  #' capacity of the airport.
  #' @param s a number in (0,1) representing the proportion
  #' of airports in the network adopting hydrogen.
  #' @param model_params a single-row dataframe of calibrated parameters
  #' for the model. The required parameters for this model:
  #'    * f_k_rev : kerosene flight operating revenue $(million) / passenger-mile
  #'    * f_k_exp : kerosene flight operating expense $(million) / passenger-mile
  #'    * f_h_rev : hydrogen flight relative (to kerosene) operating revenue
  #'    * f_h_exp : hydrogen flight relative (to kerosene) operating expense
  #'    * delta : discount rate of future revenues and costs
  #'    * gamma : growth rate of future periodic revenues
  #' 
  #' @returns a number representing the annual
  #' 
  f_k_rev <- model_params[1, "f_k_rev"]
  f_k_exp <- model_params[1, "f_k_exp"]
  f_h_rev <- model_params[1, "f_h_rev"]
  f_h_exp <- model_params[1, "f_h_exp"]
  delta <- model_params[1, "delta"]
  gamma <- model_params[1, "gamma"]
  
  benefit <- ((f_h_rev * f_k_rev - f_h_exp * f_k_exp) / (delta - gamma)) * alpha(s, model_params) * s * q
  return(benefit)
}

adoption_cost <- function(q, s, model_params) {
  #' Calculates the adoption cost (investment) from 
  #' installing infrastructure for LH2 services.
  #' 
  #' @description This function returns the present
  #' value up-front investment cost for an airport
  #' required to adopt LH2 infrastructure to service.
  #' 
  #' @param q a number representing the passenger-mile
  #' capacity of the airport.
  #' @param s a number in (0,1) representing the proportion
  #' of airports in the network adopting hydrogen.
  #' @param model_params a single-frame dataframe of calibrated parameters
  #' for the model. The required parameters for this model:
  #' x : The fixed investment cost ($millions) paid for minimal LH2 infrastructure
  #' c_x : The cost ($millions) per metric kiloton of LH2 demanded
  #' eta : A number representing passenger-miles per metric kiloton of LH2
  #' nu : A number (0, 1) representing maximum anticipated proportion
  #'      of passenger-mile capacity for hydrogen flight services desired
  #'      by the airport.
  #'      
  #' @returns a number representing the cost of investment in $millions
  x <- model_params[1, "x"]
  c_x <- model_params[1, "c_x"]
  eta <- model_params[1, "eta"]
  nu <- model_params[1, "nu"]
  cost <- x + (c_x * eta * nu * q)
}

adoption_cutoff <- function(s, model_params) {
  #' Calculates the cutoff airport capacity Q where
  #' all airports with q > Q adopt.
  #' 
  #' @description The benefits and costs of adopting LH2 infrastructure
  #' depend partially on the proportion of adopters, s, but also
  #' the size of the airport, q. For any given set of model parameters,
  #' we can find a Q such that all q >= Q will adopt, the all q < Q will
  #' not adopt. 
  #' 
  #' @param s a number [0,1] representing the believed proportion of
  #' LH2 infrastructure adoptors.
  #' @param model_params a single-row dataframe of calibrated parameters for the
  #' model.
  #' 
  #' @returns a number Q representing the cutoff airport size.
  x <- model_params[1, "x"]
  c_x <- model_params[1, "c_x"]
  eta <- model_params[1, "eta"]
  nu <- model_params[1, "nu"]
  f_k_rev <- model_params[1, "f_k_rev"]
  f_k_exp <- model_params[1, "f_k_exp"]
  f_h_rev <- model_params[1, "f_h_rev"]
  f_h_exp <- model_params[1, "f_h_exp"]
  delta <- model_params[1, "delta"]
  gamma <- model_params[1, "gamma"]

  numer <- x * (delta - gamma)
  denom <- (f_h_rev * f_k_rev - f_h_exp * f_k_exp) * alpha(s, model_params) * s - (c_x * eta * nu * (delta - gamma))
  cutoff <- numer / denom
  cutoff[cutoff < 0] = Inf
  return(cutoff)
}

adoption_cutoff_vec <- function(s, model_params) {
  #' Computes the adoption cutoff airport size for a set
  #' of different parameter values.
  #' 
  #' @description The function calculates the cutoff airport 
  #' size Q at which airports choose to adopt LH2
  #' infrastructure for each row of a dataframe of parameter values.
  #' 
  #' @param s a number in [0,1] representing the proportion of airports
  #' adopting LH2 infrastructure. 
  #' @param model_params a single-row dataframe of calibrated parameter values
  #' with each row representing a different combination or scenario of
  #' parameters.
  #' 
  #' @returns a numeric vector of cut-off values with length equal to 
  #' the number rows (parameter combinations) in model_params dataframe.
  
  n <- nrow(s)
  cutoff_path <- c()
  for (i in 1:n) {
    cutoff_path[i] <- adoption_cutoff(s[i, 1], model_params[i, ])
  }
  return(cutoff_path)
}

adoption_curve <- function(s, model_params, cdf = pexp) {
  #' Calculates the adoption proportion s = 1 - CDF(Q(s)) given
  #' cut-off value Q.
  #' 
  #' @description The function takes a current proportion of airport
  #' adoptors, s, along with model parameters, and the distribution
  #' function (CDF) for airport sizes and computes the new proportion
  #' of adoptors.
  #' 
  #' @param s a number [0, 1] representing the proportion of airports 
  #' who have adopted LH2 infrastructure.
  #' @param model_params a single-row dataframe of calibrated parameter
  #' values for the model.
  #' @param cdf a function (cumulative distribution function) for the 
  #' distribution of airport size in passenger-miles
  #' 
  #' @returns a number s in (0,1) as the proportion of airports adopting
  #' given the model parameters and CDF.
  
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
# Nash Equilibrium Related Functions
# ----------------------------------
nash_equilibria <- function(model_params, cdf) {
  #' Calculates the Nash equilibria (fixed points) of the system
  #' s = 1 - CDF(Q(s)). 
  #' 
  #' @description This function calculates the fixed points of the 
  #' equation s = 1 - CDF(Q(s)) using root solving techniques. The
  #' fixed points represent stationary or stable points in a
  #' recursive process that represent Nash equilibrium in the model.
  #' 
  #' @param model_params a dataframe of model parameters
  #' @param cdf a function (cumulative distribution function)
  #' 
  #' @returns a numeric vector of fixed points
  
  f <- function(s) {
      s - adoption_curve(s, model_params, cdf)
  }
  uniroot.all(f, c(0, 1))
}

nash_equilibria_vec <- function(model_params, cdf) {
  #' Calculates the Nash equilibria (fixed points) of the system
  #' s = 1 - CDF(Q(s)) for each set of parameters.
  #' 
  #' @description Calculates the Nash equilibrium low, high, and
  #' tipping point equilibrium of the system for a dataframe of 
  #' parameter values. Each row of the dataframe <model_params> 
  #' represents a model parameterization and this function will
  #' find and store all the associated equilibrium and they will
  #' be returned in a dataframe with a column for each type of
  #' equilibrium.
  #' 
  #' @param model_params a dataframe of calibrated model parameter
  #' values. One column for each parameter and a row for each 
  #' valid set of parameters.
  #' @param cdf a function (cumulative distribution function) for
  #' the distribution of airport sizes.
  #' 
  #' @returns a dataframe of equilibrium. The dataframe has
  #' 3 columns ("low", "tipp", "high") and a row for each set
  #' of parameters (rows) in <model_params>.
  
    num <- nrow(model_params)
    ne_df <- data.frame("low" = 0, "tipp" = 0, "high" = 0)
    if (num > 1) {
      for (i in 1:num) {
        f <- function(s) {
            s - adoption_curve(s, model_params[i, ], cdf = cdf)
          }
        ne_df[i, ] <- uniroot.all(f, c(0, 1))
      }
    } else {
    f <- function(s) { s - adoption_curve(s, model_params, cdf = cdf) }
    ne_df[1, ] <- uniroot.all(f, c(0, 1))
    }
    return(ne_df)
}

tipping_point <- function(model_params, cdf) {
  #' Extracts the tipping point equilibrium (if it exists) from
  #' the three potential equilibrium that exists for every set
  #' of model parameters and cdf function.
  #' 
  #' @description The purpose of this function is to isolate
  #' and extract just the tipping point equilibrium associated
  #' with a set of model parameters and cdf function.
  #' 
  #' @param model_params a dataframe of calibrated model
  #' parameters.
  #' @param cdf a function (cumulative distribution function)
  #' for the distribution of airport sizes.
  
    ne <- nash_equilibria(model_params, cdf)
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
  #' Generates the growth paths over time for each model parameter
  #' 
  #' @description The purpose of this function is to take initial
  #' model parameters and their growth rates and "grow" them forward
  #' for a specified number of periods to result in a dynamic set
  #' of model parameters useful for exploring model dynamics.
  #' 
  #' @param init_params a single-row dataframe of calibrated model
  #' parameters to represent the initial starting point of the
  #' system.
  #' @param growth_rates a single-row dataframe of growth rates, one for
  #' each parameter in <init_params>.
  #' @param periods an integer for the number of periods to "grow"
  #' the system. Default is fifty.
  #' 
  #' @returns a dataframe with a column for each parameter and a 
  #' row for each period.
  
  df <- init_params
  for (t in 1:(periods - 1)) {
    df[t + 1, "p_h"] <- df[t, "p_h"] * (1 + growth_rates[1, "p_h"])
    df[t + 1, "f_h_rev"] <- df[t, "f_h_rev"] * (1 + growth_rates[1, "f_h_rev"])
    df[t + 1, "f_h_exp"] <- df[t, "f_h_exp"] * (1 + growth_rates[1, "f_h_exp"])
    df[t + 1, "f_k_rev"] <- df[t, "f_k_rev"] * (1 + growth_rates[1, "f_k_rev"])
    df[t + 1, "f_k_exp"] <- df[t, "f_k_exp"] * (1 + growth_rates[1, "f_k_exp"])
    df[t + 1, "x"] <- df[t, "x"] * (1 + growth_rates[1, "x"])
    df[t + 1, "c_x"] <- df[t, "c_x"] * (1 + growth_rates[1, "c_x"])
    df[t + 1, "eta"] <- df[t, "eta"] * (1 + growth_rates[1, "eta"])
    df[t + 1, "gamma"] <- df[t, "gamma"] * (1 + growth_rates[1, "gamma"])
    df[t + 1, "delta"] <- df[t, "delta"] * (1 + growth_rates[1, "delta"])
    df[t + 1, "rho"] <- df[t, "rho"] * (1 + growth_rates[1, "rho"])
  }
  return(df)
}

# ------------------------------------------
# Adoption Dynamics
# ------------------------------------------
adoption_dynamics <- function(model_params, cdf, s_inc, s0 = 0) {
  #' Calculates model dynamics such as adoption and equilibrium
  #' over time.
  #' 
  #' @description The purpose of this function is to facilitate
  #' dynamic analysis of the network effects and equilibrium. 
  #' After the parameters have been grown over time this function
  #' will calculate the adoption curve over the periods represented 
  #' in <model_params>.
  #' 
  #' @param model_params a dataframe of model parameters over periods
  #' @param cdf a function for the cumulative distribution function (CDF)
  #' of the distribution of airport sizes.
  #' @param s_inc a number for how much the government increments the 
  #' proportion of airports adopting hydrogen if market forces don't
  #' incentivise them to do so.
  #' @param s0 a number representing the initial proportion of 
  #' airports adopting LH2 infrastructure. Default is 0.
  #' 
  #' @returns a dataframe with two columns. "s" is the proportion
  #' of airports adopting over the periods. "s_inc" is a binary
  #' column with a value of 1 if the government forced an increment
  #' of s and 0 otherwise.
  
  n <- nrow(model_params)
  s_path <- c(s0)
  s_inc_path <- c(0)
  for (i in 2:n) {
    next_s <- adoption_curve(s_path[i - 1], model_params[i, ], cdf)
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