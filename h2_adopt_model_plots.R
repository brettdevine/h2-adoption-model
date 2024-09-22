# -------------------------------------------------------------------
# FILENAME: model_plots.R
# AUTHOR:   Brett Devine
# EMAIL:    brett.r.devine@gmail.com
# PROJECT: Convergent Aeronautical Solutions discovery team
# -------------------------------------------------------------------
# This script contains the code to create various plots of the model.

library(tidyverse)
library(ggthemes)
library(ggtext)
library(latex2exp)
source("h2_adopt_model.R")

ne_plot_simple_data <- function(model_params) {
    #' Generates adoption curve along with Nash equilibrium (fixed) points
    #' for plotting.
    #'
    #' @description This function takes in the model parameters and then
    #' calculates the dataframe and equilibrium data points needed for
    #' a simple plot.
    #'
    #' @param model_params named vector. A named vector of parameter values.
    #' Specifically, we have, as an example:
    #' data.frame("p_h" = 1.2
    #'      , "f_h" = 3.0
    #'      , "delta" = 0.04
    #'      , "gamma" = 0.02
    #'      , "x" = 10.0)
    #'
    #' @return list of dataframes.
    #'  1.) Dataframe containing adoption rates
    #'      and the adoption curve values.
    #'  2.) Datafram containing the Nash equilibria (fixed points)
    #'      of the model.
    sample_df <- data.frame("s" = seq(0, 1, 0.01))
    sample_df["ac"] <- adoption_curve(sample_df["s"], model_params)
    ne_data <- data.frame("ne" = nash_equilibria(model_params))
    return(list(sample_df, ne_data))
}

ne_plot_simple <- function(curve_data, ne_data, plot_name = "ne-plot-simple.pdf") {
    df_cu <- data.frame("s" = curve_data[, 1]
        , "ks" = curve_data[, 2])
    df_ne <- data.frame("ne" = ne_data[, 1])
    # Begin the plot
    ggplot(data = df_cu) +
        geom_line(aes(x = s, y = s)
                , linetype = 2
                , alpha = 0.7) +
        geom_line(aes(x = s, y = ks)
                , linetype = 1
                , linewidth = 1.5
                , color = "aquamarine3") +
        geom_point(data = ne_data
                , aes(x = ne, y = ne)
                , size = 3
                , fill = "coral2"
                , shape = 21) +
        geom_label(data = ne_data
                , aes(x = ne, y = ne + 0.06, label = round(ne, 2))) +
        labs(title = "Nash Equilibrium of Adoption Model (Fixed Points)"
                , subtitle = "Fixed points where curve intersects 45 degree line."
                , x = "S - Portion adopted before reaction"
                , y = "S - Portion adopted after reaction") +
        theme_clean()
    ggsave(plot_name
        , device = "pdf"
        , width = 8
        , height = 5
        , units = "in"
        , dpi = "retina"
        , path = "./plot-img/")
}

iteration_path <- function(s0, model_params, n = 20) {
    s_vals <- c(s0)
    path <- NULL
    for (i in 1:n) {
        curr_s <- s_vals[i]
        next_s <- adoption_curve(curr_s, model_params)
        s_vals[i + 1] <- next_s
        temp_df <- data.frame("Iter" = i, "x" = curr_s, "y" = next_s)
        path <- rbind(path, temp_df)
        if (abs(next_s - curr_s) < 0.0005) {
            break
        }
    }
    return(path)
}

ne_plot_recursive_data <- function(model_params) {
    sample_df <- data.frame("s" = seq(0, 1, 0.01))
    ne_data <- data.frame("ne" = nash_equilibria(model_params))
    sample_df["ac"] <- adoption_curve(sample_df["s"], model_params)
    # Sort fixed points and select tipping point.
    if (nrow(ne_data) > 1) {
        ne_data_sorted <- arrange(ne_data)
        tipp <- ne_data_sorted[2, 1]
        # Setup iteration paths
        below_tipp_path <- iteration_path(tipp - 0.01, model_params)
        above_tipp_path <- iteration_path(tipp + 0.01, model_params)
        above_high_path <- iteration_path(1 - 0.01, model_params)
        return(list(sample_df
                , below_tipp_path
                , above_tipp_path
                , above_high_path
                , ne_data))
    } else {
        sample_df["ac"]
        above_zero_path <- iteration_path(0.98, model_params)
        return(list(sample_df
                , above_zero_path
                , ne_data))
    }
}

ne_plot_recursive <- function(plot_data, model_params, plot_name = "ne-plot-recursive.pdf") {
    if (length(plot_data) > 3) {
        curve_df <- plot_data[[1]]
        below_tipp_path <- plot_data[[2]]
        above_tipp_path <- plot_data[[3]]
        above_high_path <- plot_data[[4]]
        ne_df <- plot_data[[5]]

        ggplot(above_tipp_path, aes(x = x, y = y)) +
            # 45 degree line
            geom_line(data = curve_df, aes(x = s, y = s)
                , linewidth = 0.6
                , alpha = 0.7) +
            # Adoption curve
            geom_line(data = curve_df, aes(x = s, y = ac)
                , linewidth = 2.5
                , color = "aquamarine3") +
            # Dynamics below tipping point
            geom_point(data = below_tipp_path
                , fill = "magenta2"
                , size = 2
                , shape = 21) +
            geom_segment(data = below_tipp_path
                , aes(xend = after_stat(lead(x)), yend = after_stat(lead(y)))
                , arrow = arrow(length = unit(3, "mm"))
                , color = "magenta2") +
            # Dynamics above tipping point
            geom_point(data = above_tipp_path
                , fill = "coral2"
                , size = 2
                , shape = 21) +
            geom_segment(data = above_tipp_path
                , aes(xend = after_stat(lead(x)), yend = after_stat(lead(y)))
                , arrow = arrow(length = unit(3, "mm"))
                , color = "coral2") +
            # Dynamics above high equilibrium
            geom_point(data = above_high_path
                , fill = "purple3"
                , size = 2
                , shape = 21) +
            geom_segment(data = above_high_path
                , aes(xend = after_stat(lead(x)), yend = after_stat(lead(y)))
                , arrow = arrow(length = unit(3, "mm"))
                , color = "purple3") +
             # Equilibrium point markers, labels, etc.
            geom_point(data = ne_df, aes(x = ne, y = ne)
                , size = 3.5
                , shape = 19) +
            geom_label(data = ne_df
                , aes(x = ne, y = ne + 0.08, label = round(ne, 2))) +
            geom_text(label = "Tipping \nPoint"
                , x = ne_df[2, 1] - 0.01
                , y = ne_df[2, 1] + 0.22) +
            labs(
                title = "Recursive airport hydrogen adoption path"
              , subtitle = TeX(paste0(r"(Strategic forces, Nash equilibria and tipping points)", ''))
              , x = TeX(r"($S$: Proportion of airports adopting $H_2$)")
              , y = TeX(r"($1-G(S)$: Proportion of airports adopting $H_2$)")
            ) +
            theme_clean(base_size = 16)
            ggsave(plot_name
                , device = "pdf"
                , width = 8
                , height = 5
                , units = "in"
                , dpi = "retina"
                , path = "./plot-img/")
    } else {
        curve_df <- plot_data[[1]]
        above_zero_path <- plot_data[[2]]
        ne_df <- plot_data[[3]]

        ggplot(above_zero_path, aes(x = x, y = y)) +
            # 45 degree line
            geom_line(data = curve_df, aes(x = s, y = s)
                , linewidth = 0.6
                , alpha = 0.7) +
            # Adoption curve
            geom_line(data = curve_df, aes(x = s, y = ac)
                , linewidth = 2.5
                , color = "aquamarine3") +
            # Dynamics above high equilibrium
            geom_point(data = above_zero_path
                , fill = "magenta2"
                , size = 2
                , shape = 21) +
            geom_segment(data = above_zero_path
                , aes(xend = after_stat(lead(x)), yend = after_stat(lead(y)))
                , arrow = arrow(length = unit(3, "mm"))
                , color = "magenta2") +
             # Equilibrium point markers, labels, etc.
            geom_point(data = ne_df, aes(x = ne, y = ne)
                , size = 3.5
                , shape = 19) +
            geom_label(data = ne_df
                , aes(x = ne, y = ne + 0.08, label = round(ne, 2))) +
            labs(
                title = "Recursive airport hydrogen adoption path"
              , subtitle = TeX(paste0(r"(Strategic forces, Nash equilibria and tipping points)", ''))
              , x = TeX(r"($S$: Proportion of airports adopting $H_2$)")
              , y = TeX(r"($1-G(S)$: Proportion of airports adopting $H_2$)")
            ) +
            theme_clean(base_size = 16)
            ggsave(plot_name
                , device = "pdf"
                , width = 8
                , height = 5
                , units = "in"
                , dpi = "retina"
                , path = "./plot-img/")
    }
}


dynamic_adoption_plot_data <- function(model_set, s_inc) {
    ne_vec <- nash_equilibria_vec(model_set)
    ad_data <- adoption_dynamics(model_set, s_inc)
    ne_vec <- cbind(ne_vec, ad_data)
    ne_vec <- ne_vec %>%
        mutate(time = c(2026:2075))
    ne_vec["alpha"] <- alpha_vec(ne_vec["s"], model_set)
    ne_vec["q_cutoff"] <- adoption_cutoff_vec(ne_vec["s"], model_set)
    ne_vec["in_eq"] <- ifelse(abs(ne_vec["high"] - ne_vec["s"]) < 0.005, 1, 0)
    return(ne_vec)
}

dynamic_adoption_plot <- function(plot_data, plot_name = "dyn_adopt_plot.pdf") {
    fills <- c("Gov. Increment of adoption" = "lightgray"
             , "System in Equilibrium" = "darkolivegreen1")
    colors <- c("Tipping Point Nash Equilibrium" = "coral3"
              , "High Nash Equilibrium" = "deepskyblue4"
              , "Proportion Airports Adopted" = "aquamarine3"
              , "Proportion H2 Flight" = "deeppink3")

    sinc_xmin <- plot_data %>%
        filter(s_inc == 1) %>%
        select(time) %>%
        min()

    sinc_xmax <- plot_data %>%
        filter(s_inc == 1) %>%
        select(time) %>%
        max()

    in_eq_xmin <- plot_data %>%
        filter(in_eq == 1 & s > 0) %>%
        select(time) %>%
        min()

    in_eq_xmax <- plot_data %>%
        filter(in_eq == 1) %>%
        select(time) %>%
        max()

    ggplot(data = plot_data) +
        geom_rect(aes(xmin = sinc_xmin, xmax = sinc_xmax, ymin = 0, ymax = 1
            , fill = "Gov. Increment of adoption")
            , alpha = 0.1) +
        geom_rect(aes(xmin = in_eq_xmin, xmax = in_eq_xmax, ymin = 0, ymax = 1
            , fill = "System in Equilibrium")
            , alpha = 0.1) +
        geom_line(aes(x = time, y = high, color = "High Nash Equilibrium")
            , linewidth = 1) +
        geom_line(aes(x = time, y = tipp
            , color = "Tipping Point Nash Equilibrium")
            , linewidth = 1
            , linetype = "solid") +
        geom_line(aes(x = time, y = s, color = "Proportion Airports Adopted")
            , linewidth = 2
            , alpha = 0.8) +
        #geom_line(aes(x = time, y = alpha, color = "Proportion H2 Flight")) +
        scale_x_continuous(n.breaks = 10) +
        labs(x = "Time"
            , y = "Proportion of Airlines Adopting"
            , color = "Curve Legend"
            , fill = "Area Legend") +
        scale_color_manual(values = colors) +
        scale_fill_manual(values = fills) +
        theme_clean()
}