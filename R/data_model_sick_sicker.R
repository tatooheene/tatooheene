#' Example dataset: data_model_output_sick_sicker
#'
#'@description
#' A dataset containing model output objects from the **Sick-Sicker** example model developed by the **DARTH (Decision Analysis in R for Technologies in Health)**
#' workgroup. This dataset includes the results from both annual and monthly
#' Markov model runs, specifically the Markov trace files and related output
#' objects produced during simulation.

#' These data can be used for demonstrating how to apply some of the package functionalities like discounting for the results of a model-based cost-effectiveness model.
#'
#' @format A collection of R objects saved as an `.RData` file containing:
#' \describe{
#'   \item{l_m_M_annual} {List: Markov trace results for the annual-cycle model.}
#'   \item{l_m_M_monthly}{List: Markov trace results for the monthly-cycle model.}
#'   \item{v_wcc_annual} {Vector: half-cycle correction weights for the annual model.}
#'   \item{v_wcc_monthly}{Vector: half-cycle correction weights for the monthly model.}
#'   \item{v_names_str} {Vector: names of the interventions strategies used in the model.}
#'   \item{l_u_annual} {List: annual utilities for each health state.}
#'   \item{l_u_monthly}{List: monthly utilities for each health state.}
#'   \item{l_c_annual} {List: annual costs for each health state.}
#'   \item{l_c_monthly}{List: monthly costs for each health state.}
#' }
#'
#' @details
#' This dataset represents a subset of model outputs intended for demonstrating the discounting in this package.
#' The full model is described in the DARTH “Sick-Sicker” example.
#'
#'#' Source code can be found here: https://github.com/DARTH-git/cohort-modeling-tutorial-intro
#' For the full reference read: Alarid-Escudero F, Krijkamp EM, Enns EA, Yang A, Hunink MGM, Pechlivanoglou P, Jalal H. An Introductory Tutorial on Cohort State-Transition Models in R Using a Cost-Effectiveness Analysis Example. Medical Decision Making, 2023;43(1):3-20. https://doi.org/10.1177/0272989X221103163
#'
#'
#' @source Adapted from the DARTH workgroup's Sick-Sicker model materials
#' (\url{https://darthworkgroup.com}).
#'
#' @examples
#' # Load the dataset
#' data("data_model_output_sick_sicker")
#'
#' # Explore the available objects
#' ls(pattern = "l_|v_")
#'
#' # View names of health states
#' v_names_str
#'
#' # Inspect the first few cycles of the annual Markov trace for the first strategy
#' head(l_m_M_annual[[1]])
#'
#' # Compare dimensions of annual and monthly traces
#' dim(l_m_M_annual[[1]])
#' dim(l_m_M_monthly[[1]])
#'
#' # Example: compute mean annual utility
#' mean(unlist(l_u_annual))
#'
#' # Apply the half cycle correction
#' ## Loop through each strategy and calculate total utilities and costs ----
#'

#'for (i in 1:length(v_names_str)) {
#'  v_u_str <- l_u[[i]]   # select the vector of state utilities for the i-th strategy
#'  v_c_str <- l_c[[i]]   # select the vector of state costs for the i-th strategy
#'
#'   ###* Expected QALYs and costs per cycle
#'   ##* Vector of QALYs and Costs
#'   #* Apply state rewards
#'   v_qaly_str <- l_m_M[[i]] %*% v_u_str # sum the utilities of all states for each cycle
#'   v_cost_str <- l_m_M[[i]] %*% v_c_str # sum the costs of all states for each cycle
#'
#'   ###*  Total expected QALYs and costs per strategy and apply half-cycle correction (if applicable)
#'   #* QALYs
#'   v_tot_qaly[i] <- t(v_qaly_str) %*% v_wcc
#'   #* Costs
#'   v_tot_cost[i] <- t(v_cost_str) %*% v_wcc
#' }

#'
"data_model_output_sick_sicker"

