#' Simulate Quantities of interest for the Zelig ``relogit'' Model
#'
#' @S3method qi relogit
#' @param obj a zelig object, containing the fitted ``relogit'' model
#' @param x a ``setx'' object
#' @param x1 a ``setx'' object
#' @param num an integer specifying the number of simulations to compute
#' @param param a ``parameter'' obejct containing information about the link,
#' inverse-link, and simulated parameters
#' @return a param
qi.relogit <- qi.logit
