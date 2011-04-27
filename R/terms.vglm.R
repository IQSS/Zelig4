#' Model Terms for 'vglm' Models
#' @usage \method{terms}{vglm}(x, ...)
#' @S3method terms vglm
#' @param x a fitted model object from the VGAM library
#' @param ... ignored parameters
#' @return the models terms of this fitted model object
#' @author Ferdinand Alimadhi, Kosuke Imai and Olivia Lau
terms.vglm <- function(x, ...)
  x@terms$terms
