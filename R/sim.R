#' Generic Method for Computing and Organizing Simulated Quantities of Interest
#' param obj a zelig object
#' param x a setx object
#' param x1 a secondary setx object used to perform
#'          particular computations of quantities of interest
#' param y a parameter reserved for the computation of particular
#'         quantities of interest (average treatment effects). Few
#'         models currently support this parameter
#' param num an integer specifying the number of simulations to compute
#' param bootstrap unsupported
#' param boot.fn unsupported
#' param cond.data unsupported
#' value an object of type "sim"
sim <- function(
                obj,
                x=NULL, x1=NULL, y=NULL, num=1000, prev=NULL,
                bootstrap=F, boot.fn=NULL, cond.data=NULL,
                ...
                ) {
  UseMethod("sim")
}
