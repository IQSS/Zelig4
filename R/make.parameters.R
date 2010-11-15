make.parameters <- function(terms, shape = "vector", ancillary = TRUE,eqns=NULL) {
  if (!shape %in% c("matrix", "vector"))
    stop("not a valid 'shape' for parameters.  Choose from \"matrix\" or \"vector\".")
 #comment 
  if(is.null(eqns))
    eqns<-names(terms)
  ints <- attr(terms, "intercept")[eqns]
  labs <- attr(terms, "term.labels")[eqns]
  const <- attr(terms, "constraints")
  for (i in 1:length(eqns)) {
    if (ints[[i]] == 1)
      labs[[i]] <- c("(Intercept)", labs[[i]])
  }
  fixed<-eqns[eqns %in% attr(terms,"ancilEqns")]
  syst<-eqns[eqns %in% attr(terms,"systEqns")]
#  syst<-eqns
  vars <- unique(unlist(labs))
  pars <- matrix(NA, ncol = length(syst), nrow = length(vars))
  colnames(pars) <- syst
  rownames(pars) <- vars
  for (i in syst) {
    idx <- which(!is.na(match(vars, labs[[i]])))
    pars[idx,i] <- paste(labs[[i]], i, sep = ":")
  }
  if (!is.logical(const)) {
    const <- attr(terms, "constraints")[syst,,drop=FALSE]
    for (i in 1:ncol(const)) {
      cidx <- which(!is.na(const[,i]))
      ridx <- match(const[cidx, i], rownames(pars))
      pars[cbind(ridx, cidx)] <- colnames(const)[i]
    }
  }  
  if (shape == "matrix")
    out <- pars
  if (shape == "vector") {
    out <- unique(na.omit(c(t(pars))))
    if (ancillary) 
      out <- c(out, fixed)
  }
  out
}
