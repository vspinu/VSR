##' @importFrom Rcpp sourceCpp
##' @importFrom data.table fread data.table
##' @importFrom methods setClass setGeneric new show
##' @exportPattern "^[^\\.]"
#' @useDynLib VSR, .registration=TRUE
NULL

## ALIASES
DF <- function(...) {
    do.call(base::data.frame, list(...))
}
DT <- function(...) {
    do.call(data.table::data.table, list(...))
}
l <- list
mfrow <- function(r = 1, c = 1) par(mfrow = c(r, c))
mfcol <- function(r = 1, c = 1) par(mfcol = c(c, r))
ldply <- function(.data, .fun = NULL, ..., .progress = "none", .inform = FALSE, 
                  .parallel = FALSE, .paropts = NULL, .id = NA)
  setDT(plyr::ldply(.data = .data, .fun = .fun, ..., .progress = .progress, .inform = .inform,
                    .parallel = .parallel, .paropts = .paropts, .id = .id))

## EVALUATION
peval <- function(expr, across = list()){
  ## eval expr in paralel 
  library(parallel)
  nms <- names(across)
  if(!all(nzchar(nms)))
    stop("in across arg all names must be non empty")
  X <- function(so_far, rest){
    if(length(rest)){
      new_so_far <- unlist(lapply(rest[[1]],
                                  function(el) lapply(so_far, c, el)),
                           recursive = F)
      X(new_so_far, rest[-1])
    } else {
      so_far
    }
  }
  args <- X(lapply(across[[1]], list), across[-1])
  args <- lapply(args, structure, names = nms)
  expr <- substitute(expr)
  fun <- function(larg){
    expr <- substituteDirect(expr, as.list(larg))
    eval(expr)
  }
  cl <- makeForkCluster(length(args), outfile = "")
  on.exit(stopCluster(cl))
  out <- clusterApply(cl, args, fun)
  names <- sapply(args, paste, collapse = "_")
  names(out) <- names
  out
}


## other
tag <- function(...){
  dots <- list(...)
  names <- all.vars(match.call())
  paste(paste(names, dots, sep = ":"),
        collapse=" ")
}
