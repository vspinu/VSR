
##' Orderbook functions
##'
##' @rdname orderbook
##'
##' @export 
##' @param price price
##' @param size size
##' @param side side (2 or 1)
##' @param focals focal points around which to compute exp sums of the each ob
##' @param ns nr units in exp weighting - higher more compre comprehensive sum it is.
ob_exp_sum <- function(price, size, side, focals, ns){
    focals <- rep_len(focals, length(ns))
    nms <- if(is.null(names(ns))) as.character(ns) else names(ns)
    out <- c_ob_exp_sum(price, size, side, focals, ns)
    ## could be done at c++ level
    colnames(out$bid) <- nms
    colnames(out$ask) <- nms
    out
}
