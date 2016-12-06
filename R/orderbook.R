
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
    nms <- paste(focals, ns, sep = "_")
    out <- c_ob_exp_sum(price, size, side, focals, ns)
    out <- as.data.table(do.call(cbind, out))
    setnames(out, c(paste0("oa_", nms),
                    paste0("ob_", nms)))
}

##' @rdname orderbook
##' @export
ob_exp_sum_diff <- function(price, size, side, focals, ns, prob = T){
    focals <- rep_len(focals, length(ns))
    nms <- paste("ab", focals, ns, sep = "_")
    out <- c_ob_exp_sum(price, size, side, focals, ns)
    out <-
        if(prob){
            out$ask/(out$ask + out$bid)
        } else {
            out$ask - out$bid
        }
    setnames(DT(out), nms)
}

