
##' Orderbook functions
##'
##' @rdname orderbook
##'
##' @param X numeric ob levels
##' @param V numeric ob values
##' @param side bid/ask
##' @export 
ob_margin <- function(X, V, side = "bid") {
    if(side == "ask" || side == "sell" || side == 2)
        c_min_ob_margin(X, V)
    else if (side == "bid" || side == "buy" || side == 1)
        c_max_ob_margin(X, V)
    else stop("Invalid side argument")
}
