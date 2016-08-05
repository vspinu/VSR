
cumemaR <- function(x, date, n){
    theta <- 1/n
    N <- length(x)
    ex <- double(N)
    ex[[1]] <- x[[1]]
    for(i in seq_len(max(0, N-1L))){
        i1 <- i + 1L
        edelta <- exp(- (date[[i1]] - date[[i]])/theta)
        ex[[i1]] <- ex[[i]]*edelta + x[[i1]]
    }
    ex
}

emaR <- function(x, date, n){
    theta <- 1/n
    N <- length(x)
    ex <- double(N)
    ex[[1]] <- x[[1]]
    for(i in seq_len(max(0, N-1L))){
        i1 <- i + 1L
        W <- exp(-(date[[i1]] - date[[i]])/theta)
        ex[[i1]] <- W*ex[[i]] + (1-W)*x[[i1]]
    }
    ex
}

##' Exponential (weighted) moving averages for iregular time series and
##' derivatives.
##' 
##' @param x values of the seires
##' @param date time index
##' @param n ema parameter - number of periods (n = 1/theta where -theta is the
##'     exponent in the smoother). At \code{n} periods back \code{x} contributes
##'     about 36.8% of the total weight; at \code{3*n} it contributes about 5%
##'     to the average.
##' @param linear boolean, linear or not
##' @param cum boolean, cumulative or not
ema <- function(x, date, n = 10, linear = F, cum = F){
    theta <- 1/n
    f <-
        if(cum) c_cumema
        else if (linear) c_ema_lin
        else c_ema
    f(x, date, theta)
}

##' @rdname ema
##' @param weight weight
wema <- function(x, weight, date, n = 10, linear = F, cum = F){
    num <- ema(x*weight, date, n, linear, cum)
    den <- ema(weight, date, n, linear, cum)
    out <- num/den
    ## avoid infinitity for num==0
    out[num == 0] <- 0
    out
}

##' @rdname ema
##' @param nslow nr of periods for slow moving EMA
##' @param nfast nr of periods for fast moving EMA
macd <- function(x, date, nfast = 12, nslow = 26, linear = F, cum = F){
    fast <- ema(x, date, nfast, linear, cum)
    slow <- ema(x, date, nslow, linear, cum)
    out <- fast/slow - 1
    out[is.nan(out)] <- 0
    out
}

## exponential moving standard deviation
emsd <- function(x, date, n = 10, normalize = T, linear = F){
    emean <- ema(x, date, n, linear, F)
    dev <- (x - emean)^2L
    if (normalize) dev <- dev/(emean^2 + 0.0001)
    sqrt(ema(dev, date, n, linear, F))
}
