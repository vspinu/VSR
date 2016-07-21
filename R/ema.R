
cumemaR <- function(x, date, theta = 10){
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

emaR <- function(x, date, theta = 10){
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
##' @param theta ema parameter
##' @param linear boolean, linear or not
##' @param cum boolean, cumulative or not
ema <- function(x, date, theta = 10, linear = F, cum = F){
  f <-
    if(cum) c_cumema
    else if (linear) c_ema_lin
    else c_ema
  f(x, date, theta)
}

##' @rdname ema
##' @param weight weight
wema <- function(x, weight, date, theta = 10, linear = F, cum = F){
    num <- ema(x*weight, date, theta, linear, cum)
    den <- ema(weight, date, theta, linear, cum)
    out <- num/den
    ## avoid infinitity for num==0
    out[num == 0] <- 0
    out
}

##' @rdname ema
##' @param tslow theta of the slow moving EMA
##' @param tfast theta of the fast moving EMA
macd <- function(x, date, tslow = 30, tfast = 10, linear = F, cum = F){
  fast <- ema(x, date, tfast, linear, cum)
  slow <- ema(x, date, tslow, linear, cum)
  out <- fast/slow - 1
  out[is.nan(out)] <- 0
  out
}

## exponential moving standard deviation
emsd <- function(x, date, theta = 10, normalize = T, linear = F){
  emean <- ema(x, date, theta, linear, F)
  dev <- (x - emean)^2L
  if (normalize) dev <- dev/(emean^2 + 0.0001)
  sqrt(ema(dev, date, theta, linear, F))
}
