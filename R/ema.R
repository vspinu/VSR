
cumemaR <- function(x, days, theta = 10){
  N <- length(x)
  ex <- double(N)
  ex[[1]] <- x[[1]]
  for(i in seq_len(max(0, N-1L))){
    i1 <- i + 1L
    edelta <- exp(- (days[[i1]] - days[[i]])/theta)
    ex[[i1]] <- ex[[i]]*edelta + x[[i1]]
  }
  ex
}

cumema <- function(x, days, theta = 10)
  c_cumem(x, days, theta)

emaR <- function(x, days, theta = 10){
  N <- length(x)
  ex <- double(N)
  ex[[1]] <- x[[1]]
  for(i in seq_len(max(0, N-1L))){
    i1 <- i + 1L
    W <- exp(-(days[[i1]] - days[[i]])/theta)
    ex[[i1]] <- W*ex[[i]] + (1-W)*x[[i1]]
  }
  ex
}

ema <- function(x, days, theta = 10, linear = F, cum = F){
  f <-
    if(cum) c_cumema
    else if (linear) c_ema_lin
    else c_ema
  f(x, days, theta)
}

macd <- function(x, days, tslow = 30, tfast = 10, linear = F, cum = F){
  fast <- ema(x, days, tfast, linear, cum)
  slow <- ema(x, days, tslow, linear, cum)
  out <- fast/slow - 1
  out[is.nan(out)] <- 0
  out
}

## time weighted exponential moving standard deviation
emsd <- function(x, days, theta = 10, normalize = T, linear = F){
  emean <- ema(x, days, theta, linear, F)
  dev <- (x - emean)^2L
  if (normalize) dev <- dev/(emean^2 + 0.0001)
  sqrt(ema(dev, days, theta, linear, F))
}
