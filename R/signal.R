
#' @export
buy_sell_signal <- function(x,
                            sell_entry = .3, sell_exit = .6,
                            buy_exit = 1 - sell_exit, buy_entry = 1 - sell_entry, 
                            scale = FALSE, skip = FALSE, do_quantiles = FALSE,
                            check = TRUE) {
  if (check) {
    eps <- 1e-8
    stopifnot(sell_entry <= .5 + eps)
    stopifnot(buy_entry + eps >= .5)
    stopifnot(sell_exit + eps >= sell_entry)
    stopifnot(buy_exit <= buy_entry + eps)
    stopifnot(sell_entry <= buy_exit + eps)
    stopifnot(sell_exit <= buy_entry + eps)
  }
  if (scale) x <- scale(x)
  qs <- c(sell_entry, sell_exit, buy_exit, buy_entry)
  if (do_quantiles)
    qs <- quantile(x, qs)
  ## X <<- x
  ## QS<<-qs
  c_buy_sell_signal(x, skip, qs[[1]], qs[[2]], qs[[3]], qs[[4]])
}

#' @export
fill_na_locf <- function(x)
    .Call(`_VSR_c_fill_locf_nonfinite`, x)

#' @export
fill_nonfinite_locf <- function(x)
    .Call(`_VSR_c_fill_locf_nonfinite`, x)
