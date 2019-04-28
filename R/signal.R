
#' @export
buy_sell_signal <- function(x, sell_entry = .3, buy_entry = .7,
                            sell_exit = buy_entry, buy_exit = sell_entry,
                            scale = TRUE) {
    if (scale) x <- scale(x)
    qs <- quantile(x, c(sell_entry, sell_exit, buy_exit, buy_entry))
    c_buy_sell_signal(x, qs[[1]], qs[[2]], qs[[4]], qs[[3]])
}

#' @export
fill_na_locf <- function(x)
    .Call(`_VSR_c_fill_locf_nonfinite`, x)

#' @export
fill_nonfinite_locf <- function(x)
    .Call(`_VSR_c_fill_locf_nonfinite`, x)
