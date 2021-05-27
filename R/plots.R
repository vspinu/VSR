
xplot <- function(x, y = c("close", "price"),
                  sigs = NULL,  bars = NULL, 
                  main = NULL, norm = NULL,
                  subsample = 50000,  ...) {
  library(xts)
  nms <- colnames(x)
  stopifnot(all(y %in% colnames(x)))
  all_vars <- unlist(c(sigs, bars, y))
  other_vars <- setdiff(all_vars, nms)
  if (!is_empty(other_vars))
    warning("Vars not in the data set: ", paste(other_vars, collapse = ","))

  if (is.data.frame(x)) {
    if (nrow(x) > subsample)
      x <- sample_n(x, subsample)
    date.ix <- which(map_lgl(x, ~ is.Date(.x) || is.POSIXt(.x)))[[1]]
    time <- x[[date.ix]]
    x <- xts::as.xts(x[, all_vars], order.by = time)
  }

  if (is.logical(norm)) {
    norm <- if (norm) "all" else "none"
  }

  .norm <- function(x, sig_vars, range_sig) {
    for (nm in sig_vars) {
      r <- range(x[, nm], na.rm = T)
      delta <- diff(r)
      if (delta == 0)
        x[, nm] <- range_sig[[1]]
      else
        x[, nm] <- (x[, nm] - r[[1]]) / delta * diff(range_sig) + range_sig[[1]]
    }
    x
  }

  sig_ranges <- map(sigs, function(vars) {
      range <- range(x[, vars[[1]]], na.rm = T)
      for (nm in vars) {
        tt <- range(x[, nm], na.rm = T)
        range <- c(min(range[[1]], tt[[1]]), max(range[[2]], tt[[2]]))
      }
      range
  })

  bar_ranges <- map(bars, function(vars) {
      range <- range(x[, vars[[1]]], na.rm = T)
      for (nm in vars) {
        tt <- range(x[, nm], na.rm = T)
        range <- c(min(range[[1]], tt[[1]]), max(range[[2]], tt[[2]]))
      }
      range
  })

  if (norm %in% c("all", "core") && length(y) > 1)
    for (nm in c(y)) {
      x[, nm] <- x[, nm]/as.numeric(x[1, nm])
    }

  x$zero <- 0
  
  pal <- RColorBrewer::brewer.pal(12, "Paired")
  core_pal <- pal[2*1:6]
  bar_pal <- pal[2*0:5 + 1]
  sig_pal <- RColorBrewer::brewer.pal(9, "Set1")
  range1 <- range(x[, y[[1]]], na.rm = T)
  delta <- diff(range1)*0.05 + 0.0001
  pp <- plot.xts(x[, y[[1]]], main = main, lty = 1,
                 col = core_pal[[1]], ylim = c(range1[[1]] - delta, range1[[2]] + delta), 
                 yaxis.same = F, ...)
  if (length(y) > 1) {
    for(i in 2:length(y))
      pp <- lines(x[, y[[i]]], col = core_pal[[(i - 1) %% length(core_pal) + 1]])
  }
  pp <- addLegend(legend.names = y,
                  text.col = rep_along(y, core_pal),
                  ncol = length(y))

  for (i in seq_along(sigs)) {
    vars <- sigs[[i]]
    pp <- addSeries(x[, vars[[1]]], col = sig_pal[[1]], type = "l", ylim = sig_ranges[[i]], lwd = 1.2)
    pp <- lines(x[, "zero"], col = "gray10")
    if (length(vars) > 1)
      for(j in 2:length(vars))
        pp <- lines(x[, vars[[j]]], col = sig_pal[[j %% length(sig_pal)]], type = "l", lty = 1)
    pp <- addLegend(legend.names = vars, text.col = rep_along(vars, sig_pal),
                    ncol = length(vars))
  }

  for (i in seq_along(bars)) {
    vars <- bars[[i]]
    pp <- addSeries(x[, vars[[1]]], col = bar_pal[[1]], type = "h", lwd = 1.2, ylim = bar_ranges[[i]])
    if (length(vars) > 1)
      for(i in 2:length(vars))
        pp <- lines(x[, vars[[i]]], col = bar_pal[[i %% length(bar_pal)]], type = "h")
    pp <- addLegend(legend.names = vars, text.col = rep_along(vars, bar_pal),
                    ncol = length(vars))
  }

  print(pp)
}
