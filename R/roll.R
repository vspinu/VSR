.reclass <- function(new, old){
    if(is.Date(old))
        as.Date(new, origin = .POSIXct(0))
    else if(is.POSIXt(old))
        as.POSIXct(new, tz = attr(old, "tz"))
    else
        as(new, class(old))
}

##' Rolling function for iregular series
##' @rdname iroll
##' @export
##' @param ix index converible to numeric (Date, POSIXct, etc). Must be ordered
##'     increasingly.
##' @param x value to be rolled over
##' @param left left bound scalar (convertible to numeric)
##' @param right right bound scalar (convertible to numeric)
##' @param left_open if the interval is left open
##' @param right_open if the interval is right open
iroll_mean <- function(ix, x, left, right = 0, left_open = FALSE, right_open = FALSE) {
    out <- c_roll_mean(as.numeric(ix), x, left, right,
                       left_open, right_open)
    .reclass(out, x)
}

##' @rdname iroll
##' @export
iroll_min <- function(ix, x, left, right = 0, left_open = FALSE, right_open = FALSE) {
    out <- c_roll_min(as.numeric(ix), as.numeric(x), 
                      as.numeric(left)[1], as.numeric(right)[1], 
                      left_open, right_open)
    .reclass(out, x)
}

##' @rdname iroll
##' @export
iroll_max <- function(ix, x, left, right = 0, left_open = FALSE, right_open = FALSE) {
    out <- c_roll_max(as.numeric(ix), as.numeric(x), 
                      as.numeric(left)[1], as.numeric(right)[1], 
                      left_open, right_open)
    .reclass(out, x)
}

##' @rdname iroll
##' @export
iroll_first <- function(ix, x, left, right = 0, left_open = FALSE, right_open = FALSE) {
    out <- c_roll_first(as.numeric(ix), as.numeric(x), 
                        as.numeric(left)[1], as.numeric(right)[1], 
                        left_open, right_open)
    .reclass(out, x)
}

##' @rdname iroll
##' @export
iroll_last <- function(ix, x, left, right = 0, left_open = FALSE, right_open = FALSE) {
    out <- c_roll_last(as.numeric(ix), as.numeric(x), 
                       as.numeric(left)[1], as.numeric(right)[1], 
                       left_open, right_open)
    .reclass(out, x)
}

##' @rdname iroll
##' @export
##' @param prob numeric scalar in [0, 1].
iroll_quantile <- function(ix, x, left, right = 0, left_open = FALSE, right_open = FALSE, prob = .5) {
    out <- c_roll_quantile(as.numeric(ix), as.numeric(x), 
                           as.numeric(left)[1], as.numeric(right)[1], 
                           left_open, right_open)
    .reclass(out, x)
}
