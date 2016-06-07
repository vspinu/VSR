
.break <- function(x, add_breaks, k){
    out <- NULL
    .k <- k
    try(
        while((length(out) < k + 1) & .k < 3*k){
            out <- unique(sort(c(add_breaks, quantile(x, probs = seq(0, 1, by = 1/.k), na.rm = T))))
            .k <- .k + 1L
        })
    out
}
    
qcut <- function(x, k = 10, add_breaks = c(), exclude = NULL){
    if(is.numeric(x) || inherits(x, "POSIXt")){
        breaks <- .break(x, add_breaks, k)
        if(inherits(x, "POSIXt"))
            breaks <- .POSIXct(breaks)
        out <- cut(x, breaks, include.lowest = T, ordered_result = T)
        factor(out, exclude = exclude)
    } else x
}

.list.names <- function(...) {
  l <- as.list(substitute(list(...)))[-1L]
  nm <- names(l)
  fixup <- if (is.null(nm)) 
    seq_along(l)
  else nm == ""
  dep <- vapply(l[fixup], function(x) if (is.symbol(x)) as.character(x) else "", "")
  if (is.null(nm)) 
    dep
  else {
    nm[fixup] <- dep
    nm
  }
}

tab <- function(...){
  out <- base::table(..., useNA = "ifany")
  if(length(dim(out)) == 1) sort(out, decreasing = T)
  else out
}

ftab <- function(obj, topn = NULL){
    out <- c_tab(obj)
    if(is.null(topn))
        out
    else{
        topix <- top_index(out[["counts"]], topn)
        list(vals = out[["vals"]][topix],
             counts = out[["counts"]][topix])
    }
}

ptab <- ptable <- function(..., margin = NULL){
  prop.table(base::table(...), margin)
}

qtab <- qtable <- function(..., k = 10){
  dots <- lapply(list(...), qcut, k = k)
  do.call(base::table, dots)
}

len <- length

ulen <- ulength <- function(x) NROW(unique(x))
usort <- function(x, mixed = F){
    if(mixed)
        gtools::mixedsort(unique(x))
    else
        sort(unique(x))
}

tabna <- function(...){
    dots <- lapply(list(...), is.na)
    names(dots) <- .list.names(...)
    if(length(dots) == 1){
        out <- c("TRUE" = 0, "FALSE" = 0)
        tout <- do.call(tab, dots)
        out[names(tout)] <- tout
        out
    } else {
        do.call(tab, dots)
    }
}

ptabna <- function(..., margin = NULL){
    dots <- lapply(list(...), is.na)
    names(dots) <- .list.names(...)
    if(length(dots) == 1){
        out <- c("TRUE" = 0, "FALSE" = 0)
        tout <- do.call(ptab, dots)
        out[names(tout)] <- tout
        out
    } else {
        do.call(ptab, dots)
    }
}

which_qrange <- function(var, range, max_levels){
    if(is.character(var))
        rep.int(T, length(var))
    else if(is.factor(var)){
        tbl <- cumsum(rev(ptab(var)))
        nms <- names(tbl[tbl > (range[[1]] + 1 - range[[2]])])
        nms <- tail(nms, max_levels)
        var %in% nms
    } else {
        qs <- quantile(var, probs = range, na.rm = T)
        var >= qs[[1]] & var <= qs[[2]]
    }
}

##' @export
setGeneric("qrange", 
           def = function(var, range = c(.05, .95), max_levels = 20, ...){
               if(is.null(range)) return(var)
               if(is.factor(var)){
                   if(length(labels(var)) > max_levels){
                       which <- which_qrange(var, range, max_levels)
                       droplevels(var[which])
                   } else var
               } else var[which_qrange(var, range, max_levels)]
           },
           signature = "var")


.qrange_matrix <- function(var, range = c(.05, .95), max_levels = 10, kmean_clusters = 1) {
    if(is.null(range)) return(var)
    which <- kmeans_outliers(var, range[[1]] + 1 - range[[2]], centers = kmean_clusters)
    if(is.data.frame(var))
        droplevels(var[-which, ])
    else 
        var[-which, ]
}

##' @export
setMethod("qrange", "data.frame", .qrange_matrix)

##' @export
setMethod("qrange", "matrix", .qrange_matrix)

qrange1 <- function(var) qrange(var, c(.01, .99))
qrange5 <- function(var) qrange(var, c(.05, .95))
qrange10 <- function(var) qrange(var, c(.1, .9))

xdiff <- function(A, B){
    list("A-B" = setdiff(A, B),
         "B-A" = setdiff(B, A))
}
