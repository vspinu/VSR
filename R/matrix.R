


### MISC
##' @export
setGeneric("keepK", function(x, keep = 1, byrows = F, highest = T, replace = 0, ...) standardGeneric("keepK"),
           signature = "x")

.keepK_mat <- function(x, keep = 1, byrows = F, highest = T, replace = 0, ...){
    x <- unclass(as.matrix(x))
    keep_fun <- if(highest) utils::tail else utils::head
    if(byrows){
        keep <- rep_len(keep, nrow(x))
        for(i in 1:nrow(x)){
            ixs <- keep_fun(order(x[i, ]), keep[[i]])
            x[i, -ixs] <- replace
        }
    } else {
        keep <- rep_len(keep, ncol(x))
        for(i in 1:ncol(x)){
            ixs <- keep_fun(order(x[, i]), keep[[i]])
            x[-ixs, i] <- replace
        }
    }
    Matrix(x, sparse = T)
}

setMethod("keepK", "matrix", .keepK_mat)
setMethod("keepK", "crossdist", .keepK_mat)

setMethod("keepK", "CsparseMatrix", function(x, keep = 1, byrows = F, highest = T, replace = 0, ...) {
    if(byrows) {
        t(keepK(t(x), keep = keep, byrows = F, highest = highest, replace = replace, ...))
    } else {
        keep <- rep_len(keep, ncol(x))
        keep_fun <- if(highest) utils::tail else utils::head
        has_x <- .hasSlot(x, "x")
        ixs <- lapply(1:ncol(x), function(i) {
            xi <- x@p[[i]]
            xi1 <- x@p[[i + 1L]]
            if(xi == xi1) {
                integer(0)
            } else {
                range <- (x@p[[i]] + 1L):x@p[[i + 1L]]
                keep_range <-
                    if(has_x)
                        range[keep_fun(.qorder(x@x[range]), keep[[i]])]
                    else
                        range[keep_fun(x@i[range], keep[[i]]) + 1L]
                .qsort(keep_range)
            }
        })

        keepixs <- unlist(ixs)
        newp <- c(0L, cumsum(unlist(lapply(ixs, length))))

        ## out <- x

        if(replace == 0){
            x@i <- x@i[keepixs]
            if(has_x)
                x@x <- x@x[keepixs]
            x@p <- newp
        } else {
            if(!has_x)
                stop("replace argument is not meaningful for Matrixes with no 'x' slot")
            x@x[-keepixs] <- replace
        }
        x
    }
})


### COUNTS
##' @export
setGeneric("colCounts", function(x, ...) standardGeneric("colCounts"))
setMethod("colCounts", signature(x = "CsparseMatrix"), 
          function(x, ...) structure(diff(x@p), names = colnames(x)))

##' @export
setGeneric("rowCounts", function(x, ...) standardGeneric("rowCounts"))
setMethod("rowCounts", signature(x = "CsparseMatrix"), 
          function(x, ...) structure(diff(t(x)@p), names = rownames(x)))



### SDs

## compute standard deviation
.dgC2list <- function(x, row=TRUE) {
    if(row) x <- t(x)   
    lapply(2:length(x@p), FUN = function(i) {
        if(x@p[i-1L]==x@p[i]) numeric(0)
        else x@x[(x@p[i-1L]+1L):x@p[i]]
    })
}

##' @export
setGeneric("rowSds", function(x, ...) standardGeneric("rowSds"))
setMethod("rowSds", signature(x = "CsparseMatrix"),
          function(x, ...) {
              s <- sapply(.dgC2list(x, row=TRUE), sd)
              names(s) <- rownames(x)
              s
          })

##' @export
setGeneric("colSds", function(x, ...) standardGeneric("colSds"))
setMethod("colSds", signature(x = "CsparseMatrix"),
          function(x, ...) {
              s <- sapply(.dgC2list(x@data, row=FALSE), sd)
              names(s) <- colnames(x)
              s
          })
