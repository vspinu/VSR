
build_knots <- function(dfg, nknots = 4, alpha = .005, vars){
  vars <- structure(vars, names = vars)
  breaks <- seq(alpha, 1 - alpha, length.out = nknots + 2)
  lapply(vars, function(nm){
    if(!is.factor(dfg[[nm]])){
      ## cat(nm, "\n")
      unique(quantile(dfg[[nm]], breaks))
    }
  })
}


mat2long <- function(mat, row_name = "row", col_name = "col", val_name = "val", extra_by_row = NULL){
    stopifnot(is(mat, "Matrix"))
    ixs <- non0ix(mat)
    row_nms <- rownames(mat)[ixs[, 1]]
    col_nms <- colnames(mat)[ixs[, 2]]
    out <- setnames(DT(row = row_nms, col = col_nms, val = mat[ixs]), 
                    c("row", "col", "val"), c(row_name, col_name, val_name))
    if(!is.null(extra_by_row))
        out <- cbind(out, extra_by_row[ixs[, 1]])
    setkeyv(out, c(row_name, col_name))
    out
}

long2mat <- function(x, y, vals = 1, join = FALSE, use.last.ij = FALSE, ...){
    library(Matrix)
    stopifnot(length(x) == length(y))
    if(join){
        ## this is extremely slow
        ## levs <- gtools::mixedsort(as.character(unique(c(x, y))))
        levs <- usort(c(x, y))
        x <- factor(x, levels = levs)
        y <- factor(y, levels = levs)
    } else {
        x <- as.factor(x)
        x <- as.factor(y)
    }
    sparseMatrix(as.integer(x), as.integer(y),
                 x = vals, dims = c(length(levels(x)), length(levels(y))), 
                 dimnames = list(levels(x), levels(y)),
                 use.last.ij = use.last.ij, 
                 ...)
}

## tdm_long2mat <- function(terms, docs, weight){
##     library(slam)
##     terms <- as.factor(terms)
##     docs <- as.factor(docs)
##     simple_triplet_matrix(i = as.integer(terms),
##                           j = as.integer(docs),
##                           v = weight,
##                           dimnames = list(levels(terms), levels(docs)))
## }


non0ix <- function(mat, xtendSymm = TRUE){
    if(is(mat, "Matrix")) Matrix:::non0ind(mat, xtendSymm = xtendSymm) + 1L
    else which(mat != 0, arr.ind = T)
}
