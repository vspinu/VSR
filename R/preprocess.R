
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


## if k < 1, interpret as proportion of levels.
keep_k_levels <- function(f, k = if(is.null(min_in_level)) 7 else +Inf,
                          min_in_level = NULL, min_levels = 1, 
                          other_label = "OTHER", includeNA = T){
    if(includeNA){
        f <- factor(f, exclude = NULL)
    } else {
        f <- as.factor(f)
    }
    if (length(levels(f)) <= min_levels)
        return(f)
    if(k < 1){
        k <- ceiling(k*length(levels(f)))
    }
    tab <- tab(f)
    if(!is.null(min_in_level))
        tab <- tab[tab >= min_in_level]
    best <- head(names(tab), k)
    levs <- levels(f)
    levs[!levs %in% best] <- other_label
    levels(f) <- levs
    f
}

balance_factor <- function(f, nlev = NULL, min_in_lev = NULL) {
    if (!is.null(nlev)){
        if (!is.null(min_in_lev)) {
            warning("`nlev` provided, ignoring `min_in_lev`")
        }
    } else if (!is.null(min_in_lev)){
        nlev <- floor(length(f)/min_in_lev)
    } else {
        stop("at least one of nlev and min_in_lev must be specified")
    }
    out <- as.factor(f)
    tbl <- rev(tab(f))
    sums <- cumsum(tbl)
    part <- cut_interval(sums, nlev)
    df <- DT(orig_levs = names(tbl), part = part)
    df[, new_levs := paste(usort(orig_levs), collapse = "|"), by = part]
    levels(out) <- df[, new_levs[match(levels(out), orig_levs)]]
    out
}
