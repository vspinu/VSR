
kmeans_outliers <- function(data, N = .05, centers = NULL, ...){
    if(is.null(centers))
        centers <- min(max(4, ncol(data)), 10)
    ## return row indeces of biggest outliers in decreaing order 
    ## N can be a number giving the number of outliers, if < 1 it's a
    ## proportion of observations
    if(is.data.frame(data)){
        data <- mapitcbind(data, if(is.numeric(it) || is.factor(it)) as.numeric(it))
        centers <- min(nrow(unique(data)), centers)
    } else if(!is.matrix(data) && is.numeric(data))
        stop("data argument to 'kmeans_outliers' must be a numeric matrix or a data frame")

    data <- scale(data)
    data[is.na(data)] <- 0
    ## fixme: check fpc::kmeansruns again. WAF it's so slow?
    kres <- kmeans(data, centers, ...)
    centers <- fitted(kres, "centers")
    distances <- sqrt(rowSums((data - centers)^2))
    N <- if(N < 1) round(nrow(data)*N) else min(round(N), nrow(data))
    head(order(distances, decreasing = T), N)
}


model_terms <- function(fit, vars = NULL, qrange = c(.02, .98), subsample = 200){
    if(is(fit, "gbm")) {
        if(is.null(vars))
            vars <- fit$var.names
        do.call(rbind, 
                lapply(vars,  function(nm) {
                    out <- cbind(plot(fit, i.var = nm, return.grid = T), name = nm)
                    data.table(x = as.numeric(out[[1]]),
                               y = as.numeric(out[[2]]),
                               var = nm)
                }))
    } else {
        terms_data <- termplot(fit, plot = F)
        mapply(function(x, nm){
            if(!is.factor(x$x)){
                if(nrow(x) > subsample)
                    x <- x[sample(1:nrow(x), subsample), ]
                if(!is.null(qrange))
                    x <- x[which_qrange(x$x, qrange), ]
                cbind(as.data.frame(x), var = nm)
            }
        }, terms_data, names(terms_data), SIMPLIFY = F) %>% rbindlist
    }
}

## second versions because I forgot that I had theh above 
model_terms2 <- function(model, subsample = 200){
    termplot(model, plot = F) %>%
        map( ~ sample_n(., size = min(nrow(.), subsample))) %>% 
        tibble(terms = .) %>%
        unnest(terms, .id = names(.))
}

ggterms <- function(fit, vars = NULL, qrange = c(.02, .98), subsample = 200, facet = T,
                    sort = T, max_vars = Inf){
    terms <-
        if(is.data.frame(fit)) fit
        else model_terms(fit, vars, qrange, subsample)
    var0 <- factor(as.character(terms$var))
    nvars <- length(levels(var0))
    if(sort){
        range <- terms[, diff(range(y)), by = "var"]
        var0 <- factor(var0, range[, var[order(V1, decreasing = T)]], ordered = T)
        terms$var <- var0
    }
    if(max_vars < nvars){
        keep <- levels(var0)[1:max_vars]
        terms <- terms[var %in% keep]
    }
    ggplot(terms, aes_string("x", "y", group = "var")) +
        (if(facet) geom_line() else geom_line(color = var)) + 
        (if(facet) facet_wrap(~ var , scales = "free_x"))  
}

quantile_knots <- qknots <-
    function(x, k = 4, alpha = .02){
        breaks <- seq(alpha, 1 - alpha, length.out = k + 2)
        unique(quantile(x, breaks, na.rm = T))
}

sparsity <- function(mat){
    N <- prod(dim(mat))
    if(is(mat, "Matrix")){
        Matrix::nnzero(mat, T)/N
    } else if (is(mat, "simple_triplet_matrix")) {
        length(mat$v)/N
    } else {
        sum(mat > 0)/N
    }
}

## ## More regular natural splines
## knots <- list(
##   dmax = seq.int(0, .3, .05),
##   dmin = seq.int(0, -.3, -.05),
##   dsize = seq.int(-0.3, .3, .05), 
##   pdiff10 = quantile(gos$pdiff10, seq.int(.1, .9, by = .05)),
##   pdiff1 = quantile(gos$pdiff1, seq.int(.1, .9, by = .05)))

## ns2 <- function(obj, knots){
##   nm <- as.character(substitute(obj))
##   knots <- knots[[nm]]
##   ## use first-last as boundary knots
##   fl <- c(1, length(knots))
##   ns(obj, knots = knots[-fl], Boundary.knots = knots[fl])
## }


ns2 <- function(x, nknots = 5){
  start <- .5/nknots
  end <- 1 - start
  knots <- quantile(x, seq.int(start, end, length = knots))
  ## use first-last as boundary knots
  bknots <- c(1, length(knots))
  ns(obj, knots = knots, Boundary.knots = knots[bknots])
}
