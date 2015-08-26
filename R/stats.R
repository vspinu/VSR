
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

get_terms <- function(fit, vars = NULL){
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
        terms_data <- termplot(fit, plot = F, terms = vars)
        do.call(rbind,
                mapply(function(x, nm){
                    if(!is.factor(x$x)){
                        N <- 1000
                        if(nrow(x) > N)
                            x <- x[sample(1:nrow(x), N), ]
                        min <- knots[[nm]][[1]]
                        max <- knots[[nm]][length(knots[[nm]])]
                        x <- x[x$x > min & x$x < max, ]
                        cbind(as.data.frame(x), var = nm)
                    }
                }, terms_data, names(terms_data), SIMPLIFY = F))
    }
}

