
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
