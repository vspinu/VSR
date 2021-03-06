
#' @details `assignCommandArgs()`: Simple utility for automatic assignment and
#'   type conversion of command line arguments. Each argument supplied to
#'   command line is matched to arguments in `...` and converted to the type of
#'   that argument. Tail arguments not supplied at command line will be assigned
#'   their default values. See examples.
#' @param ... key=value pairs to be assigned into `.env`
#' @param .env environment where to assign arguments in `...`
#' @param .args commandArgs to be assigned
#'
#' @examples
#'
#' \dontrun{
#'    ## Script accepts 3 arguments, datetime `date`, string `bank` and logical `force`.
#'    assignCommandArgs(date = now(), bank = "targobank", force = FALSE)
#' }
#' @rdname programming
#' @export
assignCommandArgs <- function(..., .env = .GlobalEnv, .args = commandArgs(TRUE)) {
    dots <- dots_list(...)
    if (!all(nzchar(names(dots))))
      stop("All command arguments must be named")
    if (length(.args) > length(dots)) {
      stop(glue("Number of command line arguments too large ({length(.args)}). Possible arguments {paste(names(dots), collapse =',')}."))
    }
    for(i in seq_along(.args)) {
      default <- dots[[i]]
      arg <- .args[[i]]
      if (tolower(arg) %in% c("_", "nil", "null")) {
        dots[i] <- list(NULL)
      } else {
        dots[[i]] <-
          if (is.null(default)) arg
          else if (inherits(default, "POSIXt")) as.POSIXct(arg)
          else if (inherits(default, "Date")) as.Date(arg)
          else as(arg, class(default)[[1]])
      }
    }
    for(nm in names(dots)){
      assign(nm, dots[[nm]], envir = .env)
    }
    invisible(dots)
}

catlog <- function(..., .env = parent.frame()) {
  str <- paste(..., collapse = "", sep = "")
  str <- paste(glue(str, .envir = .env), collapse = " ")
  cat(glue("[{Sys.time()}] {str}"), "\n")
}

catc <- function(...) {
  do.call(cat, lapply(c(list(...), "\n"), as.character))
}

catf <- function(fmt, ...){
  cat(sprintf(fmt, ...), "\n")
}

stopif <- function (...) {
  n <- length(ll <- list(...))
  if (n == 0L) 
    return(invisible())
  mc <- match.call()
  for (i in 1L:n)
    if (!is.logical(r <- ll[[i]]) || anyNA(r) || any(r)) {
      ch <- deparse(mc[[i + 1]], width.cutoff = 60L)
      if (length(ch) > 1L)
        ch <- paste(ch[1L], "....")
      stop(sprintf("%s is TRUE", ch), call. = FALSE, domain = NA)
    }
  invisible()
}

## device hold
dh <- function(expr){
  expr <- substitute(expr)
  dev.hold()
  on.exit({
    dev.flush()
  })
  where <- parent.frame()
  eval(expr, envir = where)
}

multi_median <- function(...){
  out <- unlist(lapply(list(...), function(x) as.numeric(median(x, na.rm = T))))
  names(out) <- all.vars(substitute(c(...)))
  out
}


roxy_params <- function(file){
  ## retrive all @param from file
  params <- roxygen2:::parse_file(file, .GlobalEnv)
  params[sapply(params, is.null)] <- NULL
  params <- do.call(c, params)
  params[names(params) == "srcref"] <- NULL
  params <- lapply(params, unlist)
  params <- as.data.table(do.call(rbind, params), stringsAsFactors = F)
  rownames(params) <- NULL
  for(nm in names(params))
    params[[nm]] <- unlist(params[[nm]])
  names(params) <- toupper(names(params))
  setkey(params, NAME)
  params
}

last <- function(x, na.rm = TRUE) {
  if (is.data.frame(x))
    x[NROW(x),]
  else if (length(x) == 0)
    x
  else {
    if (na.rm) {
      x <- x[!is.na(x)]
      if (length(x) == 0) x
      else x[[length(x)]]
    } else {
      x[[length(x)]]
    }
  }
}

butlast <- function(x){
    if (is.data.frame(x))
        if (n <- NROW(x)) x[-n,]
        else x
    else if (!length(x))
        x
    else
        x[-length(x)]
}

first <- function(x){
    if (is.data.frame(x))
        x[1,]
    else if (!length(x))
        x
    else
        x[[1]]
}

rest <- butfirst <- function(x) x[-1]

del_nulls <- function(x){
    nulls <- sapply(x, is.null)
    x[!nulls]
}

## Comp <- function(...){
##     ## compose functions
##     ## this doesn't work in 3.1.1
##     ## Reduce(function(f, g){ function (...) f(g(...))}, list(...), right = T)
##     dots <- list(...)
##     function(...){
##         if(length(dots) == 1) dots[[1]](...)
##         else last(dots)(do.call(Comp, butlast(dots))(...))
##     }
## }

Comp <- function(...){
    ## compose functions
    ## this doesn't work in 3.1.1
    ## Reduce(function(f, g){ function (...) f(g(...))}, list(...), right = T)
    dots <- list(...)
    function(...){
        if(length(dots) == 1) dots[[1]](...)
        else first(dots)(do.call(Comp, rest(dots))(...))
    }
}

## map <- function(x, fun, ...) sapply(x, fun, ..., simplify = F)
## mapit <- function(x, expr, envir = parent.frame()){
##     expr <- substitute(expr)
##     sapply(x, function(it) eval(expr, envir = list(it = it), enclos = envir), simplify = F)
## }

## smap <- function(x, fun, ...) sapply(x, fun, ..., simplify = T)
## smapit <- function(x, expr, envir = parent.frame()){
##     expr <- substitute(expr)
##     sapply(x, function(it) eval(expr, envir = list(it = it), enclos = envir), simplify = T)
## }

## mapcbind <- function(x, fun, ...) do.call(cbind, sapply(x, fun, ..., simplify = F))
## mapitcbind <- function(x, expr){
##     expr <- substitute(expr)
##     do.call(cbind, sapply(x, function(it) eval(expr), simplify = F))
## }

## maprbind <- function(x, fun, ...) do.call(rbind, sapply(x, fun, ..., simplify = F))
## mapitrbind <- function(x, expr){
##     expr <- substitute(expr)
##     do.call(rbind, sapply(x, function(it) eval(expr), simplify = F))
## }

## mapdt <- function(x, fun, ...)
##     rbindlist(sapply(x, function(x) fun(x), ..., simplify = F), use.names = T, fill = T)

rdtit <- function(x, expr, envir = parent.frame()){
    expr <- substitute(expr)
    out <- sapply(x, function(it) eval(expr, envir = list(it = it), enclos = envir), simplify = F)
    rbindlist(out, use.names = T, fill = T)
}

cdtit <- function(x, expr, envir = parent.frame()){
    expr <- substitute(expr)
    out <- sapply(x, function(it) eval(expr, envir = list(it = it), enclos = envir), simplify = F)
    as.data.table(out)
}

selectit <- function(data, expr, envir = parent.frame()){
    expr <- substitute(expr)
    which <- sapply(data, function(it) eval(expr, envir = list(it = it), enclos = envir), simplify = T)
    if(is.data.table(data)) data[, which, with = F]
    else data[, which]
}

with_wd <- function(wd, expr){
    owd <- setwd(wd)
    on.exit(setwd(owd))
    eval(expr)
}

E <- function(str){
    eval(knitr::knit_expand(text = str))
}

A <- function(str, val, envir = .GlobalEnv){
    assign(knitr::knit_expand(text = str), val, envir = envir)
}

G <- function(str){
    get(knitr::knit_expand(text = str))
}

merge_lists <- function(...){
    lists <- lapply(list(...), function(x){
        tt <- setNames(x, allNames(x))
        names(tt)[!nzchar(names(tt))] <- ".X."
        tt
    }) 
    names <- unique(unlist(lapply(lists, function(x) if(is.list(x)) allNames(x))))
    out <- list()
    for(nm in names){
        inner <- del_nulls(lapply(lists, function(x) if(exists(nm, x)) x[[nm]]))
        islist <- sapply(inner, is.list)
        out[[nm]] <- del_nulls(c(list(unique(do.call(c, inner[!islist]))),
                                 do.call(merge_lists, inner[islist])))
    }
    ## empt_names <- names(out %in% c("", ".X."))
    ## empties <- out[empt_names]
    ## if(length(empties)){
    ##     out[empt_names] <- NULL
    ##     out[[".X."]] <- unlist(empties, recursive = F)
    ## }
    for(nm in names(out))
        if(is.list(out[[nm]]) && length(out[[nm]]) == 1 && !is.list(out[[nm]][[1]]))
            out[[nm]] <- out[[nm]][[1]]
    out
}

## str(merge_lists(list(c("a", "b"),
##                      a = c(1, 2),
##                      b = c(3, 4)),
##                 list(a = list(c(5, 6),
##                               d = 34),
##                      c = 7),
##                 list(a = list(c(10, 11),
##                               d = c(44, 55)))))

numerize <- function(x,
                     date2int = F, fact2int = F, chr2fact = F,
                     chr_drop = T, fact_drop = F, date_drop = F){
  if(is.data.frame(x))
    cdtit(x, numerize(it,
                      date2int = date2int, fact2int = fact2int, chr2fact = chr2fact,
                      date_drop = date_drop, fact_drop = fact_drop, chr_drop = chr_drop))
  else
    if((is.POSIXt(x) || is.Date(x))){
      if(date2int)
        as.integer(x)
      else if(date_drop)
        NULL
      else x
    } else if(is.factor(x)){
      if(fact2int)
        as.integer(x)
      else if(fact_drop)
        NULL
      else x
    } else if(is.character(x)) {
      if(chr2fact)
        as.factor(x)
      else if(chr_drop)
        NULL
      else x          
    } else x
}

## hierarchical get
hget <- function(hlist, hnames){
  if(!is.list(hnames))
    if(length(hnames) == 1) hlist[[hnames]]
    else Recall(hlist[[hnames[[1]]]], hnames[-1])
  else
    lapply(hnames, function(nm) hget(hlist, nm))
}

## tt <- list(a = list(b = 232, c = list(a = 33, b = "aaa")))
## hget(tt, c("a", "b"))
## hget(tt, c("a", "c", "b"))

hfilter <- function(hlist, ..., keep_names = c(), check_all_levels = T){
    keep_names <- c(..., keep_names)
    out <- list()
    for(nm in names(hlist)){
        if(nm %in% keep_names)
            out[[nm]] <- 
                        if(is.list(hlist[[nm]]) && check_all_levels)
                            out[[nm]] <- hfilter(hlist[[nm]], keep_names, check_all_levels)
                        else hlist[[nm]]
        else if(is.list(hlist[[nm]]))
            out[[nm]] <- hfilter(hlist[[nm]], keep_names, check_all_levels)
    }
    out
}

## tt <- list(a = list(b = 232, c = list(a = 33, b = "aaa")))
## hfilter(tt, "c", F)
## hfilter(tt, c("c", "b"))
## hfilter(tt, c("c", "a"))

multiple_matches <- function(dt){
    dt <- unique(dt)
    lens <- sapply(dt, ulen)
    lead_var <- dt[[which.min(lens)]]
    dups <- lead_var[duplicated(lead_var)]
    dt[lead_var %in% dups]
}

setsimdiff <- function(A, B){
    list(`A-B` = setdiff(A, B),
         `B-A` = setdiff(B, A))
}


dt_cols <- function(regexp){
    ## fixme: be more sophisticated in lookup
    pf <- parent.frame(3)
    .SD <- get("x", envir = pf)
    .SD[, grep(regexp, names(.SD)), with = F]
}


impute_missing_by_group <- function(dt, by = NULL, vars = NULL, fun = NULL, info = T){
    if(is.null(fun))
        fun <- function(x) mean(x, na.rm = T)
    if(is.null(vars)){
        vars <- names(dt)[sapply(dt, is.double)]
    }
    if (info)
        catlog("imputing on variables ", paste(vars, collapse = ", "))
    if(is.list(by)) {
        for(.by in by){
            catlog("Imputing by ", paste(.by, collapse = ", "))
            impute_missing_by_group(dt, by = .by, vars = vars, fun = fun, info = F);gc()
        }
        dt
    } else {
        if(!all(vars %in% names(dt)))
            stop(sprintf("vars %s are not in dt", paste(setdiff(vars, names(dt)), sep = ", ")))
        dt[, (vars) := {
            vars %>% set_names %>% 
                map(function(nm){
                    x <- .SD[[nm]]
                    if(is.numeric(x)){
                        nas <- is.na(x)
                        x[nas] <- mean(x, na.rm = T)
                    } 
                    x
                })
        }, .SDcols = vars, by = by]
        dt
    }
}

fast_paste0 <- function(str1, str2){
    if((length(str1) != length(str2)) && !(length(str1) == 1 || length(str2) == 1)){
        if(length(str1) > length(str2)){
            str2 <- rep_len(str2, length(str1))
        } else {
            str1 <- rep_len(str1, length(str1))
        }
    }
    c_fast_paste0(str1, str2)
}


renv_package_deps <- function(lockfile = NULL) {
  packs <- names(renv:::lockfile(lockfile)$data()$Packages)

  suppressPackageStartupMessages({
    library(dplyr)
    library(purrr)
  })

  cat("=== pkg --> dependencies ===\n")
  cat(paste(packs, "-->", map_chr(packs, ~ paste(names(renv:::renv_package_dependencies(.)), collapse = ","))), sep = "\n")

  cat("=== pkg <-- dependents ===\n")
  set_names(packs) %>%
    map_dfr(~ list(dep = setdiff(names(renv:::renv_package_dependencies(.)), .)), .id = "pack") %>%
    group_by(dep) %>%
    summarise(pkg = paste(pack, collapse = ",")) %>%
    ungroup() %>%
    arrange(pkg) %>%
    with(paste(dep, " <-- ", pkg)) %>%
    cat(sep = "\n")
}
