STORAGE_DIR <- "./storage"

saveit <- function(obj, fname = deparse(substitute(obj))){
    if(!dir.exists(STORAGE_DIR))
        dir.create(STORAGE_DIR, F, T)
    dir <- sprintf("%s/%s", STORAGE_DIR, dirname(fname))
    if(!dir.exists(dir))
        dir.create(dir, showWarnings = T, recursive = T)
    saveRDS(obj, sprintf("%s/%s.rds", dir, basename(fname)), compress = F)
}

readit <- function(obj, fname = deparse(substitute(obj)), env = parent.frame()){
    ## call is of same form as save1
    obj_name <- deparse(substitute(obj))
    obj <- readRDS(sprintf("%s/%s.rds", STORAGE_DIR, fname))
    assign(obj_name, obj, env = env)
    invisible(obj)
}

loadit <- readit
