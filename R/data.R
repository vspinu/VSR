
#' Fetch and save locally in batches
#'
#' Generic fetcher that fetches data in specified batches and saves it into a
#' folder.
#'
#' `fetch_to_dir()` will always re-fetch the last file to avoid "holes" from
#' incomplete previous fetches.
#'
#' @param dir directory to save into. Each batch will be saved in the file
#'   `dir/beg--end.rds`; created if not exists.
#' @param exp expression that must return an object for each batch. During the
#'   evaluation of the expressions the following variables are dynamically
#'   bound: `IBEG` and `IEND` are set to the limits of the currently processed
#'   inner interval, `file` is the file where the output will be stored.
#' @param beg,end fetch between these dates (converted internally with
#'   [`lubridate::as_date()`]) and normalized with [`lubridate::floor_date()`]
#'   in order to avoid re-fetches even if `beg-end` intervals don't overlap
#'   exactly between calls.
#' @param period period to iterate over with. Defaults to "month".
#' @param verbose either boolean or a string "new-only" to report only newly
#'   fetched files (aka no up-to-date message).
#' @examples
#'
#' \dontrun{
#'
#'   DIR <- "./data/tmp/ba"
#'
#'   fetch_to_dir(DIR,
#'                beg = "2019-10-01", end = "2019-10-10",
#'                period = "week",
#'                fetch_ba(beg = IBEG, end = IEND))
#'
#'   dir(DIR)
#'
#' }
#'
#' @export
fetch_to_dir <- function(dir, expr,
                         beg = "2020-01-01", end = today(),
                         period = "month", compress = TRUE,
                         verbose = "new-only",
                         redo_last = FALSE) {
  dir.create(dir, recursive = T, showWarnings = F)

  bkps <- dir(dir, "\\.bkp", full.names = TRUE)
  if (length(bkps) > 0) {
    for (bkp in bkps)
      file.remove(bkp)
  }

  rds <- sort(dir(dir, "\\.rds$", full.names = TRUE))
  begs <- ymd(sub("^([-0-9]+)\\.\\..*", "\\1", basename(rds)))
  if (any(is.na(begs)))
    stop("Incorrect format of rds files. Must be of the form YYYY-MM-DD..YYYY-MM-DD.rds")
  if (redo_last) {
    last_tmp_file <- last_file <- ""
    if (length(rds) > 0) {
      last_file <- rds[which.max(begs)]
      last_tmp_file <- paste0(last_file, ".bkp")
      file.rename(last_file, last_tmp_file)
    }
    on.exit({
      if (!completed && !file.exists(last_file))
        file.rename(last_tmp_file, last_file)
      if (file.exists(last_tmp_file))
        file.remove(last_tmp_file)
    })
  }
  commpleted <- FALSE

  expr <- substitute(expr)
  end <- min(today(), as_date(end))
  penv <- parent.frame()
  env <- new.env(parent = penv)
  env[["IBEG"]] <- floor_date(as_date(beg), unit = period, week_start = 1)
  while (env[["IBEG"]] < end) {
    env[["IEND"]] <- env[["IBEG"]] %m+% period(1, units = period)
    file <- glue("{dir}/{env[['IBEG']]}..{min(today(), env[['IEND']])}.rds")
    if (file.exists(file)) {
      if (isTRUE(verbose))
        catlog("Fetching {file} ... [up to date]")
    } else {
      if (isTRUE(verbose) || identical(verbose, "new-only"))
        catlog("Fetching {file} ...")
      obj <- eval(expr, envir = env)
      if (NROW(obj) > 0)
        saveRDS(obj, file, compress = compress)
    }
    env[["IBEG"]] <- env[["IEND"]]
  }
  completed <- TRUE
}


#' @rdname fetch_to_dir
#' @description `readbindRDS()`: Map over rds files in a directory, rbind into a
#'   data.frame, and make rows unique. Used to quickly read data fetched with
#'   `fetch_to_dir()`.
#' @param key key to unique-fy and sort by the final data.table. Later rows have precedence.
#' @param recursive logical; whether to recur into sub-directories
#' @param verbose logical; whether to print loading messages
#' @param processor processing function of one parameter
#' @export
readbindRDS <- function(dir, beg = NULL, end = NULL, key = NULL, recursive = TRUE, verbose = FALSE,
                        processor = NULL, processor_period = NULL) {
  files <- dir(dir, pattern = "\\.rds$", full.names = TRUE, recursive = recursive)
  if (length(files) == 0)
    warning("No rds files in {dir}. Wrong dir?")

  fnames <- basename(files)
  parser <- if (grepl("T", fnames[[1]])) ymd_hms else ymd
  begs <- parser(sub("^([-0-9T:]+)\\.\\..*", "\\1", fnames))
  ends <- parser(sub("^([-0-9T:]+)\\.\\.([-0-9T:]+).*", "\\2", fnames))

  if (!is.null(beg) || !is.null(end)) {
    which <- TRUE
    if (!is.null(beg)) {
      beg <- as_date(beg)
      which <- which & ends >= beg
    }
    if (!is.null(end)) {
      end <- as_date(end)
      which <- which & begs < end
    }
    files <- files[which]
    begs <- begs[which]
    ends <- ends[which]
  }

  out <-
    if (is.null(processor_period)) {
      map(files,
          function(f) {
            if (verbose)
              catlog("loading {f} ...")
            x <- readRDS(f)
            if (is.null(processor))
              x
            else
              processor(x)
          })
    } else {
      if (is.null(processor))
        stop("`processor_period` supplied but `processor` is NULL")
      groups <- split(files, floor_date(begs, unit = processor_period, week_start = 1))
      imap(groups,
           function(ifiles, nm) {
             if (verbose)
               catlog("loading data for {processor_period} {nm} ..")
             processor(rbindlist(map(ifiles, readRDS), fill = TRUE))
           })
    }

  out <- rbindlist(out, fill = TRUE)

  if (!is.null(key)) {
    setkeyv(out, key)
    unique(out, by = key, fromLast = TRUE)
  } else {
    unique(out)
  }
}
