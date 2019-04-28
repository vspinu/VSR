
locate_ips <- function(ips){
  library(RCurl)
  library(jsonlite)
  out <- lapply(ips,
                function(ip){
    Sys.sleep(.1)
    js <- NULL
    while(is.null(js)){
      cat("processing", ip, "\n")
      try(js <- fromJSON(getURL(sprintf("https://freegeoip.net/json/%s", ip))))
    }
    js
  })
  do.call(rbind, out)
}


norm_country_names <- function(country){
  subs_reg <- matrix(c(" *\\(.*\\) *", "", 
                       ".*Myanmar.*", "Myanmar",
                       ".*Inmarsat.*", "Inmarsat",
                       ## "Sint Maarten", "Anguilla", ## Not in vonage tables :()
                       "Antarctica.*", "Antarctica",
                       ".*Macedonia.*", "Macedonia", 
                       "St\\.", "Saint",
                       "Rep\\.", "Republic",
                       "Dem\\.", "Democratic",
                       ".*Great Britain.*", "Great Britain",
                       ".*United Kingdom.*", "Great Britain",
                       "Democratic.*Korea", "North Korea",
                       "United States", "U.S.",
                       ".*Turks.*Caicos.*", "Turks And Caicos",
                       ".*Hong Kong.*", "Hong Kong",
                       ".*Macao.*", "Macao",
                       ".*Timor.*", "East Timor", 
                       "&", "and"),
                     ncol = 2, byrow = T)

  subs <- matrix(c("Reunion Island", "Mayotte and Reunion Islands",
                   "Russia", "Russian Federation",
                   "Western Samoa", "Samoa",
                   "Faroe Islands", "Faeroe Islands",
                   "State of Palestine", "Palestine", 
                   "Guinea Bissau", "Guinea",
                   "United Republic of Tanzania", "Tanzania", 
                   "Saint Vincent and the Grenadines", "Saint Vincent and Grenadines",
                   "Brunei Darussalam", "Brunei", 
                   "Vietnam", "Viet Nam",
                   "Cabo Verde", "Cape Verde"),
                 ncol = 2, byrow = T)

  out <- country
  for(i in 1:nrow(subs)){
    which <- out == subs[[i, 1]]
    out[which] <- subs[[i, 2]]
  }
  for(i in 1:nrow(subs_reg))
    out <- gsub(subs_reg[[i, 1]], subs_reg[[i, 2]], out)
  out
}

umerge_pfips <- function(df, pfips = acs::fips.place){
  ## uniquly merge DF with PFIPS using regexp match on cities return only
  ## matched cities (one per unique matched city!!!)
  stopifnot(!is.null(df$city))
  pfips <- copy(DT(pfips))
  setnames(pfips, tolower(names(pfips)))
  setkey(pfips, "state", "placename")
  bystate <- !is.null(df$state)
  if(!bystate)
    message("No 'state' variable. Merging by city only.")
  by <- if(bystate) c("state", "city") else "city"
  setkeyv(df, by)
  match_city <- function(df){
    df[, {
      tcity <- city
      if(bystate) {
        tstate <- state                   
        pfips[.(tstate)][grepl(tcity, placename)]
      } else {
        pfips[grepl(tcity, placename)]
      }
    }, by = by]
  }
  df_matched <- match_city(df)
  ## need to match twice to get west/south/etc stuff right
  df[!(df$city %in% df_matched$city), city := gsub("(west|south|north|east)+ ", "", city, T)]
  df_matched <- match_city(df)
  setkey(df_matched, state, city)
  df_matched[, `:=`(len = nchar(placename),
    ct = grepl("city|town", placename, T),
    fsA = funcstat == "A",
    ncdp = type != "Census Designated Place")]
  ## "city" or "town" in the name
  df_matched2 <- df_matched[, if(.N > 1 && any(ct)) .SD[ct] else .SD, by = by]
  ## administrative unit
  df_matched2 <- df_matched2[, if(.N > 1 && any(fsA)) .SD[fsA] else .SD, by = by]
  ## administrative unit 2 (non CDP)
  df_matched2 <- df_matched2[, if(.N > 1 && any(ncdp)) .SD[ncdp] else .SD, by = by]
  ## take the one with the shorteds name
  df_matched2 <- df_matched2[, if(.N > 1) .SD[which.min(len)] else .SD, by = c("state", "city")]
  df_matched2[, c("len", "ct", "fsA", "ncdp")] <- NULL
  setkey(df_matched2, state, city)
  df_matched2
}

acs_tables <- function(table_numbers,
                       regnameexclude = NULL, regnamekeep = NULL,
                       regcodemexclude = NULL, regcodemkeep = NULL){
  vars <- DT(do.call(c, lapply(unname(table_numbers), function(n) acs::acs.lookup(table.number = n)))@results)
  if(length(regnameexclude)){
    subset <- !grepl(regnameexclude, vars$variable.name)
    if(length(regnamekeep))
      subset <- subset | grepl(regnamekeep, vars$variable.name)
    vars <- vars[subset]
  } else if (length(regnamekeep)) {
    vars <- vars[grepl(regnamekeep, vars$variable.name)]
  }
  if(length(regcodemexclude)){
    subset <- !grepl(regcodemexclude, vars$variable.code)
    if(length(regcodemkeep))
      subset <- subset | grepl(regcodemkeep, vars$variable.code)
    vars <- vars[subset]
  } else if(length(regcodemkeep)){
    vars <- vars[grepl(regcodemkeep, vars$variable.code)]
  }
  if(length(short.name <- names(table_numbers))){
    vars$short.name <- short.name[match(vars$table.number, table_numbers)]
    vars[, short.name := if(.N > 1) paste(short.name, tolower(gsub("^ *([^ :]+).*$", "\\1", variable.name)), sep = "_")
    else short.name,
    by = "table.number"]
  }
  vars
}

acs <- function(vars, state = NULL, countyfp = NULL, placefp = "*", year = 2013, verbose = T, char_vars = "NAME"){
  ## vars is either a df returned by acs_tables or a character vector
  ## state (can be either NULL (fetch for all), scalar character name or fips)
  ## placefp=* fetch for all
  ## placefp can be a vector of fips
  ## if countyfp is supplied placefb is ignore
  library(jsonlite)
  key <- "7c5389b237b32ed1225255367f9fd2fe4824464d"
  ## str(geo.make(state = "CA", place = "760"))
  isplace <- is.null(countyfp)
  if(!is.null(state)){
    state <- geo.make(state = state)@geo.list[[1]]@api.for[["state"]]
    state <- sprintf("&in=state:%d", state)
  } else{
    if(isplace && !identical(placefp, "*"))
      stop("When placefb is not '*', you must specify the state")
    state <- ""
  }
  place <-
    if(isplace) sprintf("place:%s", paste0(placefp, collapse = ","))
    else sprintf("county:%s", paste(countyfp, collapse = ","))

  if(is.data.frame(vars)){
    vnames <- vars$short.name
    vars <- vars$variable.code
  } else {
    vnames <- NULL
  }
  
  ## Estimation + Margin 
  ## vars <- paste(rep(vars, each = 2), c("E", "M"), sep = "", collapse = ",")
  vars <- paste0(vars, "E")
  url <- sprintf("http://api.census.gov/data/%d/acs5?get=NAME,%s&for=%s%s&key=%s",
                 year, paste(vars, collapse = ","), place, state, key)
  if(verbose)
    cat("url: ", url, "\n")
  ## jsn <- RJSONIO::fromJSON(url)
  jsn <-
    tryCatch(fromJSON(url),
             error = function(e){
      cat("error:", e$message, "\n")
      NULL
    })
  out <- DF(jsn[-1, , drop = F])
  colnames(out) <- jsn[1, ]
  spc <- c("state", "place", "county")
  for(nm in setdiff(vars, char_vars))
    out[[nm]] <- as.numeric(out[[nm]])
  setnames(out, "state", "statefp")
  if(isplace){
    setnames(out, "place", "placefp")
  } else {
    setnames(out, "county", "countyfp")
  }
  out <- DT(out, key = c("statefp", if(isplace) "placefp" else "countyfp"))
  if(length(vnames)){
    vnames <-
      if(isplace) paste0("plc_", vnames)
      else paste0("cnt_", vnames)
    setnames(out, vars, vnames)
  }
  out
}

fips2region <- function(statefp, placefp){
  ## choropleth package conventions
  as.integer(paste0(statefp, placefp))
}


plot_word_map <- function(map, var, palete = rev(brewer.pal(9, 'Spectral'))){
  ## map is produced by joinCountryData2Map from rworldmap package
  library(rworldmap)
  library(RColorBrewer)
  library(classInt)
  dh(map_par <- mapCountryData(map, var, addLegend = F, ylim = c( -40, 60), 
                               catMethod = classIntervals(map[[var]], n=9, style="jenks")$brks,
                               colourPalette = palete))
  dh(do.call(addMapLegend, c(map_par, legendLabels="all" ,
                             legendWidth=0.6, legendIntervals="data")))
}
