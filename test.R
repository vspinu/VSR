x <- rnorm( 1000 )

ema(c(NA, 1:10), 0:10)

res_cpp <- top_index( x, 30L )
res_r   <- tail( order(x), 30L )  
identical(res_cpp, res_r)

top_index(letters, 5)

x <- rnorm(100)

microbenchmark( 
    tab = head(tab(x), 5),
    ftab  = ftab(x, 5))


tt <- floor(runif(1e5, 0, 100))
tt <- as.integer(floor(runif(1e5, 0, 100)))

microbenchmark(r = table(tt), c = c_tab(tt))

all(table(tt) == c_tab(tt)$counts)

ftab(tt, 5)
ftab(c(1, 2, 3, 4, 5, 3, 4, 5, 4, 5, 5, NA, NA, NA), 2)


apk(5, 1, 1:3) == c_apk1(5, 1, 1:3)
apk(5, 2, 1:3) == c_apk1(5, 2, 1:3)
apk(5, 3, 1:3) == c_apk1(5, 3, 1:3)
apk(5, 3, rev(1:3)) == c_apk1(5, 3, rev(1:3))


expect_equal(
    c_max_ob_margin(c(1, 2, 3, 1, 2, 3, 3, 2),
                    c(1, 1, 1, 0, 1, 0, 1, 0)),
    c(1, 2, 3, 3, 3, 2, 3, 3))


expect_equal(
    c_min_ob_margin(c(1, 2, 3, 1, 2, 3, 3, 2),
                    c(1, 1, 1, 0, 1, 0, 1, 0)),
    c(1, 1, 1, 2, 2, 2, 2, 3))



(tdf <- c_ob_margin(c(1, 2, 3, 2.2, 1,  3, 2, 2.2),
                    c(1, 1, 1, 1,   0,  0, 0, 0),
                    c(1, 2, 2, 1,   1,  2, 2, 1)) %>% DF)

expect_equal(tdf, structure(list(bid = c(1, 1, 1, 2.2, 2.2, 2.2, 2.2, NA),
                                 ask = c(NA,  2, 2, 3, 3, NA, NA, NA)),
                            .Names = c("bid", "ask"), row.names = c(NA, -8L), class = "data.frame"))

(tdf <- c_ob_exp_sum(c(1, 2, 3, 2.2, 1,  3, 2, 2.2),
                     c(1, 1, 1, 1,   0,  0, 0, 0),
                     c(1, 2, 2, 1,   1,  2, 2, 1),
                     c(0, 1), ns = c(1, 2)))

expect_equal(tdf,
             structure(list(ask = structure(c(NA, 0.367879441171442, 0.503214724408055, 
                                              0.449328964117222, 0.449328964117222, NA, NA, NA, NA, 1, 1.60653065971263, 
                                              0.90483741803596, 0.90483741803596, NA, NA, NA), .Dim = c(8L, 2L)),
                            bid = structure(c(NA, 0.367879441171442, 0.367879441171442, 0.584664247353834, 0.449328964117222, NA, NA, NA, NA, 1, 1, 1.51136807774859, 
                                              0.90483741803596, NA, NA, NA), .Dim = c(8L, 2L))),
                       .Names = c("ask", "bid")))



test_that("ema works correctly with theta=0 and same consequent dates", {
    date <- c(15295.3550810185, 15295.3550810185, 15295.3551851852, 
              15295.3552314815, 15295.3552777778, 15295.3580902778, 15295.3581365741, 
              15295.3582986111, 15295.3583680556, 15295.3584722222, 15295.3585185185, 
              15295.3656018519, 15295.3800462963, 15295.3992476852, 15295.4045138889, 
              15295.4118171296, 15295.4141550926, 15295.4168981481, 15295.4232523148, 
              15295.4245023148, 15295.4281018519, 15295.4316435185)
    x <- c(2.19253, 2.18509145044662, 2.19996, 2.1851, 2.19996, 2.1851, 
           2.19996, 2.1851, 2.19996, 2.1851000878661, 2.2, 2.21, 2.18031850460013, 
           2.1978075, 2.187, 2.21208, 2.21208, 2.21208, 2.23109, 2.2311, 
           2.07891998242106, 2.24809640454591)
    weight <- c(0.0940188, 
                -0.4702, 0.44052498, -0.1, 0.09350154, -0.1, 0.0940188, -0.1, 
                0.09350154, -20.28694715, 19.07266652, 1, -69.78409791, 104, 
                -0.1, 0.06188, 9.07922239, 13.57, 4, 93, 1.11754181, 0.02333085)
    expect_true(all(is.finite(ema(x, date, n = 0))))
    expect_true(all(is.finite(ema(x, date, n = 0, linear = T))))
    expect_true(all(is.finite(ema(x, date, n = 0, cum = T))))
    expect_equal(ema(x, date, n = 0), x)
    expect_equal(ema(x, date, n = 0, linear = T), x)
    expect_equal(wema(x, weight, date, n = 0, cum = T), x)
    expect_equal(wema(x, weight, date, n = 0, linear = T), x)
})

x <- c(NA, 1:10)
ema(x, 0:10)
ema(rep.int(x, 2), 0:22)

## test_that("efilter works correctly", {
##     d <- 1:5
##     x <- (1:5)*10
##     expect_equal(efilter(x, d, 1),
##                  c(12.8565871583233, 20.9256117580224, 30,
##                    39.0743882419776, 47.1434128416767))
## })



test_that("c_roll_min works as expected", {
    ix <- 1:10
    val <- 1:10
    expect_equal(c_roll_min(ix, val, 5, 0), c(1, 1, 1, 1, 1, 2, 3, 4, 5, 6))
    expect_equal(c_roll_min(ix, val, 3, 1), c(NA, 1, 1, 2, 3, 4, 5, 6, 7, 8))
    expect_equal(c_roll_min(ix, val, 5, 3), c(NA, NA, NA, 1, 1, 2, 3, 4, 5, 6))
    dates <- seq(ymd("2016-01-01", tz = "UTC"), ymd_hms("2016-01-01 00:00:9"), by = "1 secs")
    val <- seq_along(dates)
    expect_equal(c_roll_min(dates, val, 5, 0), c(1, 1, 1, 1, 1, 2, 3, 4, 5, 6))
    expect_equal(c_roll_min(dates, val, 3, 1), c(NA, 1, 1, 2, 3, 4, 5, 6, 7, 8))
    expect_equal(c_roll_min(dates, val, 5, 3), c(NA, NA, NA, 1, 1, 2, 3, 4, 5, 6))
    dates2 <- c(dates, ymd_hms(c("2016-01-01 00:00:15", "2016-01-01 00:00:17", "2016-01-01 00:00:20")))
    val2 <- seq_along(dates2)
    expect_equal(c_roll_min(dates2, val2, 5, 0), c(1, 1, 1, 1, 1, 2, 3, 4, 5, 6, 11, 11, 12))
    expect_equal(c_roll_min(dates2, val2, 3, 1), c(NA, 1, 1, 2, 3, 4, 5, 6, 7, 8, NA, 11, NA))
    expect_equal(c_roll_min(dates2, val2, 5, 3), c(NA, NA, NA, 1, 1, 2, 3, 4, 5, 6, NA, NA, 12))
    expect_equal(c_roll_min(dates2, val2, 5, 4), c(NA, NA, NA, NA, 1, 2, 3, 4, 5, 6, NA, NA, NA))
    expect_equal(c_roll_min(dates2, val2, 6, 5), c(NA, NA, NA, NA, NA, 1, 2, 3, 4, 5, NA, NA, 11))
    expect_equal(c_roll_min(dates2, val2, 20, 19), c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2))
    expect_equal(c_roll_min(dates2, val2, 200, 190), c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                                                       NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
    ix <- c(1, 2, 2, 3, 4:9)
    val <- 1:10
    expect_equal(c_roll_min(ix, val, 5, 0), c(1, 1, 1, 1, 1, 1, 2, 4, 5, 6))
    expect_equal(c_roll_min(ix, val, -1, -3), c(4, 5, 5, 6, 7, 8, 9, 10, NA, NA))
    expect_equal(c_roll_max(ix, val, -1, -3), c(5, 6, 6, 7, 8, 9, 10, 10, NA, NA))
    expect_equal(c_roll_min(ix, val, 3, 1), c(NA, 1, 1, 1, 2, 4, 5, 6, 7, 8))
    expect_equal(c_roll_min(ix, val, 5, 3), c(NA, NA, NA, NA, 1, 1, 2, 4, 5, 6))
})

test_that("c_roll_max works as expected", {
    dates <- seq(ymd("2016-01-01", tz = "UTC"), ymd_hms("2016-01-01 00:00:9"), by = "1 secs")
    dates2 <- c(dates, ymd_hms(c("2016-01-01 00:00:15", "2016-01-01 00:00:17", "2016-01-01 00:00:20")))
    val2 <- seq_along(dates2)
    expect_equal(c_roll_max(dates2, val2, 5, 0), c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
    expect_equal(c_roll_max(dates2, val2, 0, -1), c(2, 3, 4, 5, 6, 7, 8, 9, 10, NA, NA, NA, NA))
    expect_equal(c_roll_max(dates2, val2, 0, -2), c(3, 4, 5, 6, 7, 8, 9, 10, 10, NA, 12, NA, NA))
    expect_equal(c_roll_max(dates2, val2, 3, 1), c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9, NA, 11, NA))
    expect_equal(c_roll_max(dates2, val2, 5, 4), c(NA, NA, NA, NA, 1, 2, 3, 4, 5, 6, NA, NA, NA))
    expect_equal(c_roll_max(dates2, val2, 5, 4), c_roll_max(dates2, val2, 5, 3.001))
    expect_equal(c_roll_max(dates2, val2, 5.1, 5), c(NA, NA, NA, NA, NA, 1, 2, 3, 4, 5, NA, NA, 11))
    expect_equal(c_roll_max(dates2, val2, 20, 19), c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2))
    expect_equal(c_roll_max(dates2, val2, 200, 190), c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                                                       NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
})

test_that("c_roll_max skips NAs", {
    dates <- seq(ymd("2016-01-01", tz = "UTC"), ymd_hms("2016-01-01 00:00:9"), by = "1 secs")
    dates <- c(dates, ymd_hms(c("2016-01-01 00:00:15", "2016-01-01 00:00:17", "2016-01-01 00:00:20")))
    val <- seq_along(dates)
    val[c(2, 5, 11)] <- NA
    cbind(dates, val)
    expect_equal(c_roll_max(dates, val, 5, 0), c(1, 1, 3, 4, 4, 6, 7, 8, 9, 10, NA, 12, 13))
    expect_equal(c_roll_max(dates, val, 5, 1), c(NA, 1, 1, 3, 4, 4, 6, 7, 8, 9, NA, NA, 12))
})

test_that("c_roll_mean works as expected", {
    ix <- 1:10
    val <- 1:10
    expect_equal(c_roll_mean(ix, val, 5, 0), c(1, 1.5, 2, 2.5, 3, 4, 5, 6, 7, 8))
    expect_equal(c_roll_mean(ix, val, 3, 1), c(NA, 1, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5))
    val[c(2, 5, 8)] <- NA
    expect_equal(c_roll_mean(ix, val, 3, 1), c(NA, 1, 1, 3, 3.5, 4, 6, 6.5, 7, 9))
})
