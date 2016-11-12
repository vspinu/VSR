x <- rnorm( 1000 )

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

expect_equal(tdf, structure(list(bid = c(1, 1, 1, 2.2, 2.2, 2.2, 2.2, -Inf),
                                 ask = c(Inf,  2, 2, 3, 3, Inf, Inf, Inf)),
                            .Names = c("bid", "ask"), row.names = c(NA, -8L), class = "data.frame"))
