x <- rnorm( 1000 )

res_cpp <- top_index( x, 30L )
res_r   <- tail( order(x), 30L )  
identical(res_cpp, res_r)

top_index(letters, 5)

x <- rnorm(1e5)
microbenchmark( 
    R_order = tail(order(x), 100),
    cpp2    = top_index( x, 100 )
)



tt <- floor(runif(1e5, 0, 100))
tt <- as.integer(floor(runif(1e5, 0, 100)))

microbenchmark(r = table(tt), c = c_tab(tt))

all(table(tt) == c_tab(tt)$counts)

ftab(tt[1:2], 5)
ftab(c(1, 2, 3, 4, 5, 3, 4, 5, 4, 5, 5), 3)
