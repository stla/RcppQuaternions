library(onion)
n <- 2000
o1 <- rquat(n)
o2 <- rquat(n)

oo1 <- sapply(1:n, function(i) list(as.numeric(o1[i])))
oo2 <- sapply(1:n, function(i) list(as.numeric(o2[i])))

quatProdV <- Vectorize(quatProd)

library(microbenchmark)
microbenchmark(
  onion = o1*o2,
  EigenV = quatProdV(oo1, oo2),
  Eigen = mapply(quatProd, oo1 ,oo2)
)

microbenchmark(
  onion = rquat(2000, rand="unit"),
  Eigen = rversors_(2000)
)

