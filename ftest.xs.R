#### ftest with mean and se, sample size
ftest.xs <- function(n,x,s) {
  k <- length(n)
  sst <- sum(n*x^2)-sum(n*x)^2/sum(n)
  sse <- sum((s*sqrt(n))^2*(n-1))
  f <- (sst/(k-1))/(sse/(sum(n)-k))
  p <- df(f, df1=k-1, df2=sum(n)-k)
  return(p)
}
