normalise <- function(x) {
  avg <- mean(x)
  std <- sd(x)
  ans <- (x-avg)/std
  return(ans)
}  