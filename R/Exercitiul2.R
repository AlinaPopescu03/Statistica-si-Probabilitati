#2
esteDensitate <- function(f, valMin, valMax) {
  minim <-  optimize(f, interval = c(valMin, valMax))$objective
  integrala <- integrate(Vectorize(f), -Inf, Inf)$value
  if (abs(integrala - 1) < 1e-5 & minim >= 0)
    return(TRUE)
  else
    return(FALSE)
}

h1 <- function(x)
{
  if(0 <= x && x <= 10)
    return(x^2)
  else
    return(0)
}

g3 <- function(x)
{
  if(0 <= x && x <= pi)
    return(sin(x) / 2)
  else
    return(0)
}

#2
esteDensitate(h1, 0 ,2)
esteDensitate(g3, -pi, pi)
