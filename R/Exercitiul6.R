#Ex6


EX6 <- function(f, g){
  
  calc_medie <- function(x) {
    x * f(x) * g(x)
  }
  
  calc_patrat_medie <- function(x) {
    x * x * f(x) * g(x)
  }
  
  out <- tryCatch(
    {
      medie <- integrate(f = Vectorize(calc_medie), lower = -Inf, upper = Inf)$value
      medie_patrat <- integrate(f = Vectorize(calc_patrat_medie), lower = -Inf, upper = Inf)$value
      dispersie <- medie_patrat - medie * medie
    },
    error=function(f) {
      message("Functia primita este divergenta, prin urmare nu putem calcula media!")
      return(NA)
    }
  )
  
  result <- list("Media variabilei aleatoare continue:" = medie, "Dispersia variabilei aleatoare continue:" = dispersie)
  
  if(is.na(out))
    return(out)
  return(result)
  
}

f <- function(x) {
  if(x >= 0 && x <= 1)
    (exp(1)*(exp(-x)+exp(x)))/(exp(2)-1)
  else 0
}

g <- function(x) {
  if ((x >= 0) && (x <= pi)) {
    sin(x) / 2
  } else {
    0
  }
}


EX6(f, g)