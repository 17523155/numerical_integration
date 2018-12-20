#17523155  Muhammad Abdi Humanika
#17523182  Rheza daffa Pamungkas
#contoh soal Trapezoidal Rule 

trapezoid <- function(f, a, b) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  
  h <- b - a
  
  fxdx <- (h / 2) * (f(a) + f(b))
  
  return(fxdx)
}

trapezoid(f, 0, pi/2)


#contoh soal Simpson Rule


simpsons.rule <- function(f, a, b) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  
  h <- (b - a) / 2
  x0 <- a
  x1 <- a + h
  x2 <- b
  
  s <- (h / 3) * (f(x0) + 4 * f(x1) + f(x2))
  
  return(s)
}

simpsons.rule(f, 0, pi/2)