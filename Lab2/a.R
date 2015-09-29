library(ggplot2)
library(splines)
set.seed(1)

f <- function(x) {
  x^2 - 0.2*x^2.3333
}

d = data.frame("x"=seq(0,100,0.1))
d$f = f(d$x)

# Samples