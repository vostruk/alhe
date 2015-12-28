library(stringr)
library(alhe)

message(sprintf("Simple tests start"))

## 2D Rosenbrock Banana function, global minimum at about (1,1)
message(sprintf("2D Rosenbrock Banana function, global minimum at about (1,1)"))
fr <- function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
abc(fr, 2, list(lb=-2.028, ub=2.028, criter=200))

## 5D sphere, global minimum at about (0,0,0,0,0)
message(sprintf("5D sphere, global minimum at about (0,0,0,0,0)"))
fs <- function(x) sum(x^2)
abc(fs, 5, list(lb=-100, ub=100, criter=200))


## "wild" function , global minimum at about -15.81515
message(sprintf("wild function , global minimum at about -15.81515"))
fw <- function (x)
  10*sin(0.3*x)*sin(1.3*x^2) + 0.00001*x^4 + 0.2*x+80
abc(fw, 1, list(lb=-100, ub=100, criter=100))

## Griewank function, global minimum at 0
message(sprintf("Griewank function, global minimum at 0"))
fg <- function(x)
  sum(x*x)/4000-prod(cos(x/sqrt(1:2)))+1
abc(fg, 1, list(lb=-100, ub=100, criter=100))

# Rastrigin function, global minimum at (0,0)
message(sprintf("Rastrigin function, global minimum at (0,0)"))
fra <- function(x)
  20 + x[1]^2 + x[2]^2 - 10*(cos(2*pi*x[1]) + cos(2*pi*x[2]))
abc(fra, 2, list(lb=-100, ub=100, criter=100))


# 10D Rastrigin function, global minimum at 0
message(sprintf("10D Rastrigin function, global minimum at 0"))
fra10 <- function(x) sum(x^2 - 10*cos(2*pi*x) + 10)
abc(fra10, 10, list(lb=-600, ub=600, criter=500))

message(sprintf("End of tests"))
