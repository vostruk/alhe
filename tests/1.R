##library(stringr)
library(alhe)


it<-5
it2<-5
message(sprintf("Simple tests start"))

################################################################
## 2D Rosenbrock Banana function, global minimum at about (1,1)
message(sprintf("2D Rosenbrock Banana function, global minimum at about (1,1)"))
##changing population number from 30 to 390
fr <- function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
Ilosc_populacji <- double(20)
Rosenbrock_min<- double(20)

for (i in 1:20){
  w<-abc(fr, 2, list(NP=40, lb=-2.028, ub=2.028, critical=200))
  Ilosc_populacji[i] <- i
  Rosenbrock_min[i]<- w$y
}
plot(Ilosc_populacji, Rosenbrock_min, type = "p",
     main = "Wykres 1.1: 2D Rosenbrock Banana function, NP=40",
     xlab = "Cykl",
     ylab = "Minimum globalne",
     ylim=c(0, 0.2))

n <- 30
for (i in 1:10){
  w<-abc(fr, 2, list(NP=n, lb=-2.028, ub=2.028, critical=200))
  Ilosc_populacji[i] <- n
  Rosenbrock_min[i]<- w$y
  n<- n+6
}

plot(Ilosc_populacji, Rosenbrock_min, type = "p")
title(main = "Wykres 1.2: 2D Rosenbrock Banana function, changing NP") #, xlab = "Minimum globalne", ylab = "Ilość populacji"
##changing max limit cycle from 30 to 200
n<-30
max_cycle<-double(20)
for (i in 1:it2){
  w<-abc(fr, 2, list(lb=-2.028, ub=2.028, critical=n))
  max_cycle[i] <- n
  Rosenbrock_min[i]<- w$y
  n<- n+10
}
plot(max_cycle, Rosenbrock_min, type = "p")
title(main = "Wykres 1.3 2D Rosenbrock Banana function, changing maximum cycle")#, xlab = "Minimum globalne", ylab = "Ilość cykli"


n<-30
## 5D sphere, global minimum at about (0,0,0,0,0)
D5_sphere_min<- double(20)
message(sprintf("5D sphere, global minimum at about (0,0,0,0,0)"))
fs <- function(x) sum(x^2)
for (i in 1:it){
  w<-abc(fs, 5, list(NP=n,lb=-100, ub=100, critical=200))
  Ilosc_populacji[i] <- n
  D5_sphere_min[i]<- w$y
  n<- n+6
}
plot(Ilosc_populacji, D5_sphere_min, type = "p")
title(main = "Wykres 2.1: 5D sphere, changing NP") #, xlab = "Minimum globalne", ylab = "Ilość populacji"

## "wild" function , global minimum at about -15.81515

message(sprintf("wild function , global minimum at about -15.81515"))
n<-30
wildfun_min<-double(20)
fw <- function (x)
  10*sin(0.3*x)*sin(1.3*x^2) + 0.00001*x^4 + 0.2*x+80
for (i in 1:it){
  w<-abc(fw, 1, list(NP=n, lb=-100, ub=100, critical=100))
  Ilosc_populacji[i] <- n
  wildfun_min[i]<- w$y
  n<- n+6
}
plot(Ilosc_populacji, wildfun_min, type = "p")
title(main = "Wykres 3.1: 1D wild function, changing NP ") #, xlab = "Minimum globalne", ylab = "Ilość populacji"





## Griewank function, global minimum at 0
message(sprintf("Griewank function, global minimum at 0"))
fg <- function(x)
  sum(x*x)/4000-prod(cos(x/sqrt(1:2)))+1
abc(fg, 1, list(lb=-100, ub=100, critical=100))

# Rastrigin function, global minimum at (0,0)
message(sprintf("Rastrigin function, global minimum at (0,0)"))
fra <- function(x)
  20 + x[1]^2 + x[2]^2 - 10*(cos(2*pi*x[1]) + cos(2*pi*x[2]))
abc(fra, 2, list(lb=-100, ub=100, critical=100))


# 10D Rastrigin function, global minimum at 0
message(sprintf("10D Rastrigin function, global minimum at 0"))
fra10 <- function(x) sum(x^2 - 10*cos(2*pi*x) + 10)
abc(fra10, 10, list(lb=-600, ub=600, critical=500))

message(sprintf("End of tests"))
