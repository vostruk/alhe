##library(stringr)
library(alhe)


it<-20
it2<-20
message(sprintf("Simple tests start"))

################################################################
## 2D Rosenbrock Banana function, global minimum at about (1,1)
message(sprintf("2D Rosenbrock Banana function, global minimum at about (1,1)"))

fr <- function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
Ilosc_populacji <- double(20)
Rosenbrock_min<- double(20)

for (i in 1:20){
  w<-abc(fr, 2, list(NP=40, lb=-2.028, ub=2.028, critical=200))
  Ilosc_populacji[i] <- i
  Rosenbrock_min[i]<- w$y
}
plot(Ilosc_populacji, Rosenbrock_min, type = "p",
     main = "Wykres 1.1: 2D Rosenbrock Banana function, NP = 40",
     xlab = "Cykl",
     ylab = "Minimum globalne",
     ylim=c(0, 0.05))

n <- 30
##changing population number from 30
for (i in 1:it){
  w<-abc(fr, 2, list(NP=n, lb=-2.028, ub=2.028, critical=200))
  Ilosc_populacji[i] <- n
  Rosenbrock_min[i]<- w$y
  n<- n+6
}

plot(Ilosc_populacji, Rosenbrock_min, type = "p",
     main = "Wykres 1.2: 2D RB function, zmieniajaca sie populacja",
     ylab = "Minimum globalne",
     xlab = "Ilosc populacji",
     ylim=c(0, 0.05))
n<-30
max_cycle<-double(20)
for (i in 1:it2){
  w<-abc(fr, 2, list(lb=-2.028, ub=2.028, critical=n))
  max_cycle[i] <- n
  Rosenbrock_min[i]<- w$y
  n<- n+10
}
plot(max_cycle, Rosenbrock_min, type = "p",
     main = "Wykres 1.3 2D RB function, zmieniajaca sie max liczba cykli",
     ylab = "Minimum globalne",
     xlab = "Liczba cykli",
     ylim=c(0, 0.1))

#######################################################
## 5D sphere, global minimum at about (0,0,0,0,0), criticalStart=200
D5_sphere_min<- double(20)
message(sprintf("5D sphere, global minimum at about (0,0,0,0,0)"))
fs <- function(x) sum(x^2)

for (i in 1:20){
  w<-abc(fs, 5, list(NP=40,lb=-100, ub=100, critical=100))
  Ilosc_populacji[i] <- i
  D5_sphere_min[i]<- w$y
}
plot(Ilosc_populacji, D5_sphere_min, type = "p",
     main = "Wykres 2.1: 5D sphere, NP=40",
     ylab = "Minimum globalne",
     xlab = "Cykl",
     ylim = c(0, 0.0002))

##changing population number
n<-30
for (i in 1:it){
  w<-abc(fs, 5, list(NP=n,lb=-100, ub=100, critical=100))
  Ilosc_populacji[i] <- n
  D5_sphere_min[i]<- w$y
  n<- n+6
}
plot(Ilosc_populacji, D5_sphere_min, type = "p",
     main = "Wykres 2.2: 5D sphere, zmieniajaca sie ilosc populacji",
     ylab = "Minimum globalne",
     xlab = "Ilosc populacji",
     ylim = c(0, 0.0002))

##changing max limit cycle from 30 to 200
n<-30
max_cycle<-double(20)
for (i in 1:it2){
  w<-abc(fs, 5, list(NP=40,lb=-100, ub=100, critical=n))
  max_cycle[i] <- n
  D5_sphere_min[i]<- w$y
  n<- n+10
}
plot(max_cycle, D5_sphere_min, type = "p",
     main = "Wykres 2.3 5D sphere, zmieniajaca sie maksymalna liczba cykli",
     ylab = "Minimum globalne",
     xlab = "Liczba cykli")

#######################################################################
## "wild" function , global minimum at about -15.81515
message(sprintf("wild function , global minimum at about -15.81515"))
n<-30
wildfun_min<-double(20)
fw <- function (x)
  10*sin(0.3*x)*sin(1.3*x^2) + 0.00001*x^4 + 0.2*x+80

for (i in 1:20){
  w<-abc(fw, 1, list(NP=40, lb=-100, ub=100, critical=100))
  Ilosc_populacji[i] <- i
  wildfun_min[i]<- w$y
}
plot(Ilosc_populacji, wildfun_min, type = "p",
     main = "Wykres 3.1: 1D wild function, NP = 40",
     xlab = "Cykl",
     ylab = "Minimum globalne")

###changing population
n<-30
for (i in 1:it){
  w<-abc(fw, 1, list(NP=n, lb=-100, ub=100, critical=100))
  Ilosc_populacji[i] <- n
  wildfun_min[i]<- w$y
  n<- n+6
}
plot(Ilosc_populacji, wildfun_min, type = "p",
     main = "Wykres 3.2: 1D wild function, zmieniajaca sie ilosc populacji ",
     ylab = "Minimum globalne",
     xlab = "Ilosc populacji")
###changing cycle
n<-30
max_cycle<-double(20)
for (i in 1:it2){
  w<-abc(fw, 1, list(NP=40, lb=-100, ub=100, critical=n))
  max_cycle[i] <- n
  wildfun_min[i]<- w$y
  n<- n+10
}
plot(max_cycle, wildfun_min, type = "p",
     main = "Wykres 3.3 1D wild function, zmieniajaca sie maksymalna liczba cykli",
     ylab = "Minimum globalne",
     xlab = "Liczba cykli")

###########################################################################
## Griewank function, global minimum at 0, criticalStart=100
message(sprintf("Griewank function, global minimum at 0"))
fg <- function(x)
  sum(x*x)/4000-prod(cos(x/sqrt(1:2)))+1
##abc(fg, 1, list(lb=-100, ub=100, critical=100))
n<-30
griewank_min<-double(20)
for (i in 1:20){
  w<-abc(fg, 1, list(NP=20,lb=-100, ub=100, critical=50))
  Ilosc_populacji[i] <- i
  griewank_min[i]<- w$y
}
plot(Ilosc_populacji, griewank_min, type = "p",
     main = "Wykres 4.1: Griewank function, NP = 20",
     xlab = "Cykl",
     ylab = "Minimum globalne",
     ylim = c(0, 0.0002))

###changing population
n<-10
for (i in 1:it){
  w<-abc(fg, 1, list(NP=n,lb=-100, ub=100, critical=50))
  Ilosc_populacji[i] <- n
  griewank_min[i]<- w$y
  n<- n+6
}
plot(Ilosc_populacji, griewank_min, type = "p",
     main = "Wykres 4.2: Griewank function, zmieniajaca sie ilosc populacji ",
     ylab = "Minimum globalne",
     xlab = "Ilosc populacji",
     ylim = c(0, 0.002))
###changing cycle
n<-10
max_cycle<-double(20)
for (i in 1:it2){
  w<-abc(fg, 1, list(NP=20,lb=-100, ub=100, critical=n))
  max_cycle[i] <- n
  griewank_min[i]<- w$y
  n<- n+10
}
plot(max_cycle, griewank_min, type = "p",
     main = "Wykres 4.3 Griewank function, zmieniajaca sie maksymalna liczba cykli",
     ylab = "Minimum globalne",
     xlab = "Liczba cykli",
     ylim = c(0, 0.0002))

###########################################################################
# Rastrigin function, global minimum at (0,0), criticalStart=100
message(sprintf("Rastrigin function, global minimum at (0,0)"))
fra <- function(x)
  20 + x[1]^2 + x[2]^2 - 10*(cos(2*pi*x[1]) + cos(2*pi*x[2]))
##abc(fra, 2, list(lb=-100, ub=100, critical=100))
n<-30
rastrigin_min<-double(20)
for (i in 1:20){
  w<-abc(fra, 2, list(NP=20,lb=-100, ub=100, critical=50))
  Ilosc_populacji[i] <- i
  rastrigin_min[i]<- w$y
}
plot(Ilosc_populacji, rastrigin_min, type = "p",
     main = "Wykres 5.1: Rastrigin function, NP = 20",
     xlab = "Cykl",
     ylab = "Minimum globalne",
     ylim = c(0, 0.005))

###changing population
n<-10
for (i in 1:it){
  w<-abc(fra, 2, list(NP=n,lb=-100, ub=100, critical=50))
  Ilosc_populacji[i] <- n
  rastrigin_min[i]<- w$y
  n<- n+6
}
plot(Ilosc_populacji, rastrigin_min, type = "p",
     main = "Wykres 5.2: Rastrigin function, zmieniajaca sie ilosc populacji ",
     ylab = "Minimum globalne",
     xlab = "Ilosc populacji",
     ylim = c(0, 0.0002))
###changing cycle
n<-10
max_cycle<-double(20)
for (i in 1:it2){
  w<-abc(fra, 2, list(NP=40,lb=-100, ub=100, critical=n))
  max_cycle[i] <- n
  rastrigin_min[i]<- w$y
  n<- n+10
}
plot(max_cycle, rastrigin_min, type = "p",
     main = "Wykres 5.3 Rastrigin function, zmieniajaca sie maksymalna liczba cykli",
     ylab = "Minimum globalne",
     xlab = "Liczba cykli",
     ylim = c(0, 0.002))

###########################################################################
# 10D Rastrigin function, global minimum at 0
message(sprintf("10D Rastrigin function, global minimum at 0"))
fra10 <- function(x) sum(x^2 - 10*cos(2*pi*x) + 10)
##abc(fra10, 10, list(lb=-600, ub=600, critical=500))
n<-30
rastrigin10D_min<-double(20)
for (i in 1:20){
  w<-abc(fra10, 10, list(NP=40, lb=-600, ub=600, critical=500))
  Ilosc_populacji[i] <- i
  rastrigin10D_min[i]<- w$y
}
plot(Ilosc_populacji, rastrigin10D_min, type = "p",
     main = "Wykres 6.1: 10D Rastrigin function, NP = 40",
     xlab = "Cykl",
     ylab = "Minimum globalne")
##ylim = c(0, 0.05))

###changing population
n<-10
for (i in 1:it){
  w<-abc(fra10, 10, list(NP=n, lb=-600, ub=600, critical=500))
  Ilosc_populacji[i] <- n
  rastrigin10D_min[i]<- w$y
  n<- n+6
}
plot(Ilosc_populacji, rastrigin10D_min, type = "p",
     main = "Wykres 6.2: 10D Rastrigin function, zmieniajaca sie ilosc populacji ",
     ylab = "Minimum globalne",
     xlab = "Ilosc populacji",
     ylim = c(0, 0.0002))
###changing cycle
n<-200
max_cycle<-double(20)
for (i in 1:it2){
  w<-abc(fra10, 10, list(NP=40, lb=-600, ub=600, critical=n))
  max_cycle[i] <- n
  rastrigin10D_min[i]<- w$y
  n<- n+10
}
plot(max_cycle, rastrigin10D_min, type = "p",
     main = "Wykres 6.3 10D Rastrigin function, zmieniajaca sie maksymalna liczba cykli",
     ylab = "Minimum globalne",
     xlab = "Liczba cykli")
##ylim = c(0, 0.01))

message(sprintf("End of tests"))
