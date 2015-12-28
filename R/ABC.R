#' Implementation of Artificial Bee Colony(ABC) algorithm
#' for the purposes of ALHE class on Warsaw University of Technology
#'
#' Finds a local minimums of the function(aka "the best food sources")
#'
#' @param goal f(x) f(x) - funkcja celu obliczana dla argumentu x
#' @param dim wymiarowosc dla ktorej prowadzone beda obliczenia
#' @param pars obiekt z pozostalymi parametrami metody:
#'          - NP: ilosc pszczol,
#'          - FoodNumber: ilosc zrodl jedzenia
#'          - lb:  dolna granica zasiegu przeszukiwania - dla przyspieszenia
#'          - ub:  gorna granica parametrow - dla przyspieszenia
#'          - limit: zrodlo jedzenia, ktore nie moze byc ulepszone w tylu probach jest porzucane przez pszczoly
#'          - maxCycle: maksymalna ilosc cykli(iteracji) - "stop" kryterium zatrzymania
#'          - optbin:  TRUE jezeli chcemy optymalizowac binarnie [1,0]
#'          - critical: ograniczenie na niezmiennosc rozwiazania - dla przyspieszenia
#'
#' @return res - wynik działania metody, przy czym:
#'     - res$x to wektor odpowiadający najlepszemu rozwiązaniu,
#'     - res$y to wartość f. celu dla tego wektora
#'
#' @examples
#' abc(function(x) {100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2}, 2, c(1,1), lb=-2.028, ub=2.028, critical=200)
#'
#' @export
abc <- function(goal, dim, pars)
{

  if(!("NP" %in% names(pars))) pars$NP <- 40
  if(!("FoodNumber" %in% names(pars))) pars$FoodNumber <- pars$NP/2
  if(!("lb" %in% names(pars))) pars$lb <- -Inf
  if(!("ub" %in% names(pars))) pars$ub <- +Inf
  if(!("limit" %in% names(pars))) pars$limit = 100
  if(!("maxCycle" %in% names(pars))) pars$maxCycle = 1000
  if(!("optbin" %in% names(pars))) pars$optbin=FALSE
  if(!("critical" %in% names(pars))) pars$critical=50

  pars$lb <- rep(pars$lb, dim)
  pars$ub <- rep(pars$ub, dim)
  pars$lb[is.infinite(pars$lb)] <- -(.Machine$double.xmax*1e-10)
  pars$ub[is.infinite(pars$ub)] <- +(.Machine$double.xmax*1e-10)
  # Initial params
  Foods       <- matrix(double(pars$FoodNumber*dim), nrow=pars$FoodNumber)
  f           <- double(pars$FoodNumber)  #point - history wartosc f-ji celu dla danego rozwiazania
  fitness     <- double(pars$FoodNumber)  #point - history quality of the point -
  trial       <- double(pars$FoodNumber)  #point - history
  prob        <- double(pars$FoodNumber)  #point - history
  solution    <- double(dim)  #model
  ObjValSol   <- double(1)
  FitnessSol  <- double(1)
  neighbour   <- integer(1)
  param2change<- integer(1)
  GlobalMin   <- double(1)      #model
  GlobalParams<- double(dim)    #model
  #GlobalMins  <- double(runtime)
  r           <- integer(1)

  # Fun
 # fun <- function(par) goal(par, ...)

  # Fitness function - calculates quality
  evaluation <- function(f_wart)
  {
    if (f_wart >= 0) return(1/(f_wart + 1))
    else return(1 + abs(f_wart))
  }
  # evaluation(f[1])

  # The best food source is memorized
  MemorizeBestSource <- function()
  {
    change <- 0
    for(i in seq(1,pars$FoodNumber)) {
      if (f[i] < GlobalMin) {
        change <- change + 1
        GlobalMin <<- f[i]

        # Replacing new group of parameters
        GlobalParams <<- Foods[i,]
      }
    }
    # Increasing persistance
    if (!change) p <<- p + 1
  }

  # Variables are initialized in the range [pars$lb,pars$ub]. If each parameter has
  # different range, use arrays pars$lb[j], pars$ub[j] instead of pars$lb and pars$ub
  # Counters of food sources are also initialized in this function

  init <- function(index, firstinit=FALSE, ...) {
    if (pars$optbin) Foods[index,] <<- runif(dim) > .5
    else {
      if (!firstinit) {
        Foods[index,] <<- sapply(1:dim, function(k) runif(1,pars$lb[k],pars$ub[k]) ) #uniform distr rand deriv
      }
      else {
        # For the first initialization we set the bees at
        # specific places equaly distributed through the
        # bounds.
        Foods[index,] <<-
          sapply(1:dim, function(k) {
            seq(pars$lb[k],pars$ub[k],length.out=pars$FoodNumber)[index]
          }
          )
      }
    }

    solution <<- Foods[index,]

    f[index] <<- goal(solution)

    fitness[index] <<- evaluation(f[index])
    trial[index] <<- 0

  }
  # init(2)

  # All food sources are initialized
  initialize <- function(Foods) {
    sapply(1:pars$FoodNumber, init, firstinit=TRUE)

    GlobalMin <<- f[1]
    GlobalParams <<- Foods[1,]
    return (list(Foods, fitness, trial, prob))
  }

  # initial()


  SendEmployedBees <- function() {
    for (i in 1:pars$FoodNumber) {
      r <- runif(1)
      # The parameter to be changed is determined randomly
      param2change <- floor(r*dim) + 1

      # A randomly chosen solution is used in producing a mutant solution of the solution i
      neighbour <- floor(r*pars$FoodNumber) + 1

      # Randomly selected solution must be different from the solution i
      while(neighbour==i)
        neighbour <- floor(runif(1)*pars$FoodNumber) + 1

      solution <<- Foods[i,]

      # v_{ij}=x_{ij}+\phi_{ij}*(x_{kj}-x_{ij})
      r <- runif(1)

      if (pars$optbin) solution[param2change] <<- r > 0.5
      else {
        solution[param2change] <<-
          Foods[i,param2change]+
          (Foods[i,param2change]-Foods[neighbour,param2change])*(r-0.5)*2

        # if generated parameter value is out of boundaries, it is shifted onto the boundaries
        if (solution[param2change]<pars$lb[param2change])
          solution[param2change]<<-pars$lb[param2change]

        if (solution[param2change]>pars$ub[param2change])
          solution[param2change]<<-pars$ub[param2change]
      }

      ObjValSol <<- goal(solution)
      FitnessSol <<- evaluation(ObjValSol)

      # a greedy selection is applied between the current solution i and its mutant*/
      if (FitnessSol>fitness[i]) {
        # If the mutant solution is better than the current solution i, replace the solution with the mutant and reset the trial counter of solution i*/
        trial[i] <<- 0;
        #for(j in 1:dim) Foods[i,j] <<- solution[j]
        Foods[i,] <<- solution
        f[i]<<- ObjValSol
        fitness[i]<<-FitnessSol
      }
      else {
        # the solution i can not be improved, increase its trial counter*/
        trial[i] <<- trial[i]+1
      }
    }
  }


  # A food source is chosen with the probability which is proportioal to its quality*/
  # Different schemes can be used to calculate the probability values*/
  # For example prob(i)=fitness(i)/sum(fitness)*/
  # or in a way used in the metot below prob(i)=a*fitness(i)/max(fitness)+b*/
  # probability values are calculated by using fitness values and normalized by dividing maximum fitness value*/
  evaluateList <- function(points,evaluation) {
    maxfit <- fitness[1]
    for (i in 1:pars$FoodNumber)
      if (fitness[i] > maxfit) maxfit <- fitness[i]

      prob <<- .9*(fitness/maxfit) + .1
      prob[is.nan(prob)]  <<- .1
      return(points)
  }

  SendOnlookerBees <- function()
  {
    # Onlooker Bee phase
    i <- 1
    t <- 0
    while (t < pars$FoodNumber)
    {
      r <- runif(1)
      # choose a food source depending on its probability to be chosen
      if (r < prob[i]) {
        t <- t + 1
        r <- runif(1)

        # The parameter to be changed is determined randomly
        param2change <- floor(r*dim) + 1

        # A randomly chosen solution is used in producing a mutant solution of the solution i
        neighbour <- floor(r*pars$FoodNumber) + 1

        #Randomly selected solution must be different from the solution i*/
        while(neighbour==i)
          neighbour <- floor(runif(1)*pars$FoodNumber) + 1

        solution <<- Foods[i,]

        r <- runif(1)

        if (pars$optbin) solution[param2change] <<- r > .5
        else
        {
          solution[param2change] <<-
            Foods[i,param2change]+
            (Foods[i,param2change]-Foods[neighbour,param2change])*(r-0.5)*2

          # if generated parameter value is out of boundaries, it is shifted onto the boundaries*/
          if (solution[param2change]<pars$lb[param2change])
            solution[param2change] <<- pars$lb[param2change]

          if (solution[param2change]>pars$ub[param2change])
            solution[param2change] <<- pars$ub[param2change]

        }

        ObjValSol <<- goal(solution)
        FitnessSol <<- evaluation(ObjValSol)

        # a greedy selection is applied between the current solution i and its mutant*/
        if (FitnessSol>fitness[i])
        {
          # If the mutant solution is better than the current solution i, replace the solution with the mutant and reset the trial counter of solution i*/
          trial[i] <<- 0
          Foods[i,] <<- solution

          f[i]<<-ObjValSol
          fitness[i]<<-FitnessSol
        } #if the solution i can not be improved, increase its trial counter*/
        else trial[i] <<- trial[i]+1
      }
      i <- i + 1
      if (i==pars$FoodNumber) i <- 1
      # end of onlooker bee phase
    }
    return(Foods)
  }

  # determine the food sources whose trial counter exceeds the "limit" value.
  # In Basic ABC, only one scout is allowed to occur in each cycle*/

  SendScoutBees <- function() {
    maxtrialindex <- 1
    for (i in 1:pars$FoodNumber) {
      if (trial[i] > trial[maxtrialindex]) maxtrialindex <- i
    }

    if (trial[maxtrialindex] >= pars$limit) init(maxtrialindex)
  }


 selection <- function(history, model){
   sel <- SendOnlookerBees()
   return(sel)
 }

  initModel <- function(history){
    MemorizeBestSource()
    SendEmployedBees()
    evaluateList(NULL, NULL)
    return(GlobalParams)
  }


  modelUpdate <- function(selectedPoints, oldModel){
    MemorizeBestSource()
    return(GlobalParams)
  }

  variation <- function(selectedPoints, newModel){
    SendScoutBees()
    SendEmployedBees()
    return(Foods)
  }
  #push a LIST of points into the history
  historyPush<-function(oldHistory, newPoints)
  {
    #    newHistory<-c(oldHistory,newPoints)
    #    return (newHistory)
    return(Foods)
  }
  #read a LIST of points pushed recently into the history
  historyPop<-function(history, number)
  {
    stop=length(history)
    start=max(stop-number+1,1)
    return(history[start:stop])
  }

  aggregatedOperator<-function(history, oldModel)
  {
    selectedPoints<-selection(history, oldModel)
    newModel<-modelUpdate(selectedPoints, oldModel)
    newPoints<-variation(selectedPoints, newModel)
    return (list(newPoints=newPoints,newModel=newModel))
  }

  p <- 0
  history <- initialize(Foods) #return initialized foods - points which bees will estimate
  model <- initModel(history)

  iter <- 0
  while ((iter <- iter + 1) < pars$maxCycle && p <= pars$critical)
  {
    aa <- aggregatedOperator(history, model)
    aa$newPoints <- evaluateList(aa$newPoints, model)
    history <- historyPush(history, aa$newPoints)
    model <- aa$newModel
  }

  return(
    list(
      x=GlobalParams,
      y=goal(GlobalParams)
    )
  )
}
