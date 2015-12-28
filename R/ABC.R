#' Implementation of Artificial Bee Colony(ABC) algorithm
#' for the purposes of ALHE class on Warsaw University of Technology
#'
#' Finds a local minimums of the function(aka "the best food sources")
#'
#' @param goal f(x) f(x) - funkcja celu obliczana dla argumentu x
#' @param dim wymiarowosc dla ktorej prowadzone beda obliczenia
#' @param par # wektor z parametrami dla danej konkretnej funkcji
#'
#' @return res - wynik działania metody, przy czym:
#'  - res$x to wektor odpowiadający najlepszemu rozwiązaniu,
#'  -res$y to wartość f. celu dla tego wektora
#'
#' @examples
#' abc(function(x) {100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2}, 2, c(1,1), lb=-2.028, ub=2.028, criter=200)
#'
#' @export
abc <- function(
  goal,
  dim,
#  pars = ls(               # obiekt z parametrami metody
    par,
 #   D=length(par),     # ilosc(wymiarowosc parametrow) - dla sprawdzenia
    ...,               # argumenty funckcji (M, x0, X, etc.)
    NP = 40,           # ilosc pszczol
    FoodNumber = NP/2, # ilosc zrodl jedzenia
    lb = -Inf,         # dolna granica parametrow - dla przyspieszenia
    ub = +Inf,         # gorna granica parametrow - dla przyspieszenia
    limit = 100,       # zrodlo jedzenia, ktore nie moze byc ulepszone w tylu probach jest porzucane przez pszczoly
    maxCycle = 1000,   # maksymalna ilosc cykli(iteracji) - stop kryterium
    optbin=FALSE, # TRUE jezeli chcemy optymalizowac binarnie [1,0]
    criter=50
 # )
)
{

  # Checking limits
  if (length(lb)>0) lb <- rep(lb, dim)
  if (length(ub)>0) ub <- rep(ub, dim)
  lb[is.infinite(lb)] <- -(.Machine$double.xmax*1e-10)
  ub[is.infinite(ub)] <- +(.Machine$double.xmax*1e-10)

  # Initial params
  Foods       <- matrix(double(FoodNumber*dim), nrow=FoodNumber)
  f           <- double(FoodNumber)  #point - history wartosc f-ji celu dla danego rozwiazania
  fitness     <- double(FoodNumber)  #point - history quality of the point -
  trial       <- double(FoodNumber)  #point - history
  prob        <- double(FoodNumber)  #point - history
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
    for(i in seq(1,FoodNumber)) {
      if (f[i] < GlobalMin) {
        change <- change + 1
        GlobalMin <<- f[i]

        # Replacing new group of parameters
        GlobalParams <<- Foods[i,]
      }
    }
    # Increasing persistance
    if (!change) persistance <<- persistance + 1
  }

  # Variables are initialized in the range [lb,ub]. If each parameter has
  # different range, use arrays lb[j], ub[j] instead of lb and ub
  # Counters of food sources are also initialized in this function

  init <- function(index, firstinit=FALSE, ...) {
    if (optbin) Foods[index,] <<- runif(dim) > .5
    else {
      if (!firstinit) {
        Foods[index,] <<- sapply(1:dim, function(k) runif(1,lb[k],ub[k]) ) #uniform distr rand deriv
      }
      else {
        # For the first initialization we set the bees at
        # specific places equaly distributed through the
        # bounds.
        Foods[index,] <<-
          sapply(1:dim, function(k) {
            seq(lb[k],ub[k],length.out=FoodNumber)[index]
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
    sapply(1:FoodNumber, init, firstinit=TRUE)

    GlobalMin <<- f[1]
    GlobalParams <<- Foods[1,]
    return (list(Foods, fitness, trial, prob))
  }

  # initial()


  SendEmployedBees <- function() {
    for (i in 1:FoodNumber) {
      r <- runif(1)
      # The parameter to be changed is determined randomly
      param2change <- floor(r*dim) + 1

      # A randomly chosen solution is used in producing a mutant solution of the solution i
      neighbour <- floor(r*FoodNumber) + 1

      # Randomly selected solution must be different from the solution i
      while(neighbour==i)
        neighbour <- floor(runif(1)*FoodNumber) + 1

      solution <<- Foods[i,]

      # v_{ij}=x_{ij}+\phi_{ij}*(x_{kj}-x_{ij})
      r <- runif(1)

      if (optbin) solution[param2change] <<- r > 0.5
      else {
        solution[param2change] <<-
          Foods[i,param2change]+
          (Foods[i,param2change]-Foods[neighbour,param2change])*(r-0.5)*2

        # if generated parameter value is out of boundaries, it is shifted onto the boundaries
        if (solution[param2change]<lb[param2change])
          solution[param2change]<<-lb[param2change]

        if (solution[param2change]>ub[param2change])
          solution[param2change]<<-ub[param2change]
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
  evaluateList <- function() {
    maxfit <- fitness[1]
    for (i in 1:FoodNumber)
      if (fitness[i] > maxfit) maxfit <- fitness[i]

      prob <<- .9*(fitness/maxfit) + .1
      #     prob[is.nan(prob)]  <<- .1
  }

  SendOnlookerBees <- function()
  {
    # Onlooker Bee phase
    i <- 1
    t <- 0
    while (t < FoodNumber)
    {
      r <- runif(1)
      # choose a food source depending on its probability to be chosen
      if (r < prob[i]) {
        t <- t + 1
        r <- runif(1)

        # The parameter to be changed is determined randomly
        param2change <- floor(r*dim) + 1

        # A randomly chosen solution is used in producing a mutant solution of the solution i
        neighbour <- floor(r*FoodNumber) + 1

        #Randomly selected solution must be different from the solution i*/
        while(neighbour==i)
          neighbour <- floor(runif(1)*FoodNumber) + 1

        solution <<- Foods[i,]

        # v_{ij}=x_{ij}+\phi_{ij}*(x_{kj}-x_{ij}) */
        r <- runif(1)

        if (optbin) solution[param2change] <<- r > .5
        else
        {
          solution[param2change] <<-
            Foods[i,param2change]+
            (Foods[i,param2change]-Foods[neighbour,param2change])*(r-0.5)*2

          # if generated parameter value is out of boundaries, it is shifted onto the boundaries*/
          if (solution[param2change]<lb[param2change])
            solution[param2change] <<- lb[param2change]

          if (solution[param2change]>ub[param2change])
            solution[param2change] <<- ub[param2change]

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
      if (i==FoodNumber) i <- 1
      # end of onlooker bee phase
    }
    return(Foods)
  }

  # determine the food sources whose trial counter exceeds the "limit" value.
  # In Basic ABC, only one scout is allowed to occur in each cycle*/

  SendScoutBees <- function() {
    maxtrialindex <- 1
    for (i in 1:FoodNumber) {
      if (trial[i] > trial[maxtrialindex]) maxtrialindex <- i
    }

    if (trial[maxtrialindex] >= limit) init(maxtrialindex)
  }


 selection <- function(history, model){
   sel <- SendOnlookerBees()
   return(sel)
 }

initModel <- function(history){
  MemorizeBestSource()
  SendEmployedBees()
  evaluateList()
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

  persistance <- 0
  history <- initialize(Foods) #return initialized foods - points which bees will estimate
  model <- initModel(history)

  iter <- 0
  #
  while ((iter <- iter + 1) < maxCycle)
  {
    selectedPoints <- selection(history, model)  #agrop
    newModel <- modelUpdate(selectedPoints, model)  #agrop
    if (persistance > criter) break     #opcjonalne - optymalizacja przy tluczeniu sie w tym samym miejscu
    newPoints <- variation(selectedPoints, newModel)              #agrop

    evaluateList()
    history <- historyPush(history, newPoints)
    model <- newModel
  }

  return(
    list(
      x=GlobalParams,
      y=goal(GlobalParams)
    )
  )
}
