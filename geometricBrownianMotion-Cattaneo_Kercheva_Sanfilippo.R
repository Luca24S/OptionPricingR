# geometricBrownianMotion-Cattaneo_Kercheva_Sanfilippo.R

# Programming in Finance - Final Project
# Option Pricing using different techniques
# Date:     November 28, 2018
# Authors:  Marco Cattaneo  - marco.maria.cattaneo@usi.ch
#           Reneta Kercheva - reneta.kercheva@usi.ch 
#           Luca Sanfilippo - luca.sanfilippo@usi.ch

# The following script is responsible for implementing the Geometric Brownian Motion
# as well as run it with the Monte Carlo Simulation algorithm, compute the option 
# prices and plot the results.

# This method has been implemented for pricing the following types of options:
# - Plain Vanilla European Call / Put,
# - Plain Vanilla American Call / Put,
# - Plain Vanilla Asian Call / Put.

# ==== Simulating Asset Paths ====

# The geometricBrownianMotion() function is responsible for creating a path of
# simulated prices using the Geometric Brownian Motion for the given values.
# This function is considered a helper function and it is run by the 
# runMonteCarloOnGBM() function.

# INPUTS:
# - S0 (double):        the stock price from which we want to start simulating the
#                       future prices path (usually the latest stock price).
# - riskFree (double):  the appropriate risk free rate for pricing the option.
# - sdev (double):      the volatility of the returns of the underlying
#                       asset. In this case we use the function 
#                       computeLastYearVolatility to calculate the volatility of
#                       last year's returns.
# - TIME (double):      the time to maturity of the option in years.
# - nPeriods (integer): the number of periods in which the path has to be
#                       split.

# OUTPUT:
# - pricePath (array of doubles): an array of length nPeriods containing the
#                                 simulated price using the Geometric 
#                                 Brownian Motion formula.

geometricBrownianMotion = function (S0, riskFree, sdev, TIME, nPeriods){
  # Defining the distance between two simulated prices as dT (delta T).
  dT      <- TIME/nPeriods
  # Defining a vector of simulated prices dS which contains as first
  # element the stock price given as parameter S0. 
  dS      <- c(S0)
  # For each delta T (dT) we compute the simulated price and append it
  # to the dS vector.
  for (i in 2:nPeriods){
    # Defining the drift term
    drift         <- (riskFree - (sdev^2)/2) * dT
    # Creating the random shock
    randomShock   <- sdev * sqrt(dT) * rnorm(1)
    # Simulating the price at time i and storing it in 
    # the vector of prices dS.
    dS[i]         <- dS[i-1] * exp(drift + randomShock)
  }
  return(dS)
}

# ==== Monte Carlo on Geometric Brownian Motion ====

# The runMonteCarloOnGBM() function is responsible for running the geometricBrownianMotion()
# several times in order to produce a various possible scenarios of the future price of the
# underlying.

# INPUTS:
# - nSim (integer):     the number of simulations.
# - S0 (double):        the stock price from which we want to start simulating the
#                       future prices path (usually the latest stock price).
# - K (double):         the option strike price.
# - riskFree (double):  the appropriate risk free rate for pricing the option.
# - sdev (double):      the volatility of the returns of the underlying
#                       asset. In this case we use the function 
#                       computeLastYearVolatility to calculate the volatility of
#                       last year's returns.
# - TIME (double):      the time to maturity of the option in years.
# - nPeriods (integer): the number of periods in which the path has to be
#                       split.

# OUTPUT:
# - simulatedPaths (matrix of doubles): a matrix where each column is a simulated
#                                       path and each row is a the price for each simulation
#                                       at a given period.


runMonteCarloOnGBM = function (nSim, S0, K, riskFree, sdev, TIME, nPeriods){
  
  # The following is a safe clause that checks whether the number of
  # simulations is greater or equal to two. 
  # In the case it is not, nSim is set to 2.
  if (nSim < 2){
    cat("The number of simulations is too low.\n")
    cat("Setting the number of simulations to 2.\n")
    nSim <- 2
    cat("Running the simulation...\n")
  }
  
  # Recording time at which the simulation started.
  ptm   <- proc.time()
  
  # Storing some parameters of the function as global variables for
  # easier future access. 
  # For details about the parameters refer to the function signature.
  K           <<- K # The strike price is stored in the environment 
                    # for future and more immediate use.
  riskFree    <<- riskFree
  TIME        <<- TIME
  sdev        <<- sdev
  nSim        <<- nSim
  nPeriods    <<- nPeriods
  
  # Defining the matrix X which will contain the simulated paths.
  # The matrix X will be a nPeriods by nSim column.
  X   <-  matrix(nrow = nPeriods, ncol = nSim)
  
  # Running the geometricBrownianMotion() function for nSim times.
  # Each simulated path is stored in a different column.
  for (i in 1:nSim){
    X[, i] <-  geometricBrownianMotion(S0, riskFree, sdev, TIME, nPeriods)
  }
  
  # Making the X matrix available in the environment.
  X <<- X 
  
  # Calculating the time taken for running the simulations and printing
  # it in the console.
  totalTime <- (proc.time() - ptm) 
  cat(nSim, "simulations completed in", round(totalTime[1], digits = 3), "secs.\n")
}


# ==== Calculating the Option Price from the simulated prices ====

# The discountPayoff() function is responsible for pricing the option
# using the underlying price simulations previously generated by the 
# runMonteCarloOnGBM() function.
# In the case the strike price is totally out-of-the-money, the function
# will return NA since it is not possible to price the option.

# With this function the following options can be priced:
#   > Plain Vanilla European Call and Put,
#   > Plain Vanilla American Call and Put,
#   > Plain Vanilla Asian Call and Put.

# INPUTS:
# - type (string):      the type of option.
# (non case-sensitive)  One of the following: call, put.
# - variant (string):   the variant of the option.
# (non case-sensitive)  One of the following: european, american, asian.

# OUTPUT:
# - optionPrice (double): the price of the option based on the strike price
#                         stored in the environment, the type of option and
#                         the variant.

discountPayoff <- function(type = "call", variant = "european"){
  
  # To make the parameters case unsensitive, the tolower() function
  # is used, which castfolds the string to a whole lower case string
  # so that the program runs even if the type or variant are specified
  # in any combination of upper / lower case characters.
  type    <- tolower(type)
  variant <- tolower(variant)
  
  # The payoff array will be used to store the payoffs of each simulation.
  payoff <- c()
  # The payoff array is indexed using a separate index, i, which will be 
  # incremented by 1 only when a payoff is added to the array.
  i <- 1
  
  # The following block is reponsible for pricing a Call option.
  if(type == "call"){
    
    # In the case the option to be priced is a European call, 
    # the payoff at the end of the path is computed per each
    # simulation, if the payoff is positive, it will be added
    # to the payoff array.
    if (variant == "european"){
      for (g in 1:nSim){
        result <- max((X[nPeriods, g]) - K, 0)
        if (result > 0){
          payoff[i] <- result
          i <-  i + 1
        }
      }
      # The average of all the payoffs in the array
      # is then discounted to today using the risk free rate.
      # The result is the price of the Plain Vanilla European Call.
      return(mean(payoff) * exp(-riskFree * TIME))
    } 
    
    # When this option is an Asian Call, the following block
    # is triggered. In this case, the average stock price for each
    # simulation is taken, from this average the strike price is 
    # subtracted, if the result is <= 0 it is discarded, otherwise
    # it gets added to the payoff array.
    else if (variant == "asian"){
      for (g in 1:nSim){
        result <- max((mean(X[, g])) - K, 0)
        if (result > 0){
          payoff[i] <- result
          i <-  i + 1
        }
      }
      # The average of all the payoffs in the array
      # is then discounted to today using the risk free rate.
      # The result is the price of the Plain Vanilla Asian Call.
      return(mean(payoff) * exp(-riskFree * TIME))
    } 
    
    # In the case of the Plain Vanilla American Call,
    # at each period of each simulation (path) the function
    # compares the payoff of the European Call to the stock
    # price at that specific period net of the strike price. 
    # In the case the payoff of the latter is greater it means
    # that the American Call option is exercised. The discounted
    # payoff is then stored into the payoff array.
    # This analysis is performed starting from time 1 to time T
    # (total number of periods). 
    else if (variant == "american"){
      for (g in 1:nSim){
        for (k in 1:nPeriods){
          # Computing the discounted payoff of the option
          # when stock price is k.
          am <- exp(-riskFree * TIME) * max(X[k, g] - K, 0)
          eu <- exp(-riskFree * TIME) * max((X[nPeriods, g]) - K, 0)
          if (am > eu){
            payoff[i] <- am
            i <- i + 1
          }
        }
      }
      # Once each simulation has been tested for the earliest
      # opportunity of exercising the american option, the average 
      # of the array containing all the payoffs is returned.
      # The result is the price of a Plain Vanilla American Call.
      return(mean(payoff))
    }
  }
  
  # The following block is reponsible for pricing a Put option.  
  if(type == "put"){
    
    # In the case the put option to be priced is European, 
    # the payoff at the end of the path is computed per each
    # simulation, if the payoff is positive, it will be added
    # to the payoff array.
    if (variant == "european"){
      for (g in 1:nSim){
        # In the case of the put option, the payoff is computed
        # as Strike Price minus Stock Price at maturity.
        result <- max(K - X[nPeriods, g], 0)
        if (result > 0){
          payoff[i] <- result
          i <-  i + 1
        }
      }
      # The average of all the payoffs in the array
      # is then discounted to today using the risk free rate.
      # The result is the price of the Plain Vanilla European Put.
      return(mean(payoff) * exp(-riskFree * TIME))
    }
    
    # For a Plain Vanilla Asian Put option, the setting is the same
    # as for a Plain Vanilla Asian Call option, however, the payoff
    # is computed as Strike Price minus the average of the Stock Prices
    # along the entire path.
    if (variant == "asian"){
      for (g in 1:nSim){
        result <- max(K - (mean(X[, g])), 0)
        if (result > 0){
          payoff[i] <- result
          i <-  i + 1
        }
      }
      # The payoff array that holds all the positive payoffs
      # is the discounted to today at the given risk free rate.
      # The result is the price of a Plain Vanilla Asian Put.
      return(mean(payoff) * exp(-riskFree * TIME))
    } 
    
    # The Plain Vanilla American Put option works the same way,
    # as the Call American Put, however, also in this case, the 
    # payoff is computed in the opposite way, as Strike Price minus
    # Stock price at that specific period along the path.
    # The discounted payoff of the European Put Option is compared to the
    # payoff of the American Put Option. As soon as the latter is greater,
    # the American Put is exercised, its payoff is stored in the payoff array.
    # This check is run for each period in each simulated path 
    # (as in the American call).
    else if (variant == "american"){
      for (g in 1:nSim){
        for (k in 1:nPeriods){
          # Computing the discounted payoff of the option
          # when stock price is k. The payoff
          # is computed as Strike Price minus the stock price.
          am <- exp(-riskFree * TIME) * max(K - X[k, g], 0)
          eu <- exp(-riskFree * TIME) * max(K - (X[nPeriods, g]), 0)
          if (am > eu){
            payoff[i] <- am
            i <- i + 1
          }
        }
      }
      # The price of a Plain Vanilla American Put option is the 
      # average of all the positive payoffs discounted.
      return(mean(payoff))
    }
  } 
  
  # In the case the user enters a type of option that is not "put" or "call"
  # the following message is displayed to the console.
  # Please note that this hardly ever happens since this is a helper function, whose
  # parameters are carefully checked at a higher level. In other words, the user
  # will probabily never call this function directly.
  else {
    cat("Type of option not recognized, please use one of the following: 'put' or 'call'\n")
  }
}


# ==== Plotting the simulated paths ====

# The plotPricePaths() function is responsible for plotting
# the simulated paths in a single plot, with each path being
# plotted in a different color.
# The plot is automatically scaled on the y axis based on the
# maximum and minimum prices generated in the simulations.

plotPricePaths <- function(){
  cat("Plot in progress...\n") # To keep the user entertained.
  
  # First the function creates a plot using the first simulated
  # price path.
  # Since the number of simulations is usually high (> 100), the
  # tickness of the lines has been kept very low (lwd = 0.1).
  plot((X[, 1]), 
       lwd = 0.1, 
       type = "l", 
       ylim = c(min(X), max(X)),
       ylab = "Price",
       xlab = "Periods",
       main = paste("Brownian Motion simulation of", 
                    nSim,
                    "paths"))
  
  # Then, for each other simulated path, the function
  # adds a new price trajectory to the plot.
  for (i in 2:ncol(X)){
    temp <- X[, i]
    lines(temp, lwd = 0.1, col = i)
  }
  
  # Adding a legend containing the risk free rate
  # and the volatility used in the simulation.
  legend(0, max(X), 
         legend = c(paste("Risk-free:", round(riskFree, digits = 4)),
                    paste("Volatility:", round(sdev, digits = 4))),
         col = c("red", "red"),
         title = "Simulation details",
         seg.len = 0,
         fill = "grey",
         box.col = "lightgrey")
}


brownianMotionOptionPricing <- function(type, variant, todaysPrice, K, riskFree, sdev, TIME, nSim = 2000, nPeriods = 1825){
  cat("Starting Monte Carlo Simulation...\n")
  runMonteCarloOnGBM(nSim, S0 = todaysPrice, K, riskFree,
                     sdev, 
                     TIME, nPeriods)
  plotPricePaths()
  res <- discountPayoff(type, variant)
  return(res)
  
}

# ==== Testing ====

# brownianMotionOptionPricing("put", "european", 100, 100, 0.01, 0.02, 5)

# runMonteCarloOnGBM(nSim = 2000, S0 = 98, K = 100, riskFree = 0.003,
#                     sdev = computeLastYearVolatility("MSFT"),
#                     TIME = 3, nPeriods = 1825)

 # discountPayoff("call", "european")
 # discountPayoff("call", "asian")
 # discountPayoff("call", "american")
 # discountPayoff("put", "european")
 # discountPayoff("put", "asian")
 # discountPayoff("put", "american")

# In the case an invalid argument is passed to the function.
# discountPayoff("abc", "asian")

# The function automatically castfolds the string.
# discountPayoff("CaLl", "EUROPean") 

# It will run with default arguments "call" and "european".
# discountPayoff() 

# ==== Plotting ====
# plotPricePaths()

# We hereby certify that
# – We have written the program ourselves except for clearly marked pieces of code 
# – We have tested the program and it ran without crashing
# Marco Cattaneo, Reneta Kercheva, Luca Sanfilippo