# binomialTree.R

# Programming in Finance - Final Project
# Option Pricing using different techniques
# Date:     November 28, 2018
# Authors:  Marco Cattaneo  - marco.maria.cattaneo@usi.ch
#           Reneta Kercheva - reneta.kercheva@usi.ch 
#           Luca Sanfilippo - luca.sanfilippo@usi.ch

# The following script is responsible for creating the Binomial Tree.

# This method has been implemented for pricing the following types of options:
# - Plain Vanilla European Call / Put,
# - Plain Vanilla American Call / Put.


# ==== How it's implemented ====
# The Binomial Tree is implemented using a list of lists, this structure is
# easy to maintain and efficient at the same time. 
# Each i-th element of the binomialTree list contains a list with the nodes
# at that level.
# Each node is represented by a vector c(stockPrice, payoff) which contains 
# its own data.


# ==== Setting Tree's probabilities ====

# The setProbabilities() function is responsible for computing and storing 
# the variables u, d and p in the global environment. These three variables
# are essential for building a binomial option tree.

# INPUTS:
# - sigma (double):     the volatility of the underlying asset.
# - deltaT (double):    the length of each step in the binary three expressed
#                       in months. In other words, the time that separates one
#                       node from the other. 
# - riskFree (double):  the risk free rate.

# OUTPUTS:
# - u (double):         the up value in the binomial tree.
# - d (double):         the down value in the binomial tree.
# - p (double):         the probability in the binomial tree.

setProbabilities <- function(sigma, deltaT, riskFree){
  deltaT    <<- deltaT
  riskFree  <<- riskFree
  u     <<- exp(sigma * sqrt(deltaT))
  d     <<- exp(-sigma * sqrt(deltaT))
  a     <-  exp(riskFree * deltaT)
  p     <<- (a - d)/(u - d)
}


# ==== Calculating payoff of a node ====
# The computePayoff() function is responsible for calculating the 
# payoff of the option at a specific node using the information at
# the next two nodes (up and down). The result is discounted using
# the given risk free rate and time interval (time * deltaT).
# This is a helper function used by the insertPayoff() function.

# INPUTS:
# - up (double):        the payoff at the next upper node in respect to the
#                       node for which the function is computing the payoff.
# - down (double):      the payoff at the next lower node in respect to the
#                       node for which the function is computing the payoff.
# - riskFree (double):  the risk free rate.
# - time (integer):     it is the time in the tree at which we are computing
#                       the payoff.

# OUTPUT:
# - payoff (double):    the option payoff at that specific node.

computePayoff <- function(up,
                          down,
                          riskFree, 
                          time){
  res <- exp(-riskFree * (time * deltaT)) * (p * up + (1-p) * down)
  return(res)
}


# ==== Adding the payoff of each node to the tree ====

# The insertPayoffs() function is responsible for computing and inserting
# the payoffs in the tree. No visible output is returned.

# INPUTS:
# - strike (double):    the strike price of the option to be priced.
# - type (string):      the type of option to be priced.
#                       One of the following: call, put.
# - variant (string):   the variant of the option.
#                       One of the following: european.

insertPayoffs <-  function(strike, type, variant){
  
  # To make the type parameter case unsensitive, the tolower() function
  # is used, which castfolds the string to a whole lower case string
  # so that the program runs even if the type or variant are specified
  # in any combination of upper / lower case characters.
  
  type    <- tolower(type)
  variant <- tolower(variant)
  
  # Setting up node payoffs at time T.
  # Since for the latest nodes in the tree the payoffs cannot be computed using
  # the computePayoff() function, they are computed separately based on whether
  # the option to price is a call or a put.
  if (type == "call"){
    for (i in 1:length(binomialTree)){
      binomialTree[[length(binomialTree)]][[i]][2] <<- (max(binomialTree[[length(binomialTree)]][[i]][1] - strike, 0))
    }
  } else if (type == "put"){
    for (i in 1:length(binomialTree)){
      binomialTree[[length(binomialTree)]][[i]][2] <<- (max(strike - binomialTree[[length(binomialTree)]][[i]][1], 0))
    }
  }
  
  if (variant == "european"){
    
    # The following block computes the payoff of an European
    # option using the computePayoff() function.
    # The payoffs are computed iterating from the right-most nodes
    # to the left-most node (root).
    for (i in length(binomialTree):2){
      for (k in 1:(i-1)){
        binomialTree[[i-1]][[k]][2] <<-
          computePayoff(binomialTree[[i]][[k]][2],
                        binomialTree[[i]][[k+1]][2],
                        riskFree,
                        i)
      }
    }
  }
  
  # When an american option has to be priced, the payoff of the option at
  # each node is the maximum between the instrinsic value and the early exercise.
  
  # American Call option
  
  else if (variant == "american" & type == "call"){
    
    for (i in length(binomialTree):2){
      for (k in 1:(i-1)){
        
        intrinsicValue <- 
          computePayoff(binomialTree[[i]][[k]][2],
                        binomialTree[[i]][[k+1]][2],
                        riskFree,
                        i)
        earlyExercise <- 
          max(binomialTree[[i-1]][[k]][1] - strike, 0)
        
        nodeValue <- 
          max(intrinsicValue, earlyExercise)
          
        
        binomialTree[[i-1]][[k]][2] <<-
          nodeValue
      }
    }
  }
  
  # American Put option
  else if (variant == "american" & type == "put"){
    
    for (i in length(binomialTree):2){
      for (k in 1:(i-1)){
        
        intrinsicValue <- 
          computePayoff(binomialTree[[i]][[k]][2],
                        binomialTree[[i]][[k+1]][2],
                        riskFree,
                        i)
        earlyExercise <- 
          max(strike - binomialTree[[i-1]][[k]][1], 0)
        
        nodeValue <- 
          max(intrinsicValue, earlyExercise)
        
        
        binomialTree[[i-1]][[k]][2] <<-
          nodeValue
      }
    }
  }
}

# ==== Computing the option price ====

# The computeOptionPrice() function responsible for pricing an option
# for the given type and variant. 

# INPUTS:
# - strike (double):    the strike price of the option.
# - type (string):      the type of option.
#                       One of the following: call, put.
# - variant (string):   the variant of the option.
#                       One of the following: european.
#



computeOptionPrice <- function(strike, type = "call", variant = "european"){
 
  # The payoff array is where all positive payoffs will be stored,
  # this array has a specific index, k, which is incremented only
  # when a new payoff is added to the array, therefore, it is independent
  # from time and position in the tree.
  payoff <- c()
  k <- 1
  
  # When dealing with a Plain Vanilla European Call or Put option, the  
  # following block is repsonsible for calculating the option price, which
  # in this case consists in weighting all the payoffs and discounting them
  # to today.
  # Formula for European Call and Put from:
  # http://homepage.ntu.edu.tw/~jryanwang/course/Financial%20Computation%20
  # or%20Financial%20Engineering%20(graduate%20level)/FE_Ch04%20Binomial%20Tree%20Model.pdf
  if (type == "call" & variant == "european"){
    N <- length(binomialTree)
    S0 <- binomialTree[[1]][[1]][1]
    
    for(i in 1:N){
      
      payoff[k] <-  choose(N, i) * p^(N-i) * (1-p)^i * max((S0 * u^(N-i) * d^i) - strike, 0) 
      k <- k + 1
    }
    
    optionPrice <- exp(-riskFree * (N * deltaT)) * sum(payoff)
    
    return(optionPrice)
  }
  
  else if (type == "put" & variant == "european"){
    N <- length(binomialTree)
    S0 <- binomialTree[[1]][[1]][1]
    
    for(i in 1:N){
      
      payoff[k] <-  choose(N, i) * p^(N-i) * (1-p)^i * max(strike - (S0 * u^(N-i) * d^i), 0) 
      k <- k + 1
    }
    
    optionPrice <- exp(-riskFree * (N * deltaT)) * sum(payoff)
    
    return(optionPrice)
  }
  
  # In the case of an American option, the price of the option is just the 
  # payoff at the first node (root) which has been computed by the insertPayoff()
  # function.
  
  else if ( (type == "call" & variant == "american") | (type == "put" & variant == "american") ){
    return(binomialTree[[1]][[1]][2])
  }
  
}


# ==== Building the binomial tree structure ====

# The following function is responsible for building the binomial tree.
# The data structure has been built using lists, which is an efficient and easy way
# of building a binomial tree data structure.

# INPUT:
# - time (integer):             the number of steps in the tree.
# - initialStockPrice (double): the initial stock price from which the tree has 
#                               to start.

# OUTPUT:
# - binomialTree (list of doubles): the output will be a list of lists of vectors, with 
#                                   each vector containing in the first position the 
#                                   stock price in that specific scenario.
#                                   Payoffs will be computed and added to the BST with the
#                                   insertPayoffs() function.

growBinomialTree <- function(time, 
                             initialStockPrice){
  # Creating the root of the tree.
  binomialTree <<- list()
  ptm   <- proc.time() # Recording time at which the tree creation started.
  for (i in 1:time){
    binomialTree[[i]] <<- list()
    for (k in 1:i){
      if (i == 1){
        binomialTree[[i]][k] <<- c(initialStockPrice)
      } else {
        if (k == 1){
          binomialTree[[i]][[1]] <<- c((binomialTree[[i-1]][[1]]) * u)
        } else {
          binomialTree[[i]][[k]] <<- c((binomialTree[[i-1]][[k-1]]) * d)
        }
      }
    }
  }
  totalTime <- (proc.time() - ptm)
  cat("Tree built in", round(totalTime[1], digits = 3), "secs.\n")
}


# ==== Price an option using the binomialTree ====

# The following function is responsible for putting all the pieces together, 
# it takes advantage of the functions defined above to price the option for the given
# parameters.

# INPUTS:
# - type (string):        the type of option.
#                         One of the following: call, put.
# - variant (string):     the variant of the option.
#                         One of the following: european, american digital.
# - todaysPrice (double): latest stock price.
# - K (double):           the option strike price.
# - riskFree (double):    the return of a risk free asset.
# - sdev (double):        the volatility of the returns of the underlying
#                         asset. In this case we use the function 
#                         computeLastYearVolatility to calculate the volatility of
#                         last year's returns.
# - TIME (double):        the time to maturity of the option expressed in years.
# - deltaT (double):      defines the length of each step in the binomial tree.
# (optional)              It is an optional parameter and predefined at 6/12.

# OUTPUT:
# - Option price (double): the price of the option for the given parameters.

binomialTreeOptionPricing <- function(type, variant, todaysPrice, K, riskFree, sdev, TIME, deltaT = 6/12){
  t <- round(TIME, digits = 0) * (deltaT)^(-1) # Since the binomial tree has predefined steps of 6 months,
                                   # we compute the number of steps required to compute the number
                                   # of steps. Being a discrete time model, the time is rounded.
  setProbabilities(sdev, deltaT, riskFree)
  growBinomialTree(t, todaysPrice)
  insertPayoffs(K, type, variant)
  computeOptionPrice(K, type, variant)
  
}


# ==== Testing ==== 
# (Uncomment to run)
## Plain Vanilla
# Call European
# binomialTreeOptionPricing("call", "european", todaysPrice = 100,
#                          K = 100, riskFree = 0.01, sdev = computeLastYearVolatility("AAPL"),
#                          TIME = 5)

# Put European
# binomialTreeOptionPricing("put", "european", todaysPrice = 100,
#                           K = 100, riskFree = 0.01, sdev = computeLastYearVolatility("AAPL"),
#                           TIME = 5)


# We hereby certify that
# – We have written the program ourselves except for clearly marked pieces of code 
# – We have tested the program and it ran without crashing
# Marco Cattaneo, Reneta Kercheva, Luca Sanfilippo