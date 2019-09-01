# blackScholes-Cattaneo_Kercheva_Sanfilippo.R

# Programming in Finance - Final Project
# Option Pricing using different techniques
# Date:     November 28, 2018
# Authors:  Marco Cattaneo  - marco.maria.cattaneo@usi.ch
#           Reneta Kercheva - reneta.kercheva@usi.ch 
#           Luca Sanfilippo - luca.sanfilippo@usi.ch

# This formula has been implemented for pricing the following types of options:
# - Plain Vanilla European Call / Put,
# - American Digital Call / Put.

# ==== Implementing the Black-Scholes formula ====

# The blackScholes function implements the blackScholes formula.

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

# OUTPUT:
# - Option price (double): the price of the option for the given parameters.

blackScholes <- function(type, variant, todaysPrice, K, riskFree, sdev, TIME){
  
  # To make the parameters case unsensitive, the tolower() function
  # is used, which castfolds the string to a whole lower case string
  # so that the program runs even if the type or variant are specified
  # in any combination of upper / lower case characters.
  
  type    <- tolower(type)
  variant <- tolower(variant)
  
  # First the function assesses whether the option 
  # to be priced is a put or a call, once the type is determined
  # the price is computed based on the variant specified by the user
  # for the Black-Scholes it can be a European or an American Digital.
  
  if (type == "call"){
    
    if (tolower(variant) == "european"){
      # Plain Vanilla European Call option price is computed using the traditional
      # Black-Scholes formula.
      d1 <- (1/(sdev*sqrt(TIME))) * (log(todaysPrice/K) + (riskFree + sdev^2/2) * TIME)
      d2 <- d1 - sdev * TIME
      res <- pnorm(d1) * todaysPrice - pnorm(d2) * (K * exp(-riskFree * TIME))
      return(res)
    } 
    
    else if (variant == "american digital"){
      # American digital option (call) in this case is considered as a Cash-or-nothing call.
      # Which means that pays one if the price of the underlying is above the strike at
      # maturity and zero otherwise. In other words, Q (the amount of the payoff) is assumed
      # to be 1.
      d1 <- (1/(sdev*sqrt(TIME))) * (log(todaysPrice/K) + (riskFree + sdev^2/2) * TIME)
      d2 <- d1 - sdev * TIME
      res <- exp(-riskFree * TIME) * pnorm(d2)
      return(res)
    }
  } 
  
  else if(type == "put"){
    
    if (variant == "european"){
      # Plain Vanilla European Put option price is computed using the traditional
      # Black-Scholes formula.
      d1 <- (1/(sdev*sqrt(TIME))) * (log(todaysPrice/K) + (riskFree + sdev^2/2) * TIME)
      d2 <- d1 - sdev * TIME
      res <- K * exp(-riskFree * TIME) * pnorm(-d2) - todaysPrice * pnorm(-d1)
      return(res)
    } 
    
    else if (variant == "american digital"){
      # American digital option (put) in this case is considered as a Cash-or-nothing put.
      # Which means that pays one if the price of the underlying is below the strike at
      # maturity and zero otherwise. In other words, Q (the amount of the payoff) is assumed
      # to be 1.
      d1 <- (1/(sdev*sqrt(TIME))) * (log(todaysPrice/K) + (riskFree + sdev^2/2) * TIME)
      d2 <- d1 - sdev * TIME
      res <- exp(-riskFree * TIME) * pnorm(-d2)
      return(res)
    }
  } 
  
  else {
    # In the case a type of option different from put or call is specified, the following
    # message will be printed to the console. 
    # The return F (FALSE) is used to define a manual exception that will be handled by
    # the main program. 
    print("Black-Scholes formula is used only for pricing European and American Digital Options.")
    return(F)
  }
  
}


# ==== Testing ====
## Plain Vanilla
# European Call
#blackScholes("CALL","european", 100, 100, 0.01, computeLastYearVolatility("AAPL"), TIME = 5)
# European Put
#blackScholes("put","european", 100, 100, 0.01, computeLastYearVolatility("AAPL"), TIME = 5)

## American Digital
# American Digital Call
#blackScholes("call","american digital", 100, 100, 0.01, computeLastYearVolatility("AAPL"), TIME = 5)
# American Digital Put
#blackScholes("put","american digital", 100, 100, 0.01, computeLastYearVolatility("AAPL"), TIME = 5)


# We hereby certify that
# – We have written the program ourselves except for clearly marked pieces of code 
# – We have tested the program and it ran without crashing
# Marco Cattaneo, Reneta Kercheva, Luca Sanfilippo