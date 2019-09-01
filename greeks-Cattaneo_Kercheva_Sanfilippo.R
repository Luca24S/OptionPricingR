# greeks-Cattaneo_Kercheva_Sanfilippo.R

# Programming in Finance - Final Project
# Option Pricing using different techniques
# Date:     November 28, 2018
# Authors:  Marco Cattaneo  - marco.maria.cattaneo@usi.ch
#           Reneta Kercheva - reneta.kercheva@usi.ch 
#           Luca Sanfilippo - luca.sanfilippo@usi.ch

# ==== Calculating the Greeks using Black-Scholes ====
# The following function is responsible for computing the Greeks of the options.
# The greeks are calculated using the Black-Scholes formula and are available for
# the following kinds of options:
#   > Plain Vanilla European Call/Put
#   > Plain Vanilla American Call/Put
#   > American Digital Call/Put

# INPUTS:
# - type (string):        the type of option.
#                         One of the following: call, put.
# - variant (string):     the variant of the option.
#                         One of the following: european, american, american digital.
# - todaysPrice (double): latest stock price.
# - K (double):           the option strike price.
# - TIME (double):        the time to maturity of the option expressed in years.
# - riskFree (double):    the return of a risk free asset.
# - sdev (double):        the volatility of the returns of the underlying
#                         asset. In this case we use the function 
#                         computeLastYearVolatility to calculate the volatility of
#                         last year's returns.

# OUTPUTS:
# - greeks (array of strings and doubles):  an array containing the names of the Greeks
#                                           and their respective values.

computeGreeks = function (type, variant, todaysPrice, K, TIME, riskFree, sdev){
  # As per the other functions the strings parameters are castfolded,
  # to eliminate case sensitivity.
  type    <- tolower(type)
  variant <- tolower(variant)
  
  d1 <- (log(todaysPrice/K) + (riskFree + sdev^2/2) * TIME) / (sdev * sqrt(TIME))
  d2 <- d1 - sdev * sqrt(TIME)
  
  # For European or American Plain Vanilla Put/Call options.
  if (variant == "european" | variant == "american"){
    # for the EUROPEAN, TIME = time to maturity of the option.
    # for the AMERICAN, TIME = time at which the option is exercised.  
    if(type == "call"){
      delta  = pnorm(d1)
      gamma  = (exp(-d1^2/2))/(todaysPrice * sdev * sqrt(2 * pi * TIME))
      vega   = todaysPrice * sqrt(TIME) *  exp(-d1^2/2)/ (sqrt(2 * pi)*100)
      rho    = K * TIME * exp(-riskFree * TIME) * pnorm(d2)/100
      
      theta  = ((-todaysPrice*sdev*exp(-d1^2/2)/(sqrt(8*pi*TIME)))-(riskFree*K*exp(-riskFree*TIME)*pnorm(d2)))
      theta_calendar_days = theta/365

      cat("Delta:\t", delta, "\nGamma:\t", gamma, "\nVega:\t", vega, "\nTheta:\t", theta_calendar_days, "\nRho:\t", rho)
    }
    
    else if (type == "put"){
      delta  = (pnorm(d1)-1)
      gamma  = (exp(-d1^2/2))/(todaysPrice * sdev * sqrt(2 * pi * TIME))
      vega   = todaysPrice * sqrt(TIME) *  exp(-d1^2/2)/ (sqrt(2 * pi)*100)
      rho    = - K * TIME * exp(-riskFree * TIME) * pnorm(-d2)/100
      theta  = ((-todaysPrice*sdev*exp(-d1^2/2)/(sqrt(8*pi*TIME)))+(riskFree*K*exp(-riskFree*TIME)*pnorm(-d2)))
      theta_calendar_days = theta/365
      cat("Delta:\t", delta, "\nGamma:\t", gamma, "\nVega:\t", vega, "\nTheta:\t", theta_calendar_days, "\nRho:\t", rho)
    }
    
    else {
      cat("I am sorry, I do not know", type, "options.\n")
      cat("I can only compute the Greeks for PUT or CALL options.")
    } 
    
  }
  # For American Digital Put/Call options.
  else if (variant == "american digital" | variant == "digital american"){
    # for the AMERICAN Digital, TIME = time when the option touches the barrier.  
    if(type == "call"){
      delta  = (exp(-riskFree*TIME)* exp(-0.5*d2^2)/(sqrt(2*pi)))/(sdev * todaysPrice * sqrt(TIME))
      gamma  = -(exp(-riskFree*TIME)* d1 * exp(-0.5*d2^2)/(sqrt(2*pi)))/(sdev^2 * todaysPrice^2 * TIME)
      vega   = -(exp(-riskFree*TIME)* exp(-0.5*d2^2)/(sqrt(2*pi)))*(d1/sdev)
      rho    = - TIME * exp(-riskFree * TIME) * pnorm(d2)+ ((sqrt(TIME)/sdev) * exp(-riskFree * TIME) *(exp(-0.5*d2^2)/(sqrt(2*pi))) )
      theta  = (-(exp(-riskFree*TIME)* exp(-0.5*d2^2)/(sqrt(2*pi)))/(sdev^2 * todaysPrice^3 * TIME))*(-2*d1 + (1-d1*d2)/(sdev*sqrt(TIME)))
      theta_calendar_days = theta/365

      cat("Delta:\t", delta, "\nGamma:\t", gamma, "\nVega:\t", vega, "\nTheta:\t", theta_calendar_days, "\nRho:\t", rho)
    }
    
    else if (type == "put"){
      delta  = -(exp(-riskFree*TIME)* exp(-0.5*d2^2)/(sqrt(2*pi)))/(sdev * todaysPrice * sqrt(TIME))
      gamma  = (exp(-riskFree*TIME)* d1 * exp(-0.5*d2^2)/(sqrt(2*pi)))/(sdev^2 * todaysPrice^2 * TIME)
      vega   = (exp(-riskFree*TIME)* exp(-0.5*d2^2)/(sqrt(2*pi)))*(d1/sdev)
      rho    = (- TIME * exp(-riskFree * TIME) * (1 - pnorm(d2)) - ((sqrt(TIME)/sdev) * exp(-riskFree * TIME) * (exp(-0.5*d2^2) / (sqrt(2*pi)))))
      theta  = ((exp(-riskFree*TIME)* exp(-0.5*d2^2)/(sqrt(2*pi)))/(sdev^2 * todaysPrice^3 * TIME))*(-2*d1 + (1-d1*d2)/(sdev*sqrt(TIME)))
      theta_calendar_days = theta/365

      cat("Delta:\t", delta, "\nGamma:\t", gamma, "\nVega:\t", vega, "\nTheta:\t", theta_calendar_days, "\nRho:\t", rho)
    } 
    
    # In the case some type of option other than PUT or CALL is entered, the following is printed
    # to the console.
    else {
      cat("I am sorry, I do not know", type, "options.\n")
      cat("I can only compute the Greeks for PUT or CALL options.")
    } 
    
  } 
  
  # When an undefined variant is specified.
  else {
    cat("I am sorry, I can't calculate the Greeks of a", variant, type, "option.\n")
    cat("The following variants of Call/Put options are accepted:\n\t> European\n\t> American\n\t> American Digital")
  }
}

# ==== Testing ====
# computeGreeks("call", "european", 100, 100, 5, 0.06, 0.2)

# Example of error.
# computeGreeks("put", "vanilla", 100, 100, 5, 0.06, 0.2) 

# computeGreeks("call", "american digital", 100, 100, 5, 0.06, 0.2)
# computeGreeks("put", "american digital", 100, 100, 5, 0.06, 0.2)

# We hereby certify that
# - We have written the program ourselves except for clearly marked pieces of code 
# - We have tested the program and it ran without crashing
# Marco Cattaneo, Reneta Kercheva, Luca Sanfilippo