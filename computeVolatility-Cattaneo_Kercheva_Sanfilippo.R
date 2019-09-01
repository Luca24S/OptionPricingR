# computeVolatility-Cattaneo_Kercheva_Sanfilippo.R

# Programming in Finance - Final Project
# Option Pricing using different techniques
# Date:     November 28, 2018
# Authors:  Marco Cattaneo  - marco.maria.cattaneo@usi.ch
#           Reneta Kercheva - reneta.kercheva@usi.ch 
#           Luca Sanfilippo - luca.sanfilippo@usi.ch

# ==== Installing Quantmod ====

# The following block checks whether the user has already installed
# the "quantmod" package, in the case it is not installed, the setup
# will start. 
# Lines from 9 to 11 have been adapted from:
# https://stackoverflow.com/questions/9341635/check-for-installed-packages-before-running-install-packages
if ("quantmod" %in% rownames(installed.packages()) == FALSE) {
  install.packages("quantmod")
}
# After having confirmed the presence of "quantmod", we import
# the package which will be used to download stock data from
# Yahoo!Finance.
library(quantmod)

# ==== Compute the volatility of the stock ====

# The following script downloades stock data for a given company
# ticker, computes the returns on the Adjusted Close price and
# returns the volatility of the returns for the data after 
# January 1, 2018. 

# This purpose of this function is to simplify the calculation of the
# market volatility of a stock and can be used as a very approximative
# implied volatility for the given stock in the option pricing models
# implemented in this project.

# INPUTS:
# - ticker (string):    a valid ticker recognized by Yahoo!Finance.
# OUTPUTS:
# - standardDeviation (double):   the standard deviation of the returns
#                                 computed on the stock Adjusted close prices 
#                                 from January 1, 2018 until the latest trading
#                                 day available.

computeLastYearVolatility <- function(ticker){
  returns <- c()
  getSymbols(ticker, verbose = T)
  temp <- eval(as.name(ticker))
  temp <- temp[, 6]
  temp <- temp[index(temp) > "2018-01-01"]
  temp <- as.data.frame(temp)
  num <- temp[2:nrow(temp), 1]
  den <- temp[1:nrow(temp)-1, 1]
  for (i in 1:length(den)){
    returns[i] <- num[i]/den[i] - 1
  }
  return(sd(returns))
}

# We hereby certify that
# – We have written the program ourselves except for clearly marked pieces of code 
# – We have tested the program and it ran without crashing
# Marco Cattaneo, Reneta Kercheva, Luca Sanfilippo