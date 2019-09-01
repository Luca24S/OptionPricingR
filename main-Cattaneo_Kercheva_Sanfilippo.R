# main-Cattaneo_Kercheva_Sanfilippo.R

# Programming in Finance - Final Project
# Option Pricing using different techniques
# Date:     November 28, 2018
# Authors:  Marco Cattaneo  - marco.maria.cattaneo@usi.ch
#           Reneta Kercheva - reneta.kercheva@usi.ch 
#           Luca Sanfilippo - luca.sanfilippo@usi.ch

source("computeVolatility-Cattaneo_Kercheva_Sanfilippo.R") 
source("binomialTree-Cattaneo_Kercheva_Sanfilippo.R") 
source("blackScholes-Cattaneo_Kercheva_Sanfilippo.R")
source("geometricBrownianMotion-Cattaneo_Kercheva_Sanfilippo.R")
source("greeks-Cattaneo_Kercheva_Sanfilippo.R")

## INSTRUCTIONS:
# Run the functions priceOption and run. 
# Once they are in your environment, just run line 144 or type 'run()' in your console.
# Follow the instructions and get your option priced.

priceOption <- function(type, variant, todaysPrice, strike, riskFree, volatility, maturityInYears){
  
  type <- tolower(type)
  variant <- tolower(variant)
  
  if( (type == "call" | type == "put") & variant == "european"){
    # Computing price with the Brownian Motion
    BM <- brownianMotionOptionPricing(type, variant, todaysPrice, strike, riskFree, volatility, maturityInYears)
    # Computing price with Black-Scholes
    BS <- blackScholes(type, variant, todaysPrice, strike, riskFree, volatility, maturityInYears)
    # Computing price with Binomial Tree
    BT <- binomialTreeOptionPricing(type, variant, todaysPrice, strike, riskFree, volatility, maturityInYears)
    
    cat(" - - - - - - - - - - - - - - - \n")
    cat("Option Price:\n")
    cat("Brownian Motion\t", BM, "\nBlack-Scholes\t", BS, "\nBinomial Tree\t", BT, "\n")
    cat("\nGreeks:\n")
    computeGreeks(type, variant, todaysPrice, K = strike, TIME = maturityInYears, riskFree, sdev = volatility)
  }
  
  else if ( (type == "call" | type == "put") & variant == "american"){
    # Computing price with the Brownian Motion
    BM <- brownianMotionOptionPricing(type, variant, todaysPrice, strike, riskFree, volatility, maturityInYears)
    # Computing price with Binomial Tree
    BT <- binomialTreeOptionPricing(type, variant, todaysPrice, strike, riskFree, volatility, maturityInYears)
    
    cat(" - - - - - - - - - - - - - - - \n")
    cat("Option Price:\n")
    cat("Brownian Motion\t", BM, "\nBinomial Tree\t", BT, "\n")
    cat("\nGreeks:\n")
    computeGreeks(type, variant, todaysPrice, K = strike, TIME = maturityInYears, riskFree, sdev = volatility)
  }
  else if( (type == "call" | type == "put") & variant == "asian"){
    # Computing price with the Brownian Motion
    BM <- brownianMotionOptionPricing(type, variant, todaysPrice, strike, riskFree, volatility, maturityInYears)
    cat(" - - - - - - - - - - - - - - - \n")
    cat("Option Price:\n")
    cat("Brownian Motion\t", BM, "\n")
    cat("\nGreeks:\n")
    computeGreeks(type, variant, todaysPrice, K = strike, TIME = maturityInYears, riskFree, sdev = volatility)
  }
  
  else if( (type == "call" | type == "put") & (variant == "american digital" | variant == "digital american") ){
    # Computing price with Black-Scholes
    BS <- blackScholes(type, variant = "american digital", todaysPrice, strike, riskFree, volatility, maturityInYears)
    
    cat(" - - - - - - - - - - - - - - - \n")
    cat("Option Price:\n")
    cat("Black-Scholes\t", BS, "\n")
    cat("\nGreeks:\n")
    computeGreeks(type, variant, todaysPrice, K = strike, TIME = maturityInYears, riskFree, sdev = volatility)
  }
  
  else {
    cat("I am sorry, I can't calculate the price of a", type, variant, "option.\n")
    cat("The following variants of Call/Put options are accepted: european, asian, american, american digital.")
  }
  
}

run <- function(){
  
  cat("Programming in Finance 1 - Final Project\nOption Pricing using different techniques\nDeveloped by:\n
      \t > Marco Cattaneo:\t  marco.maria.cattaneo@usi.ch\n
      \t > Reneta Kercheva:\t  reneta.kercheva@usi.ch\n
      \t > Luca Sanfilippo:\t  luca.sanfilippo@usi.ch\n
         \n")
  
  cat("Option pricing using Brownian Motion, Black-Scholes and Binary Tree.\nThis software is capable of pricing the following Put/Call options:\n
      European, Asian, American and American Digital options.\n")
  
  
  type <- readline(prompt = "Is it a CALL or a PUT? ")
  type <- tolower(type)
  
  variant <- readline(prompt="European, Asian, American? ")
  variant <- tolower(variant)

  if (variant == "american"){
    variant <- readline(prompt="Is it an American or an American Digital? ")
    variant <- tolower(variant)
  }
  
  ticker <- readline(prompt="What's the underlying stock of this option? Please specify its ticker: ")
  ticker <- toupper(ticker)
  options(warn = -1)
  downloadedData <- try(getQuote(ticker), silent = TRUE)
  
  if (class(downloadedData) != "try-error"){
    print(paste("I found a price for ", ticker))
    ans <- readline(
      prompt=paste("The latest price for", ticker, "is", downloadedData[2], "Would you like use this price for pricing the option? (y/n) "))
    if(tolower(ans) == "y" | tolower(ans) == "yes"){
     todaysPrice <- as.numeric(downloadedData[2])
    } else if (tolower(ans) == "n" | tolower(ans) == "no"){
      todaysPrice <- as.numeric(readline(prompt="Please enter the price: "))
    }
  } else {
    print(paste("Sorry, I can't find info about", ticker))
    todaysPrice <- as.numeric(readline(prompt="Please specify an underlying price: "))
  }
  
  if (class(downloadedData) != "try-error"){
    value <- computeLastYearVolatility(ticker)
    cat("The volatility of the returns for 2018 for", ticker, "is", value, "\n")
    ans <- readline(prompt=paste("Would you like to use", value, "as the volatility? (y/n) "))
    if(tolower(ans) == "y" | tolower(ans) == "yes"){
      volatility <- as.numeric(value)
    } else if (tolower(ans) == "n" | tolower(ans) == "no"){
      volatility <- as.numeric(readline(prompt="Please enter a value for the volatility: "))
    }
  } else {
    volatility <- as.numeric(readline(prompt="Please specify the volatility: "))
  }
  
  options(warn = 0)
  riskFree <- as.numeric(readline(prompt="Enter the risk free rate: "))
  strike <- as.numeric(readline(prompt="Enter the strike price of the option: "))
  maturityInYears <- as.numeric(readline(prompt="Enter the maturity in years of the option: "))
  
  args <- c("type", "variant", "todaysPrice", "strike", "riskFree", "volatility", "maturityInYears")
  for (i in 1:7){
    if(is.na(eval(as.name(args[i])))){
      cat("\t >> ERROR: Pricing halted. You left one or more arguments blank. <<\n")
      return()
      # We preferred not to use the Error or stop functions so that the debugger is not triggered.
    } 
  }
  
  cat("I am now pricing the option... Please wait.\n")
  priceOption(type, variant, todaysPrice, strike, riskFree, volatility, maturityInYears)
  
}

run()

## PLEASE NOTE:
# If the risk free rate is too high, the brownian motion will not generate "negative" paths
# this means that there may be issues when pricing a put option using the GBM.
# In the case the option is fully out-of-the-money, meaning that no simulated path will
# go past the strike price, the result will be NA. It will not be possible to price the
# option.


# ==== Testing ====
# priceOption("call", "european", todaysPrice = 100, 
#             strike = 100, riskFree = 0.01, volatility = computeLastYearVolatility("AAPL"),
#             maturityInYears = 5)
# 
# priceOption("put", "european", todaysPrice = 100, 
#             strike = 100, riskFree = 0.01, volatility = computeLastYearVolatility("MSFT"), 
#             maturityInYears = 5)
# 
# priceOption("put", "knock-in", todaysPrice = 100, 
#             strike = 100, riskFree = 0.01, volatility = computeLastYearVolatility("MSFT"), 
#             maturityInYears = 5)


# We hereby certify that
# - We have written the program ourselves except for clearly marked pieces of code 
# - We have tested the program and it ran without crashing
# Marco Cattaneo, Reneta Kercheva, Luca Sanfilippo