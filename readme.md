# Financial Modeling Tools in R

This repository contains R scripts implementing financial models for option pricing and risk management. The models were developed by **Cattaneo, Kercheva, and Sanfilippo**. 

## Table of Contents

- [Financial Modeling Tools in R](#financial-modeling-tools-in-r)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Features](#features)
  - [Getting Started](#getting-started)
  - [How to Use](#how-to-use)
    - [Example:](#example)
  - [References](#references)
  - [License](#license)
  - [Authors](#authors)
---

## Overview

This project provides tools for option pricing and analysis, focusing on:

1. **Binomial Tree Model**: A discrete-time model for valuing options.
2. **Black-Scholes Model**: A continuous-time model for European option pricing.
3. **Greeks**: Calculations for risk measures (Delta, Gamma, Vega, Theta, and Rho).

These tools are designed for educational and practical purposes in financial modeling.

---

## Features

- Flexible implementation of the **Binomial Tree Model** for pricing European and American options.
- Precise computation of option prices using the **Black-Scholes formula**.
- Calculation of option Greeks for sensitivity analysis.
- Modular and reusable R scripts.

---

## Getting Started

1. Clone this repository:
   ```bash
   git clone https://github.com/your_username/financial-modeling-tools.git
   ```
2. Navigate to the directory:
   ```bash
    cd financial-modeling-tools
    ```

3. Open the .R files in your preferred R editor or IDE (e.g., RStudio).

# Scripts Description

#### `binomialTree-Cattaneo_Kercheva_Sanfilippo.R`
- Implements a binomial tree for option pricing.
- Supports European and American options.

#### `blackScholes-Cattaneo_Kercheva_Sanfilippo.R`
- Computes European option prices using the Black-Scholes formula.

#### `greeks-Cattaneo_Kercheva_Sanfilippo.R`
- Calculates option Greeks: Delta, Gamma, Vega, Theta, and Rho.

#### `main-Cattaneo_Kercheva_Sanfilippo.R`
- Combines the functionalities of the above scripts into a cohesive workflow.

---

# Requirements

### Software
- **R (≥ 4.0.0)**: Ensure R is installed on your system.

### Required Packages
- `dplyr`
- `ggplot2`
- Additional libraries specified in the scripts.

Install packages using the following command:

```r 
install.packages(c("dplyr", "ggplot2"))
```

## How to Use

1. **Load the necessary libraries.**
2. **Run the `main-Cattaneo_Kercheva_Sanfilippo.R` script** for a complete demonstration.
3. **Modify input parameters** in the scripts to customize the analysis:
   - Strike price
   - Volatility
   - Interest rate

### Example:
```r
source("main-Cattaneo_Kercheva_Sanfilippo.R")
```

## References
- **Binomial Tree Model**: [Wikipedia](https://en.wikipedia.org/wiki/Binomial_options_pricing_model)
- **Black-Scholes Formula**: [Wikipedia](https://en.wikipedia.org/wiki/Black%E2%80%93Scholes_model)
- **Greeks**: [Investopedia](https://www.investopedia.com/terms/o/optionsgreeks.asp)

---

## License

This project is licensed under the **MIT License**. See the `LICENSE` file for details.

---

## Authors
- **Cattaneo**
- **Kercheva**
- **Sanfilippo**