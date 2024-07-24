
##################################################################### 
# R code: Heterogeneous Portfolio Reactions to Monetary Policy Shocks
# Author: Yuya Ogawa
####################################################################

library(fpp3)
library(vars)
library(dplyr)
library(fable)
library(lpirfs)
library(readxl)
library(quantmod)
library(tidyquant)

# ================= Load Shock data ====================
# Set working directory here
setwd('Set up your working directory here')

# This data contains the monetary policy shock using Romer and Romer method
temp <- read_excel("Monetarydat.xlsx", sheet = "Monthly")

# Create a sequence of monthly dates
temp$date = seq.Date(from = as.Date("1959-01-01"), by = "month", length.out = 684)


# ===============Take Data from Yahoo finance API =================

ptflo_ticker<- c("XLE", "XLF", 
                  "XLV", "XLI",
                  "XLB", "XLU", 
                  "XLK", "^GSPC")

# Function to get monthly data
get_monthly_data <- function(ticker) {
  tq_get(ticker, from = "1959-01-01", to = Sys.Date(), periodicity = "monthly")
}

# Retrieve data
ptflo <- lapply(ptflo_ticker, get_monthly_data)


# Name the list elements
names(ptflo) <- c("Energy", "Financial", 
                  "Healthcare", "Industrial",
                  "Materials", "Utilities", 
                  "Technology","SP500")

# Combine all data frames into a single data frame with ticker column
ptflo <- bind_rows(ptflo, .id = "ticker")

# Pivot data to wide format
ptflo_pd <- ptflo %>%
  dplyr::select(date, ticker, adjusted) %>%
  pivot_wider(names_from = ticker, values_from = adjusted) 


# ==================== Combine Yahoo and Monetary policy shock data ==============================

# Time series data
ts_data <- right_join(temp, ptflo_pd, by = "date")
pn_data <- right_join(temp, ptflo, by = "date")

# ==================== Analysis ==============================

# I run VARs seperately for each Portfolio using BCSHOCK
# I run Local Projection Jorda (2005) for each of the three tech firms
# I include 4 lags of S&P500, ffrate, and BCHSHOCK

#### VAR ####

# I choose the following period for BCSHOCK data availability
# Set up data for analysis
ts_analysis <- ts_data %>% 
  mutate(date=yearmonth(ymd(date))) %>% 
  as_tsibble(index=date) %>% 
  filter_index('1998 Jan' ~ '2008 Jun')


### ------- Run VARs separately ------- 

ts_analysis_diff <- ts_analysis %>% 
  mutate(across(c("Energy", "Financial", 
                  "Healthcare", "Industrial",
                  "Materials", "Utilities", 
                  "Technology", "SP500",
                  "FFR", "LPCOM"), ~ difference(log(.)))) %>% 
  mutate(infl = difference(LCPI), BCSHOCK = BCSHOCK/100)

# Create a function
plot_irf <- function(sector) {
  # Filter and prepare the data
  ts_sectr <- as.data.frame(ts_analysis_diff) %>%
    dplyr::select(all_of(sector), SP500, FFR, BCSHOCK) %>%
    drop_na()
  # Convert to time series
  ts <- ts(ts_sectr, start = c(1998, 12), frequency = 12)
  # Fit the VAR model
  var_model <- vars::VAR(ts, p = 6, type = "const")
  # Calculate impulse response function
  irf_model <- irf(var_model, impulse = "BCSHOCK", response = sector, n.ahead = 24)
  plot(irf_model)
}

# Loop through each sector and perform the analysis
sectors <- c("Energy", "Financial", "Healthcare", "Industrial",
             "Materials", "Utilities", "Technology")

lapply(sectors, plot_irf)

### ------- Run VAR all together ------- 
ts_all <- as.data.frame(ts_analysis_diff) %>% 
  dplyr::select(Energy, Financial, 
                Healthcare, Industrial, 
                Materials, Utilities, 
                Technology, SP500, 
                FFR, BCSHOCK) %>% drop_na()

ts <- ts(ts_all, start = c(1998, 12), frequency = 12)

var_model <- vars::VAR(ts, p = 6, type = "const")

irf_model <- irf(var_model, impulse = "BCSHOCK", n.ahead = 24)

plot(irf_model)



#### Local Projection #### 



plot_irf2 <- function(sector) {
  ts_sectr <- as.data.frame(ts_analysis) %>%
    dplyr::select(all_of(sector), SP500, FFR, BCSHOCK) %>%
    drop_na()
  
  results_lin <- 
    lp_lin(ts_sectr,
           lags_endog_lin = 6,
           trend = 0,
           shock_type =1,
           confint = 1.96,
           hor =24)
  plot_lin(results_lin)[4]
}

sectors <- c("Energy", "Financial", "Healthcare", "Industrial",
             "Materials", "Utilities", "Technology")

lapply(sectors, plot_irf2)


