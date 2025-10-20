# Serie mensili simulate
set.seed(123)
livelli <- ts(rnorm(120, mean = 10, sd = 2), 
              start = c(2015, 1), 
              frequency = 12)
n_ind   <- ts(0.5 * livelli + rnorm(120, mean = 0, sd = 1), 
              start = c(2015, 1), frequency = 12)

library(forecast)
autoplot(cbind(livelli, n_ind)) + ggtitle("Serie Livelli e n_ind")

# Cross-correlazione per capire se ci sono ritardi
ccf(livelli, n_ind, main = "Cross-correlazione Livelli → n_ind")

mod_arimax <- auto.arima(n_ind, xreg = livelli)
summary(mod_arimax)

livelli_lag1 <- stats::lag(livelli, -1)  # livelli del mese precedente
mod_arimax_lag1 <- auto.arima(n_ind, xreg = livelli_lag1)
summary(mod_arimax_lag1)

AIC(mod_arimax, mod_arimax_lag1)

library(lmtest)

# Serve un modello VAR (più generale)
library(vars)

data <- cbind(livelli, n_ind)
mod_var <- VAR(data, p = 2)  # p = numero di lag

causality(mod_var, cause = "livelli")
