# Analisi serie temporale
# LIVELLI
livello <- read.table(here('input/idrometro/idrometroSSavino.txt'),
                      header = T) 
liv <- livello %>% 
    pivot_longer(cols = X01:X12, values_to = 'avg', names_to = 'month')
liv$month <- rep(1:12,4)
liv <- liv %>%
    mutate(date=as_date(paste(year,month,'01',sep='-'))) %>% 
    as_tsibble(index=date)

monthly_liv <- liv |>
    mutate(date = yearmonth(date)) |>      # converte la data in formato mensile
    index_by(date) |>                      # imposta l’indice temporale mensile
    summarise(
        avg = mean(avg, na.rm = TRUE)
    ) |> 
    fill_gaps()

monthly_liv %>% ACF(avg) %>%  autoplot()

# MUTE
mute <- pc %>% 
    filter(muta=='SI') %>% 
    count(sesso,date) %>% 
    mutate(perc=n/sum(n))

monthly_mute <- mute %>% 
    pivot_wider(id_cols = date,names_from = 'sesso', values_from = 'n') %>% 
    mutate(date=yearmonth(date)) %>% 
    as_tsibble(index = date) %>% 
    fill_gaps()

monthly_mute %>% ACF(M) %>%  autoplot()

ts.mute <- left_join(monthly_liv,monthly_mute)


# ANALISI MENSILE
ts.mute %>% gg_season(avg)
ts.mute %>% gg_season(F)
ts.mute %>% gg_season(M)

ts.mute %>% gg_lag(M)

# SCOMPOSIZIONE DEI TREND
library(imputeTS)
dcmp <- ts.mute %>%
    dplyr::select(date,avg,F) %>%
    fill_gaps() %>%
    mutate(F = na_interpolation(F)) %>% 
    model(STL(F ~ season(window = "periodic")))
components(dcmp)
components(dcmp) %>% autoplot()

# CROSS-CORRELATION
ts.mute %>% 
    CCF(avg,M) %>% 
    autoplot()

library(fable)       # per ARIMA e ARIMAX

ts_cleanF <- ts.mute %>%
    dplyr::select(date,avg,F) %>% 
    mutate(avg_lag1 = lag(avg, 1))

fit_arima <- ts_cleanF %>%
    model(
        arimax = ARIMA(F ~ pdq() + PDQ())
    )
report(fit_arima)

fit_arimax <- ts_cleanF %>%
    model(
        arimax = ARIMA(F ~ pdq() + PDQ() + avg_lag1)
    )
report(fit_arimax)
# Un aumento di 1 unità nel livello idrometrico un mese prima si associa in media a una diminuzione di circa 12.8 esemplari nel mese corrente.







fit <- ts.mute %>%
    model(VAR(vars(avg, F) ~ AR(1)))
report(fit)


test di Granger
causality(fit, cause = "livelli")

