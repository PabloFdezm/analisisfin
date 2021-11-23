##### Librerias
library(tidyverse)
library(tidyquant)
library(quantmod)
library(patchwork)
library(AER)
##### Others
acciones <- c("AAPL", "GOOG", "NFLX", "AMZN")

datos_descargados <-  acciones %>% tq_get(get  = "stock.prices",
                                          from = "2016-10-31",
                                          to   = "2021-10-31") 

descargar_base <- TRUE

##### Function

portfolio <- function(
  stocks,
  desde = NULL,
  hasta = NULL,
  datosprevios = NULL,
  rf = NULL,
  nportfolios = NULL
) {
  ##### Data  
  if (is.null(datosprevios)) {
    if (is.null(desde)) {
      desde = Sys.Date()-365*5
    }
    
    if (is.null(hasta)) {
      hasta = Sys.Date() - 2
    }
    
    datos <- stocks %>% tq_get(get = 'stock.prices',
                               from = desde,
                               to = hasta)
  }else {
    datos <- datosprevios %>% filter(symbol %in% stocks)
  }
  ##### Others Options
  
  if (is.null(rf)) {
    rf = 0.0012
  }
  if (is.null(nportfolios)){
    nportfolios = 100000
  }
  ##### Min. Var 
  log_ret_tidy <- datos %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = 'daily',
                 col_rename = 'ret',
                 type = 'log')
  
  log_ret_wider <- log_ret_tidy %>%
    pivot_wider(names_from = symbol, values_from =ret)
  
  mean_ret <- colMeans(log_ret_wider %>% select(-date))
  cov_mat <- cov(log_ret_wider %>% select(-date))*252
  
  wts <- runif(n = length(stocks))
  wts <- wts/sum(wts)
  
  port_returns <- (sum(wts * mean_ret) + 1)^252 - 1
  port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))
  
  sharpe_ratio <- (port_returns-rf)/port_risk
  
  all_wts <- matrix(nrow = nportfolios,
                    ncol = length(stocks))
  
  port_returns <- vector('numeric', length = nportfolios)
  
  port_risk <- vector('numeric', length = nportfolios)
  
  sharpe_ratio <- vector('numeric', length = nportfolios)
  
  for (i in seq_along(port_returns)) {
    
    wts <- runif(length(stocks))
    wts <- wts/sum(wts)
    
    # Storing weight in the matrix
    all_wts[i,] <- wts
    
    # Portfolio returns
    
    port_ret <- sum(wts * mean_ret)
    port_ret <- ((port_ret + 1)^252) - 1
    
    # Storing Portfolio Returns values
    port_returns[i] <- port_ret
    
    
    # Creating and storing portfolio risk
    port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
    port_risk[i] <- port_sd
    
    # Creating and storing Portfolio Sharpe Ratios
    # Assuming 0% Risk free rate
    
    sr <- (port_ret-rf)/port_sd
    sharpe_ratio[i] <- sr
    
  }
  
  portfolio_values <- tibble(Return = port_returns,
                             Risk = port_risk,
                             SharpeRatio = sharpe_ratio)
  
  all_wts <- as_tibble(all_wts)
  
  colnames(all_wts) <- colnames(log_ret_wider %>% select(-date))
  
  portfolio_values <- cbind(all_wts, portfolio_values)
  
  min_var <- portfolio_values[which.min(portfolio_values$Risk),]
  
  max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio), ]
  
  max_plot <- max_sr %>%
    gather(stocks, key = Asset,
           value = Weights) %>%
    mutate(Asset = as.factor(Asset)) %>%
    ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
    geom_bar(stat = 'identity') +
    theme_minimal() +
    labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
    scale_y_continuous(labels = scales::percent) 
  
  min_var_plot <- min_var %>%
    gather(stocks, key = Asset,
           value = Weights) %>%
    mutate(Asset = as.factor(Asset)) %>%
    ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
    geom_bar(stat = 'identity') +
    theme_minimal() +
    labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
    scale_y_continuous(labels = scales::percent) 
  
  efficent_frontier <- portfolio_values %>%
    ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
    geom_point() +
    theme_classic() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = 'Annualized Risk',
         y = 'Annualized Returns',
         title = "Portfolio Optimization & Efficient Frontier") +
    geom_point(aes(x = Risk,
                   y = Return), data = min_var, color = 'orange') +
    geom_point(aes(x = Risk,
                   y = Return), data = max_sr, color = 'red') +
    annotate('text', x = 0.15, y = 0.17, label = "Tangency Portfolio", 
             color = 'red') +
    annotate('text', x = 0.15, y = 0.08, label = "Minimum variance portfolio",
             color = 'orange') 
  
  q <- portfolio_values %>%
    ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
    geom_point() +
    theme_classic() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = 'Annualized Risk',
         y = 'Annualized Returns',
         title = "Portfolio Optimization & Efficient Frontier")+
    geom_point(aes(x = Risk,
                   y = Return), data = max_sr, color = 'red', size =2)
  
  q <- q + geom_abline(intercept = rf, slope = max(portfolio_values$SharpeRatio), color="red", 
                       linetype="dashed", size=1)
  
  r <- portfolio_values %>%
    ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
    geom_point() +
    theme_classic() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = 'Annualized Risk',
         y = 'Annualized Returns',
         title = "Portfolio Optimization & Efficient Frontier")+
    geom_point(aes(x = Risk,
                   y = Return), data = max_sr, color = 'red', size =2)+
    xlim(0,0.25)+
    ylim(0,0.2)
  
  
  r <- r + geom_abline(intercept = rf, slope = max(portfolio_values$SharpeRatio),
                       color="red", 
                       linetype="dashed", size=1)
  
  plots <- (max_plot + min_var_plot) / efficent_frontier
  
  min_var <- as.tibble(min_var)
  
  portfolio_values <- as.tibble(portfolio_values)
  
  max_sr <- as.tibble(max_sr)
  
  respuestas <- list(pregunta_6 = portfolio_values ,pregunta_7 = min_var, pregunta_8 = max_sr, pregunta_9 = plots, pregunta_9a = q)
  
  return(respuestas)
}

a <- portfolio(stocks=acciones)
a
