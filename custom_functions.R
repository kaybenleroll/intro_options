library(RQuantLib)
library(data.table)
library(ggplot2)



S <- seq(50, 150, by = 0.1)
K <- 100

call_payoff <- pmax(S - K, 0);
put_payoff  <- pmax(K - S, 0);


call_plot <- qplot(S, call_payoff, geom = 'line'
                  ,xlab = 'Stock Price, S'
                  ,ylab = 'Payoff'
                  ,main = 'Payoff of a Long Call Option with Strike Price K = 100')

ggsave(call_plot, file = 'longcalloption_payoff.png', height = 10, width = 14)


put_plot <- qplot(S, put_payoff, geom = 'line'
                  ,xlab = 'Stock Price, S'
                  ,ylab = 'Payoff'
                  ,main = 'Payoff of a Long Put Option with Strike Price K = 100')

ggsave(put_plot, file = 'longputoption_payoff.png', height = 10, width = 14)



straddle_plot <- qplot(S, call_payoff + put_payoff, geom = 'line'
                      ,xlab = 'Stock Price, S'
                      ,ylab = 'Payoff'
                      ,main = 'Payoff of a Long Straddle Spread at Strike Price K = 100')

ggsave(straddle_plot, file = 'straddlespread_payoff.png', height = 10, width = 14)


call1_payoff <- pmax(S - 100, 0)
call2_payoff <- pmax(S - 110, 0)

callspread_plot <- qplot(S, call1_payoff - call2_payoff, geom = 'line'
                        ,xlab = 'Stock Price, S'
                        ,ylab = 'Payoff'
                        ,main = 'Payoff of a 100/110 Bullish Call Spread')

ggsave(callspread_plot, file = 'callspread_payoff.png', height = 10, width = 14)



### First calculate the AmericanOption price
interest.rate <- 0.01;

implied.vol <- 0.24;
t           <- 20 / 252;
K           <- 100;

AmericanOption(type          = 'call'
              ,underlying    = 100
              ,strike        = K
              ,dividendYield = 0
              ,riskFreeRate  = interest.rate
              ,maturity      = t
              ,volatility    = implied.vol)


EuropeanOption(type          = 'call'
              ,underlying    = 100
              ,strike        = K
              ,dividendYield = 0
              ,riskFreeRate  = interest.rate
              ,maturity      = t
              ,volatility    = implied.vol)


### Check the finite difference approximation for the
### American option price

h <- 0.0001;

V_d <- AmericanOption(type          = 'call'
                     ,underlying    = 100 - h
                     ,strike        = K
                     ,dividendYield = 0
                     ,riskFreeRate  = interest.rate
                     ,maturity      = t
                     ,volatility    = implied.vol)$value

V_u <- AmericanOption(type          = 'call'
                     ,underlying    = 100 + h
                     ,strike        = K
                     ,dividendYield = 0
                     ,riskFreeRate  = interest.rate
                     ,maturity      = t
                     ,volatility    = implied.vol)$value

V_0 <- AmericanOption(type          = 'call'
                     ,underlying    = 100
                     ,strike        = K
                     ,dividendYield = 0
                     ,riskFreeRate  = interest.rate
                     ,maturity      = t
                     ,volatility    = implied.vol)$value

delta <- (V_u - V_d) / (2 * h);         print(delta);
gamma <- (V_u - 2 * V_0 + V_d) / (h^2); print(gamma);




### Create plot of option price by stock price

S_seq <- seq(50, 150, by = 0.1)

price_seq <- sapply(S_seq, function(iterS)
    AmericanOption(type          = 'call'
                  ,underlying    = iterS
                  ,strike        = K
                  ,dividendYield = 0
                  ,riskFreeRate  = interest.rate
                  ,maturity      = t
                  ,volatility    = implied.vol)$value)

option_price_plot <- qplot(S, price_seq
                          ,geom = 'line'
                          ,xlab = 'S'
                          ,ylab = 'Option Value/Payoff') +
    geom_line(aes(y = call1_payoff), colour = 'red')

ggsave(option_price_plot, file = 'option_price_plot.png', height = 10, width = 14)


### Create plot of option price by stock price around the Strike

S_focus_seq <- seq(85, 115, by = 0.01)

focus_price_seq <- sapply(S_focus_seq, function(iterS)
    AmericanOption(type          = 'call'
                  ,underlying    = iterS
                  ,strike        = K
                  ,dividendYield = 0
                  ,riskFreeRate  = interest.rate
                  ,maturity      = t
                  ,volatility    = implied.vol)$value)

option_focus_plot <- qplot(S_focus_seq, focus_price_seq
                          ,geom = 'line'
                          ,xlab = 'S'
                          ,ylab = 'Option Value/Payoff') +
    geom_line(aes(y = pmax(0, S_focus_seq - 100)), colour = 'red')

ggsave(option_focus_plot, file = 'option_focus_plot.png', height = 10, width = 14)


### Comparison of call and put prices

interest.rate <- 0.01;

implied.vol <- 0.24;
t           <- 20 / 252;
K           <- 100;

# Calls
call_090 <- EuropeanOption(type = 'call'
                          ,underlying    = 90
                          ,strike        = K
                          ,dividendYield = 0
                          ,riskFreeRate  = interest.rate
                          ,maturity      = t
                          ,volatility    = implied.vol)

call_100 <- EuropeanOption(type = 'call'
                          ,underlying    = 100
                          ,strike        = K
                          ,dividendYield = 0
                          ,riskFreeRate  = interest.rate
                          ,maturity      = t
                          ,volatility    = implied.vol)

call_110 <- EuropeanOption(type = 'call'
                          ,underlying    = 110
                          ,strike        = K
                          ,dividendYield = 0
                          ,riskFreeRate  = interest.rate
                          ,maturity      = t
                          ,volatility    = implied.vol)

# Puts
put_090 <- EuropeanOption(type = 'put'
                         ,underlying    = 90
                         ,strike        = K
                         ,dividendYield = 0
                         ,riskFreeRate  = interest.rate
                         ,maturity      = t
                         ,volatility    = implied.vol)

put_100 <- EuropeanOption(type = 'put'
                         ,underlying    = 100
                         ,strike        = K
                         ,dividendYield = 0
                         ,riskFreeRate  = interest.rate
                         ,maturity      = t
                         ,volatility    = implied.vol)

put_110 <- EuropeanOption(type = 'put'
                         ,underlying    = 110
                         ,strike        = K
                         ,dividendYield = 0
                         ,riskFreeRate  = interest.rate
                         ,maturity      = t
                         ,volatility    = implied.vol)

### Output prices for S = 90
cat("Put price for S = 90\n");
print(put_090);

cat("\n");

cat("Call price for S = 90\n");
print(call_090);

cat("\n\n");


### Output prices for S = 100
cat("Put price for S = 100\n");
print(put_100);

cat("\n");

cat("Call price for S = 100\n");
print(call_100);

cat("\n\n");


### Output prices for S = 110
cat("Put price for S = 110\n");
print(put_110);

cat("\n");

cat("Call price for S = 110\n");
print(call_110);

cat("\n\n");
