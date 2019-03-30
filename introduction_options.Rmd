---
title: "An Introduction Options"
author: "Mick Cooney <mickcooney@gmail.com>"
date: "`r Sys.Date()`"
output:
  html_document:
    theme: spacelab #sandstone #spacelab #flatly
    highlight: pygments
    number_sections: TRUE
    toc: TRUE
    toc_depth: 3
    toc_float:
      smooth_scroll: FALSE
  pdf_document: default
---


```{r knit_opts, include = FALSE}
library(tidyverse)
library(cowplot)
library(data.table)
library(RQuantLib)
library(xts)
library(quantmod)


# import::from(rlang     ,as_integer)
# import::from(tibble    ,as_tibble, tibble, tribble, add_column, glimpse)
# import::from(magrittr  ,"%>%", set_colnames)
# import::from(readr     ,read_csv, write_csv, cols, col_character)
# import::from(dplyr     ,mutate, filter, select, group_by, left_join, inner_join
#                        ,summarise, summarise_at, vars, distinct, sample_n
#                        ,pull, if_else, count)
# import::from(tidyr     ,gather, spread, nest, unnest)
# import::from(ggplot2   ,ggplot, aes, xlab, ylab, ggtitle
#                        ,geom_histogram, geom_boxplot, geom_bar, geom_col
#                        ,geom_line
#                        ,scale_x_continuous, scale_y_continuous 
#                        ,expand_limits, theme, element_text, facet_grid
#                        ,facet_wrap, theme_set, theme)
# import::from(scales    ,comma)
# import::from(cowplot   ,theme_cowplot)
# import::from(RQuantLib ,EuropeanOption, AmericanOption)


knitr::opts_chunk$set(tidy  = FALSE
                     ,cache = FALSE
                     ,message = FALSE
                     ,warning = FALSE
                     ,fig.height =  8
                     ,fig.width  = 11)



options(width = 80L
       ,warn  = 1
       ,mc.cores = parallel::detectCores()
        )

theme_set(theme_cowplot())

set.seed(42)

```


# First Post


> "If you intelligently trade derivatives, it’s like a license to
  steal" - Charlie Munger

I have spent most of my professional life as a 'quant' - a term in
finance that is short for quantitative analyst - a person who performs
statistical analysis on financial data for all sorts of different
reasons, but usually focused on providing a competitive advantage to a
firm's trading strategies.

The above quote was made in mid-2014, and succinctly summarises why
small financial firms can make such huge profits despite neither
having the technical advantages that firms engaging in
currently-controversial high-frequency trading (HFT) strategies, nor
the scale and diversified advantages enjoyed by well-known investment
banks like Goldman Sachs and Morgan Stanley.

By the time this series is finished, I aim to help people understand
why this is possible, what is it about options that allows for such
profitable trading? In the securities business, it is not uncommon for
small-sized firms to excel at their primary business, and not because
they engage in deceitful, fraudulent or exploitative practices.

Generally, it is because they have identified an edge over the
competition. This edge can come in different forms, and one such form
is knowledge-based. They know something the rest of the market does
not.

Options allow this for a simple reason: they are complex, and most
people do not understand them. Hopefully, this article will help you
with this.

I will gloss over some technical points for the purposes of clarity -
though not too much. My aim is the Einsteinian principle of "as simple
as possible, but not simpler".

There are many sources on pricing methodology and other quantitative
theory of options. This will not be one of them. I doubt I would do
the topic justice but will provide some links at the end of the series
for those interested.

Instead, I want to discuss practical issues with options, options
trading, and the various trading infrastructures that are in place. A
fascinating topic, it is rarely covered, despite its importance. For
clarity, I will focus solely on vanilla, exchange-traded options
(terms that will be explained in time), and will not discuss options
with more esoteric features like path-dependent payoffs.

We will get to price and other quantitative behaviour in future
articles of this series, but before that, it is important to know a
little about the infrastructure and details of the options markets
themselves.


## What is an Option?

Options are derivative contracts that allow the holder the right, but
not the obligation, to trade (buy or sell) a specific security for a
specific price for a specific period of time. The asset for which the
option confers the right to trade is termed the *underlying asset* (or
underlying for short).

Any type of asset can be used as the underlying, and this article will
focus on equity options (options on stocks such as Google, Apple, IBM,
3M and WalMart). It is the most common type of option, and that with
which I am most familiar. Most of the principles discussed here have
analogies with other underlying assets, such as currencies, futures or
bonds[^futures], so we do not lose much generality by this focus.

Before we begin, it is best to first lay out some terminology and
notation.

A *long* position is one where the instrument is 'owned' by the
holder, profiting from a rise in the price of that instrument. A
*short* position is one where the instrument has been sold by the
holder without owning it, profiting from a fall in the price.

These terms or used very generically in finance. Thus, a speculator
will state she is 'long interest rates' - meaning they hold a
combination of assets that will be profitable in the event of a rise
in interest rates.

While these terms often seem to be abused - what does it mean to be
'short gamma in US equities' for example[^longshort] - the key thing to
remember is that long positions want a rise in price and short
positions want a decline.

An option that confers the right to buy the underlying is termed a
*call option* (*call* for short); an option that confers the right to
sell is termed a *put option*.

Calls and puts are very closely linked in terms of behaviour and
price, called *put/call parity*, and we will discuss this in a future
article.

The specified trade price for the option contract is termed the
*strike price*, and the time period for which this right is conferred
is the *lifetime* of the option. The date at which this right ends is
the *expiration date* or *expiry date*.

Options are insurance policies against the movement of the underlying
price. A call is a policy against the stock price going up, a put is a
policy against the stock price going down, and the policy lasts until
expiration.

Finally, most option contracts have a feature that is known as *early
exercise* - that is, the right to buy or sell the stock can be
exercised prior to expiration. Most options traded have this feature
and are termed *American* options. This has nothing to do with
geography and is presumably some historical artefact. Options that do
not have early exercise are termed *European* options.

The vast majority of options traded have early exercise rights and we
will largely ignore European options. They are still very important
from a modelling point of view as they are easier to price than
options with early exercise.[^europeopt]

## Option Trading Infrastructure

The mechanics of trading equity options is very similar in principle
to trading other common financial assets such as equities or
futures. Options are traded on an exchange, and are treated as assets
in your account.

Two of the most important concerns in financial trading are
*counterparty risk* and *liquidity risk*. Both important concepts,
attempts at their mitigation explain the existence of a lot of
infrastructure that has built up around the asset markets.[^risktypes]

### Counterparty Risk

Counterparty risk is the risk the person you trade with (your
*counterparty*) is not fit or willing to make good on the trade when
due.

The sudden reappearance of counterparty risk was the major
contributing factor to Credit Crisis of 2008. Huge losses in subprime
mortgages threatened the existence of a number of large financial
institutions. As a result, other institutions that had existing
agreements with the distressed counterparties were now concerned about
their ability to sustain current financial agreements.

A good analogy is insurance companies. If your house burns down, you
want to be sure that the company that wrote your policy is in business
to pay the compensation.

Usually this is not a consideration, but if a lot of houses all burn
down at once (say due to a huge forest fire) - this can become a huge
problem. Very large hurricanes and natural disasters can bankrupt
insurance companies, and large companies insuring against natural
disasters try hard to diversify risks geographically.

Similarly, if you buy a call option and the stock rockets up through
the strike price and is now worth many, many multiples of what you
paid for it, you want to ensure that the person you bought it from
pays up.

### Liquidity Risk

*Liquidity risk* is the risk of the asset losing its
*liquidity*. Liquidity is a commonly-used but nebulous term
describing how difficult it is to find counterparties to trade an
asset at a reasonable price. Liquid assets are easy to trade in large
quantities, and such trades do not have a large effect on the price.

As you might imagine from the lack of precision in the terms used in
its definition, liquidity is difficult to quantify[^liquidity]. In broad
terms, currencies tend to be extremely liquid, followed by equities
and commodity futures. At the other end of the spectrum, real estate
is highly illiquid, even in a booming property market[^property].

Both of these issues are serious business risks, and were even more so
in the early days of finance[^earlyfinance]. Such concerns led to the creation of
exchanges and clearing.

An *exchange* is a legal entity that serves as a central marketplace
for traders of a particular asset type. It standardises contracts -
especially important for options and futures - and centralises the
liquidity in a central venue.

Trading on an exchange is a special privilege given only to members of
the exchange, so members either trade for themselves or act as brokers
for third parties. Most participants are customers of brokers as
becoming a member of an exchange is expensive, time consuming and
costly. As such, it is rarely worth becoming a member unless it is a
primary focus of your business.

Once a trade occurs between a buyer and seller, it is recorded on the
exchange, with trade notifications sent to a number of interested
parties including both primary participants in the trade, regulatory
authorities, and market data providers. Most importantly, the trade is
registered with the *clearing* system.

The clearing system is how trades are settled, and helps mitigate
against counterparty risk: once your trade is reported it is the
responsibility of the clearing system to ensure participants
receive/deliver their assets and cash. Once a trade is registered your
counterparty is now the clearing system NOT the person or company on
the other side of your trade. Thus, counterparty risk is much
reduced.[^counterparty]

Settlement of trades usually happens a number of days after the date
of trade, usually three days (for historical reasons), but attempts
have begun to reduce this down to a T+1 system: cash and assets are
transferred a day after the trade date.

An interesting consequence of the old T+3 settlement system is the
fact that US exchanges are never closed for more three days in a row:
this ensured people could always liquidate assets to meet settlement
obligations. This is why an unfortunate junior trader gets the job of
watching the screens on Black Friday or during the Christmas holidays,
despite nothing ever really happening. Someone needs to be there when
the markets are open, just in case.

Clearing fees is an additional cost to trading financial assets, but
provides a valuable service to the system as a whole. As they are
counterparties of last resort, they focus heavily on the risks taken
by their clients, ensuring that losses incurred do not exceed the
capital clients have on deposit with them. Should that occur, further
losses are the responsibility of the clearing firm.

## The Option Market

Almost all financial markets are *two-sided, open outcry* markets.

A two-sided market is one where there is a buy price (the *bid*) and a
sell price (the *ask* or *offer*). The difference between the bid and
the ask is known as the *bid/ask spread*, and is the price charged by
*market makers* to always quote prices on both sides. The bid/ask
spread is the most common way that traders make a profit; they try to
take as little risk as possible and just earn the spread. Most
market-makers want to carry no position overnight if possible, hedging
out any residual positions they may have left at the end of the
trading day.

In an open outcry market, prices are constantly being updated and
published. All the quotes published are aggregated and the highest bid
and lowest ask across all the options exchanges for that contract is
termed the *National Best Bid and Offer* or NBBO. Of course, any
individual market maker may have a spread wider than that implied by
the NBBO, and that is perfectly acceptable - that market maker will
get less trades as other people are willing to pay more or take less
and so are ahead in the queue.

Another consequence of not matching the NBBO on both sides is that any
trades you get will all be on one side; you will only get trades that
involve you buying or selling only. Indeed this may be the point, a
market maker may have taken down a big order earlier and is now
looking to reduce her net risk by subsequently trading in the other
direction.

Watching a market in motion is fascinating, it is the aggregation of
many different participants, each with different aims, priorities, and
goals, expressed in the dynamics of four numbers: the bid and ask
price, and the size of the quote on both sides, the amount of
contracts/shares/currencies available at those prices.

Option volumes are expressed in contracts. An option contract is for
100 shares, the same size as a round-lot of shares on stock
exchanges. Despite this, contracts are quoted as if only 1 share of
underlying is involved. I assume this is historical as that is how
futures contracts are traded. It is also the most natural unit for
pricing the option, and gives the exchange flexibility in terms of how
contracts are standardised - contract sizes could be changed without
requiring any change in how they quote the prices.

Thus, if you buy 1 call for 1.25 USD, you will pay 125 USD, as an
option contract is for 100 shares, but the price is quoted in terms of
1 share.[^contractprice]

Exchanges standardise the expiration date and strike prices for
options. This makes things manageable, only a finite number of
contracts are available for trade. Till 2012, options expired on a
monthly basis, then weekly options for the large indexes were
added. These additional expirations proved hugely popular, so weekly
expirations were added for large single stock options in the last few
years. There are now expirations every Friday in almost all liquid
options.

Strike prices are also set by the exchanges, largely set by
demand. Large equity index exchange-traded funds (ETFs) - shares in
funds that mirror the composition of the large indexes - are so liquid
that there are strikes every 50c close to the stock price, despite
underlying prices over 150 USD per share. The demand is there so the
exchange provides those contracts.

It is worth giving a concrete example of this. Consider the stock
symbol SPY, the ETF based on the famous S&P-500 index of large US
public companies. At the time of writing, this ETF is around 205 USD
per share, and for the closest expiration date in a few days time,
there are strikes every 50c from at least 190 to 220, i.e. the current
price plus/minus 15 USD.

For less liquid stocks, strikes are relatively further apart. For a
lot of stocks in the range of 40-80 USD per share, strike may be still
be 50c apart, possibly even 1 USD.

## Conclusion

We have discussed the trading environment and infrastructure involved
in trading, as well as how the markets themselves are structured,
focusing on options in particular.

In the next article I will quickly discussed the basic assumptions of
option prices and the most common methodologies for pricing them, then
discuss some of the consequences of those models. We will also discuss
some price behaviour, and talk about effective ways for using
options. Hopefully this will provide some insights to how focused
firms make so much money trading them.



# Second Post

In the last article we introduced the concept of options and how to
trade them, along with some of the infrastructure involved in trading
them.

With the core concepts introduced, we move on to the basics of option
pricing and the consequences of these models. As mentioned before, we
will only discuss the pricing models themselves briefly, as that topic
is well-covered in many other resources, in far more detail than is
possible here.[^optionpricing]


## Option Payoff Graphs

Before we discuss option pricing, it is worth discussing the concept
of *payoff* first. Simply stated, it is the realised profit earned
from owning the option. This concept is a little more subtle than it
first appears: when discussing profit do we include or ignore the
amount paid to buy the option?

Going on personal experience, but term splits down the lines of
traders and quants: traders include the cost of the option when
discussing payoffs, but quants do not.

I imagine this is due to focus, traders are always thinking about the
trading profit, so it only makes sense to include the cost in that
case. Quants try to build models and price them, so it is much more
natural to ignore the price paid for the option and focus instead on
the value of the option at expiration - how *in the money* is it?[^moneyness]

Being a quant, I will largely ignore the price paid for an option when
discussing payoffs, unless explicitly noted otherwise.

The most basic distinction for options is whether it is a call or a
put, i.e. it confers the right to buy or sell the stock. To get a feel
for how options work, it is worth looking at some charts.

Suppose we have a call option for stock XYZ with a strike price of 100
USD: what does the payoff for the option look like as a function of
the stock price $S$ of XYZ at expiration? Symbolically, it is $(S -
100)$ but bounded below at 0:

$$ \text{Payoff} = \text{max}(0, S - 100) $$

Why is this?

If the option is above 100 USD, say 105, then we can exercise the
option to buy XYZ for 100 USD and immediately sell those shares into
the market for 105 USD, creating a 5 USD profit. Thus, the call option
is worth 5 USD.

On the other hand, if XYZ is less than 100 USD, say 90, then we do not
exercise the option as we could just buy the stock cheaper than the
strike price allows us. Thus, the option is worthless and has a payoff
of 0.

The payoff chart for this particular option is shown in the chart below:

```{r longcalloption_payoff, echo=TRUE}
S <- seq(50, 150, by = 0.1)
K <- 100

call_payoff <- pmax(S - K, 0);

ggplot() +
    geom_line(aes(x = S, y = call_payoff)) +
    xlab('Stock Price, S') +
    ylab('Payoff') +
    ggtitle('Payoff of a Long Call Option with Strike Price K = 100')
```


A put option is exactly the opposite: In the above scenario but with a put instead of a call, we have:

$$ \text{Payoff} = \text{max}(0, 100 - S) $$

If the stock is at 105 USD the option is valueless as we only have the
right to sell at 100 USD. When the stock is at 90 USD, we can buy the
shares for 90, exercise the put and sell them for 100, netting 10 USD
profit. Thus, the put is worth 10 USD.

```{r longputoption_payoff, echo=TRUE}
put_payoff  <- pmax(K - S, 0);

put_plot <- ggplot() +
    geom_line(aes(x = S, y = call_payoff)) +
    xlab('Stock Price, S') +
    ylab('Payoff') +
    ggtitle('Payoff of a Long Put Option with Strike Price K = 100')
```


Payoff curves are often overkill for simple options once you have a
grasp of the basics, but are still a very useful tool for *option
spreads*: combinations of different option contracts with the same
underlying. We will not discuss spreads too much in this series as
that is topic all in itself, but it is worth mentioning a few of the
most common here as they are an excellent illustration of the use of
payoff curves.

A *straddle spread* is the combination of a long (or short) call and
put option with the same expiration and strike price. Straddle spreads
tend to be used to trade volatility - the trader is betting on the size
of the movement of the underlying rather than on the direction of the
movement.

```{r straddlespread_payoff, echo=TRUE}
ggplot() +
    geom_line(aes(x = S, y = call_payoff + put_payoff)) +
    xlab('Stock Price, S') +
    ylab('Payoff') +
    ggtitle('Payoff of a Long Straddle Spread with Strike Price K = 100')
```

A *call spread* is the combination of a long and short call option at
different strikes[^callspread]. If the long strike is lower than the short
strike, it is a *bullish* spread since it profits from a rise in stock
price. If the short strike is lower, it is a *bearish* spread. In
either case, the maximum profit is capped at the difference between
the strikes.

```{r callspread_payoff, echo=TRUE}
call1_payoff <- pmax(S - 100, 0)
call2_payoff <- pmax(S - 110, 0)

callspread_plot <- ggplot() +
    geom_line(aes(x = S, y = call1_payoff - call2_payoff)) +
    xlab('Stock Price, S') +
    ylab('Payoff') +
    ggtitle('Payoff of a 100/110 Bullish Call Spread')
```



## Pricing Options

With the basics dealt with, we now start discussing the interesting
parts of options: how to price them, and how their price depends on
their inputs.

There are two main models for pricing options: the binomial model and
the Black-Scholes model.

The binomial model uses a tree-like structure to model the price
changes of the underlying over time, and has the advantage of making
early-exercise features easy to implement. It is also computationally
fast.

The Black-Scholes model is the workhorse of option pricing theory and
results in the famous Black-Scholes partial differential equation for
option prices:

$$ \frac{dV}{dt} + \frac{1}{2} \sigma^2 S^2 \frac{d^2 V}{dS^2} + rS \frac{dV}{dS} - rV = 0 $$

where $V$ is the price of the option, $S$ is the stock price, $r$ is
the risk-free interest rate, $t$ is the time to expiration and
$\sigma$ is the expected volatility of the underlying over the
lifetime of the option.

Notice the absence of the strike price, $K$ in the equation. Why is this?

When solving the above equation, $K$ is important in setting boundary
conditions for the solution, but the fact it is not in the equation
itself is a manifestion of the close connection between the price of a
call and a put.

We have mentioned the idea of volatility a few times now without
actually explaining it. As its name suggests, volatility is a measure
of the size of the relative moves of the underlying price.

Quantitative finance models price changes in assets in terms of
percentage changes, termed the *returns* of the asset. This is for a
number of reasons:

* Percentage changes are often easier to understand and remember when
  it comes to interpreting the output of models
* It allows more natural comparisons, without needing to know the
  underlying price level as a reference point, and allows comparisons
  across asset classes
* Much of quantitative finance deals with time series, and a sequence
  of percentage changes tends to behave more independently than a
  series of price changes, making it more amenable to statistical
  methods

A basic assumption of the Black-Scholes model is that returns of the
underlying asset prices are distributed according to a lognormal
distribution - the volatility of the asset is the standard deviation
of this distribution.

Underlying assets that pay dividends can also be accounted for but we
will keep things simple and assume no dividends are paid on the
underlying.

All of the above inputs to the option pricing model are observable,
with the exception of the volatility, $\sigma$, as we do not know the
value of this quantity till after the option expires[^expireprice].

We ignore the philosophical consequences of this, and focus on the
most important practical one: we can use volatility and option price
interchangeably. For a given set of observed values of $S$, $t$, $r$
and $K$, there is a one-to-one relationship between the option price
and the volatility. Used this way, the volatility is called the
*implied volatility* (or *implied vol* or *implied*). Thus, implied
vols are proxies for option prices, and are independent of the current
stock price.

In financial markets, volatility levels tend to be more stable than
price levels, so traders quote option prices in terms of implieds[^priceimplieds],
plugging in the current stock value when executing the trade to get
the dollar amount.

Before we move on to actual calculations, we have one more concept to
discuss: units of time.

Markets in various asset classes are open from Monday to Friday. For
equities, they open between the hours of 0930 to 1600. It often makes
sense to measure time in terms of trading days rather than calendar
days. Thus, years have 252 days in them (the approximate count of
business days in a year).

As mentioned, volatility is a standard deviation and so scales by the
square root of time.[^volsqrtime] Thus, if we have a daily volatility of 1%,
this becomes $\sqrt{252} = 15.8\%$ in annual terms. For simplicity,
this factor is often rounded up to 16, meaning that an asset with an
annualised volatility of 16% has a daily volatility of 1%.[^dailyvol]

The convention is to express volatility in annualised terms.


## Calculating Option Prices

As mentioned in the previous post, there are many excellent books on
option pricing, and I would not do it justice[^calcpricing]. Instead I want to
focus on using R for this, and what we can learn from that.

We will use [QuantLib](http://www.quantlib.org), an extensive
open-source library containing a cornucopia of useful functions for
quantitative finance. It is available in R through the `RQuantLib`
package - this is what we use for all subsequent work.

Suppose we have an American call option on a stock XYZ at price level
100 USD with a month to expiry (20 trading days) with 24% annualised
volatility. How do we calculate a price for this option?

```{r american_option_price, echo=TRUE}
interest_rate <- 0.01;

implied_vol <- 0.24
t           <- 20 / 252;
K           <- 100;

AmericanOption(type          = 'call'
              ,underlying    = 100
              ,strike        = K
              ,dividendYield = 0
              ,riskFreeRate  = interest_rate
              ,maturity      = t
              ,volatility    = implied_vol)
```

According to the output, this option is worth 2.76 USD.

The other quantities, delta, gamma, vega, theta, rho and divRho are
collectively known as the 'Greeks' and are the analytic derivatives of
the price function with respect to the various parameters. They are
termed such as they use Greek letters for their symbols, but also I
suspect to avoid confusion from over-using the word 'derivative'.

For some reason, QuantLib does not calculate the Greeks using the
American option routines, so let us check what we get for European
options:

```{r european_option_price, echo=TRUE}
EuropeanOption(type          = 'call'
              ,underlying    = 100
              ,strike        = K
              ,dividendYield = 0
              ,riskFreeRate  = interest_rate
              ,maturity      = t
              ,volatility    = implied_vol)
```

The option price calculated is almost identical, and now we also have
values for the Greeks. A quick finite difference calculation shows
that the Greeks for the European options are equivalent to those for
the equivalent American options, but we leave that as an exercise for
the reader.

Now we look at the effect of underlying on the call price. We can
check this by calculation this price for a sequence of prices and
plotting the output. We plot this against the payoffs to get a sense
of perspective for the price.

```{r option_price_plot, echo=TRUE}
S_seq <- seq(50, 150, by = 0.1)

call_payoff <- pmax(S_seq - K, 0)

price_seq <- sapply(S_seq, function(iterS)
    AmericanOption(type          = 'call'
                  ,underlying    = iterS
                  ,strike        = K
                  ,dividendYield = 0
                  ,riskFreeRate  = interest_rate
                  ,maturity      = t
                  ,volatility    = implied_vol)$value)

plot_dt <- rbind(tibble(label = 'Option Price', S_seq = S_seq, value = price_seq)
                ,tibble(label = 'Payoff',       S_seq = S_seq, value = call_payoff)
                )


ggplot(data = plot_dt) +
    geom_line(aes(x = S_seq, y = value, colour = label)) +
    xlab("Stock Price, S") +
    ylab("Option Price / Payoff")
```


#### Option Intrinsic Value and Option Time Value

Option prices can be split into two components, the *intrinsic value*
- the value of the option were it to be immediately exercised - and
the *time value* of the option which is the remainder. The time value
is also referred to as the *option premium*.

Options that are in the money always have positive intrinsic
value. Options that are out of the money only have premium in them.

If we look at a zoomed-in version of the previous plot, we can see how
the premium behaves as the underlying changes. We will discuss this
further when we talk about the Greeks, but visuals will work for now.

```{r option_focus_plot, echo=FALSE, results='show'}
S_focus_seq <- seq(85, 115, by = 0.01)

call_payoff <- pmax(S_focus_seq - K, 0)

focus_price_seq <- sapply(S_focus_seq, function(iterS)
    AmericanOption(type          = 'call'
                  ,underlying    = iterS
                  ,strike        = K
                  ,dividendYield = 0
                  ,riskFreeRate  = interest_rate
                  ,maturity      = t
                  ,volatility    = implied_vol)$value)

plot_dt <- rbind(tibble(label = 'Option Price', S_seq = S_focus_seq, value = focus_price_seq)
                ,tibble(label = 'Payoff',       S_seq = S_focus_seq, value = call_payoff)
                )

ggplot(data = plot_dt) +
    geom_line(aes(x = S_seq, y = value, colour = label)) +
    xlab("Stock Price, S") +
    ylab("Option Price / Payoff")
```

Looking at the plot, we see the premium increases as the underlying
approaches the strike price. At the strike price, the premium is at
its maximum, and beyond that the intrinsic value becomes non-zero and
takes an increasing proportion of the option value.


### Comparing Calls and Puts

We mentioned earlier that there is a relationship between the price of
a call and a put for a given set of parameters. This relationship,
put/call parity, can be expressed in closed form for European options:

$$ C = P + S - K e^{-rt} $$

Let us investigate this by calculating calls and puts with strike
price $K = 100$, for $S = 90, 100, 110$, starting with $S = 90$:

```{r callput_price_comparison, echo=TRUE}
### Comparison of call and put prices

interest.rate <- 0.01

implied.vol <- 0.24
t           <- 20 / 252
K           <- 100

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
```

```{r option_price_S090, echo=FALSE, results='show'}
### Output prices for S = 90
cat("Put price for S = 90\n")
print(put_090)

cat("\n")

cat("Call price for S = 90\n")
print(call_090)

cat("\n\n")
```

A quick inspection of the numbers shows that gamma and vega are the
same, and $\Delta\_C - 1 = \Delta\_P$. This will make sense once we
have discussed the Greeks.

The premium in both options are not the same, the put has $0.0941$ of
premium compared to $0.1747$ in the call.

Moving on to $S = 100$:

```{r option_price_S100, echo=FALSE, results='show'}
### Output prices for S = 100
cat("Put price for S = 100\n")
print(put_100)

cat("\n")

cat("Call price for S = 100\n")
print(call_100)

cat("\n\n")
```

The pattern for delta, gamma and vega we observed continues for the
at-the-money options and the option premia are similar at values
$2.6758$ and $2.7563$ respectively.

Finally, we look at $S = 110$. Given the symmetry, it would not be too
surprising to see a similar result for $S = 90$ but with calls and
puts switched:


```{r option_price_S110, echo=FALSE, results='show'}
### Output prices for S = 110
cat("Put price for S = 110\n")
print(put_110)

cat("\n")

cat("Call price for S = 110\n")
print(call_110)

cat("\n\n")
```

Now we see the premium in the put and call is $0.2555$ and $0.3360$
respectively, higher than the premium in the options when $S = 90$.

This asymmetry arises as a consequence of the assumptions in
Black-Scholes model. The asset is assumed to move according to a
lognormal distribution. The volatility is the standard deviation of
this distribution, but the mean is not zero, it is slightly positive
due to the risk free rate. As a result there is a slight bias upwards
in the price movements. Hence the higher premium on the upside prices
for $S$.[^priceskew]

We will discuss this further once we have an understanding of the
Greeks.


## The First and Second Derivatives - The 'Greeks'

Every discussion of option pricing involves describing the 'Greeks' -
derivatives of the option price with respect to different input
quantities such as stock price and volatility.


### Delta: $\Delta = \frac{dV}{dS}$

The delta, $\Delta$, of an option is the first derivative of the
option price with respect to underlying price. It can be interpreted
in two ways:

* it is the equivalent amount of shares the option corresponds to at
  this instant. Being long a 30-delta call option is equivalent to
  owning 30 shares of the underlying.
* the absolute value of the delta is the probability of that option
  ending up being in the money

Numerically, delta varies from -1 to 1, but as option contracts are
for 100 shares, we generally multiply the delta by 100. Historically,
this made it simpler for traders to know their exact exposure to the
underlying stock in terms of shares. Also, the human brain finds it
easier to think in terms of whole numbers than with decimals.

Calls have a positive delta as they are a bullish instrument and so
are like being long the underlying, whereas puts are bearish and so
have negative deltas.

At the money options have absolute delta values close to 50. The
strike that is the closest will have deltas closest to 50.

Conversely, options where the delta is close to 100 or -100 behave
like the underlying and are often treated as such for risk management
purposes.

One counter-intuitive result of using options is that deltas tend to
be simultaneously the most important Greek from a risk point of view
while being the least interesting from a trading and portfolio
management point of view.


### Gamma: $\Gamma = \frac{d^2V}{dS^2}$

The Gamma, $\Gamma$, of an option is the second derivative of the
price with respect to the underlying price - it describes the change
in delta as the underlying changes.

Similar to delta values, gamma values are multiplied by 100 when
quoted, and represent the instantaneous change in delta when the
underlying moves by 1 USD.

Gamma is hugely important for options and holds a similar position to
convexity in bond pricing. Its existence is one of the reasons why the
behaviour of option prices can be counter-intuitive - the presence of
a non-zero second derivative causes non-linear behaviour.

To see the importance of gamma, suppose we have a straddle spread
where the strike of the spread is at the money. Such a spread has
almost no delta but a lot of gamma. This means that while the delta of
the spread is zero right now, it is likely to change significantly as
the underlying price moves.

The gamma of an option is positive for both calls and puts.


### Vega: $\text{Vega} = \frac{dV}{d\sigma}$

The vega of an option is the first derivative of the option price with
respect to the implied volatility.

It is quoted in units of dollar amounts and is scaled to represent the
change in value of an option when the implied vol moves by 1 'vol
click' i.e. when the vol moves from 24% to 25%.

The RQuantLib functions calculate vega on the scale of changes of 1
unit of vol, 100 vol clicks, so this needs to be accounted for.

The vega of an option is always positive.


### Theta: $\Theta = \frac{dV}{dt}$

The theta, $\Theta$, of an option is the first derivative of the price
with respect to time.

For practical reasons it is usually expressed in terms of change in
price per day, requiring a transformation of the output of the
QuantLib routines as the default amount is the same unit of time for
the maturity and volatility (usually annualised in years).

Theta represents the passive change in option price if nothing else
changes. It is always negative as option values decay as time
passes. This is because the reduced lifetime of the options results in
less opportunity for the underlying to move, and thus is worth less.


### Rho: $\rho = \frac{dV}{dt}$

The Rho, $\rho$, of an option is the first derivative of the option
price with respect to interest rate.

This also needs the QuantLib output to be modified as it is more
natural to think in terms of change in price per change in interest
rate points (100 basis points).

Interest rate moves tend to be well signposted and most option trading
tends to be for short timescales, so rho is not as important for most
use cases as its effect is limited. It can be extremely important for
very long-dated options though.


## Summary

In this article we introduced payoff graphs and looked at the charts
for some option spreads. We also used the QuantLib library in R
through the `RQuantLib` package to calculate option prices.

Finally, we introduced the Greeks and talked a little about why they
are important.

In the next article, we will continue this discussion and show how
calls and puts behave similarly. We will also talk a little about the
consequences of the non-linearity in options.


# Third Post

In the first two articles of this series we discussed various aspects of
options and options trading, necessary background for the real meat of the
series: how do options behave under various circumstances, and what
implications do these behaviours have in their use?

In this article we will start that exploration, but we will only have time to
scratch the surface: there is much more content than we have time for in this
series so we will look at a few different things, perhaps suggest a few more
avenues of investigation, and try to bring it all together in the fourth and
final post of this series.

Most of my personal experience with options is for equitiess and
exchange-traded funds (ETFs) - a financial instrument that closely tracks an
underlying index - but behaves in most repects like equity. There are options
on other instruments: bonds, futures, currencies for example, and while there
are subtle differences between these options that are crucial to understand
when trading them, they behave in similar ways for our purposes.

To help clarity, we focus on options on equities and equity ETFs but bear in
mind that many of these behaviours translate to all options.

For the first few sections we hold volatility constant: a huge and unrealistic
simplification. The behaviour of implied volatility is a major component of
option trading, so we will discuss it in the final article. Things are complex
enough with vol constant.

Finally, we focus almost entirely on options with a shorter expiration, in most
cases 40 trading days or less. The majority of trading liquidity in options is
short-term focused, and though there are exchange-traded options with longer
expirations (out to a few years in some cases), we will ignore these for the
main.

In quite a few cases, some behaviours dicussed do not hold for all options,
especially for options with longer expirations, so beware!


## Units of Greeks Redux

We also recall that options are traded as contracts for 100 shares, but are
priced in respect to a single share. In most cases we can ignore this point and
focus on options as if they were for a single share, but it may be prudent to
point it out at times. We also will discuss the Greeks as a contract for
consistency with convention, so we quote the deltas and gammas in terms of
contracts and multiply them by 100 in general. We will take note of this
distinction when it becomes relevant.

Despite this convention for deltas and gammas, vega and theta is quoted on a
per-share basis. This is likely due to both vega and theta are in units of
currency, so the quoting convention is more natural here.

As discussed in the previous article, theta is quoted in terms of change per
vol 'click', so from 20% to 21% say. Thus, if an option has a price of 3 USD at
vol level 20% and a vega of 0.50, we expect the price at 21% to be about 3.50
USD.[^pricechange]

Similarly, theta is quoted in time units of one day, so if an option with 10
trading days remaining is worth 2 USD and has a theta of -0.20, we expect the
price at the same time tomorrow to be about 1.80 USD.

We also largely ignore interest-rate effects on pricing as we focus on more
short term maturities of two months or less - 40 trading days.


## Revisiting Put-Call Parity

We have discussed a few times in previous posts the close relationship between
the price of a call and put option. For future brevity, we will introduce one
more piece of terminology in options - the *line*, a specific combination of
expiration and strike price. Historically, option prices were quoted in
expiration and strike order, with the strike prices in a column down the
centre.


The reason for this close relationship is somewhat counterintuitive: from an
optionality point of view, call and put options are the same - that is, the
only difference between between a call and put option is 100 deltas. This is
exactly true for European options but still holds approximately for American
options.

This seems quite the claim so let us at least check this. We will take a series
of values for stock price, strike price, volatility etc, calculate the various
option prices, and see how they compare.

```{r option_price_compare, echo=TRUE}
S_vals    <- seq(25, 100, by = 25)
K_vals    <- seq(25, 100, by = 25)
vol_vals  <- seq(0.1, 0.5, by = 0.1)
r_vals    <- c(0.01, 0.02, 0.05, 0.10)
T_vals    <- c(5, 10, 20, 60, 120, 252) / 252

params_dt <- CJ(S = S_vals, K = K_vals, vol = vol_vals, r = r_vals, T = T_vals)
params_dt[, contract_id := .I]

calc_option_price <- function(type, S, K, r, t, vol) {
    dS <- S * 1e-6
    dt <- t * 1e-6
    do <- vol * 1e-6

    V <- AmericanOption(type          = type
                       ,underlying    = S
                       ,strike        = K
                       ,dividendYield = 0
                       ,riskFreeRate  = r
                       ,maturity      = t
                       ,volatility    = vol)$value

    VpdS <- AmericanOption(type          = type
                          ,underlying    = S + dS
                          ,strike        = K
                          ,dividendYield = 0
                          ,riskFreeRate  = r
                          ,maturity      = t
                          ,volatility    = vol)$value

    VmdS <- AmericanOption(type          = type
                          ,underlying    = S - dS
                          ,strike        = K
                          ,dividendYield = 0
                          ,riskFreeRate  = r
                          ,maturity      = t
                          ,volatility    = vol)$value

    Vpdt <- AmericanOption(type          = type
                          ,underlying    = S
                          ,strike        = K
                          ,dividendYield = 0
                          ,riskFreeRate  = r
                          ,maturity      = t + dt
                          ,volatility    = vol)$value

    Vmdt <- AmericanOption(type          = type
                          ,underlying    = S
                          ,strike        = K
                          ,dividendYield = 0
                          ,riskFreeRate  = r
                          ,maturity      = t - dt
                          ,volatility    = vol)$value

    Vpdo <- AmericanOption(type          = type
                          ,underlying    = S
                          ,strike        = K
                          ,dividendYield = 0
                          ,riskFreeRate  = r
                          ,maturity      = t
                          ,volatility    = vol + do)$value

    Vmdo <- AmericanOption(type          = type
                          ,underlying    = S
                          ,strike        = K
                          ,dividendYield = 0
                          ,riskFreeRate  = r
                          ,maturity      = t
                          ,volatility    = vol - do)$value


    delta <- (VpdS - VmdS) / (2 * dS)
    gamma <- (VpdS - 2 * V + VmdS) / (dS^2)
    vega  <- (Vpdo - Vmdo) / (2 * do)
    theta <- (Vpdt - Vmdt) / (2 *dt)

    return(c(price = V
            ,delta = delta * 100
            ,gamma = gamma * 100
            ,vega  = vega * 0.01
            ,theta = theta / 252))
}

call_dt <- params_dt[, data.table(t(mapply(calc_option_price, 'call', S, K, r, T, vol)))]
put_dt  <- params_dt[, data.table(t(mapply(calc_option_price, 'put',  S, K, r, T, vol)))]

data_dt <- rbind(cbind(type = 'call', params_dt, call_dt)
                ,cbind(type = 'put',  params_dt, put_dt))

data_dt <- dcast(data_dt, contract_id + S + K + r + T + vol ~ type
                ,value.var = c("price", "delta", "gamma", "vega", "theta"))

compare_dt <- data_dt[, .(contract_id, price_call, price_put
                         ,S, K, r, T, vol
                         ,d_delta = delta_call - delta_put
                         ,d_gamma = gamma_call - gamma_put
                         ,d_vega  = vega_call  - vega_put
                         ,d_theta = theta_call - theta_put)]

print(compare_dt[, .(S, K, T = round(T, 4), vol, d_delta, d_gamma, d_vega, d_theta)])
```

From what we can see, it appears that this statement is holding. The theta
differences are not hugely suprising as it is the amount of value decay due to
time, and it makes sense that the higher price contract would have a higher
value: it has more value to decay.

We can quickly check this data for differences:

```{r option_callput_diffs, echo=TRUE}
compare_dt[abs(d_delta - 100) > 1 | abs(d_gamma) > 1 | abs(d_vega) > 0.01]
```

Numerical rounding and precision is an issue in the calculations of the Greeks
here, but we can see that broadly speaking, there is almost no difference
between any of the Greeks along a line.

What is the consequence of this?

Simply stated, it means that being long 1 call contract (for 100 shares) and
short 100 shares gives the same profit and loss (PnL for short) as being long
1 put contract. Conversely, being long 1 put and long 100 shares is the same
as being long 1 call contract.

To check this, we start with an option with a strike price of 100 with a
volatility of 20%. Suppose we are 40 days out and the underlying stock is at
95. We buy the 100 call (the call option at the 100 strike), paying the price.
Suppose after one day's trading the underlying has moved up to 100. What is the
PnL in this case, and how does it compare to being long the 100 put and long
the stock instead?

If our previous assertion is correct, they are the same.

```{r callputparity_pnlplot_setup, echo=TRUE}
create_line_pricer <- function(K, r, vol) {
    calc_prices <- function(S, t) {
        call_price <- AmericanOption(type          = 'call'
                                    ,underlying    = S
                                    ,strike        = K
                                    ,dividendYield = 0
                                    ,riskFreeRate  = r
                                    ,maturity      = t
                                    ,volatility    = vol)$value

        put_price  <- AmericanOption(type          = 'put'
                                    ,underlying    = S
                                    ,strike        = K
                                    ,dividendYield = 0
                                    ,riskFreeRate  = r
                                    ,maturity      = t
                                    ,volatility    = vol)$value

        return(c(c = call_price, p = put_price))
    }

    return(calc_prices)
}

K   <- 100
r   <- 0.01
vol <- 0.20

option_pricer <- create_line_pricer(K = K, r = r, vol = vol)
```

I have created a little utility function `option_pricer` which calculates the
call and put price with the above parameters for a given underlying price and
time.

```{r callputparity_pnlplot_show, echo=TRUE}
op_day1 <- option_pricer(S = 95,  t = 40/252)
op_day2 <- option_pricer(S = 100, t = 39/252)

pnl_call     <- (op_day2['c'] - op_day1['c'])
pnl_putstock <- (op_day2['p'] - op_day1['p']) + (100 - 95)

print(op_day1, digits = 5)
print(op_day2, digits = 5)
print(c(pnl_call, pnl_putstock), digits = 5)
```

It is worth spending a little time unpicking all this as there are a few
things to consider. These calculations should be exact but may not be due to
rounding. Discrepancies will be small.

We start with the calculation for the call.

On day 1, 40 days to expiration, the call is worth about 1.25 USD and the stock
is at 95 USD. We buy the call, so our account has a 100 call and a negative
cash balance of -1.25 USD, the price we paid for the call.

After one day, the stock moved up to 100 USD, and we need to recalculate the
price of the call for the new stock price and time to maturity. We also need
to account for the interest rate charge on the cash balance. It will be small,
but is important to remember. For trading operations, interest rate charges and
fees are a significant aspect of the business and need attention.

As a quick exercise, without looking below, will the move in the underlying
result in a profit or loss in the call and by how much? The answer may seem
obvious, but it is not.

The new value is 3.22 USD, so our profit is

$$ 3.22 - 1.25 = 1.97 $$

Assuming liquid markets, this profit is more than just a paper profit - we
could sell the call and take the profit if we wished.

Now we look at the PnL for a put and a share.

On day 1 the put is worth 6.11. Note that we could split this price into 5 USD
of intrinsic value - the put is 5 USD in the money - and 1.11 USD of option
premium. We pay 6.11 for the put, and we buy a share for 95 USD. Thus, we now
have a long 100 put, a share of the stock, and a cash balance of -101.11, the
sum of the cost of the put and the share.

After day 2, the stock has moved up to 100, so what is our PnL?

The new value for the put is 3.07, so the profit from that is

$$ 3.07 - 6.11 = -3.04 $$

The put has lost value, but we are also long a share, which has gained in value
by 5 USD. Our total profit is

$$ (3.07 - 6.11) + (100 - 95) = 1.9610 $$

Quite a narrow disparity, but we have forgotten to include interest on the cash
balances. Does this have much effect?

For the long call we have a negative cash balance of 1.25 USD and so pay
interest for one day on this. For the long put and stock, it is negative
101.11, so we check the difference in these charges.

```{r callputparity_pnlplot_intrate, echo=TRUE}
intrate_call     <- (op_day1['c']     ) * (exp(-r * 1/252) - 1)
intrate_putstock <- (op_day1['p'] + 95) * (exp(-r * 1/252) - 1)

print(c(intrate_call, intrate_putstock), digits = 5)
print(c(pnl_call + intrate_call, pnl_putstock + intrate_putstock), digits = 5)
```

Even allowing for interest charges, the discrepancy due to the options being
American is narrow enough to be ignored for our purposes.

As one final check, let us see what happens if we used a straddle in the above
scenario, and then replace the call with the put and stock. Recall that a
straddle spread is a call and a put on the same line. We compare this to a
having two puts and being long a share.

```{r callputparity_pnlplot_spread, echo=TRUE}
### The straddle is the sum of the prices
pnl_straddle <- sum(op_day2) - sum(op_day1)

### Switch the call for a put and a share, so 2 puts 1 share
pnl_2putshare <- 2 * (op_day2['p'] - op_day1['p']) + (100 - 95)

print(c(pnl_straddle, pnl_2putshare))
```

We see that the straddle loses about 1.07 in value, roughly the same in the
decline in value of the 2 puts and the stock price.

This explains why all the Greeks for a call and put on the same line are the
same apart from the delta (which differs by 100). Price parity forces this to
be the case.[^priceparity]


## Option Premium Decay

We discovered in the last article that theta for an option is negative: that
is, as time passes the value of an option decreases. This makes sense, option
premium is an expectation of future possible value and as time passes there is
less opportunity for the stock to move and realise that value. How does this
decay happen? Is it gradual and linear, exponential? Before looking below,
think about what may make sense.

Suppose we an 40-day at-the-money option with 20% volatility and
underlying/strike of 100. We can calculate the value of this option over time,
using the unrealistic assumption that nothing else will change.


### Time Decay for At-the-Money Options

We start with at-the-money options as they are usually the most interesting. We
do a plot from 40 days out to expiration, and hold all other inputs constant.
The intrinsic value of the option is zero throughout, so the price decreases
to zero at expiration.

```{r premdecay_atm_plot, echo=TRUE}
days_remaining <- seq(40, 0, by = -1)

S <- 100
K <- 100

option_pricer <- create_line_pricer(K = K, r = r, vol = vol)

decay_atm_price <- sapply(days_remaining / 252, function(iterT) option_pricer(S, iterT)['c'])

ggplot() +
    geom_line(aes(x = -days_remaining, y = decay_atm_price)) +
    expand_limits(y = 0) +
    xlab("Days Remaining") +
    ylab("Option Price") +
    ggtitle("Plot of Theta Decay for At-the-Money Option\nS = 100, K = 100")
```


The decay is slow at the beginning, approximating a constant decay,
accelerating towards zero in the last 5-10 days of lifetime. The option
sustains its value as there is a large probability of the option expiring in
the money right up to expiration, the decay reflects the fact that it takes
time for the underlying to move, and so shorter lifetimes reduce the variance
of this distribution of prices of the underlying at expiration, in turn
reducing the value of the option.

### Time Decay for Out-of-the-Money Options

OTM options also have zero intrinsic value throughout its lifetime, but also
have a distance to cross before they are in the money. We would expect the
option prices to be lower than at-the-money options.

```{r premdecay_otm_plot, echo=TRUE}
S <- 100
K <- 105

option_pricer <- create_line_pricer(K = K, r = r, vol = vol)

decay_otm_price <- sapply(days_remaining / 252, function(iterT) option_pricer(S, iterT)['c'])

ggplot() +
    geom_line(aes(x = -days_remaining, y = decay_otm_price)) +
    expand_limits(y = 0) +
    xlab("Days Remaining") +
    ylab("Option Price") +
    ggtitle("Plot of Theta Decay for Out-of-the-Money Option\nS = 100, K = 105")
```

Similar to the ATM case, the option value decay approximates a constant rate,
but loses almost all value earlier than the ATM option. In terms of
distributions of possible outcomes, a positive payoff for the option requires
us to go further into the right tail as time passes. This reduction in
expectation implies a low price for the option with days left before
expiration.


### Time Decay for In-the-Money Options

For ITM options, the intrinsic value of the option is positive, so the price
decays to this value at expiration, as we see in the plot.

```{r premdecay_itm_plot, echo=TRUE}
S <- 100
K <-  95

option_pricer <- create_line_pricer(K = K, r = r, vol = vol)

decay_itm_price <- sapply(days_remaining / 252, function(iterT) option_pricer(S, iterT)['c'])

decay_itm_price[length(decay_itm_price)] <- S - K

ggplot() +
    geom_line(aes(x = -days_remaining, y = decay_itm_price)) +
    expand_limits(y = 0) +
    xlab("Days Remaining") +
    ylab("Option Price") +
    ggtitle("Plot of Theta Decay for In-the-Money Option\nS = 100, K = 95")
```

If we remove the intrinsic value and focus solely on the premium in the ITM
option, how does this behave? We will draw all three plots together.


```{r premdecay_prem_plot, echo=FALSE, results='show'}
plot_dt <- rbind(data.table(contract = '095 Call', days = -days_remaining, premium = decay_itm_price - 5)
                ,data.table(contract = '100 Call', days = -days_remaining, premium = decay_atm_price)
                ,data.table(contract = '105 Call', days = -days_remaining, premium = decay_otm_price)
                )

ggplot(data = plot_dt) +
    geom_line(aes(x = days, y = premium, colour = contract)) +
    expand_limits(y = 0) +
    xlab("Days Remaining") +
    ylab("Option Premium") +
    ggtitle("Plot of Theta Decay in Premium")
```


This plot was surprising, and confirmed something suggested at by the earlier
plots: the premium decay for ITM and OTM options are very similar. Note that
the ITM and OTM options are 5 USD from the underlying of 100.

When viewed in terms of put-call parity, it makes sense: the premium in the
ITM call is similar to the premium in the put from the same line. The 105 Put
will decay in a very similar way to the 95 Call. We can check this:

```{r premdecay_prem_put_plot, echo=TRUE}
S <- 100
K <-  95

option_pricer <- create_line_pricer(K = K, r = r, vol = vol)

decay_105Put_price <- sapply(days_remaining / 252, function(iterT) option_pricer(S, iterT)['p'])

plot_dt <- rbind(data.table(contract = '105 Call', days = -days_remaining, premium = decay_itm_price - 5)
                ,data.table(contract = '105 Put',  days = -days_remaining, premium = decay_105Put_price)
                )

ggplot(data = plot_dt) +
    geom_line(aes(x = days, y = premium, colour = contract)) +
    expand_limits(y = 0) +
    xlab("Days Remaining") +
    ylab("Option Premium") +
    ggtitle("Comparison Plot of Theta Decay in Option Premium for 105 Call and Put")
```

The slightly larger premium in the call over the put is the positive expected
drift in the share price over time due to the risk-free interest rate.

### Consequences of Theta Decay

An immediate consequence of the time decay of premium is that owning options is
expensive. As each day passes, more and more of the value of your portfolio
erodes away, and this is difficult from a psychological point of view. If you
are long options, the stock has to move in your favour at least as much as
your decay or you will suffer a loss in your account.

This makes trading long option positions complicated: it often requires
active trading, the success of which is heavily dependent on making good
estimates of the short term direction of the market - a difficult task. This
will become more apparent in the next section when we discuss how Gamma works.


## Gamma, Vega and Nonlinear Behaviour

It is becoming apparent how complex option behaviour can be. Options are
non-linear instruments, and this nonlinearity results in behaviour that is not
intuitive and surprising.

The Gamma of an option is the second derivative of the option price with
respect to the price of the underlying. It gives us the instantaneous rate of
change of delta as the underlying price changes. For example, suppose we are
long a 30 delta call. The gamma of the option is 10. This means if the stock
price goes up, the delta of the call will be around 40. Long option positions
are always long gamma.

As an exercise for how all this works, we ponder another question. Suppose we
have a 100 call and the stock is at 95, we are 20 days from expiration, the
volatility is 20%. We will use European options for this, purely because
QuantLib gives use the Greeks automatically. The price and Greeks for this
options is as follows:

```{r gvnl_price, echo=TRUE}
calc_price_greeks <- function(...) {
    option_price <- EuropeanOption(...)

    option_price$delta <- option_price$delta * 100
    option_price$gamma <- option_price$gamma * 100
    option_price$vega  <- option_price$vega  * 0.01
    option_price$theta <- option_price$theta/252

    return(unlist(option_price[c('value','delta','gamma','vega','theta')]))
}


price_greeks <- calc_price_greeks(type          = 'call'
                                 ,underlying    = 95
                                 ,strike        = 100
                                 ,riskFreeRate  = 0.01
                                 ,maturity      = 20/252
                                 ,dividendYield = 0
                                 ,volatility    = 0.20)

print(price_greeks)
```

We are long a 20 delta call 5 USD from the money, and the gamma is about 5. If
the stock price goes up 1 USD in a short period of time (so we do not have to
modify the time to maturity), what happens?

We will calculate it and see, but before that, it is worth making some
educated guesses. It will help develop our intuition for this.

The delta of the option is positive, so we are long deltas. This means the
option gains value from a rising share price, so we expect the stock price to
go up. We are also long gamma though, gamma is positive, so this means that as
the stock price goes up, the delta of the option also goes up. Thus, there is
an acceleration in the increase in the price, so the over small increases at
least, we expect the increase in option value to be larger than that implied
by the delta.

So, for a 19 delta call, a contract for 100 shares behaves like it is 19
shares. In our pricing terms (single shares rather than contracts), we thus
expect the option price to go up at least by 0.19 USD. The current price of
the option is 0.57 USD, so we will guess the new price is at least
$0.57 + 0.19 = 0.76$.

The gamma is 5, so we expect the new delta to be about 24, implying a rise
of 0.24 USD due to deltas, so it is $0.57 + 0.24 = 0.81$

We are just trying to get a sense for this, so we could split the difference
and expect the new value of the option to be worth about 0.79 USD.

Let us see how accurate we are:

```{r gvnl_pricechange_up, echo=TRUE}
price_greeks_up <- calc_price_greeks(type          = 'call'
                                    ,underlying    = 96
                                    ,strike        = 100
                                    ,riskFreeRate  = 0.01
                                    ,maturity      = 20/252
                                    ,dividendYield = 0
                                    ,volatility    = 0.20)

print(price_greeks_up)
```

Not too bad for a quick calculation! We got the price about right, as we did
the delta. Note that the gamma has also increased, so further increases in
share price will accelerate the gains in option price further.

What would happen if the price had gone down by 1 USD instead of up?

As we are long deltas, a fall in share price reduces the price of the option,
and the positive gamma means that the delta also decreases. In this scenario,
this is a good thing - a falling share price means a falling delta slows down
the decrease in value. Our intuition might also suggest that the new value for
gamma will be lower.

The current value of the option is 0.57 USD, so with a 19 delta we expect the
new price to be about $0.57 - 0.19 = 0.38$. Accounting for the gamma, the new
delta is about 14, so that price is $0.57 - 0.14 = 0.43$. Overall, we guess a
new price of about 0.40 USD.

```{r gvnl_pricechange_dn, echo=TRUE}
price_greeks_dn <- calc_price_greeks(type          = 'call'
                                    ,underlying    = 94
                                    ,strike        = 100
                                    ,riskFreeRate  = 0.01
                                    ,maturity      = 20/252
                                    ,dividendYield = 0
                                    ,volatility    = 0.20)

print(price_greeks_dn)
```

### Time Effects

What happens if we relax the time assumption of this moves happening over a
short period of time? What happens if the stock price moved 1 USD over the
period of a trading day? Theta decay will certainly be important, and in the
first case, we say that the option had a theta value of about -0.037 USD, so
we expect the price after 1 trading day to be about $0.787 - 0.037 = 0.75$ USD.
It is easy to check (showing the values at the original price level for ease
of reference)

```{r gvnl_pricechange_time, echo=TRUE}
price_greeks_time <- calc_price_greeks(type          = 'call'
                                      ,underlying    = 96
                                      ,strike        = 100
                                      ,riskFreeRate  = 0.01
                                      ,maturity      = 19/252
                                      ,dividendYield = 0
                                      ,volatility    = 0.20)

print(price_greeks)
print(price_greeks_time)
```

Now we are out by a few cents, so our quick calculation is not as good. The
new delta and gamma differ also, which must be due to the time effect. The
delta is lower (24.06 compared to 24.93), and the gamma is higher (5.92
compared to 5.82). Is there an intuitive reason for this?

In the first instance, we still had 20 days left in the option, but in this new
case, we have 19 days. As there is less time for the underlying to move around,
it makes sense that if the stock is at 96, the 100 call with 20 days left is
'closer' to the money than with 19 days left: in the former case, the stock
has more time to get to 100. Thus, the delta of the 19-day option is lower as
delta is a measure of the 'moneyness' of the option.[^memoryaid]

The gamma increase is puzzling. Why does the shortened time horizon lead to an
increase in gamma? Furthermore, what does it mean if the gamma is increasing?

We again rely on intuition about the outcomes for an explanation. For deep in
or out of the money options, the gamma of the option is zero, and the delta of
the option is close to either zero or 100. Gamma only moves off zero as the
underlying price gets close to the strike. How close is 'close'? It depends on
the time remaining in the option, and the volatility. Both affect this outcome
distribution.

As option expiration approaches, the distribution of outcomes narrows so Gamma
will increase if the option is still close to the strike. This effect becomes
much more pronounced in the final few days before expiration.

In our example with the 100 call, 96 is still close to the strike at a vol
level of 20% and so the gamma increases. We expect this to reverse at some
point before expiration, so let us check that:

```{r gvnl_gamma_plot, echo=TRUE}
days_remaining <- seq(40, 0, by = -1)

gamma_value <- sapply(days_remaining, function(iterday) {
    val <- calc_price_greeks(type          = 'call'
                            ,underlying    = 96
                            ,strike        = 100
                            ,riskFreeRate  = 0.01
                            ,maturity      = iterday / 252
                            ,dividendYield = 0
                            ,volatility    = 0.20)

    return(val['gamma'])
})

ggplot() +
    geom_line(aes(x = -days_remaining, y = gamma_value)) +
    xlab("Days Remaining") +
    ylab("Option Gamma")
```


So, if nothing happens but the passing of time, the Gamma of the option will
increase, then rapidly move to zero once 10 days or less are left in the
option.

Why is this relevant?

It shows that as expiration approaches, deltas change a lot. In a live
environment, the stock price is moving around, meaning that when the stock is
close to the strike, the change in deltas is large, and hedging deltas is
likely to prove expensive and counterproductive. It also shows how options can
provide significant leverage, large gammas mean that even small changes in the
underlying can have a huge effect in the value of an option.

To illustrate, suppose a 20% vol stock is at 97 and there are 2 days left on an
option. Suppose we buy the option, and the price opens at 99 on Friday morning
(1 day left), then moves up to 100 at lunchtime, a lifetime of 0.5 days. What
does the option price do?

```{r gvnl_leverage, echo=TRUE}
leverage_pricer <- create_line_pricer(K = 100, r = 0.01, vol = 0.20)

price1 <- leverage_pricer(S = 97,  t = 2.0 / 252)
price2 <- leverage_pricer(S = 99,  t = 1.0 / 252)
price3 <- leverage_pricer(S = 100, t = 0.5 / 252)

print(c(price1['c'], price2['c'], price3['c']), digits = 4)
```

An option that was worth 0.035 USD on Wednesday morning is worth 0.096 USD on
Friday morning, and 0.42 USD at lunchtime on Friday. The call value first
increases threefold over the course of a single trading day due to a 2%
increase in the stock price. It then increases another 450% over the course of
half a trading day where the price increased 1%.[^priceincrease]

This non-linear response of options to changes in the underlying is why options
are so complex. Get it wrong, and you can lose a lot of money very
quickly.[^pnlquickly]

Bear in mind that when trading options, trade sizes are usually in hundreds or
thousands of contracts. 100 contracts is the equivalent of 10,000 shares, so
imagine in the above scenario we bought 1,000 contracts. We paid 3,500 USD
(0.035 * 100 share per contract * 1000 contracts) which was worth 9,500 USD on
Friday morning, and then just over 42,000 USD at lunchtime.


## Summary

My original plan for this series was to have three articles, and this final
post would include discussions on the effect of volatility and how to view
option contracts as insurance, but this was wishful thinking on my behalf. We
will stop for now and digest all we have discussed. There is a lot going on
and requires some thinking about why things behave as they do.

First we looked at the effect of time on option premium, in particular how
premium erodes away as time passes and how the patterns of behaviour of this
decay is different depending on the moneyness of the option. Premium in ATM
options is more durable but then decays rapidly as expiration is imminent.

We then discussed non-linear behaviour and Gamma, the second derivative of the
option price to changes in the underlying. In particular, we discussed how
Gamma functions as a sort of accelerant for option price, greatly magnifying
the profit or loss of the option due to movements in the share price.

The code used to produce all of the above graphs and numbers is available in
BitBucket repo if you would like to play with it yourself. Please get in touch
with any of us here if you would like access.

In the fourth and final article of this series we will discuss volatility: its
effect on prices, how we think about it, and how it behaves as the underlying
moves.


# Fourth Post

In the final post on this series we discuss the use of options as insurance
and try to bring everything together.

In the previous posts we introduced options and discussed various issues
involving them - from a trading perspective and in terms of how they are
priced and how they behave. In this final post we will discuss the use of
options as an insurance product and how the volatility (vol) can add extra
complexity to their behaviour.


## Volatility Effects - Options as Insurance

Till now we ignored the effect of volatility on option prices, assuming
volatility does not change. We discussed in a previous article how option price
and volatility are synonymous: option prices are largely quoted by vol levels
as this is a much more stable measure. We also discussed how the vega of an
option is always positive: higher volatility means higher option
prices.[footnote on how this is true for puts and calls]

When using options, we need to pay attention to two price levels: the price
of the underlying and the volatility level. Both impact the profitability of
the option so it is important to understand this behaviour: the stock price
change is directional, and the volatility change is probabilistic.

When the stock price moves, the option will lose or gain value due to the
delta position - calls gain from a price rise, and lose from a price drop,
puts are the other way around.

Volatility is representation of the future uncertainty of how the underlying
will move - think of it as the cost of insuring against price moves. Highly
volatile stocks have a lot of uncertainty around where the price will be at
expiration - so it is understandable that insuring movements on this stock
will be more expensive.

Furthermore, the underlying price and the volatility level are not independent:
vol levels react to changes in the price of the underlying (and the market as
a whole). Options are forward-looking instruments, so events that impact
expectations of future price changes will impact the price of the option and
hence the volatility level.

To give a simple example, imagine we are trading an option on SPY, an ETF that
tracks the S&P 500 Index of large public companies in the US. Thinking of the
options as insurance against price movements then puts are insurance against
falls in price, and calls are insurance against rises in price.

Suppose the markets have a bad day, and fall by 2.5% - a reasonably large move
by the standards of the last five years or so[footnote but not historically].
What will happen to the price of options on the SPY?

In theory, it depends. In practice, most participants in the markets are long
stocks[footnote why this is so], so this fall in price is bad for them. This
rises the demand for insurance, and option prices go up, bringing up
volatility. When market levels drop, volatility tends to go up.

Conversely, on a good day where the share price rises, demand for insurance
drops, and vol levels fall.

Thus, there is a negative correlation between stock prices and
volatility[footnote on how this is still true for puts].

Thinking of an option contract as an insurance policy may also help explain
why it is difficult to make money from owning options: insurance is often
priced above fair value. This means the vol level implied in the option price
is higher than the price in the underlying is moving.

If you own the option, the option expires in the money but less than the
premium paid to buy the option - losing money overall.

On the plus side, you will not go out of business over night either, but that
may be scant comfort looking at your account balance. Almost all successful
option traders make money by selling options, accepting the risk that adverse
market events may prove ruinous.

Diligent, informed and prudent risk management is essential for staying in
business, so understanding and anticipating what will happen is hugely
important.


## Volatility Smile and Skew

In most option pricing models, the volatility of the underlying is independent
of the strike price of the option. A plot of volatility against strike price
should be a flat, horizontal line. I have SPY option data from May 25, 2015
for the June 26, 2015 expiration.

```{r load_spy_option_data, echo=TRUE}
spy_option_dt <- read_rds("data/spy_options_20150526.rds")

head(spy_option_dt)
```

Now we do a plot for the strike against the implied volatility and see what
we get (we plot calls and puts separately). Note that the closing price for SPY
on that date was 210.70 USD

```{r skew_data_plot, echo=TRUE}
ggplot(data = spy_option_dt) +
    geom_line(aes(x = strike, y = impliedvol, colour = callput)) +
    expand_limits(y = 0) +
    xlab("Strike Price") +
    ylab("Implied Volatility")
```

We see that for both calls and puts there is 'volatility smile' rather than a
flat line. We looked at all strikes for that expiration. There will be almost
no liquidity that far away from the money so we zoom in closer: we look at
strikes from 190 to 230 as these options will be more liquid and hence their
prices are more trustworthy:

```{r skew_data_zoom_plot, echo=TRUE}
ggplot(data = spy_option_dt[strike >= 190 & strike <= 230]) +
    geom_line(aes(x = strike, y = impliedvol, colour = callput)) +
    expand_limits(y = 0) +
    xlab("Strike Price") +
    ylab("Implied Volatility")
```

Implied volatility is higher both below and above the stock price - rounding
out when the strikes are close to and at the money. Why might this be?

Also, it is interesting that the implied vols for the calls below the strike
price end up cheaper than the puts, and the reverse above the strike price.
Why might this happen?

A couple of issues are at play here, so we list them first and discuss them
after

  * The Black-Scholes Model is Wrong
  * Options are Insurance
  * Trader Psychology


### The Black-Scholes Model is Wrong

The Black-Scholes pricing model is a model for pricing options[footnote on
this]. It makes a number of underlying assumptions the most important of which
is that the underlying price follows a lognormal distribution.

This assumption is known to be wrong, and has been from the very early days of
option pricing theory - but as it is simplifying assumption that allows us to
make progress, it is undeniably useful. Nevertheless, it is wrong. Stock price
movements are not lognormal, they have much heavier tails.

Consequently, the Black Scholes model tends to underestimate the probability of
larger price movements.

As an exercise, what are the implications for pricing options? Are there any?

We may guess that it has implications - and we would be right. If option
pricing models systemically underestimate the probability of very large
movements it means that option prices far away from the money are systemically
priced too cheap. What happens to the implied vol for those strikes if we put
our prices up? Implied vol has an increasing relationship with price, so the
implied vols at those strikes are raised.

This makes sense - as strikes move away from the money, the prices rise above
the price given by the model to account for the model error. In turn, this
pulls up the implied vols as we move away from the at the money strikes
creating the 'volatility smile'.

### Options are Insurance

Options are insurance policies against the movement of the underlying stock,
so when we buy an option we buy an insurance policy. Another way to look at
this is that someone else is selling us an insurance policy - taking on the
uncertain risk of the stock moving adversely for them. As the option holder, we
know how much we are putting at risk: the most we can possibly lose is the
premium we paid for the option. For the seller, the opposite is true, they
receive money upfront but may have to pay out much, much more in the future.

As a seller of risk, a trader wants enough premium upfront to make it
worthwhile. This is more straightforward when the strike price of the sold
option is around the money, but if it is far away there is more uncertainty -
as we discussed, real-life price movements only approximately match the
assumptions of the Black-Scholes model.

In the previous post we looked at the leverage effect of options: almost
worthless options can become extremely valuable fast when the markets move
violently - and that turmoil often arrives without a huge amount of warning.
Even worse, when the warnings do come they are often only obvious in retrospect[footnote on false market signals].

We try to think like an option seller. Suppose the volatility of a stock is
around 20% and the stock is at 100. We are approached by a broker to sell 90
strike puts that expire in a month - put options deep out of the money
(often called 'deep puts' for short).

```{r calc_deep_puts, echo=TRUE}
AmericanOption(type          = 'put'
              ,underlying    = 100
              ,strike        = 90
              ,dividendYield = 0
              ,riskFreeRate  = 0.01
              ,maturity      = 20/252
              ,volatility    = 0.2)
```

According to model, this option is worth about 6c. The stock is 10 USD above
the strike, so the chances of this option expiring in the money are very slim,
but if something bad does happen in the next 20 days, there is the chance to
lose a lot, lot more than the 6c we earned by selling it.

There is a price for every risk though, so suppose we decide to quote 15c. What
is the implied vol at this price?

```{r calc_deep_puts_impliedvol, echo=TRUE}
AmericanOptionImpliedVolatility(type          = 'put'
                               ,value         = 0.15
                               ,underlying    = 100
                               ,strike        = 90
                               ,dividendYield = 0
                               ,riskFreeRate  = 0.01
                               ,maturity      = 20/252
                               ,volatility    = 0.2)
```

By putting the price up to 15c we are now quoting an implied vol of 23.6%.
This behaviour is wholly rational. Prior to the October 1987 crash, vol curves
were flat, and a lot of people selling options or 'portfolio insurance' - a
product that functioned like an option - lost a lot of money as they were not
being compensated appropriately for the risk they were taking.


### Trader Psychology

One final contributor to this behaviour is human nature and natural responses
to asymmetric risk. In our previous example we priced at option at a strike of
90. What if the strike were 80?

```{r calc_80deep_puts, echo=TRUE}
print(AmericanOption(type          = 'put'
                    ,underlying    = 100
                    ,strike        = 80
                    ,dividendYield = 0
                    ,riskFreeRate  = 0.01
                    ,maturity      = 20/252
                    ,volatility    = 0.2)
      ,digits = 6)
```

According to the model, this option is worth less than 1% of 1c. No person will
ever sell that risk at that price - the amount is too small and the asymmetric
nature of the risk means it is not a sensible thing to do. At a very minimum,
a trader may be willing to sell it for 1c, but will probably want to do it for
more. We can see what vols are implied by these prices:

```{r calc_80deep_puts_impliedvol, echo=TRUE}
AmericanOptionImpliedVolatility(type          = 'put'
                               ,value         = 0.01
                               ,underlying    = 100
                               ,strike        = 80
                               ,dividendYield = 0
                               ,riskFreeRate  = 0.01
                               ,maturity      = 20/252
                               ,volatility    = 0.2)

AmericanOptionImpliedVolatility(type          = 'put'
                               ,value         = 0.03
                               ,underlying    = 100
                               ,strike        = 80
                               ,dividendYield = 0
                               ,riskFreeRate  = 0.01
                               ,maturity      = 20/252
                               ,volatility    = 0.2)
```

To get these prices of 1c and 3c we need to raise the vol to about 30% and 34%
respectively.


## Volatility Skew

We discussed the volatility smile, but often we observe a 'volatility skew' -
vol is higher on the downside strikes than at equivalent upside strikes. The
curve is not symmetric around the current stock price.

Again, we stop for a minute to think why? We observe the implied vols across
the strikes and see a skew toward the downside. Can we infer anything from
this?

Naturally, we can.

We discuss this further in the next section, but for now we can state that
stock price movements are skewed: the tendency is to have a large number of
smaller positive price moves and a smaller number of large negative moves. When
the market falls, it tends to be in larger drops that happen quickly.

```{r show_spy_returns, echo=TRUE}
spy_data_xts <- getSymbols("SPY"
                          ,start       = '1990-01-01'
                          ,end         = '2015-12-31'
                          ,type        = 'price'
                          ,auto.assign = FALSE)

spy_returns <- (spy_data_xts$SPY.Close / lag(spy_data_xts$SPY.Close))[-1] - 1

ggplot() +
    geom_density(aes(x = as.numeric(spy_returns))) +
    xlab("SPY Daily Return") +
    ylab("Probability Density")


ggplot() +
    geom_line(aes(x = seq_along(spy_returns) / length(spy_returns)
                 ,y = sort(as.numeric(spy_returns)))) +
    xlab("Cumulative Probability") +
    ylab("SPY Daily Return")
```

In the two curves above, we see the distribution of returns is skewed to the
right slightly. The mean of the distribution is not zero - the S&P 500 tends
to grow over the long run. Also, there is a concentration of density in the
smaller numbers above zero, and then a fatter density below zero.

Those plots are a little surprising, the distributions are not as skewed as
expected. Then again, we have been in a cyclic bull market since the early 80s
due to falling interest rates so that affects the return distribution. The
very large positive days in the market occurred around the Credit Crisis of
2008, and are anomalous.

The skewed distribution of returns induces the skew in the strike/implied vol
curve. The skew is how the market 'corrects' for the mismatch between the basic
assumptions of option pricing models and empirical reality.


## Miscellanous Issues

Before we start to wrap everything up, there are a few phenomenon and issues to
discuss.

### Volatility and Stock Price Direction

Capital markets have a natural bias towards the bull side. A large majority of
investors are long equities, and herding behaviour is common and well-known in
market behaviour. Fear and greed are significant drivers of price changes.

Ignoring options for a moment, a number of patterns are observed. In a falling
market, panic sets in. Most investors have long positions, and the
psychological effects of loss aversion prompts selling. Trading volumes rise.
This exacerbates negative moves, so negative days are larger. Unless there is a
total crash, the strong selling resolves the issue faster, running its course
quicker, As a result, while larger in magnitude, the count of those negative
days is smaller.

Conversely, in a rising market, complacency sets in as everyone profits and is
happy. Greed sets in as investors ride the wave of rising prices and do not
take profits for fear of missing out of further profits, so trade volumes fall.
This pattern of behaviour is well-known.

From a volatility perspective, things are a little complex. It is important to
draw a clear distinction between realised volatility (the volatility of the
underlying price movements), and implied volatility. Recall that implied vol is
the vol input to the pricing model required to obtain the price of the options
on the market.

Implied and realised vols are not the same, though they are coupled. Implied
vol is best thought of as a measure of the price of insurance - higher implied
vols means the option market is charging more to insure market moves. As
implied vols are forward-looking in nature, implieds are likely to move ahead
of the realised vol. For example, suppose a company is expected bad earnings,
but releases results that are better than expected. Realised volatility in the
period of time after the announcement (say a day or two) will be high are the
news is digested, but implied volatilities are likely to drop. From an
insurance perspective, we now have much less uncertainty about that company,
so the cost of insuring it is lower.

Another observed phenomenon is that stock price and implied volatility tend to
be inversely correlated - when stock prices drop, implieds go up, and vice
versa. This makes sense from both a mathematical and psychological point of
view.

From the mathematical point of view, when markets fall, the falls are likely to
be big, and get bigger, so there is more risk. As a result, the cost of
insurance goes up and implied volatilities rise. In rising markets, we have
less risk: the smaller, positive rises reduce the risk of adverse events (in
the short term) and so the cost of insuring it go down. Implied vols fall.

Psychologically, in falling markets people panic and want to buy insurance,
caring less than they probably should about the cost. They just want the
protection. Supply has not really changed despite the greater demand, so the
price of insurance and implied vols rise.

In rising markets, investors get complacent and do not want insurance - why
spend premium on protection you do not need? Demand drops and options sellers
are willing to sell more risk as it is lower, so prices and implied vols fall.

The relationship between stock price and implied vol is loose, and is stronger
for index-linked products like ETFs. For single stocks there is much more
individual risk so the pattern is not as strongly observed. It is a phenomenon
worth bearing in mind though, especially for someone who is long volatility -
periods of calm will kill your PnL.


### Option Portfolios

When trading options seriously, it is rare to own individual contracts. In
fact, it is rare to have a trade involving an option alone. Often, option
trades are tied to the underlying stock trade for the deltas. It is also
common to trade *option spreads* - combinations of contracts mentioned in the
second post in this series.

Call and put spreads are especially common: we buy and sell calls at two
different strikes but with the same expiration for example. This allows people
to take bearish and bullish positions while reducing the risk from trading a
lone contract.

This begs the question: how do you manage a portfolio of options? Is it
possible to aggregate positions in some way so we can look at the portfolio as
a whole, rather than have to think about it every time we need to make a
decision? [footnote on the importance of this]

Thankfully, we can, and we can do so easily - at least for options on the same
underlying. The greeks are derivatives, and can be added: if we are long 1500
delta as a result of one option contract and short 800 from another, our net
delta position is long 700 deltas.

Similarly, gamma, theta, and vega are also additive so a portfolio of options
can be analysed by adding all the greeks of all our option position. At a
glance we can see what our net positions are in the greeks, making it easier
for traders and portfolio managers to make decisions on managing risks.

That said, it is still important to pay attention to the individual contracts
in the portfolio - as the composition of the portfolio will have second order
effects.

To illustrate, suppose we have bought and sold a number of calls and puts from
our trading on Monday, when the underlying stock was around 100 USD. As a
result, we have a collection of positions with strikes ranging from 90 to 110.
A few days pass, we do no trades and the stock is now around 80. Regardless of
what our net greek positions are, it makes sense that the stock moving back
towards 100 will be very different to the stock falling further. If the stock
goes up, our overall gamma will pick up, as the stock is moving back near
strikes where we have positions. If it falls, our gamma will get smaller.
This will not be reflected in our aggregate Greek positions.

Reading some of the work on looking at further derivatives of $V$, such
approaches may capture this additional knowledge: second derivatives like
Vega-delta, $\frac{d^2V}{dS d\sigma}$, but I am not sure yet of the utlity of
them. Estimating them numerically is problematic due to numerical rounding
issues, etc.

I could be wrong but my intuition tells me it may be best to stick with the
standard greeks and keep the composition of the portfolio in mind when doing
risk assessments. That said, this is not a strongly held opinion, and could
easily be wrong. It is something I can imagine myself implementing in the
future despite my misgivings.


## Final Thoughts and Conclusions

This series covered a lot of ground! We started with an explanation of what
options are, how they are traded and the infrastructure that has built up
around them. We then discussed some basics of option pricing and option
spreads, and discussed the behaviour of option prices as the values of inputs
change.

You can probably guess there is much, much more to the topic. Options seem
straightforward when explained, but the practical issues of their usage is
deceptively complex. The nonlinearity of their behaviour often surprises, and
it is not well understood that their value is determined along two axes: the
stock price and the volatility level. It is very possible to buy a call,
have the stock do nothing but go up and still lose money on
trade![^readerexercise]

Options, like insurance, are a fascinating topic for anyone interested in
probability and statistics - and they are not well understood. It was my aim
in this series to discuss some intricacies and issues I have not seen elsewhere
in quantitative finance books, but there is a whole lot more in the topic.

I did not need to use too much code for this blog post series, but it is
available to anyone on request - as always, get in touch with us if you have
further questions or comments or would like access to the code.

Quantative finance is a huge topic, and one closely tied to actuarial studies
so understanding the basics is very useful in insurance.





















[^futures]: A futures contract (*future*) is a simple type of derivative
that allows your to buy or sell an asset today and take delivery of
the asset at a future point in time. Futures differ from options in
that entering into a futures contract obligates you to trade and so
function in many ways like stock. I will not really discuss futures
much in this article but a lot of the idiosyncratic nature of options
contracts seems related to the fact that the first options exchanges
were offshoots of futures exchanges. Please let me know if I am wrong
about this.

[^longshort]: This term does make sense, and should be understood by the end
of this series.

[^europeopt]: American options will always be at least as valuable as the
European equivalent as you can always decide to hold the option to
expiration. Thus, it is sometimes useful to price an option as if it
were European purely to obtain a lower bound on the price.

[^risktypes]: My inner cynic also insists that the consequent erection of
competitive barriers to entry plays a non-trivial role too.

[^liquidity]: I've tried a few times and have never been wholly satisfied - it
is a concept that tends to mean different things in different
contexts, but you can usually determine some measure that is close to
what you are after.

[^property]: If this surprises you, think about the expense in time and fees
involved in the buying or selling of a house or piece of commercial
property. It is not something you can do in a few minutes or even
days, and the price is always prone to uncertainty. In contrast, you
can trade a few billion USD or EUR in the currency markets in seconds
or minutes without much problem.

[^earlyfinance]: There are stories of traders on the New York Stock Exchange in
the 1800s carrying revolvers with them when they went to settle trades
with counterparties. Similarly, in the early days of poker-playing in
US a lot of players were armed to ensure they left the card-rooms with
their winnings.

[^counterparty]: Of course, like all risk mitigation strategies, this means
there is now a massive systemic risk of the clearing system
failing. However, were that to occur, it is likely you are looking for
a shotgun, a stock of canned food, and are not thinking about
collecting those call options you bought.

[^contractprice]: Back in the days of floor trading, order sizes of 10
contracts or less were often met with a derisive "would you like a lollipop
with that?"

[^optionpricing]: Wilmott on Quantitative Finance is an excellent resource for
this.

[^moneyness]: An option that is *in-the-money* is an option contract where the
the exercise value of the option is positive. If the exercise value is
negative, the option is *out-of-the-money*.

[^callspread]: The options can also have different expirations, though this is
generally termed a *calendar spread*.

[^expireprice]: Even then, it is probably more fair to say that the 'true'
value stays latent, instead observing a realization of it.

[^priceimplieds]: Some traders at the desks of the larger banks had a
reputation for trying to get you to honour dollar prices on trades, even when
based on stale stock prices. A common response was "why don't I just
write you a cheque right now and save us all the time?"

[^volsqrtime]: It is probably no surprise to learn this is a large
simplification: asset volatility does not scale smoothly across
time. Intra-day volatility is often higher than that measured at
longer time scales.

[^dailyvol]: A common misinterpretation of this is that the 'average move' of
the asset is then 1%, which is false. It is more like 0.80%. The
correct interpretation is that you expect the movement to be 1% or
less two-thirds of the time.

[^calcpricing]: Not to mention that I would probably get some technical details
wrong and look stupid...

[^priceskew]: At first glance, this may seem to only apply to calls, as the
value of a put has a negative relationship to the underlying but that
is not the case due to parity. This will be discussed more in the
third article.

[^pricechange]: This is approximate as vega will have a second derivative, but
for small changes in vol it is close enough. 

[^priceparity]: If the market moves out of line on this, trading arbitrage
will force it back. I heard a story (which I believe) that the first person
to figure out that puts were the same as calls quietly made a huge fortune on
the Chicago option floor with no risk

[^memoryaid]: To aid memory, moneyness describes the intrinsic value of the
option. In-the-money options have positive intrinsic value. Out-of-the-money
options have no intrinsic value. At-the-money options have strike prices very
close to the current underlying price. Recall that delta for calls ranges from
0 to 100, and puts from -100 to 0. In terms of approaching the strike, we
should technically say approach 50 (or -50) delta.

[^priceincrease]: If the price had gone through the strike and kept going,
it would start to lose pace, as the gamma would start to decrease, but the
options would also have intrinsic value.

[^pnlquickly]: Or make it. One of my favourite trading stories which I have
been unable to verify is that one of the larger trading firms today largely
owes its existence to Black Monday in 1987. A market-maker in options,
through pure chance they owned a huge amount of put options that were way
below the market level when the crash happened. Those options, which had cost
them pennies, ended up being worth 50 or 60 USD each and made the firm
millions. This provided them with the capital base to grow their operations
and they admitted themselves it was pure luck.



[^readerexercise]: As a quick spot test for the reader, can you think of a
scenario where this happens? If you can, I will be pleased. It means I have
managed to successfully convey the core concepts to at least one other person! 
