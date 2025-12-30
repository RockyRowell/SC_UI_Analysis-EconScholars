## ----message=FALSE, warning=FALSE, include=FALSE-------------------------------------------------------------
library(tidyverse); library(scales); library(dplyr)
library(markovchain); library(zoo); library(ggplot2)
library(ggthemes)


## ------------------------------------------------------------------------------------------------------------
# for this file we will have to restart with the original claims data 
# in order to use multiple states

# load in claims data
claims <- read.csv("Datasets/ClaimsData/ar5159.csv", header = T)

# fix date column
colnames(claims)[colnames(claims) == "rptdate"] <- "Date"
claims$Date <- as.Date(claims$Date, format = "%m/%d/%Y")

# rename state column (personal preference)
claims$State <- as.character(claims$st)

# create InitialClaims column by combining c2-c7
claims <- claims %>%
  mutate(InitialClaims = c2 + c3 + c4 + c5 + c6 + c7) %>%
  select(Date, State, InitialClaims) # drop unnecessary columns

# drop 2020, 2021 and 2025 from dataset to avoid outlier issues
claims <- claims %>%
  filter(!year(Date) %in% c(2020, 2021, 2025))

# make data quarterly to avoid frequent fluctuations
claims.quarterly <- claims %>%
  group_by(State, year_quarter = as.yearqtr(Date)) %>%
  summarise(InitialClaims = sum(InitialClaims, na.rm = TRUE), .groups = "drop") %>%
  mutate(Date = as.Date(year_quarter, frac = 1)) %>%
  select(State, Date, InitialClaims)


## ------------------------------------------------------------------------------------------------------------
# this function will be used to collect the data for a given state

CollectData <- function(state) {
  # filter dataset for the matching state
  data <- claims.quarterly %>%
    filter(State == state)
  
  # return filtered dataset
  return(data)
} 


## ------------------------------------------------------------------------------------------------------------
# this function will be used to graph the initial claims for a given state

GraphClaims <- function(data) {
  ggplot(data, aes(x = Date, y = InitialClaims)) +
    geom_line() +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") + 
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Initial Claims over Time (excluding 2020, 2021)", 
         x = "", y = "") +
    theme_minimal()
}


## ------------------------------------------------------------------------------------------------------------
# this function will be used to create the markov chain 
# for a given state (data must be filtered first)

CreateMarkovChain <- function(data) {
  # find quantiles
  quantiles <- quantile(data$InitialClaims, probs = c(0.33, 0.66))
  
  # add quantiles to dataset
  data <- data %>%
    mutate(
      InitialClaimsQuantile = case_when(
        InitialClaims <= quantiles[1] ~ "Low",
        InitialClaims > quantiles[1] & InitialClaims <= quantiles[2] ~ "Medium",
        InitialClaims > quantiles[2] ~ "High"
      )
    )
  
  # create vector of states
  states <- data$InitialClaimsQuantile
  
  # fit the markov chain model
  mc_fit <- markovchainFit(data = states)
  mc <- mc_fit$estimate
  
  # vector of correct order (MC doesn't want to do the correct order)
  order <- c("Low", "Medium", "High")
  
  # reorder the matrix
  mc_ordered <- new("markovchain",
                    states = order,
                    transitionMatrix = mc@transitionMatrix[order, order],
                    name = "UI Claims (Ordered)")
  
  # print all results of the markov chain
  print("Summary:")
  summary(mc_ordered)
  print("Transition Matrix:")
  print(round(mc_ordered@transitionMatrix, 3))
  
  # Steady-State Probabilities are here but I excluded them
  # since quantiles implies an even split meaning the probabilities
  # would all be approximately 1/3
  
  # print("Steady-State Probabilities:") 
  # print(steadyStates(mc_ordered))
  
  # plot
  plot(mc_ordered)
}


## ----echo=FALSE, fig.align="center", fig.height=3, fig.width=5, message=FALSE, warning=FALSE-----------------
sc <- CollectData("SC")
GraphClaims(sc)


## ----echo=FALSE, fig.align="center", fig.height=3, fig.width=5, message=FALSE, warning=FALSE-----------------
nc <- CollectData("NC")
GraphClaims(nc)


## ----echo=FALSE, fig.align="center", fig.height=3, fig.width=5, message=FALSE, warning=FALSE-----------------
ga <- CollectData("GA")
GraphClaims(ga)


## ----echo=FALSE, fig.align="center", fig.height=3, fig.width=5, message=FALSE, warning=FALSE-----------------
tn <- CollectData("TN")
GraphClaims(tn)


## ----echo=FALSE, fig.align="center", fig.height=3, fig.width=5, message=FALSE, warning=FALSE-----------------
fl <- CollectData("FL")
GraphClaims(fl)


## ----echo=FALSE, fig.align="center", fig.height=3, fig.width=5, message=FALSE, warning=FALSE-----------------
ca <- CollectData("CA")
GraphClaims(ca)


## ----echo=FALSE, fig.align="center", fig.height=3, fig.width=5, message=FALSE, warning=FALSE-----------------
ny <- CollectData("NY")
GraphClaims(ny)


## ----fig.align = "center", fig.width=4, fig.height=4---------------------------------------------------------
sc <- CollectData("SC")
CreateMarkovChain(sc)


## ----fig.align = "center", fig.width=4, fig.height=4---------------------------------------------------------
nc <- CollectData("NC")
CreateMarkovChain(nc)


## ----fig.align = "center", fig.width=4, fig.height=4---------------------------------------------------------
ga <- CollectData("GA")
CreateMarkovChain(ga)


## ----fig.align = "center", fig.width=4, fig.height=4---------------------------------------------------------
tn <- CollectData("TN")
CreateMarkovChain(tn)


## ----fig.align = "center", fig.width=4, fig.height=4---------------------------------------------------------
fl <- CollectData("FL")
CreateMarkovChain(fl)


## ------------------------------------------------------------------------------------------------------------
ca <- CollectData("CA")
CreateMarkovChain(ca)


## ------------------------------------------------------------------------------------------------------------
ny <- CollectData("NY")
CreateMarkovChain(ny)

