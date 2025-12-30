## ----message=FALSE, warning=FALSE----------------------------------------------------------------------------
library(ggplot2); library(tidyverse); library(ggthemes); library(scales)
library(lubridate); library(zoo)


## ------------------------------------------------------------------------------------------------------------
# data is from the CleaningData.rmd file that combines all datasets used into one final dataset
data <- read.csv("Datasets/FinalData.csv", header=T)

# convert date to date format
data$Date <- as.Date(data$Date)

# create recipiciency rate
data <- data %>%
  mutate(RecipiencyRate = InitialClaims / Unemployment)

# check data
tail(data, 10)


## ------------------------------------------------------------------------------------------------------------



## ------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x=Date, y=InitialClaims)) +
  # add recession bars
  geom_rect(aes(xmin=as.Date("1973-11-01"), xmax=as.Date("1975-3-01"), ymin=0, ymax=Inf), fill="red2", alpha=0.002) +
  geom_rect(aes(xmin=as.Date("1980-1-01"), xmax=as.Date("1980-7-01"), ymin=0, ymax=Inf), fill="red2", alpha=0.002) +
  geom_rect(aes(xmin=as.Date("1981-7-01"), xmax=as.Date("1982-11-01"), ymin=0, ymax=Inf), fill="red2", alpha=0.002) +
  geom_rect(aes(xmin=as.Date("1990-7-01"), xmax=as.Date("1991-3-01"), ymin=0, ymax=Inf), fill="red2", alpha=0.002) +
  geom_rect(aes(xmin=as.Date("2001-3-01"), xmax=as.Date("2001-11-01"), ymin=0, ymax=Inf), fill="red2", alpha=0.002) +
  geom_rect(aes(xmin=as.Date("2007-12-01"), xmax=as.Date("2009-06-01"), ymin=0, ymax=Inf), fill="red2", alpha=0.002) +
  geom_rect(aes(xmin=as.Date("2020-2-01"), xmax=as.Date("2020-6-01"), ymin=0, ymax=Inf), fill="red2", alpha=0.002) +
  # add the rest
  geom_line(color = "steelblue4") +
  labs(title="UI Initial Claims From 1976 to 2025", x="", y="") +
  scale_y_continuous(labels = comma) +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(hjust = 0.5))


## ------------------------------------------------------------------------------------------------------------
# cut 2020
data.no2020 <- data %>%
  filter(Date < as.Date("2020-01-01") | Date > as.Date("2020-12-31"))


ggplot(data.no2020, aes(x=Date, y=InitialClaims)) +
  # add recession bars
  geom_rect(aes(xmin=as.Date("1973-11-01"), xmax=as.Date("1975-3-01"), ymin=0, ymax=Inf), fill="red2", alpha=0.002) +
  geom_rect(aes(xmin=as.Date("1980-1-01"), xmax=as.Date("1980-7-01"), ymin=0, ymax=Inf), fill="red2", alpha=0.002) +
  geom_rect(aes(xmin=as.Date("1981-7-01"), xmax=as.Date("1982-11-01"), ymin=0, ymax=Inf), fill="red2", alpha=0.002) +
  geom_rect(aes(xmin=as.Date("1990-7-01"), xmax=as.Date("1991-3-01"), ymin=0, ymax=Inf), fill="red2", alpha=0.002) +
  geom_rect(aes(xmin=as.Date("2001-3-01"), xmax=as.Date("2001-11-01"), ymin=0, ymax=Inf), fill="red2", alpha=0.002) +
  geom_rect(aes(xmin=as.Date("2007-12-01"), xmax=as.Date("2009-06-01"), ymin=0, ymax=Inf), fill="red2", alpha=0.002) +
  geom_rect(aes(xmin=as.Date("2020-2-01"), xmax=as.Date("2020-6-01"), ymin=0, ymax=Inf), fill="red2", alpha=0.002) +
  # add the rest
  geom_line(color = "steelblue4") +
  labs(title="UI Initial Claims From 1976 to 2025 (Zoomed In)", x="", y="") +
  scale_y_continuous(labels = comma) +
  theme_fivethirtyeight() + 
  theme(plot.title = element_text(hjust = 0.5))


## ------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x=Date, y=RecipiencyRate)) +
  geom_line(color = "steelblue") +
  labs(title="UI Recipiency Amongst Unemployed Over Time", x="", y="") +
  ylim(0,1) +
  xlim(as.Date("1976-01-01"), as.Date("2024-12-31")) +
  theme_minimal() + 
  # add recession shading
  geom_rect(aes(xmin=as.Date("2007-12-01"), xmax=as.Date("2009-06-01"), ymin=-Inf, ymax=Inf), fill="salmon", alpha=0.005) +
  geom_rect(aes(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-05-01"), ymin=-Inf, ymax=Inf), fill="salmon", alpha=0.005) +
  geom_rect(aes(xmin=as.Date("2011-01-01"), xmax=as.Date("2012-01-01"), ymin=-Inf, ymax=Inf), fill="steelblue", alpha=0.005)


## ------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x=Date, y=InitialClaims.PerCapita)) +
  geom_line(color = "steelblue") +
  labs(title="Initial Claims Per Capita Over Time", x="", y="") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  # add recession shading
  geom_rect(aes(xmin=as.Date("2007-12-01"), xmax=as.Date("2009-06-01"), ymin=-Inf, ymax=Inf), fill="salmon", alpha=0.005) +
  geom_rect(aes(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-05-01"), ymin=-Inf, ymax=Inf), fill="salmon", alpha=0.005) +
  geom_rect(aes(xmin=as.Date("2011-01-01"), xmax=as.Date("2012-01-01"), ymin=-Inf, ymax=Inf), fill="steelblue", alpha=0.005)


## ------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x=Date, y=InitialClaims.PerCapita)) +
  geom_line(color = "steelblue") +
  labs(title="Initial Claims Per Capita Over Time (Zoomed In)", x="", y="") +
  ylim(0,0.05) +
  theme_minimal() +
  # add recession shading
  geom_rect(aes(xmin=as.Date("2007-12-01"), xmax=as.Date("2009-06-01"), ymin=-Inf, ymax=Inf), fill="salmon", alpha=0.005) +
  geom_rect(aes(xmin=as.Date("2020-02-01"), xmax=as.Date("2020-05-01"), ymin=-Inf, ymax=Inf), fill="salmon", alpha=0.005) +
  geom_rect(aes(xmin=as.Date("2011-01-01"), xmax=as.Date("2012-01-01"), ymin=-Inf, ymax=Inf), fill="steelblue", alpha=0.005)


## ------------------------------------------------------------------------------------------------------------
plot(dataQuarterlyNo2020$InitialClaims.PerCapita, dataQuarterlyNo2020$InitialClaims)
plot(data$Population, data$InitialClaims)
plot(data$Population, data$InitialClaims.PerCapita)
plot(data$Population, data$InitialClaims)

