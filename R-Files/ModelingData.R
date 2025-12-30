## ----message=FALSE, warning=FALSE---------------------------------------------------------------------------
library(tidyverse); library(ggplot2); library(ggthemes); library(lubridate); library(car); library(reshape2); library(caret)
library(mgcv); library(gratia)


## -----------------------------------------------------------------------------------------------------------
# data is from the CleaningData.rmd file that combines all datasets used into one final dataset
data <- read.csv("Datasets/FinalData.csv", header=T)

# convert date to date format
data$Date <- as.Date(data$Date)

# check data
tail(data, 10)


## -----------------------------------------------------------------------------------------------------------
# dataset with all variables (2009)
data.2009 <- data %>%
  drop_na()

print(data.2009)

# dataset with all variables (2009) without Covid which is row 134, 135, 136
data.2009.nocovid <- data.2009 %>% slice(-c(134, 135, 136, 137, 138))

# check data
head(data.2009.nocovid)


## -----------------------------------------------------------------------------------------------------------
# dataset starting in 1976
data.1976 <- data %>%
  filter(Date >= "1976-01-01",
         !year(Date) %in% c(2020, 2025))


## -----------------------------------------------------------------------------------------------------------
# dataset starting in 1981
data.1981 <- data %>%
  filter(Date >= "1981-01-01",
         !year(Date) %in% c(2020, 2025))


## -----------------------------------------------------------------------------------------------------------
# dataset starting in 2001
data.2001 <- data %>%
  filter(Date >= "2001-01-01",
         !year(Date) %in% c(2020, 2025),
         Date < "2024-12-31")

print(data.2001)


## -----------------------------------------------------------------------------------------------------------
# create model
full.model <- lm(InitialClaims.LagPer ~ Unemployment + Unemployment.Per + Employment + Employment.Per + LaborForce + LaborForce.Per + SP500.Per + SP500_Health.Per + 
                   SP500_Energy.Per + LFPR + LFPR.Per + Hires + Hires.Per + Separations + Separations.Per + Quits + Quits.Per + Layoffs + Layoffs.Per + Openings + 
                   Openings.Per, data = data.2009) 

# check model
summary(full.model)


## -----------------------------------------------------------------------------------------------------------
# use stepwise regression
full.model.step <- step(full.model, direction = "both", trace = F)

# check model
summary(full.model.step)


## -----------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(full.model.step)


## -----------------------------------------------------------------------------------------------------------
# create model
full.model.nocovid <- lm(InitialClaims.LagPer ~ Unemployment + Unemployment.Per + Employment + Employment.Per + LaborForce + LaborForce.Per + SP500.Per + SP500_Health.Per + 
                   SP500_Energy.Per + LFPR + LFPR.Per + Hires + Hires.Per + Separations + Separations.Per + Quits + Quits.Per + Layoffs + Layoffs.Per + Openings + 
                   Openings.Per, data = data.2009.nocovid) 

# check model
summary(full.model.nocovid)


## -----------------------------------------------------------------------------------------------------------
# use stepwise regression
full.model.nocovid.step <- step(full.model.nocovid, direction = "both", trace = F)

# check model
summary(full.model.nocovid.step)


## -----------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(full.model.nocovid.step)


## -----------------------------------------------------------------------------------------------------------
# Calculate the correlation matrix
cor_matrix <- cor(data.2009.nocovid[, c("Unemployment", "Employment", "LaborForce", "Unemployment.Per", "Employment.Per", "LaborForce.Per", "SP500", "SP500.Per", 
                                        "SP500_Health", "SP500_Health.Per", "SP500_Energy", "SP500_Energy.Per", "LFPR", "LFPR.Per", "Hires", "Hires.Per", 
                                        "Separations", "Separations.Per", "Quits", "Quits.Per", "Layoffs", "Layoffs.Per", "Openings", "Openings.Per")], use = "complete.obs")

# reshape data
cor_long <- melt(cor_matrix)

# create heatmap
ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  # Add grid lines for clarity
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10), # Adjust axis label size
    axis.text.y = element_text(size = 10),  # Adjust y-axis label size
    axis.title.x = element_text(size = 12), # Adjust axis title size
    axis.title.y = element_text(size = 12)
  ) +
  labs(
    title = "Correlation Matrix Heatmap", 
    x = "", 
    y = ""
  )


## -----------------------------------------------------------------------------------------------------------
# Labor Force was a Perfect Predictor so let's make a model without it then get vif values
full.model.nocovid.nolf <- lm(InitialClaims.LagPer ~ Unemployment + Unemployment.Per + Employment + Employment.Per + LaborForce.Per + SP500.Per + SP500_Health.Per + 
                   SP500_Energy.Per + LFPR + LFPR.Per + Hires + Hires.Per + Separations + Separations.Per + Quits + Quits.Per + Layoffs + Layoffs.Per + Openings + 
                   Openings.Per, data = data.2009.nocovid) 


# Calculate VIF values
vif_values <- vif(full.model.nocovid.nolf)
print(vif_values)

# Identify predictors with high VIF
high_vif <- names(vif_values[vif_values > 5])
print(high_vif)


## -----------------------------------------------------------------------------------------------------------
# Create Model
full.model.nocovid.perchange <- lm(InitialClaims.LagPer ~ Unemployment.Per + Employment.Per + LaborForce.Per + SP500.Per + SP500_Health.Per + 
                   SP500_Energy.Per + LFPR.Per + Hires.Per + Separations.Per + Quits.Per + Layoffs.Per + Openings.Per, data = data.2009.nocovid) 


# Calculate VIF values
vif_values <- vif(full.model.nocovid.perchange)
print(vif_values)

# Identify predictors with high VIF
high_vif <- names(vif_values[vif_values > 5])
print(high_vif)


## -----------------------------------------------------------------------------------------------------------
# create model
Mod3 <- lm(InitialClaims.Lag2Per ~ Unemployment.Per + Employment.Per + LaborForce.Per + SP500.Per + SP500_Health.Per + 
                   SP500_Energy.Per + LFPR.Per + Hires.Per + Separations.Per + Quits.Per + Layoffs.Per + Openings.Per, data = data.2009.nocovid) 

# check model
summary(Mod3)


## -----------------------------------------------------------------------------------------------------------
# use stepwise regression
Mod3.step <- step(Mod3, direction = "both", trace = F)

# check model
summary(Mod3.step)


## -----------------------------------------------------------------------------------------------------------
colnames(data.2009.nocovid)


## -----------------------------------------------------------------------------------------------------------
# create plot
ggplot(data.2009.nocovid, aes(x = Unemployment.Per, y = InitialClaims.Lag2Per)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = " ",
       x = " ",
       y = "Initial Claims Percentage Change") +
  theme_minimal()


## -----------------------------------------------------------------------------------------------------------
# create model
model.unemployment <- lm(InitialClaims ~ Unemployment, data = data.2009.nocovid)

# check model
summary(model.unemployment)


## -----------------------------------------------------------------------------------------------------------
# create model
model.unemployment <- lm(InitialClaims.Lag ~ Unemployment, data = data.2009.nocovid)

# check model
summary(model.unemployment)


## -----------------------------------------------------------------------------------------------------------
# create model
model.unemployment <- lm(InitialClaims.Lag2 ~ Unemployment, data = data.2009.nocovid)

# check model
summary(model.unemployment)


## -----------------------------------------------------------------------------------------------------------
# create model
mod1.percap <- lm(InitialClaims.Lag.PerCapita ~ Unemployment + Unemployment.Per + Unemployment.PerCapita + Employment + Employment.Per + Employment.PerCapita +
                    LaborForce + LaborForce.Per + LaborForce.PerCapita + SP500 + SP500.Per +SP500_Health + SP500_Health.Per + SP500_Energy + SP500_Energy.Per + 
                    LFPR + LFPR.Per + Hires + 
                    Hires.Per + Separations + Separations.Per + Quits + Quits.Per + Layoffs + Layoffs.Per + Openings + Openings.Per, data = data.2009.nocovid)

# check model
summary(mod1.percap)


## -----------------------------------------------------------------------------------------------------------
# reduce model 
mod1.percap.step <- step(mod1.percap, direction = "both", trace = F)

# check model 
summary(mod1.percap.step)


## -----------------------------------------------------------------------------------------------------------
# create model
mod2.percap <- lm(InitialClaims.Lag.PerCapita ~ Unemployment.Per + Employment.Per + LaborForce.Per + SP500.Per + SP500_Health.Per + SP500_Energy.Per + LFPR.Per + 
                    Hires.Per + Separations.Per + Quits.Per + Layoffs.Per + Openings.Per, data = data.2009.nocovid)

# check model
summary(mod2.percap)


## -----------------------------------------------------------------------------------------------------------
# reduce model 
mod2.percap.step <- step(mod2.percap, direction = "both", trace = F)

# check model 
summary(mod2.percap.step)


## -----------------------------------------------------------------------------------------------------------
# create model
mod3.percap <- lm(InitialClaims.Lag.PerCapita ~ Unemployment + Employment + LaborForce + SP500 + SP500_Health + SP500_Energy + LFPR + 
                    Hires + Separations + Quits + Layoffs + Openings, data = data.2009.nocovid)

# check model
summary(mod3.percap)


## -----------------------------------------------------------------------------------------------------------
# reduce model 
mod3.percap.step <- step(mod3.percap, direction = "both", trace = F)

# check model 
summary(mod3.percap.step)


## -----------------------------------------------------------------------------------------------------------
# split datasets
set.seed(112233)

# 70% training, 30% testing
train.indices.data.2009.nocovid <- createDataPartition(data.2009.nocovid$InitialClaims.Lag.PerCapita, p = 0.7, list = FALSE)

train_data.data.2009.nocovid <- data.2009.nocovid[train.indices.data.2009.nocovid, ]
test_data.data.2009.nocovid <- data.2009.nocovid[-train.indices.data.2009.nocovid, ]

tail(train_data.data.2009.nocovid)


## -----------------------------------------------------------------------------------------------------------
# create model
mod1.percap.train <- lm(InitialClaims.Lag.PerCapita ~ Unemployment + Unemployment.Per + Unemployment.PerCapita + Employment + Employment.Per + Employment.PerCapita +
                    LaborForce + LaborForce.Per + LaborForce.PerCapita + SP500 + SP500.Per +SP500_Health + SP500_Health.Per + SP500_Energy + SP500_Energy.Per + 
                    LFPR + LFPR.Per + Hires + 
                    Hires.Per + Separations + Separations.Per + Quits + Quits.Per + Layoffs + Layoffs.Per + Openings + Openings.Per, data = train_data.data.2009.nocovid)

# reduce model
mod1.percap.step.train <- step(mod1.percap.train, direction = "both", trace = F)

# gather predictions
mod1.percap.train.pred <- predict(mod1.percap.train, newdata = test_data.data.2009.nocovid)
mod1.percap.step.train.pred <- predict(mod1.percap.step.train, newdata = test_data.data.2009.nocovid)

# MSE
mod1.percap.train.pred.MSE <- mean((test_data.data.2009.nocovid$InitialClaims.Lag.PerCapita - mod1.percap.train.pred)^2)
mod1.percap.step.train.pred.MSE <- mean((test_data.data.2009.nocovid$InitialClaims.Lag.PerCapita - mod1.percap.step.train.pred)^2)

mod1.percap.train.pred.RMSE <- sqrt(mod1.percap.train.pred.MSE)
mod1.percap.step.train.pred.RMSE <- sqrt(mod1.percap.step.train.pred.MSE)

# print values
print(paste("MSE:", mod1.percap.train.pred.MSE))
print(paste("RMSE:", mod1.percap.train.pred.RMSE))
print(paste("MSE (step):", mod1.percap.step.train.pred.MSE))
print(paste("RMSE (step):", mod1.percap.step.train.pred.RMSE))


## -----------------------------------------------------------------------------------------------------------
ggplot(test_data.data.2009.nocovid, aes(x = InitialClaims.Lag.PerCapita, y = mod1.percap.train.pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual",
       x = "Actual Values",
       y = "Predicted Values")


## -----------------------------------------------------------------------------------------------------------
ggplot(test_data.data.2009.nocovid, aes(x = InitialClaims.Lag.PerCapita, y = mod1.percap.step.train.pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual",
       x = "Actual Values",
       y = "Predicted Values")


## -----------------------------------------------------------------------------------------------------------
# create model
mod3.percap.train <- lm(InitialClaims.Lag.PerCapita ~ Unemployment + Employment + LaborForce + SP500 + SP500_Health + SP500_Energy + LFPR + 
                    Hires + Separations + Quits + Layoffs + Openings, data = train_data.data.2009.nocovid)

# reduce model
mod3.percap.step.train <- step(mod3.percap.train, direction = "both", trace = F)

# gather predictions
mod3.percap.train.pred <- predict(mod3.percap.train, newdata = test_data.data.2009.nocovid)
mod3.percap.step.train.pred <- predict(mod3.percap.step.train, newdata = test_data.data.2009.nocovid)

# MSE
mod3.percap.train.pred.MSE <- mean((test_data.data.2009.nocovid$InitialClaims.Lag.PerCapita - mod3.percap.train.pred)^2)
mod3.percap.step.train.pred.MSE <- mean((test_data.data.2009.nocovid$InitialClaims.Lag.PerCapita - mod3.percap.step.train.pred)^2)

mod3.percap.train.pred.RMSE <- sqrt(mod3.percap.train.pred.MSE)
mod3.percap.step.train.pred.RMSE <- sqrt(mod3.percap.step.train.pred.MSE)

# print values
print(paste("MSE:", mod3.percap.train.pred.MSE))
print(paste("RMSE:", mod3.percap.train.pred.RMSE))
print(paste("MSE (step):", mod3.percap.step.train.pred.MSE))
print(paste("RMSE (step):", mod3.percap.step.train.pred.RMSE))


## -----------------------------------------------------------------------------------------------------------
ggplot(test_data.data.2009.nocovid, aes(x = InitialClaims.Lag.PerCapita, y = mod3.percap.train.pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual",
       x = "Actual Values",
       y = "Predicted Values")


## -----------------------------------------------------------------------------------------------------------
ggplot(test_data.data.2009.nocovid, aes(x = InitialClaims.Lag.PerCapita, y = mod3.percap.step.train.pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual",
       x = "Actual Values",
       y = "Predicted Values")


## -----------------------------------------------------------------------------------------------------------
# split datasets
set.seed(112233)

# 70% training, 30% testing
train.indices.data.2009.nocovid <- createDataPartition(data.2009.nocovid$InitialClaims.Lag, p = 0.7, list = FALSE)

train_data.data.2009.nocovid <- data.2009.nocovid[train.indices.data.2009.nocovid, ]
test_data.data.2009.nocovid <- data.2009.nocovid[-train.indices.data.2009.nocovid, ]

tail(train_data.data.2009.nocovid)


## -----------------------------------------------------------------------------------------------------------
# create model
mod1.nom.train <- lm(InitialClaims.Lag ~ Unemployment + Unemployment.Per + Unemployment.PerCapita + Employment + Employment.Per + Employment.PerCapita +
                    LaborForce + LaborForce.Per + LaborForce.PerCapita + SP500 + SP500.Per +SP500_Health + SP500_Health.Per + SP500_Energy + SP500_Energy.Per + 
                    LFPR + LFPR.Per + Hires + 
                    Hires.Per + Separations + Separations.Per + Quits + Quits.Per + Layoffs + Layoffs.Per + Openings + Openings.Per, data = train_data.data.2009.nocovid)

# reduce model
mod1.nom.step.train <- step(mod1.nom.train, direction = "both", trace = F)

# gather predictions
mod1.nom.train.pred <- predict(mod1.nom.train, newdata = test_data.data.2009.nocovid)
mod1.nom.step.train.pred <- predict(mod1.nom.step.train, newdata = test_data.data.2009.nocovid)

# MSE
mod1.nom.train.pred.MSE <- mean((test_data.data.2009.nocovid$InitialClaims.Lag - mod1.nom.train.pred)^2)
mod1.nom.step.train.pred.MSE <- mean((test_data.data.2009.nocovid$InitialClaims.Lag - mod1.nom.step.train.pred)^2)

mod1.nom.train.pred.RMSE <- sqrt(mod1.nom.train.pred.MSE)
mod1.nom.step.train.pred.RMSE <- sqrt(mod1.nom.step.train.pred.MSE)

# print values
print(paste("MSE:", mod1.nom.train.pred.MSE))
print(paste("RMSE:", mod1.nom.train.pred.RMSE))
print(paste("MSE (step):", mod1.nom.step.train.pred.MSE))
print(paste("RMSE (step):", mod1.nom.step.train.pred.RMSE))


## -----------------------------------------------------------------------------------------------------------
ggplot(test_data.data.2009.nocovid, aes(x = InitialClaims.Lag.PerCapita, y = mod3.percap.train.pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual",
       x = "Actual Values",
       y = "Predicted Values")


## -----------------------------------------------------------------------------------------------------------
ggplot(test_data.data.2009.nocovid, aes(x = InitialClaims.Lag.PerCapita, y = mod3.percap.step.train.pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual",
       x = "Actual Values",
       y = "Predicted Values")


## -----------------------------------------------------------------------------------------------------------
# create model
mod3.nom.train <- lm(InitialClaims.Lag ~ Unemployment + Employment + LaborForce + SP500 + SP500_Health + SP500_Energy + LFPR + 
                    Hires + Separations + Quits + Layoffs + Openings, data = train_data.data.2009.nocovid)

# reduce model
mod3.nom.step.train <- step(mod3.nom.train, direction = "both", trace = F)

# gather predictions
mod3.nom.train.pred <- predict(mod3.nom.train, newdata = test_data.data.2009.nocovid)
mod3.nom.step.train.pred <- predict(mod3.nom.step.train, newdata = test_data.data.2009.nocovid)

# MSE
mod3.nom.train.pred.MSE <- mean((test_data.data.2009.nocovid$InitialClaims.Lag - mod3.nom.train.pred)^2)
mod3.nom.step.train.pred.MSE <- mean((test_data.data.2009.nocovid$InitialClaims.Lag - mod3.nom.step.train.pred)^2)

mod3.nom.train.pred.RMSE <- sqrt(mod3.nom.train.pred.MSE)
mod3.nom.step.train.pred.RMSE <- sqrt(mod3.nom.step.train.pred.MSE)

# print values
print(paste("MSE:", mod3.nom.train.pred.MSE))
print(paste("RMSE:", mod3.nom.train.pred.RMSE))
print(paste("MSE (step):", mod3.nom.step.train.pred.MSE))
print(paste("RMSE (step):", mod3.nom.step.train.pred.RMSE))


## -----------------------------------------------------------------------------------------------------------
ggplot(test_data.data.2009.nocovid, aes(x = InitialClaims.Lag.PerCapita, y = mod3.percap.train.pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual",
       x = "Actual Values",
       y = "Predicted Values")


## -----------------------------------------------------------------------------------------------------------
ggplot(test_data.data.2009.nocovid, aes(x = InitialClaims.Lag.PerCapita, y = mod3.percap.step.train.pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual",
       x = "Actual Values",
       y = "Predicted Values")


## -----------------------------------------------------------------------------------------------------------
# Calculate MAPE
MAPE <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# Model 1 Per Capita
mod1.percap.MAPE <- MAPE(test_data.data.2009.nocovid$InitialClaims.Lag.PerCapita, mod1.percap.train.pred)
mod1.percap.step.MAPE <- MAPE(test_data.data.2009.nocovid$InitialClaims.Lag.PerCapita, mod1.percap.step.train.pred)

# Model 3 Per Capita
mod3.percap.MAPE <- MAPE(test_data.data.2009.nocovid$InitialClaims.Lag.PerCapita, mod3.percap.train.pred)
mod3.percap.step.MAPE <- MAPE(test_data.data.2009.nocovid$InitialClaims.Lag.PerCapita, mod3.percap.step.train.pred)

# Model 1 Nominal
mod1.nom.MAPE <- MAPE(test_data.data.2009.nocovid$InitialClaims.Lag, mod1.nom.train.pred)
mod1.nom.step.MAPE <- MAPE(test_data.data.2009.nocovid$InitialClaims.Lag, mod3.nom.step.train.pred)

# Model 3 Nominal
mod3.nom.MAPE <- MAPE(test_data.data.2009.nocovid$InitialClaims.Lag, mod3.nom.train.pred)
mod3.nom.step.MAPE <- MAPE(test_data.data.2009.nocovid$InitialClaims.Lag, mod3.nom.step.train.pred)

# print
print(paste("MAPE Model 1 - Per Capita: ", mod1.percap.MAPE))
print(paste("MAPE Model 1 - Per Capita (step): ", mod1.percap.step.MAPE))
print(paste("MAPE Model 3 - Per Capita: ", mod3.percap.MAPE))
print(paste("MAPE Model 3 - Per Capita (step): ", mod3.percap.step.MAPE))
print(paste("MAPE Model 1 - Nominal: ", mod1.nom.MAPE))
print(paste("MAPE Model 1 - Nominal (step): ", mod1.nom.step.MAPE))
print(paste("MAPE Model 3 - Nominal: ", mod3.nom.MAPE))
print(paste("MAPE Model 3 - Nominal (step): ", mod3.nom.step.MAPE))


## -----------------------------------------------------------------------------------------------------------
examplemod <- lm(InitialClaims.Lag.PerCapita ~ Unemployment.PerCapita, data = data.2009.nocovid)

summary(examplemod)


## -----------------------------------------------------------------------------------------------------------
# create model (drop labor force)
PerCapGAM <- gam(InitialClaims.Lag.PerCapita ~ s(Unemployment.PerCapita) + s(Employment.PerCapita) + s(LaborForce.PerCapita) + s(UnemploymentRate) + s(SP500) + s(SP500_Health) + 
                  s(SP500_Energy) + s(LFPR) + s(Hires) + s(Separations) + s(Quits) + s(Layoffs) + s(Openings), data = data.2009.nocovid, family=binomial)

# check model
summary(PerCapGAM)


## -----------------------------------------------------------------------------------------------------------
exampleGAM <- gam(InitialClaims.Lag.PerCapita ~ s(Unemployment.PerCapita), data = data.2009.nocovid, family=binomial)

draw(exampleGAM)


## -----------------------------------------------------------------------------------------------------------
# create intial model
PerCapMod <- lm(InitialClaims.Lag.PerCapita ~ Unemployment.PerCapita + Employment.PerCapita + LaborForce.PerCapita + UnemploymentRate + SP500 + SP500_Health + 
                  SP500_Energy + LFPR + Hires + Separations + Quits + Layoffs + Openings, data = data.2009.nocovid)

# check model
summary(PerCapMod)


## -----------------------------------------------------------------------------------------------------------
# reduce model
PerCapMod.Step <- step(PerCapMod, trace = F, direction = "both")

# check model
summary(PerCapMod.Step)


## -----------------------------------------------------------------------------------------------------------
confint(PerCapMod.Step)


## -----------------------------------------------------------------------------------------------------------
print(vif(PerCapMod.Step))


## -----------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(PerCapMod)

par(mfrow=c(1,1))
plot(PerCapMod, 4)


## -----------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(PerCapMod.Step)

par(mfrow=c(1,1))
plot(PerCapMod.Step, 4)


## -----------------------------------------------------------------------------------------------------------
# create model
PerCapMod.reduced1 <- lm(InitialClaims.Lag.PerCapita ~ Unemployment.PerCapita + SP500_Energy + Layoffs, data = data.2009.nocovid)

# check model
summary(PerCapMod.reduced1)


## -----------------------------------------------------------------------------------------------------------
# create model
PerCapMod.reduced2 <- lm(InitialClaims.Lag.PerCapita ~ Unemployment.PerCapita, data = data.2009.nocovid)

# check model
summary(PerCapMod.reduced2)


## -----------------------------------------------------------------------------------------------------------
# create model
PerCapMod.train1 <- lm(InitialClaims.Lag.PerCapita ~ Unemployment.PerCapita + SP500_Energy + Layoffs, data = train_data.data.2009.nocovid)
PerCapMod.train2 <- lm(InitialClaims.Lag.PerCapita ~ Unemployment.PerCapita, data = train_data.data.2009.nocovid)

# gather predictions
PerCapMod.train1.pred <- predict(PerCapMod.train1, newdata = test_data.data.2009.nocovid)
PerCapMod.train2.pred <- predict(PerCapMod.train2, newdata = test_data.data.2009.nocovid)

# MSE and RMSE
PerCapMod.train1.pred.MSE <- mean((test_data.data.2009.nocovid$InitialClaims.Lag.PerCapita - PerCapMod.train1.pred)^2)
PerCapMod.train2.pred.MSE <- mean((test_data.data.2009.nocovid$InitialClaims.Lag.PerCapita - PerCapMod.train2.pred)^2)

PerCapMod.train1.pred.RMSE <- sqrt(PerCapMod.train1.pred.MSE)
PerCapMod.train2.pred.RMSE <- sqrt(PerCapMod.train2.pred.MSE)

# print values
print(paste("MSE 1:", PerCapMod.train1.pred.MSE))
print(paste("RMSE 1:", PerCapMod.train1.pred.RMSE))

print(paste("MSE 2:", PerCapMod.train2.pred.MSE))
print(paste("RMSE 2:", PerCapMod.train2.pred.RMSE))


## -----------------------------------------------------------------------------------------------------------
ggplot(test_data.data.2009.nocovid, aes(x = InitialClaims.Lag.PerCapita, y = PerCapMod.train1.pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual of Model 1",
       x = "Actual Values",
       y = "Predicted Values")


## -----------------------------------------------------------------------------------------------------------
ggplot(test_data.data.2009.nocovid, aes(x = InitialClaims.Lag.PerCapita, y = PerCapMod.train2.pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual of Model 2",
       x = "Actual Values",
       y = "Predicted Values")


## -----------------------------------------------------------------------------------------------------------
# model 1
par(mfrow=c(2,2))
plot(PerCapMod.reduced1)

par(mfrow=c(1,1))
plot(PerCapMod.reduced1, 4)


# model 2
par(mfrow=c(2,2))
plot(PerCapMod.reduced2)

par(mfrow=c(1,1))
plot(PerCapMod.reduced2, 4)


## -----------------------------------------------------------------------------------------------------------
# create model
mod1.percaponly <- lm(InitialClaims.Lag.PerCapita ~ Unemployment.PerCapita + Employment.PerCapita + LaborForce.PerCapita, data = data.2009.nocovid)

# check model
summary(mod1.percaponly)


## -----------------------------------------------------------------------------------------------------------
# model
final.model <- lm(log(InitialClaims.Lag.PerCapita) ~ Unemployment.PerCapita + LaborForce.PerCapita + Layoffs, data = data.2001)

# check model
summary(final.model)


## -----------------------------------------------------------------------------------------------------------
# plot diagnostic plots
par(mfrow=c(2,2))
plot(final.model)

# check for outliers
par(mfrow=c(1,1))
plot(final.model, 4)


## -----------------------------------------------------------------------------------------------------------
# check vif values
print(vif(final.model))


## -----------------------------------------------------------------------------------------------------------
# confidence intervals
confint(final.model)


## -----------------------------------------------------------------------------------------------------------
# set seed
set.seed(112233)

# split the data (70-30)
train.indices <- createDataPartition(data.2001$InitialClaims.Lag.PerCapita, p = 0.7, list = FALSE)

train.data <- data.2001[train.indices, ]
test.data <- data.2001[-train.indices, ]

# create training model
training.model <- lm(log(InitialClaims.Lag.PerCapita) ~ Unemployment.PerCapita + LaborForce.PerCapita + Layoffs, data = train.data)

# create predictions
training.model.pred <- predict(training.model, newdata = test.data)

# MSE and RMSE
MSE <- mean((test.data$InitialClaims.Lag.PerCapita - training.model.pred)^2)
RMSE <- sqrt(MSE)

print(MSE)
print(RMSE)

# plot predictions
ggplot(test.data, aes(x = log(InitialClaims.Lag.PerCapita), y = training.model.pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual",
       x = "Actual Values",
       y = "Predicted Values")


## -----------------------------------------------------------------------------------------------------------
test.mod <- lm(log(InitialClaims.Lag.PerCapita) ~ Unemployment.PerCapita + LaborForce.PerCapita + Layoffs + SP500, data = data.2001)

# test.mod.step <- step(test.mod, direction = "both", trace = F)

summary(test.mod)

par(mfrow=c(2,2))
plot(test.mod)


## -----------------------------------------------------------------------------------------------------------
print(vif(test.mod))


## -----------------------------------------------------------------------------------------------------------
# create plot of initial claims % change
ggplot(data, aes(x = Date, y = InitialClaims.Per)) +
  geom_line() +
  labs(title = "Initial Claims Percentage Change Over Time",
       x = "Date",
       y = "Initial Claims Percentage Change") +
  ylim(-100, 100) +
  theme_minimal()


## -----------------------------------------------------------------------------------------------------------
# plot lagged claims
ggplot(data, aes(x = Date, y = InitialClaims.Lag)) +
  geom_line() +
  labs(title = "Initial Claims Over Time (Lagged)",
       x = "Date",
       y = "Initial Claims") +
  xlim(as.Date("2008-01-01"), as.Date("2019-01-01")) +
  ylim(0, 100000) +
  theme_minimal()


## -----------------------------------------------------------------------------------------------------------
# plot 
plot(log(data.1981$InitialClaims.Lag.PerCapita), data.1981$SP500)


## -----------------------------------------------------------------------------------------------------------
# plot
plot(data.2001$LaborForce.PerCapita, data.2001$Unemployment.PerCapita)


## -----------------------------------------------------------------------------------------------------------
# plot 
plot(log(data.2009$InitialClaims.Lag.PerCapita), data.2009$SP500_Health)


## -----------------------------------------------------------------------------------------------------------
# Calculate and reshape the correlation matrix
cor_long <- melt(cor(data.2009.nocovid[, c("Unemployment.PerCapita", "Employment.PerCapita", 
                                           "LaborForce.PerCapita", "SP500", "SP500_Health", 
                                           "SP500_Energy", "LFPR", "Hires", "Separations", 
                                           "Quits", "Layoffs", "Openings")], use = "complete.obs"))

# Assign color groups based on correlation thresholds
cor_long$color_group <- ifelse(cor_long$value > 0.7, "red",
                         ifelse(cor_long$value < -0.7, "blue", "white"))

# Plot
ggplot(cor_long, aes(x = Var1, y = Var2, fill = color_group)) +
  geom_tile(color = "grey80") +
  scale_fill_manual(values = c("red" = "red", "blue" = "blue", "white" = "white")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) +
  labs(
    title = "Correlation Matrix Heatmap (|r| > 0.7 Highlighted)",
    x = "", y = "", fill = "Color"
  )


## -----------------------------------------------------------------------------------------------------------
# Calculate and reshape the correlation matrix
cor_long <- melt(cor(data.2001[, c("Unemployment.PerCapita", "Employment.PerCapita", 
                                           "LaborForce.PerCapita", "LFPR", "Hires", "Separations", 
                                           "Quits", "Layoffs", "Openings")], use = "complete.obs"))

# Assign color groups based on correlation thresholds
cor_long$color_group <- ifelse(cor_long$value > 0.7, "red",
                         ifelse(cor_long$value < -0.7, "blue", "white"))

# Plot
ggplot(cor_long, aes(x = Var1, y = Var2, fill = color_group)) +
  geom_tile(color = "grey80") +
  scale_fill_manual(values = c("red" = "red", "blue" = "blue", "white" = "white")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) +
  labs(
    title = "Correlation Matrix Heatmap (|r| > 0.7 Highlighted)",
    x = "", y = "", fill = "Color"
  )


## -----------------------------------------------------------------------------------------------------------
head(data)

