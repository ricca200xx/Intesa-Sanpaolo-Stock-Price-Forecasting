### clearing the global environment
rm(list = ls())

#library requirement
if (!require("quantmod")) install.packages("quantmod")
if (!require("fpp2")) install.packages("fpp2")
if (!require("zoo")) install.packages("zoo")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("forecast")) install.packages("forecast")
if (!require("gam")) install.packages("gam")
if (!require("prophet")) install.packages("prophet")

library(quantmod)
library(zoo)
library(fpp2)   
library(gridExtra)
library(urca)
library(forecast)
library(gam)
library(prophet)

#################################################################################
#### custom functions
#################################################################################

arima.aic.manual <- function(mydata, intord = 0, seasord = 0){
  aicm <- matrix(0,5,5)
  
  LowestIndex <- 0.0
  MAIndex <- 0
  ARIndex <- 0
  
  SecondLowestIndex <- 0.0  
  MAIndexSecond <- 0
  ARIndexSecond <- 0
  
  for (i in 0:4) for (j in 0:4) {
    fit<-arima(mydata, order = c(i,intord,j), 
               seasonal = list(order = c(0,seasord,0), period = 12), method = "ML")
    aicm[i+1,j+1] <- fit$aic
    
    if (i==0 & j==0) {
      LowestIndex <- fit$aic
      SecondLowestIndex <- fit$aic
    } else if (LowestIndex > aicm[i+1,j+1])  {
      LowestIndex <- fit$aic
      ARIndex <- i
      MAIndex <- j
      
    }else if (SecondLowestIndex > aicm[i+1,j+1]) {
      SecondLowestIndex <- fit$aic
      ARIndexSecond <- i
      MAIndexSecond <- j      
    }
  }
  cat("Lowest Index =",LowestIndex, "\n")
  cat("AR =",ARIndex, "\n")
  cat("MA =",MAIndex, "\n")
  cat("Second Lowest Index =",SecondLowestIndex, "\n")
  cat("AR 2° =",ARIndexSecond, "\n")
  cat("MA 2° =",MAIndexSecond, "\n")
  rownames(aicm) <- c(0,1,2,3,4)
  colnames(aicm) <- c(0,1,2,3,4)
  print(aicm)
  
}

#################################################################################
#Phase 1 Add the dataset and data cleaning
#################################################################################

symbol <- "ISP.MI"
data_fine <- as.Date("2026-01-01")
data_inizio <- data_fine - (365 * 5)

# getSymbols scarica automaticamente solo i giorni di borsa aperta
data_raw <- getSymbols(symbol, src = "yahoo", auto.assign = FALSE,
                       from = data_inizio, to = data_fine)

# Selezioniamo il prezzo rettificato ed eliminiamo eventuali NA residui
all_price <- na.omit(data_raw[, 6]) 

# Divisione train e test (80/20)
n_obs <- length(all_price)
split_point <- floor(0.8 * n_obs)

train <- all_price[1:split_point]
test  <- all_price[(split_point + 1):n_obs]

# Creazione serie storica con frequenza 252 (Standard finanziario)
train_set <- ts(as.numeric(train), frequency = 252)
test_set  <- ts(as.numeric(test), frequency = 252)

# Valori reali del test set per calcolo MSE
test_actual <- as.numeric(test)

# Plot dei dati reali (senza segmenti piatti nei weekend)
df_plot <- data.frame(
  Data = index(all_price),
  price = as.numeric(all_price),
  type = c(rep("Train", length(train)), rep("Test", length(test)))
)

##NB: looking manually at the source it seems that the quotation is done in EUR rather then
##    dollars, so I changed the axes labels accordingly, please make sure its correct. Stefano
ggplot(df_plot, aes(x = Data, y = price, color = type)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("Train" = "#0072B2", "Test" = "#D55E00")) +
  labs(title = "Intesa Sanpaolo S.p.A. (ISP.MI) - Trading days only",
       subtitle = "Annual Frequency set to 252 days",
       x = "Data", y = "Price Adjusted (€)") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(legend.position = "bottom")

#################################################################################
#Phasev2 (Ace e Pacf normal/differentation)
#################################################################################

p1 <- autoplot(Acf(train_set, plot = FALSE, lag.max = 60)) + 
  ggtitle("ACF- Original") + theme_minimal()

p2 <- autoplot(Pacf(train_set, plot = FALSE, lag.max = 60)) + 
  ggtitle("PACF - Original") + theme_minimal()

grid.arrange(p1, p2)

#differentation of the series
diff_train <- diff(train_set)

p3 <- autoplot(Acf(diff_train, plot = FALSE, lag.max = 60)) + 
  ggtitle("ACF - Differentition") + theme_minimal()

p4 <- autoplot(Pacf(diff_train, plot = FALSE, lag.max = 60)) + 
  ggtitle("PACF - Differentition") + theme_minimal()

grid.arrange(p3, p4)

################################################################################
# Phase 3: Auto ARIMA Model
################################################################################

# 1. Stima del modello automatico
arima.aic.manual(train_set, intord = 1, seasord = 0)

fit_arima <- arima(train_set, order = c(4, 1, 4), method = "ML")
summary(fit_arima)

fit_arima <- arima(train_set, order = c(0, 1, 0), method = "ML")
summary(fit_arima)

# 2. Diagnostica dei residui
checkresiduals(fit_arima)

# 3. Previsione sul test set
forecast_arima <- forecast(fit_arima, h = length(test))
pred_arima <- as.numeric(forecast_arima$mean)

# 4. Calcolo MSE
mse_arima <- mean((test_actual - pred_arima)^2)

cat("ARIMA AIC:", fit_arima$aic, "\n")
cat("ARIMA MSE:", mse_arima, "\n")

################################################################################
# Phase 4: Prophet Model
################################################################################
df_prophet <- data.frame(ds = index(train), y  = as.numeric(train))

m_prophet <- prophet(df_prophet, 
                     daily.seasonality = TRUE, 
                     yearly.seasonality = TRUE,
                     weekly.seasonality = TRUE)

future <- make_future_dataframe(m_prophet, periods = length(test), freq = "day")
forecast_prophet <- predict(m_prophet, future)

# AIC (calcolo manuale dai residui training)
y_hat_train <- forecast_prophet$yhat[1:nrow(df_prophet)]
residuals_prophet <- df_prophet$y - y_hat_train
rss_prophet <- sum(residuals_prophet^2)
# Plot residui
plot(df_prophet$ds, residuals_prophet, type="l", main="Prophet residuals (train)",
     xlab="Date", ylab="Residual")
abline(0, 0, 0)
Acf(residuals_prophet, main="Prophet residuals ACF")
Box.test(residuals_prophet, lag = 20, type = "Ljung-Box")

n <- nrow(df_prophet)
k_prophet <- sum(m_prophet$params$delta != 0) + 5 

aic_prophet <- n * log(rss_prophet/n) + 2 * k_prophet

# MSE (su test set)
pred_prophet_test <- forecast_prophet$yhat[(n_obs - length(test) + 1):n_obs]
mse_prophet <- mean((test_actual - pred_prophet_test)^2)

################################################################################
#Holt (as by agreement, just Holt, since missing seasonality)
################################################################################

# setting up the data to account for the limitations of the model
monthly_train <- c()

# old method (past try, found a more elegant and overall better way, so don't 
#             look at this, is kept just for reference)
# Calculate how many full 20-day blocks exist
# num_months <- floor(length(train_set) / 20)
# 
# for (i in 0:(num_months - 1)) {
#   #Defining start and end indices clearly
#   start_idx <- (i * 20) + 1
#   end_idx   <- (i + 1) * 20
#   
#   month_mean <- mean(train_set[start_idx:end_idx])
#   
#   #re-assigning the result to the vector
#   monthly_train <- c(monthly_train, month_mean)
# }

#Creating a grouping index: 1, 1... (20 times), 2, 2... (20 times)
# This handles the end of the vector automatically even if it's < 20
group_index <- ceiling(seq_along(train_set) / 20)

#Using aggregate() to find the mean per group
monthly_train <- aggregate(train_set ~ group_index, FUN = mean)$train_set

#Fitting Holt model and forecasting
holt_fit <- holt(monthly_train, h = 13)
summary(holt_fit)

# 5. Computing MSE
group_index_test <- ceiling(seq_along(test_set) / 20)
monthly_test <- aggregate(test_set ~ group_index_test, FUN = mean)$test_set

raw_holt_forecast <- as.numeric(holt_fit$mean)

mse_holt <- mean((monthly_test - raw_holt_forecast)^2)
#I do have some qualms about this approach tho, that came up to me while writing
#this code, regarding the possibility of too much alteration on the data for
#this forecast to be useful, A. and R. let me know what you think

################################################################################
# Phase 5: GAM MODEL
################################################################################
t_train <- 1:length(train)
gam_data_train <- data.frame(y = as.numeric(train), t = t_train)
gam_data_test  <- data.frame(t = (length(train) + 1):n_obs)

gam_1 <- gam(y ~ t, data = gam_data_train)
gam_2 <- gam(y ~ s(t, df=4), data = gam_data_train)
gam_3 <- gam(y ~ s(t, df=10), data = gam_data_train)

aic_gam <- min(AIC(gam_1), AIC(gam_2), AIC(gam_3))
best_gam <- list(gam_1, gam_2, gam_3)[[which.min(c(AIC(gam_1), AIC(gam_2), AIC(gam_3)))]]

# Previsione e MSE
pred_gam_test <- predict(best_gam, newdata = gam_data_test)
mse_gam <- mean((test_actual - as.numeric(pred_gam_test))^2)


################################################################################
# Final Summary Table
################################################################################

modelli_nomi <- c("Auto-ARIMA", "Prophet", "Diffusion (Best)", "GAM (Best)")
aic_valori   <- c(fit_arima$aic, aic_prophet, aic_ggm, aic_gam)
mse_valori   <- c(mse_arima, mse_prophet, mse_diffusion, mse_gam)

performance_table <- data.frame(
  Modello = modelli_nomi,
  AIC = round(aic_valori, 2),
  MSE = round(mse_valori, 2)
)

print("--- Performance Comparison Table ---")
print(performance_table)


################################################################################
# Final results: comparing Fit e Forecast
################################################################################

# Prepariamo i vettori completi (Fitted + Forecast) per ogni modello
# ARIMA
fit_arima_full <- c(as.numeric(fitted(fit_arima)), pred_arima)

# Prophet 
pred_prophet_full <- forecast_prophet$yhat

#holt
#Retrieve the fitted values from the Holt model
#This is the model's estimate for each of the 63 months
monthly_fitted <- as.numeric(holt_fit$fitted)

#Mapping them back to daily frequency using your existing group_index
#R will repeat monthly_fitted[1] for all days where group_index == 1, etc.
daily_fitted_holt <- monthly_fitted[group_index]

#Expanding the 13 forecast points to the daily test_set length
daily_forecast_holt <- raw_holt_forecast[group_index_test]

#Combining the expanded fitted and forecast values
full_daily_holt <- c(daily_fitted_holt, daily_forecast_holt)

#I don't know if needed but now we can calculate Daily MSE for a direct comparison
mse_daily_holt <- mean((test_set - daily_forecast_holt)^2)

# GAM
pred_gam_full <- c(as.numeric(fitted(best_gam)), as.numeric(pred_gam_test))

# Creazione del Grafico
plot(index(all_price), as.numeric(all_price), type="l", col="lightgray", lwd=2,
     main="Comparison of Models: In-Sample Fit & Out-of-Sample Forecast",
     ylab="Price ($)", xlab="Date")

# Aggiungiamo le linee dei modelli
lines(index(all_price), fit_arima_full, col="red", lwd=1, lty=1)       # ARIMA in Rosso
lines(index(all_price), pred_prophet_full, col="blue", lwd=1, lty=1)   # Prophet in Blu
lines(index(all_price), full_daily_holt, col="darkgreen", lwd=1, lty=1) # Diffusion in Verde
lines(index(all_price), pred_gam_full, col="purple", lwd=1, lty=1)     # GAM in Viola

# Linea verticale per indicare l'inizio del Test Set
abline(v=index(all_price)[split_point], col="black", lty=2, lwd=1.5)
text(index(all_price)[split_point], min(all_price), "Start Test Set", pos=4, cex=0.8)

# Legenda
legend("topleft", 
       legend=c("Actual Price", "Auto-ARIMA", "Prophet", "Diffusion (GGM)", "GAM"),
       col=c("lightgray", "red", "blue", "darkgreen", "purple"), 
       lwd=2, bty="n", cex=0.8)

################################################################################
# Zoom sul Test Set (per vedere meglio l'accuratezza della previsione)
################################################################################

plot(index(test), test_actual, type="l", col="black", lwd=2,
     main="Zoom: Test Set Forecasting Accuracy",
     ylab="Price ($)", xlab="Date", ylim=c(min(test_actual)*0.9, max(test_actual)*1.1))

lines(index(test), pred_arima, col="red", lwd=2)
lines(index(test), pred_prophet_test, col="blue", lwd=2)
lines(index(test), pred_diffusion_test, col="darkgreen", lwd=2)
lines(index(test), pred_gam_test, col="purple", lwd=2)

legend("bottomleft", 
       legend=c("Actual", "ARIMA", "Prophet", "Diffusion", "GAM"),
       col=c("black", "red", "blue", "darkgreen", "purple"), 
       lwd=2, bty="n", cex=0.8)

