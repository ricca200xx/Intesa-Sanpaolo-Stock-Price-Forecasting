### clearing the global environment
rm(list = ls())

##### temp some function I made in the past that could prove useful #####
aic.manual <- function(myarima, intord = 0, seasord = 0){
  ##temp notes: the function takes the actual data (time series) and then fits 
  ## the arimas, so the input should NOT be an arima, but rather raw data
  aicm <- matrix(0,4,4)
  
  LowestIndex <- 0.0
  MAIndex <- 0
  ARIndex <- 0
  
  for (i in 0:3) for (j in 0:3) {
    fit<-arima(myarima, order = c(i,intord,j), 
               seasonal = list(order = c(0,seasord,0), period = 12), method = "ML")
    aicm[i+1,j+1] <- fit$aic
    
    if (i==0 & j==0) {
      LowestIndex <- fit$aic
    } else if (LowestIndex>aicm[i+1,j+1])  {
      LowestIndex <- fit$aic
      ARIndex <- i
      MAIndex <- j
      
    }
  }
  cat("Lowest Index =",LowestIndex, "\n")
  cat("AR =",ARIndex, "\n")
  cat("MA =",MAIndex, "\n")
  rownames(aicm) <- c(0,1,2,3)
  colnames(aicm) <- c(0,1,2,3)
  aicm
  
}

int.conf <- function(fit.arima) {
  cat("Upper Bound =", fit.arima$coef + (1.96 * diag(fit.arima$var.coef ^ 0.5)), "\n")
  cat("Lower Bound =", fit.arima$coef - (1.96 * diag(fit.arima$var.coef ^ 0.5)), "\n")
}


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

################################################################################
#Phase 0 revenue data
################################################################################
library(readxl)
# revenue_apple_qurter <- read_excel("C:/Users/ricky/OneDrive/Desktop/BEFD project/BEFD-project/revenue_apple_qurter.xlsx")

#################################################################################
#Phase 1 Add the dataset and data cleaning
#################################################################################

symbol <- "AAPL"
data_fine <- Sys.Date()
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
# Non usiamo più 365 perché i weekend non esistono in questo dataset
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

ggplot(df_plot, aes(x = Data, y = price, color = type)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("Train" = "#0072B2", "Test" = "#D55E00")) +
  labs(title = "Apple Stock (AAPL) - Solo Giorni di Trading",
       subtitle = "Frequenza annuale impostata a 252 giorni",
       x = "Data", y = "Price Adjusted ($)") +
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
best_par <- aic.manual(train_set, intord = 1, seasord = 0)
fit_arima <- arima(train_set, order = c(0, 1, 1), method = "ML")

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
library(prophet)

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
n <- nrow(df_prophet)
k_prophet <- sum(m_prophet$params$delta != 0) + 5 

aic_prophet <- n * log(rss_prophet/n) + 2 * k_prophet

# MSE (su test set)
pred_prophet_test <- forecast_prophet$yhat[(n_obs - length(test) + 1):n_obs]
mse_prophet <- mean((test_actual - pred_prophet_test)^2)

################################################################################
# Phase 5: Diffusion Models (DIMORA)
################################################################################
library(DIMORA)

train_values <- as.numeric(train)
diff_price_train <- diff(train_values)
diff_price_train <- na.omit(diff_price_train)

bm_model <- BM(diff_price_train, display = FALSE)
summary(bm_model)

m_base <- 9.479264e+03
p_base <- 4.155503e-06
q_base <- 1.747104e-03

prelim_gbm <- c(m_base, p_base, q_base, 500, 750, -0.2)

gbm_model <- tryCatch({
  GBM(diff_train, shock = "rett", nshock = 1, 
      prelimestimates = prelim_gbm, display = FALSE)
}, error = function(e) { bm_model })

ggm_model <- GGM(diff_train, prelimestimates = c(m_base, 0.001, 0.1, p_base, q_base), display = FALSE)

# Selezione AIC e Calcolo MSE per il vincitore
n_diff <- length(diff_train)
aic_bm <- n_diff * log(sum(residuals(bm_model)^2)/n_diff) + 2 * 3
aic_gbm <- n_diff * log(sum(residuals(gbm_model)^2)/n_diff) + 2 * length(gbm_model$pars)
aic_ggm <- n_diff * log(sum(residuals(ggm_model)^2)/n_diff) + 2 * 5

best_aic_diff <- min(aic_bm, aic_gbm, aic_ggm)

# Ricostruzione Prezzo per MSE
pred_cum_diff <- predict(if(best_aic_diff == aic_gbm) gbm_model else if(best_aic_diff == aic_ggm) ggm_model else bm_model, 
                         newx = 1:(length(all_price) - 1))
pred_price_full <- as.numeric(train[1]) + c(0, pred_cum_diff)
pred_diffusion_test <- pred_price_full[(length(train) + 1):length(all_price)]

mse_diffusion <- mean((test_actual - pred_diffusion_test)^2)

################################################################################
# Phase 6: GAM MODEL
################################################################################
library(gam)

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
# Tabella Riassuntiva Finale
################################################################################

modelli_nomi <- c("Auto-ARIMA", "Prophet", "Diffusion (Best)", "GAM (Best)")
aic_valori   <- c(fit_arima$aic, aic_prophet, best_aic_diff, aic_gam)
mse_valori   <- c(mse_arima, mse_prophet, mse_diffusion, mse_gam)

performance_table <- data.frame(
  Modello = modelli_nomi,
  AIC = round(aic_valori, 2),
  MSE = round(mse_valori, 2)
)

print("--- Performance Comparison Table ---")
print(performance_table)

# Suggerimento sul modello migliore
best_mse_model <- performance_table$Modello[which.min(performance_table$MSE)]
cat("\nIl modello con il minor errore di previsione (MSE) è:", best_mse_model, "\n")



################################################################################
# Visualizzazione Finale: Confronto Fit e Forecast
################################################################################

# Prepariamo i vettori completi (Fitted + Forecast) per ogni modello
# ARIMA
fit_arima_full <- c(as.numeric(fitted(fit_arima)), pred_arima)

# Prophet 
pred_prophet_full <- forecast_prophet$yhat

# Diffusion (già calcolato come pred_price_full)
pred_diff_full <- pred_price_full

# GAM
pred_gam_full <- c(as.numeric(fitted(best_gam)), as.numeric(pred_gam_test))

# Creazione del Grafico
plot(index(all_price), as.numeric(all_price), type="l", col="lightgray", lwd=2,
     main="Comparison of Models: In-Sample Fit & Out-of-Sample Forecast",
     ylab="Price ($)", xlab="Date")

# Aggiungiamo le linee dei modelli
lines(index(all_price), fit_arima_full, col="red", lwd=1, lty=1)       # ARIMA in Rosso
lines(index(all_price), pred_prophet_full, col="blue", lwd=1, lty=1)   # Prophet in Blu
lines(index(all_price), pred_diff_full, col="darkgreen", lwd=1, lty=1) # Diffusion in Verde
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
