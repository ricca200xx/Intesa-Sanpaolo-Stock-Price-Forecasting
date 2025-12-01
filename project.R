#library requirement
if (!require("quantmod")) install.packages("quantmod")
if (!require("fpp2")) install.packages("fpp2")
if (!require("zoo")) install.packages("zoo")
if (!require("ggplot2")) install.packages("ggplot2")

library(quantmod)
library(zoo)
library(fpp2)   
library(gridExtra)
library(urca)

#################################################################################
#Phase 1 Add the dataset and data cleaning
#################################################################################

symbol <- "AAPL"
data_fine <- Sys.Date()
data_inizio <- data_fine - (365 * 5)

#caricamento e preparazione dati
data_raw <- getSymbols(symbol, src = "yahoo", auto.assign = FALSE,
                       from = data_inizio, to = data_fine)

adj_price <- data_raw[, 6]

# creazione sequenza completa 365 giorni
all_date <- seq.Date(from = start(adj_price), 
                     to = end(adj_price), 
                     by = "day")

# riempimento dei buchi (Weekends/Festivi)
emp <- xts(order.by = all_date)
full_ts <- merge(emp, adj_price)
all_price <- na.locf(full_ts) 

# Divisione train e test
n_obs <- length(all_price)
split_point <- floor(0.8 * n_obs)

train <- all_price[1:split_point]
test  <- all_price[(split_point + 1):n_obs]

# creazione ts frequency = 365
train_set <- ts(as.numeric(train), frequency = 365)
test_set  <- ts(as.numeric(test), frequency = 365)

# plot
df_plot <- data.frame(
  Data = index(all_price),
  price = as.numeric(all_price),
  type = c(rep("Train", length(train)), rep("Test", length(test))))

ggplot(df_plot, aes(x = Data, y = price, color = type)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("Train" = "#0072B2", "Test" = "#D55E00")) +
  labs(title = "Apple Stock (AAPL)",
       x = "Data", y = "price adjusted ($)") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 year") +
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

