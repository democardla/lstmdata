file <- read_csv("/Users/democardla/Desktop/R/布病时间序列/data/2017-2023_copy.csv")
file$`Unnamed: 0` <- NULL
file$date <- as.character(file$date)

splited.sample <- arima.split.sample(file, cut.point = "2023-01-01")

train <- splited.sample$train %>% arima.ts.data.frame()

check <- splited.sample$check %>% arima.ts.data.frame()

temp <- splited.sample$check %>% arima.ts.data.frame()


model.list <- list(
  fit.1 = arima(train$incidence.rate, order = c(2, 1, 4)),
  fit.2 = arima(train$incidence.rate, order = c(7, 1, 4)),
  fit.3 = arima(train$incidence.rate, order = c(4, 1, 4))
)



seasonal.model.list <- list(
  fit.1 = Arima(train$incidence.rate, order = c(0, 1, 0), seasonal = c(0, 1, 1))
)

# fit.1 <- auto.arima(ts(splited.sample$train$incidence.rate, frequency = 12), seasonal = TRUE)







temp <- arima.coef.se(seasonal.model.list$fit.1)
temp.1 <- arima.coef.se(model.list$fit.1)
