# 02_time_series.R

# 라이브러리 로드
library(forecast)
library(dplyr)

# 전처리된 데이터 로드
data_use <- read.csv("data/processed/preprocessed_data.csv")

# 시계열 데이터 생성
ts_data_list <- list(
  구성비0_14 = ts(data_use$`- 구성비(%): 0-14세`, start = c(1960), frequency = 1),
  구성비15_64 = ts(data_use$`- 구성비(%): 15-64세`, start = c(1960), frequency = 1),
  구성비65 = ts(data_use$`- 구성비(%): 65세 이상`, start = c(1960), frequency = 1),
  남자명 = ts(data_use$`남자(명)`, start = c(1960), frequency = 1),
  노년부양비 = ts(data_use$노년부양비, start = c(1960), frequency = 1),
  노령화지수 = ts(data_use$노령화지수, start = c(1960), frequency = 1),
  성비 = ts(data_use$`성비(여자1백명당)`, start = c(1960), frequency = 1),
  여자 = ts(data_use$`여자(명)`, start = c(1960), frequency = 1),
  유소년부양비 = ts(data_use$유소년부양비, start = c(1960), frequency = 1),
  인구0_14 = ts(data_use$`인구(명): 0-14세`, start = c(1960), frequency = 1),
  인구15_64 = ts(data_use$`인구(명): 15-64세`, start = c(1960), frequency = 1),
  인구65 = ts(data_use$`인구(명): 65세 이상`, start = c(1960), frequency = 1),
  중위연령 = ts(data_use$`중위연령(세)`, start = c(1960), frequency = 1),
  중위연령_남자 = ts(data_use$`중위연령(세)-남자`, start = c(1960), frequency = 1),
  중위연령_여자 = ts(data_use$`중위연령(세)-여자`, start = c(1960), frequency = 1),
  총부양비 = ts(data_use$총부양비, start = c(1960), frequency = 1),
  평균연령 = ts(data_use$`평균연령(세)`, start = c(1960), frequency = 1),
  평균연령_남자 = ts(data_use$`평균연령(세)-남자`, start = c(1960), frequency = 1),
  평균연령_여자 = ts(data_use$`평균연령(세)-여자`, start = c(1960), frequency = 1)
)

# ARIMA 모델 적합 및 예측
pre_year <- 100
forecast_results <- list()
arima_models <- list()

for (name in names(ts_data_list)) {
  # auto.arima로 최적 모델 찾기
  arima_model <- auto.arima(ts_data_list[[name]])
  arima_models[[name]] <- arima_model
  
  # 예측 수행
  forecast_results[[name]] <- forecast(arima_model, h = pre_year)
}

# 예측 결과를 데이터프레임으로 변환
years <- seq(2073, by = 1, length.out = pre_year)
forecast_df <- data.frame(연도 = years)

for (name in names(forecast_results)) {
  forecast_df[[name]] <- forecast_results[[name]]$mean
}

# 결과 저장
write.csv(forecast_df, "data/processed/forecast_arima.csv", row.names = FALSE)

# ARIMA 모델 파라미터 저장
arima_params <- data.frame(
  Variable = names(arima_models),
  ARIMA_Order = sapply(arima_models, function(x) paste(x$arma[c(1,6,2)], collapse=",")),
  AIC = sapply(arima_models, function(x) x$aic)
)

write.csv(arima_params, "data/processed/arima_parameters.csv", row.names = FALSE)