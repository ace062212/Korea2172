# 03_ml_models.R

# 라이브러리 로드
library(randomForest)
library(glmnet)
library(dplyr)

# 데이터 로드
data_use <- read.csv("data/processed/preprocessed_data.csv")
predict_data <- read.csv("data/processed/forecast_arima.csv")

# 훈련 데이터 준비
train_x <- data_use[, !names(data_use) %in% c("총인구(명)")]
train_y <- data_use$`총인구(명)`

# Random Forest 모델
set.seed(123)
rf_model <- randomForest(
  x = train_x,
  y = train_y,
  ntree = 500,
  importance = TRUE
)

# Random Forest 중요도 저장
importance_rf <- importance(rf_model)
write.csv(importance_rf, "results/models/rf_importance.csv")

# LASSO 모델
x_matrix <- model.matrix(~., data = train_x)[,-1]  # 절편 제거
cv_model <- cv.glmnet(x_matrix, train_y, alpha = 1)
best_lambda <- cv_model$lambda.min
lasso_model <- glmnet(x_matrix, train_y, alpha = 1, lambda = best_lambda)

# 예측 수행
# Random Forest 예측
rf_pred <- predict(rf_model, predict_data)

# LASSO 예측
x_test <- model.matrix(~., data = predict_data)[,-1]
lasso_pred <- predict(lasso_model, s = best_lambda, newx = x_test)

# 앙상블 예측 (RF와 LASSO의 평균)
ensemble_pred <- (rf_pred + lasso_pred) / 2

# 예측 결과 데이터프레임 생성
years <- seq(2073, by = 1, length.out = 100)
predictions_df <- data.frame(
  연도 = years,
  RandomForest = rf_pred,
  LASSO = as.vector(lasso_pred),
  Ensemble = as.vector(ensemble_pred)
)

# 예측 결과 저장
write.csv(predictions_df, "results/models/ml_predictions.csv", row.names = FALSE)

# 모델 성능 지표 계산 (훈련 데이터에 대한)
rf_train_pred <- predict(rf_model, train_x)
x_train_matrix <- model.matrix(~., data = train_x)[,-1]
lasso_train_pred <- predict(lasso_model, s = best_lambda, newx = x_train_matrix)
ensemble_train_pred <- (rf_train_pred + lasso_train_pred) / 2

# MAE 계산
mae_results <- data.frame(
  Model = c("RandomForest", "LASSO", "Ensemble"),
  MAE = c(
    mean(abs(train_y - rf_train_pred)),
    mean(abs(train_y - lasso_train_pred)),
    mean(abs(train_y - ensemble_train_pred))
  )
)

# 성능 지표 저장
write.csv(mae_results, "results/models/model_performance.csv", row.names = FALSE)

# 모델 저장
saveRDS(rf_model, "results/models/rf_model.rds")
saveRDS(lasso_model, "results/models/lasso_model.rds")