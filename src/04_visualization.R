# 04_visualization.R

# 라이브러리 로드
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# 데이터 로드
historical_data <- read.csv("data/processed/preprocessed_data.csv")
ml_predictions <- read.csv("results/models/ml_predictions.csv")
rf_importance <- read.csv("results/models/rf_importance.csv")

# 1. 모델별 예측 결과 시각화
# 과거 데이터와 예측 데이터 결합
historical_pop <- data.frame(
  연도 = historical_data$연도,
  인구 = historical_data$`총인구(명)`,
  Type = "Historical"
)

predictions_long <- ml_predictions %>%
  pivot_longer(cols = c(RandomForest, LASSO, Ensemble),
               names_to = "Type",
               values_to = "인구")

combined_data <- rbind(
  historical_pop,
  data.frame(
    연도 = predictions_long$연도,
    인구 = predictions_long$인구,
    Type = predictions_long$Type
  )
)

# 인구 예측 그래프
population_plot <- ggplot(combined_data, aes(x = 연도, y = 인구, color = Type)) +
  geom_line(size = 1.2) +
  labs(title = "Population Prediction by Models",
       x = "Year",
       y = "Population",
       color = "Model") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  scale_color_manual(values = c(
    "Historical" = "gray",
    "RandomForest" = "green2",
    "LASSO" = "red2",
    "Ensemble" = "blue2"
  ))

# 2. Random Forest 변수 중요도 시각화
importance_plot <- ggplot(rf_importance, aes(x = reorder(rownames(rf_importance), IncNodePurity), 
                                           y = IncNodePurity)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Variable Importance in Random Forest Model",
       x = "Variables",
       y = "Importance (IncNodePurity)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

# 3. 모델 성능 비교 시각화
model_performance <- read.csv("results/models/model_performance.csv")
performance_plot <- ggplot(model_performance, aes(x = Model, y = MAE)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Model Performance Comparison",
       x = "Model",
       y = "Mean Absolute Error") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# 그래프 저장
ggsave("results/figures/population_predictions.png", population_plot, width = 12, height = 8)
ggsave("results/figures/variable_importance.png", importance_plot, width = 10, height = 8)
ggsave("results/figures/model_performance.png", performance_plot, width = 8, height = 6)

# 4. 예측 신뢰구간 시각화 (Ensemble 모델)
confidence_data <- ml_predictions %>%
  mutate(
    upper = Ensemble + 2 * sd(Ensemble),
    lower = Ensemble - 2 * sd(Ensemble)
  )

confidence_plot <- ggplot(confidence_data, aes(x = 연도)) +
  geom_line(aes(y = Ensemble), color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(title = "Population Prediction with Confidence Interval",
       x = "Year",
       y = "Population") +
  theme_minimal() +
  scale_y_continuous(labels = comma)

ggsave("results/figures/prediction_confidence.png", confidence_plot, width = 12, height = 8)