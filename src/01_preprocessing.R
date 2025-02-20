# 01_preprocessing.R

# 라이브러리 로드
options(scipen = 999)
library(readxl)
library(dplyr)
library(tidyr)

# 데이터 로드
pop_data <- read_excel("data/raw/population_indicators_2024.xlsx")
pop_data = pop_data[,-1]

# 데이터 재구성
data_melt <- pop_data %>%
  reshape2::melt(id.vars = "인구구조,부양비별", 
                 variable.name = "연도",
                 value.name = "값")

# wide format으로 변환
data_use <- spread(data_melt, key = "인구구조,부양비별", value = "값")

# 데이터 타입 변환
data_use$연도 <-  as.numeric(as.character(data_use$연도))
data_use$`성비(여자1백명당)` <- as.numeric(data_use$`성비(여자1백명당)`)
data_use$`- 구성비(%): 0-14세` <- as.numeric(data_use$`- 구성비(%): 0-14세`)
data_use$`- 구성비(%): 15-64세` <- as.numeric(data_use$`- 구성비(%): 15-64세`)
data_use$`- 구성비(%): 65세 이상` <- as.numeric(data_use$`- 구성비(%): 65세 이상`)

# 나머지 수치형 변환
numeric_columns <- c("총인구(명)", "남자(명)", "여자(명)", "인구성장률",
                    "인구(명): 0-14세", "인구(명): 15-64세", "인구(명): 65세 이상",
                    "총부양비", "유소년부양비", "노년부양비", "노령화지수",
                    "중위연령(세)", "중위연령(세)-남자", "중위연령(세)-여자",
                    "평균연령(세)", "평균연령(세)-남자", "평균연령(세)-여자")

data_use[numeric_columns] <- lapply(data_use[numeric_columns], as.numeric)

# NA 처리
data_use[is.na(data_use)] <- 0

# 인구성장률 제거 (ARIMA 분석에서 제외)
data_use <- data_use[, !names(data_use) %in% c("인구성장률")]

# 전처리된 데이터 저장
write.csv(data_use, "data/processed/preprocessed_data.csv", row.names = FALSE)

# 데이터 확인
print(str(data_use))
print(summary(data_use))