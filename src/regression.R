library('openintro')
library('tidyverse')
library("lubridate")
library("caret")
options(digits = 4)

# https://www.kaggle.com/datasets/yellowj4acket/real-estate-california
estate <- read.csv('D:\\Facultate\\Anul 3\\Analiza Datelor\\Scurtu Corneliu\\lab2\\RealEstate_California.csv')

estate <- filter(estate, homeType != "APARTMENT")

estate <- filter(estate, livingArea < 5000 & livingArea > 100 & livingArea < 3500 & price < 5000000 & price > 50000)

selected_data <- estate %>%
  select(price, livingArea, bathrooms, homeType, county, isNewConstruction, event, hasGarage, levels)

set.seed(123) # Set seed for reproducibility
train_index <- sample(1:nrow(selected_data), 0.8 * nrow(selected_data))
train_data <- selected_data[train_index, ]
test_data <- selected_data[-train_index, ]

lm_model <- lm(price ~ livingArea, data = train_data)

lm_model <- train(
  price ~ livingArea ,
  data = train_data,
  method = "lm",  
  trControl = trainControl(method = "cv", number = 10), 
  metric = "RMSE" 
)

print(lm_model)
lm_model <- train(
  price ~ livingArea + bathrooms + homeType + county ,
  data = train_data,
  method = "lm",  
  trControl = trainControl(method = "cv", number = 10), 
  metric = "RMSE" 
)

print(lm_model)

lm_model <- train(
  price ~ .,
  data = train_data,
  method = "lm",  
  trControl = trainControl(method = "cv", number = 10), 
  metric = "RMSE" 
)

predict(lm_model, test_data)

print(lm_model)

# dependenta pret - living area
ggplot(estate, aes(x = livingArea, y = price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Living Area", y = "Price") +
  ggtitle("Scatter Plot of Price vs Living Area")

  
lm(livingArea ~ price, data = estate)



