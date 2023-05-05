rm(list = ls())
heart <- read.csv('heart.csv', header = T)
data <- heart[c('Age', 'Sex', 'RestBP', 'Chol', 'AHD')]
data$AHD <- ifelse(data$AHD == "Yes", 1, 0)

# a
glm_fit <- glm(AHD ~ ., data = data, family = binomial)
summary(glm_fit)

# b
cv.fn <- function(data, k){
  set.seed(1)
  cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
  fit <- glm(AHD ~ ., data = data, family = binomial)
  cv.error <- cv.glm(data, fit, cost, K = k)$delta
  return(cv.error[1])
}

# c
cv.fn(data, 10)

# d
boot.fn <- function(data){
  set.seed(1)
  age <- c()
  sex <- c()
  restbp <- c()
  chol <- c()
  for (i in 1:1000) {
    sample = data[sample(1:nrow(data), nrow(data), replace = TRUE), ]
    model <-  glm(AHD ~ ., data = sample, family = binomial)
    age <- c(age, model$coefficients[2])
    sex <- c(sex, model$coefficients[3])
    restbp <- c(restbp, model$coefficients[4])
    chol <- c(chol, model$coefficients[5])
  }
  print(mean(age))
  print(sqrt(mean(age^2) - (mean(age))^2))
  
  print(mean(sex))
  print(sqrt(mean(sex^2) - (mean(sex))^2))
  
  print(mean(restbp))
  print(sqrt(mean(restbp^2) - (mean(restbp))^2))
  
  print(mean(chol))
  print(sqrt(mean(chol^2) - (mean(chol))^2))
}
boot.fn(data)
