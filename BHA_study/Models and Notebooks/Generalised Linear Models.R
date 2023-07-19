# GENERALISED LINEAR MODELS 
install.packages("glmm")
library(glm2)


data <- read.csv("C:/Users/romar/OneDrive/Desktop/R studio/overall concordance matrix f1 to f4.csv")


x <- as.matrix(data[, c("f1", "f2", "f3", "f4")])
y <- data$App_ID


model <- glm(y ~  , family = "binomial", data = data)


summary(model)

