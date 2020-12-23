library(rpart)
z.auto <- rpart(Mileage ~ Weight, car.test.frame)
predict(z.auto)

fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
predict(fit, type = "prob")   # class probabilities (default)
predict(fit, type = "vector") # level numbers
predict(fit, type = "class")  # factor
predict(fit, type = "matrix")
        
nrow(car.test.frame)

head(kyphosis)


head()

head(predict(fit, type = "prob"))  # class probabilities (default)
head(predict(fit, type = "vector")) # level numbers
head(predict(fit, type = "class")) # factor
head(predict(fit, type = "matrix"))

as.character(head(predict(fit, type = "class"))) 

plot(fit)

library(rpart)

rpart.plot::rpart.plot(fit)
fit

62/81
