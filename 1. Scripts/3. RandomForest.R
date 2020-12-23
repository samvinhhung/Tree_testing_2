
# randomForest ------------------------------------------------------------

library(randomForest)
rfNews()


Boston

MASS::Boston

randomForest(medv ~., data = MASS::Boston, mtry = 13, importance = TRUE)
iris

randomForest(Species ~., data = iris, mtry = 4, importance = TRUE)


df_predict_by_dev <- function(x, data = df){
  Open_index <- grep(colnames(data), pattern = "Open")
  data %>% 
    filter(AY >= sort(unique(df$AY), decreasing = T)[x]) %>% 
    filter(RepDel < x) %>% 
    filter(.[[Open_index[x]]] >= 1)
}

prediction_function <- function(dev){
  rpart.dev1 <- rpart(dev_formula(dev), 
                      data = df_train_by_dev(dev), method = "class", 
                      control = rpart.control(cp = 8.143322e-04))
  
  
  dev1.predict <- predict(rpart.dev1, newdata = df_predict_by_dev(dev))
  dev1.predict.sim <- apply(dev1.predict, 1, function(x) sample(0:3, 1,T, prob = x))
  
  # YZXX location
  
  YZXX <- paste0("YZ", pads[dev+1]) 
  
  
  df[df$ClNr %in% df_predict_by_dev(dev)$ClNr,] <- df[df$ClNr %in% df_predict_by_dev(dev)$ClNr,] %>% 
    mutate({{YZXX}} := dev1.predict.sim)
  
  list(rpart.dev1 = rpart.dev1,df = df)
}

for (i in 1:11) {
  df <- prediction_function(i)$df
  print(i)
}



rf_vs_rpart <- randomForest(dev_formula(1), 
                                  data = df_train_by_dev(1), method = "class", 
                                  control = rpart.control(cp = 8.143322e-04))

rf_vs_rpart <- randomForest(YZ01 ~ Pay00 + Open00 + RepDel + inj_part_light + 
               cc_light + LoB, 
             data = df_train_by_dev(1) %>% mutate(YZ01 = as.factor(YZ01)))




# test -------------------------------------------------------------------

library(ipred)

# assess 10-50 bagged trees
ntree <- 10:30

# create empty vector to store OOB RMSE values
rmse <- vector(mode = "numeric", length = length(ntree))

for (i in seq_along(ntree)) {
  # reproducibility
  set.seed(123)
  
  # perform bagged model
  model <- bagging(
    YZ01 ~ Pay00 + Open00 + RepDel + inj_part_light + 
      cc_light + LoB, 
    data = df_train_by_dev(1) %>% mutate(YZ01 = as.factor(YZ01)),
    coob    = TRUE,
    nbagg   = ntree[i]
  )
  # get OOB error
  rmse[i] <- model$err
  print(i)
}
model$call
model$err

rm(model)

plot(ntree[1:30], rmse[1:30], type = 'l', lwd = 2)
abline(v = 25, col = "red", lty = "dashed")

