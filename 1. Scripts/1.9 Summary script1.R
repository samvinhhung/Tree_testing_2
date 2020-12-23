# cc14+ cc17+ cc3 + cc33 +cc47 +cc48+ cc7

c(3, 7, 14, 17, 33, 47, 47)

head(sample_3$cc) %in% c(14, 37)



useful_cc <- c(3, 7, 14, 17, 33, 47, 47)

sample_4 <- sample_3
sample_4 <- sample_4 %>% 
  mutate(freq = (ifelse(cc %in% useful_cc,
                              as.character(cc), 
                       "general"))) %>% 
  mutate(freq  = factor(freq))


rpart.sample_4 <- rpart(YZ ~ freq, data = sample_4, 
                        method = "class", 
                        control = rpart.control(cp = 0.000))

plotcp(rpart.sample_4)
printcp(rpart.sample_4)

rpart.sample_4 <- rpart(YZ ~ factor(AQ) + freq, data = sample_4, control = rpart.control(cp = 0.000))


plotcp(rpart.sample_4)
printcp(rpart.sample_4)

predict(rpart.sample_4, sample_4, type = )
rpart::xpred.rpart(rpart.sample_4, cp = 0, )




# 2nd dev -----------------------------------------------------------------




