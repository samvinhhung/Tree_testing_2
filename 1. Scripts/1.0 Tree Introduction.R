library(tidyverse)
library(rpart)
library(rpart.plot)
# Load the data -----------------------------------------------------------



# Prep the data -----------------------------------------------------------



# Launch Tree on first development ----------------------------------------



# split by adapted information --------------------------------------------

sample_1
(test2 <- test %>% 
    mutate(key2 = gsub(x = key, "[0-9]", "")))


test2 %>% 
  pivot_wider(names_from = key2)

test2 %>% 
  arrange(ClNr) %>% 
  `[`(1:20,) %>% 
  pivot_wider(names_from = key2)

test2 %>% 
  arrange(ClNr) %>% 
  `[`(1:20,) %>% 
  pivot_wider(names_from = key2, values_from = value)


sample_1[1:10,] %>% 
  pivot_longer(c(starts_with("Pay"), starts_with("Open")), 
               names_to = c("Pay", "Open"), names_pattern = "(.*)([0-9]{2})")



sample_1[1:10,]

gsub("\\d", "", "Pay00")
gsub("[0-9]{1}", "a", "Pay00")  


names_prefix = "Pay")

billboard %>%
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank",
    values_drop_na = TRUE,
  )


test3 <- test[1:40,]


sample_1[1:10,] %>% 
  pivot_longer(c(starts_with("Pay"), starts_with("Open")),
               names_pattern = "(.*[0-9]{2})") %>% 
  mutate(keyP = gsub(x = name, "[0-9]", "")) %>% 
  mutate(keyN = gsub(x = name, "[^0-9]", "")) %>% 
  pivot_wider(names_from = keyP) %>% 
  View()



readr::parse_number("Pay11")


sample_1[1:10,] %>% 
  pivot_longer(c(starts_with("Pay")), names_to = "Pay")  %>% 
  pivot_longer(starts_with("Open"), names_to = "Open", values_to = "Open_value")

test3 <- sample_1[1:10,]

sample_1[1:10,] %>% 
  select(starts_with("Pay"), starts_with("Open")) %>% 
  pivot_longer(everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.)([0-9]{2})"
  )


sample_1[1:10,] %>% 
  select(starts_with("Pay"), starts_with("Open")) %>% 
  pivot_longer(everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.*)([0-9]{2})"
  )


test_final <- sample_1[1:10,] %>% 
  pivot_longer(c(starts_with("Pay"), starts_with("Open")),
               names_to = c(".value", "set"),
               names_pattern = "(.*)([0-9]{2})"
  )


test_final_2 <- test_final %>% 
  mutate(YZ = ifelse((Open >= 1) & (Pay >= 1), 3, 
                     ifelse(Open >= 1 & Pay < 1, 2, 
                            ifelse(Open <= 0 & Pay >= 1, 1, 
                                   ifelse(Open <= 0 & Pay < 1, 0, NA))))) %>% 
  mutate(cal = AY + RepDel)



test_final_2 %>% 
  pivot_wider()

sample_2 <- sample_1 %>% 
  pivot_longer(c(starts_with("Pay"), starts_with("Open")),
               names_to = c(".value", "set"),
               names_pattern = "(.*)([0-9]{2})"
  )

sample_2 <- sample_2 %>% 
  mutate(YZ = ifelse((Open >= 1) & (Pay >= 1), 3, 
                     ifelse(Open >= 1 & Pay < 1, 2, 
                            ifelse(Open <= 0 & Pay >= 1, 1, 
                                   ifelse(Open <= 0 & Pay < 1, 0, NA))))) %>% 
  mutate(cal = AY + RepDel)

sample_2 %>% 
  filter(cal <= 1994+10) %>% 
  mutate(set2 = as.numeric(set)) %>% 
  filter(set2 <= 2004 - AY)



# rpart(YZ ~ inj_part + RepDel, method = "class",  sample_2)

library(rpart)
# split available
rpart(YZ ~  AQ, 
      data =  sample_2, 
      control = rpart.control(maxdepth = 3), method = "class")

.getXlevels()


sample_2

sample_2 %>% filter(Open == 0)

levels(sample_2$inj_part)

# comment because too long
# output_tibble %>% 
#   mutate(PaySum = rowSums(.[starts_with("Pay", vars = .)]))



# Pivot wider for 1st dev prediction -------------------------------------------------------------------------


test <- sample_2 %>% 
  filter(set %in% c("00", "01")) %>% 
  filter(AY != 2005) 



# 1st dev analysis --------------------------------------------------------
# YZ is the result of output of end of Year01


sample_3 <- sample_1 %>% 
  select(ClNr, LoB, cc, AY, AQ, age, inj_part, RepDel, contains("00"), contains("01"))

sample_3 <- sample_3 %>% 
  filter(Open00 != 0) %>% 
  mutate(YZ = ifelse((Open01 >= 1) & (Pay01 >= 1), 3, 
                     ifelse(Open01 >= 1 & Pay01 < 1, 2, 
                            ifelse(Open01 <= 0 & Pay01 >= 1, 1, 
                                   ifelse(Open01 <= 0 & Pay01 < 1, 0, NA))))) 

# take AY 2005 out of sample

sample_3.test <- sample_3 %>% filter(AY == 2005)

sample_3 <- sample_3 %>% filter(AY == 2005)




sample_2 %>% 
  mutate(YZ = ifelse((Open >= 1) & (Pay >= 1), 3, 
                     ifelse(Open >= 1 & Pay < 1, 2, 
                            ifelse(Open <= 0 & Pay >= 1, 1, 
                                   ifelse(Open <= 0 & Pay < 1, 0, NA))))) %>% 
  mutate(cal = AY + RepDel)


df_split <- sample_2 %>% 
  group_by(cc) %>% 
  summarise(ratio = sum(Open) / n())


low <- (df_split[df_split$ratio <= 0.09, ]$cc)
high <- (df_split[df_split$ratio > 0.09, ]$cc)

sample_3.test <- sample_3.test %>% 
  mutate(freq = ifelse(cc %in% low, "low", "high"))  

sample_3 <- sample_3 %>% 
  mutate(freq = ifelse(cc %in% low, "low", "high"))

sample_3.rpart <- rpart(YZ ~ LoB + freq + AQ, data = sample_3, method = "class")

rpart(YZ ~ LoB + freq + AQ, data = sample_3, method = "class", control = rpart.control(cp = 0.1))
rpart(YZ ~ LoB + freq + AQ, data = sample_3, method = "class", control = rpart.control(cp = 0.01))
rpart(YZ ~ LoB + freq + AQ, data = sample_3, method = "class", control = rpart.control(cp = 0.001))  
sample_3.rpart <- rpart(YZ ~ LoB + freq + AQ, data = sample_3, method = "class", control = rpart.control(cp = 0.00))  
rpart(YZ ~ freq, data = sample_3, method = "class", control = rpart.control(cp = 0.00))


for (i in sort(unique(sample_3$cc))) {
  cat("i is: ", i, "\n" )
  temp <- rpart(YZ ~ factor(cc == i), data = sample_3, method = "class", control = rpart.control(cp = 0.00))
  if (length(temp$frame$var) > 1) {
    cat("the cc has been used")
  }
  cat("\n")
}



summary(sample_3.rpart)
rpart.plot(sample_3.rpart)
sum(sample_3.rpart$frame$var == "<leaf>")
library(tree)
cv.tree
prune.misclass
model.frame(sample_3.rpart)


glm(YZ ~ inj_part, data = sample_3)


library(rpart.plot)

sample_3.rpart$call
rpart.plot(sample_3.rpart)


sample_3.test <- sample_3 

table(predict(sample_3.rpart, newdata = sample_3.test, method = "class"), sample_3.test$YZ)

test <- predict(sample_3.rpart, newdata = sample_3.test, type = "class")

sum(diag(table(test, sample_3.test$YZ))) / sum(table(test, sample_3.test$YZ))

table(sample_3$YZ) / length(sample_3$YZ)


summary(test)
sample_3.test

# error row has remplacemet issue
# test  %>% 
#   pivot_wider( id_cols = c("Pay", "Open", values_from = c(Pay, Open)))



# sample_3 ----------------------------------------------------------------

sample_2 %>% 
  filter(Open == 1)

rpart(YZ ~ AQ + freq, data = sample_3, method = "class", 
      control = rpart.control(cp = 0.00001))


sample_3 <- sample_3 %>% filter(Open == 1)



# Desc stats on long tailed -----------------------------------------------

test1 <- sample_2 %>% 
  filter(Open == 1) %>% 
  mutate(set = as.numeric(set)) %>% 
  group_by(ClNr) %>% 
  filter(set == max(set))

plot(test1$set)

hist((test1$set), breaks = 20)
plot(table(test1$set) / sum(table(test1$set)), type = "l")
