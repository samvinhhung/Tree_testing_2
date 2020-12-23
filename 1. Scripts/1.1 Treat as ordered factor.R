

cars

lm(dist ~speed, data = cars)

lm(dist ~ factor(speed == 4), data = cars)

mean(cars[cars$speed > 4,]$dist)


sample_ordered <- sample_3

is.ordered(sample_ordered$cc)

sample_ordered <- sample_ordered %>% 
  mutate(cc = factor(cc, ordered = TRUE))

rpart.plot((rpart(YZ ~ cc, data = sample_ordered, method = "class", control = rpart.control(cp = 0.0))), faclen = 1)


test <- sample_ordered

test
model.matrix(~cc +0 - ClNr , data = test)

test <- cbind(test, model.matrix(~cc +0 , data = test))

test

rpart.plot((rpart(YZ ~ . -ClNr-LoB-cc-AY-AQ-age-inj_part-RepDel-Pay00-Open00-Pay01-Open01-YZ-freq, data = test, method = "class", control = rpart.control(cp = 0.0))))

as.formula()

paste(names(test[1:14]), collapse = "-")

which(is.na(test))

glimpse(test)

glimpse(test %>% 
  mutate_at(vars(starts_with("cc")), factor)
)

paste(names(test)[starts_with("cc", vars = names(test))][-1], collapse = "+")

test.rpart <- ((rpart(YZ ~ cc1+cc3+cc4+cc5+cc6+cc7+cc8+cc9+cc10+cc11+cc12+cc13+cc14+cc15+cc16+cc17+cc18+cc19+cc20+cc21+cc22+cc23+cc24+cc25+cc26+cc27+cc28+cc29+cc30+cc31+cc32+cc33+cc34+cc36+cc37+cc38+cc39+cc40+cc41+cc42+cc43+cc44+cc45+cc46+cc47+cc48+cc49+cc50+cc51+cc52+cc53,
       data = test %>% 
         mutate_at(vars(starts_with("cc")), factor), 
       method = "class", control = rpart.control(cp = 0.0))))

rpart::printcp(test.rpart)

# cc14+ cc17+ cc3 + cc33 +cc47 +cc48+ cc7
test <- test %>% 
  mutate_at(vars(starts_with("cc")), factor)

test.rpart <- rpart(YZ ~ cc14+ cc17+ cc3 + cc33 +cc47 +cc48+ cc7, 
                    data = test,
                    method = "class",
                    control = rpart.control(cp = 0, xval = 10))

length(unique(test$cc))
length(unique(sample_3$cc))

rpart.plot(test.rpart)

test.rpart$variable.importanceframe$var



# Compare Leaves ----------------------------------------------------------


test.rpart

rpart.ordered <- (rpart(YZ ~ cc, data = sample_ordered, method = "class", control = rpart.control(cp = 0.0)))

rpart.ordered$frame$n
test.rpart$frame$var == "<leaf>" $n

as_tibble(test.rpart$frame[test.rpart$frame$var == "<leaf>",]) %>% arrange(desc(n))
as_tibble(rpart.ordered$frame[rpart.ordered$frame$var == "<leaf>", ]) %>% arrange(desc(n))


sort(cars)



test.predict_dy1 <- predict(test.rpart, newdata = test[1:10000,])

test[1:10000,]

test.predict_dy1


my_tib <- tibble(A = rep(0.8, 100), 
       B = rep(0.2, 100))

my_tib %>% 
  rowwise() %>% 
  mutate(C = sample(c(1,2),size = 1, prob = c(A,B)))

# concatenate predictions and original column
colnames(test.predict_dy1) <- c("p_0", "p_1", "p_2", "p_3")
head(test.predict_dy1)

cbind(test[1:10000,], test.predict_dy1)


checking <- cbind(test[1:10000,], test.predict_dy1) %>% 
  rowwise() %>% 
  mutate(C = sample(c(0:3),size = 1, prob = c(p_0, p_1, p_2, p_3))) %>% 
  ungroup()

checking %>% 
  select(YZ, C) %>% 
  mutate(Fail = (YZ == C)) %>% 
  group_by(YZ, Fail) %>% 
  summarise(n())


table(test[1:10000,]$YZ)
table(test[1:10000,]$YZ, checking$C)

# réalisations en lignes
table(test[1:10000,]$YZ, checking$C) %>% rowSums()

# predictions en colonnes
table(test[1:10000,]$YZ, checking$C) %>% colSums()

# SO question questions/27354087
samp_prob <- data.frame(A = rep(.25, 4), B = c(.5, .1, .2, .2), C = c(.3, .6, .05, .05))

df <- data.frame(a = 1:4, b = 2:5, c = 3:6)
df
sam <- mapply(function(x, y) sample(x, 200, T, y), df, samp_prob)
head(sam)

sample(df[,1], 200, T, samp_prob[,1])

apply(test.predict_dy1[1:2,], 1, sample, replace = T, size = 10, prob = c(0.999,0.001,0.001,0.001))

# with apply we can sample the results of the rpart function

apply(test.predict_dy1[1:3,], 1, function(x) sample(0:3, 1000,T, prob = x)) %>% 
  as_tibble() %>% 
  group_by(`1`)  %>% 
  summarise(n = n()) %>% 
  ungroup() %>% mutate(p = n/sum(n))


simulations <- apply(test.predict_dy1, 1, function(x) sample(0:3, 1000,T, prob = x))

t_simulations <- t(simulations)

table(t(simulations)[1:1,])

dim(test.predict_dy1)

as_tibble(sam) %>% group_by(a) %>% summarise(n = n()) %>% ungroup() %>% mutate(p = n/sum(n))
as_tibble(sam) %>% group_by(b) %>% summarise(n = n())  %>% ungroup() %>% mutate(p = n/sum(n))
as_tibble(sam) %>% group_by(c) %>% summarise(n = n()) %>% ungroup() %>% mutate(p = n/sum(n))

sample()

my_test <- t_simulations[1:1000, 1:1000] %>% as_tibble()

my_test_2 <- my_test %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  summarise(n = n())

# extract histogram
hist((my_test_2[my_test_2$value == 1 ,] %>% 
  group_by(name) %>% 
  summarise(n = sum(n)))$n, breaks = 40)

for (i in 0:3) {
    hist(my_test_2[my_test_2$value == i ,]$n, breaks = 40, main = i) 
    abline(v = sum(test[1:1000,]$YZ == i), col = "red")
}

my_test_2[my_test_2$value == 1 ,]

sum(test[1:1000,]$YZ == 1)
abline(v = sum(test[1:1000,]$YZ == 1), col ="red")

abline(v = sum(test[1:1000,]$YZ == 1 | test[1:1000,]$YZ == 3))

table(test[1:1000,]$YZ)

my_tib %>% 
  rowwise() %>% 
  mutate(C = sample(c(0,1), 1, prob = c(A,B))) %>% 
  group_by(C) %>% 
  summarise(n())
