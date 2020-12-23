library(tidyverse)
library(rpart)

# 1. Data Simulation  -------------------------------------------------------------------------
vect <- LETTERS[1:4]
df <- data.frame(as.list(vect))

assign_year0 <- function(x, n = 200){
  if (x == "A") {
    sample(1:4, size = n, replace = T, prob = c(1,2,3,4))
  }  else if (x == "B") {
    sample(1:4, size = n, replace = T, prob = c(4,3,2,1))
  }  else if (x == "C") {
    sample(1:4, size = n, replace = T, prob = c(1,2,4,5))
  } else if (x == "D") {
    sample(1:4, size = n, replace = T, prob = c(2,6,3,1))
  }
}

test_rmd2 <- lapply(vect, assign_year0, n = 100)
names(test_rmd2) <- LETTERS[1:4]
test_rmd2 <- as.data.frame(test_rmd2) %>% 
  gather(key , value) %>% as_tibble()


# Rpart classification ----------------------------------------------------


rpart.rmd2 <- rpart( value ~ key, data = test_rmd2, method = "class", parms = list(split = "gini"))
rpart.rmd2 <- rpart( value ~ key, data = test_rmd2, method = "class", parms = list(split = "information"))

rpart( value ~ key, data = test_rmd2, method = "class", parms = list(split = "gini"), control = rpart.control(cp = 0))
rpart( value ~ key, data = test_rmd2, method = "class", parms = list(split = "information"),  control = rpart.control(cp = 0))

kyph_info <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,
      parms = list(split = "information"))
kyph_gini <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,
      parms = list(split = "gini"))

summary(kyph_gini)

unique(kyphosis$Start)

test_kyph <- data.frame(split = 1:length(unique(kyphosis$Start)))

index <- 0
for (i in unique(kyphosis$Start)) {
  index <- index  + 1
  test_kyph[index, "split"] <- i
  test_kyph[index, c("Gini", "Entropy")] <-   kyphosis %>% 
    group_by(Start > i, Kyphosis) %>% 
    summarise(n = n()) %>% 
    mutate(prop = n / sum(n)) %>% 
    mutate(Gini = prop * (1-prop)) %>% 
    mutate(Entropy = - n * log(prop)) %>% ungroup() %>% 
    summarise(Gini = sum(Gini * n),
              Entropy = sum(Entropy)) %>% unlist() %>% as.numeric()
}
i
kyphosis %>% 
  group_by(Start > 18, Kyphosis) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(Gini = prop * (1-prop)) %>% 
  mutate(Entropy = - n * log(prop)) %>% ungroup() %>% 
  summarise(Gini = sum(Gini * n),
            Entropy = sum(Entropy)) %>% unlist() %>% as.numeric()

0.244 * (1-0.244) + 0.244 * (1-0.244) + (1-0.0874) * 0.0874 + (1-0.0874) * 0.0874
1- 0.244^2 - (1-0.244)^2

test_kyph %>% 
  gather(key, value, -split) %>% 
  ggplot(aes(x = as.numeric(split), y = (value), col = key)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(aes(yintercept = min(value))) + 
  scale_x_continuous("split") + 
  theme_classic() + 
  facet_wrap(~key, scales = "free_y", ncol = 1)

plot(test_kyph$split, test_kyph$Gini)
plot(test_kyph$split, test_kyph$Entropy)
test_kyph[1,2:3] <- c(1,1)


test_kyph %>% arrange(desc(split))
plot(test_kyph)

test_rmd2 

test_rmd2 %>% 
  group_by(key %in% c("A", "B", "C", "D"), value) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(Gini = prop * (1-prop)) %>% 
  mutate(Entropy = - n * log(prop)) %>% 
  summarise(sum(Gini))

rpart.rmd2
summary(rpart.rmd)

# 0.6975   * 400 
rpart.rmd2$frame$dev[1] == 400* (1 - max(rpart.rmd$frame[1,]$yval2[6:9]))


is(rpart.rmd$frame[1,]$yval2)


# expected loss 
400 * 0.6975

(1 - sum(rpart.rmd$frame[1,]$yval2[6:9]^2)) * 400

sum((1- (rpart.rmd$frame[1,]$yval2[6:9]^2)) * rpart.rmd$frame[1,]$yval2[2:5])
rpart.rmd$terms


sum(rpart.rmd$frame[1,]$yval2[2:5] * log(rpart.rmd$frame[1,]$yval2[6:9])) /  2 
sum(rpart.rmd$frame[1,]$yval2[2:5] * log(rpart.rmd$frame[1,]$yval2[6:9]))

sum(rpart.rmd$frame[1,]$yval2[2:5] * (rpart.rmd$frame[1,]$yval2[6:9] * (1 - rpart.rmd$frame[1,]$yval2[6:9])))

sum(100 *  (rpart.rmd$frame[1,]$yval2[6:9] * (1 - rpart.rmd$frame[1,]$yval2[6:9])))


test_rmd2 %>% 
  group_by(value) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(deviance = -2 * n * log(prop)) %>% 
  summarise(sum(deviance))

kyphosis

test_rmd2 %>% 
  group_by(key %in% c("B", "D"), value) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(deviance = -2 * n * log(prop)) %>% 
  summarise(sum(deviance))

test_rmd2 %>% 
  group_by(key %in% c("B", "D"), value) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(deviance = -2 * n * log(prop))


test_rmd2 %>% 
  group_by(key %in% c("A", "B", "C", "D"), value) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(deviance = -2 * n * log(prop)) %>% 
  summarise(sum(deviance))


# deviance function for each split ----------------------------------------

class_impurity <- function(split, df = test_rmd2, measure = "Entropy"){
  if (measure == "Entropy") {
    df %>% 
      group_by(key %in% split, value) %>% 
      summarise(n = n()) %>% 
      mutate(prop = n / sum(n)) %>% 
      mutate(deviance = -  n * log(prop)) %>% 
      ungroup() %>% 
      summarise(sum(deviance))
  } else if (measure == "Gini"){
    df %>% 
      group_by(key %in% split, value) %>% 
      summarise(n = n()) %>% 
      mutate(prop = n / sum(n)) %>% 
      mutate(deviance = n * prop * (1-prop)) %>% 
      ungroup() %>% 
      summarise(sum(deviance))
  }

}

test_rmd2 %>% 
  group_by(key %in% c("A", "B", "C", "D"), value) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(Gini = prop * (1-prop)) %>% 
  mutate(Entropy = - n * log(prop)) %>% 
  summarise(sum(Gini))


as.numeric(class_deviance(split))

split
test <- combn(1:4, 4)
combn(1:4, 3)
combn(1:4, 2)
is(combn(1:4, 1))

test[,1]

deviances <- tibble(rule = rep("", 15))
index <- 0

for (j in 4:1) {
  possible_splits <- combn(1:4, j)
  for (i in 1:ncol(possible_splits)) {
    index <- index + 1
    split <- LETTERS[possible_splits[,i]]
    deviances[index,"Entropy"] <- as.numeric(class_impurity(split, measure = "Entropy"))
    deviances[index,"Gini"] <- as.numeric(class_impurity(split, measure = "Gini"))
    deviances[index,"rule"] <- paste0(split, collapse = "-")
    cat("splitting rule: ", split, "\n", as.numeric(deviances[index,"Entropy"]), "\n")
  }
}

p_test <- deviances %>% 
  gather(key, value, -rule) %>% 
  arrange(key, desc(value)) %>% 
  mutate(filtre = paste(key, value)) %>% 
  filter(!duplicated(filtre)) %>% 
  mutate(rule = factor(rule, levels = unique(rule))) 
  # old 
# filter(!duplicated(c(key, value))) %>% 

p_test %>% 
  ggplot(aes(x = as.numeric(rule), y = (value), col = key)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous("split", 
                     breaks = 1:length(levels(p$rule)),
                     labels = levels(p$rule)) + 
  theme_classic() + 
  facet_wrap(~key, scales = "free_y", ncol = 1)


p <-   deviances %>% 
  arrange(desc(Entropy)) %>% 
  filter(!duplicated(Entropy)) %>% 
  mutate(rule = factor(rule, levels = unique(rule)))

p %>% 
  ggplot(aes(x = as.numeric(rule), y = (Entropy))) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous("split", 
                     breaks = 1:length(levels(p$rule)),
                     labels = levels(p$rule)) + 
  theme_classic() + 
  ggtitle("Entropie en fonction du critère de scission")

duplicated(deviances$dev)

deviances %>% 
  gather(key, value, -rule) %>% 
  arrange(key, desc(value)) %>% 
  mutate(filtre = paste(key, value)) %>% 
  filter(!duplicated(filtre)) %>% 
  mutate(rule = factor(rule, levels = unique(rule)))


# outer sum and inner sum of article --------------------------------------------------

test_rmd2 %>% 
  group_by(key %in% c("A", "B", "C", "D"), value) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(deviance = -2 * n * log(prop)) %>% 
  summarise(sum(deviance))


test_rmd2 %>% 
  group_by(key %in% c("B", "D"), value) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(deviance = -2 * n * log(prop))


y 1 2 3 1 2 3 1 2
x1 1 2 3 4 5 6 7 8
x2 1 2 3 4 5 6 1 2
x3 NA 22 38 12 NA 48 14 32

y 3 1 2 3 1 2 1
x1 9 10 11 12 13 14 15
x2 3 4 5 6 1 2 3
x3 40 NA 30 46 28 34 48
