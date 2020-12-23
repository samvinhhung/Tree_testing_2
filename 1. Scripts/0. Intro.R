library(tidyverse)
load("0. Data/output_tibble.Rda")

output_tibble

output_tibble %>% 
  gather(key,value, - 1:-8) %>% 
  mutate(ind_payment = gsub(x = key, "[^0-9]", replacement = ""))


sample_1 <- output_tibble %>% 
  sample_n(size = 50000) %>% 
  arrange(ClNr)


sample_1 %>% 
  gather(key,value, - 1:-8) %>% 
  mutate(ind_payment = gsub(x = key, "[^0-9]", replacement = "")) %>% 
  filter(value != 0) %>% 
  ggplot(aes(x = value)) + geom_histogram(bins = 1000)


test <- sample_1 %>% 
  gather(key,value, - 1:-8) %>% 
  mutate(ind_payment = gsub(x = key, "[^0-9]", replacement = ""))

test %>% 
  filter(value > 0) %>% 
  mutate(my_rank = percent_rank(value)) %>% 
  filter(my_rank < 0.8) %>% 
  ggplot(aes(x = value)) + geom_histogram(bins = 1000)

test2 <- test %>% 
  filter(value > 1) %>% 
  mutate(my_rank = percent_rank(value)) %>% 
  filter(my_rank < 0.8)

test3 <- test %>% 
  filter(value > 1) %>% 
  mutate(my_rank = percent_rank(value)) %>% 
  filter(my_rank > 0.8)


test %>% 
  filter(value > 1) %>% 
  mutate(my_rank = percent_rank(value)) %>% 
  filter(my_rank < 0.98) %>% 
  ggplot(aes(x = value)) + geom_histogram(bins = 50)

test %>% 
  filter(value > 1) %>% 
  mutate(my_rank = percent_rank(value)) %>% 
  filter(my_rank >= 0.98) %>% 
  ggplot(aes(x = value)) + geom_histogram(bins = 50)


test2 %>% 
  ggplot(aes(x = value)) + geom_histogram(bins = 20)

test3 %>% 
  filter(my_rank < 0.99) %>% 
  ggplot(aes(x = value)) + geom_histogram(bins = 30)

min(test2$value)
sum(test2$value == 1)

hist(test2$value)

test %>% filter(ind_payment == "11") %>% filter(value > 0)

unique(test$ind_payment)


unique(sample_1$AY)
unique(sample_1$RepDel)

yearly_tibble <- sample_1 %>% 
  group_by(AY) %>% 
  summarise_at(vars(c(starts_with("Pay"), matches("[0-9]$"))), sum)

yearly_tibble %>% 
  group_by(AY) %>% 
  gather(key, value, - AY) %>%
  filter(grepl(key, pattern = "Open") == TRUE) %>%
  arrange(AY, key) %>%
  mutate(DY = 1:n())


# Last data ---------------------------------------------------------------

# save
sa

# load

