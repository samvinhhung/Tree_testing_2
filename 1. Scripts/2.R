
# 1.0.1 Descriptive stats on sample.R -------------------------------------

test <- sample_2 %>% 
  mutate(diff_z = c(0,diff(Open)))

diff(c(1,1,1,1,0))

test %>% 
  filter(diff_z == 1)

c(0,diff(c(1,0,1)))

test %>% 
  filter(ClNr == 157)

test %>% 
  filter(ClNr == 157)

test  %>% 
  filter(diff_z == 1)

unique(test$diff_z)
