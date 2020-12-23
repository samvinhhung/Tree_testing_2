library(rpart)
library(tidyverse)
library(rpart.plot)


test_bias <- rpart(Kyphosis ~.-Number, kyphosis, control = rpart.control(cp = 0, xval = 0, minsplit = 1))

nrow(kyphosis)
rpart.plot::rpart.plot(rpart(Kyphosis ~.-Number, kyphosis, control = rpart.control(cp = 0, xval = 0, minsplit = 1)))


test_bias$splits

test_bias$where

test_bias$cptable


(df2 <- data.frame(split=rpart:::labels.rpart(test_bias), n=test_bias$frame$n))


df3 <- df2 %>% 
  as_tibble() %>% 
  mutate(split = (as.character(split))) %>% 
  mutate(test = str_extract(string = split, pattern = "[a-zA-Z]+")) %>% 
  mutate(val = as.numeric(str_extract(string = split, pattern = "\\d+\\.*\\d*")))

df3[df3$test == "Age", ]$val

df3 <- df3 %>% 
  group_by(test) %>% 
  mutate(n2 = 1:n())


kyphosis %>% 
  ggplot(aes(x = Age, y = Start, col = Kyphosis)) + 
  geom_point() + 
  theme_classic() +
  geom_hline(yintercept = df3[df3$test == "Start", ]$val, linetype = 2) + 
  geom_vline(xintercept = df3[df3$test == "Age", ]$val, linetype = 2) + 
  ggtitle("Splits avec mesure d'impurité de Gini") + 
  theme(
    legend.position = c(0.95, 0.05),
    legend.justification = c("right", "bottom"), legend.box.background = element_rect(colour = "black")
  ) + 
  ggtitle("Scissions pour obtenir un fitting parfait")

rpart.plot::rpart.plot(rpart(Kyphosis ~.-Number, kyphosis, control = rpart.control(cp = 0, xval = 0, minsplit = 1)), fallen.leaves = FALSE)

rpart.plot()

# geom_tile(aes(x = df3[df3$test == "Age", ]$val,
#               y = df3[df3$test == "Start", ]$val), alpha = 0.5, fill = "grey") +

p_test + 
  geom_tile(aes(x = Age, y = Start), data = df3 %>% pivot_wider(names_from = test, values_from = val), 
            alpha = 0.5, fill = "grey")


kyphosis %>% 
  ggplot(aes(x = Age, y = Start, col = Kyphosis)) + 
  geom_point() +
  theme_classic() +
  geom_vline(xintercept = df3[df3$test == "Age", ]$val, linetype = 2) + 
  ggtitle("Splits avec mesure d'impurité de Gini") + 
  theme(
    legend.position = c(0.95, 0.05),
    legend.justification = c("right", "bottom"), legend.box.background = element_rect(colour = "black")
  )


# geom_tile need 

df <- data.frame(
  x = rep(c(2, 5, 7, 9, 12), 2),
  y = rep(c(1, 2), each = 5),
  z = factor(rep(1:5, each = 2)),
  w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
)

df %>% arrange(z,x)

ggplot(df, aes(x, y)) +
  geom_tile(aes(fill = z), colour = "grey50") + 
  scale_x_continuous(breaks = 1:15)+ 
  geom_point()

ggplot(df, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y, ymax = y + 1)) +
  geom_rect(aes(fill = z), colour = "grey50") + 
  geom_point(aes(x = x, y = y))


rpart.control()