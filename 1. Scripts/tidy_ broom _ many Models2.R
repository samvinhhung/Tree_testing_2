
# David Robinson ----------------------------------------------------------

crossing(trial = 1:50000,
         step = 1:1000)


# nest


cars %>% 
  group_by(speed, dist) %>% 
  nest() %>% 
  mutate(lm_model = lapply(data, function(df) lm(dist ~ +0, data = df)))


df <- mtcars %>%
  group_by(cyl) %>%
  nest() %>%
  mutate(models = lapply(data, function(df) lm(mpg ~ wt, data = df)))

df$models

df %>% 
  unnest_wider(A = list(data, 1L))


df <- tibble(
  x = 1:3,
  y = list(NULL, 1:3, 4:5)
)

df %>% unnest_longer(y)
df %>% unnest_wider(y)





# R 4 ds ------------------------------------------------------------------

library(gapminder)

by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()

str(by_country$data[1])

by_country$model[1]


as.list(by_country$data[1])

country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

by_country <- by_country %>% 
  mutate(model  = purrr::map(data, country_model))

library(modelr)
modelr::add_predictions()

df <- tibble::data_frame(
  x = sort(runif(100)),
  y = 5 * x + 0.5 * x ^ 2 + 3 + rnorm(length(x))
)
plot(df)

m1 <- lm(y ~ x, data = df)
grid <- data.frame(x = seq(0, 1, length = 10))

grid %>% 
  add_predictions(var = "pred2", m1) %>% 
  add_predictions(var = "pred1", m1)


modelr::gather_predictions()







by_country <- by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_country

unnest(by_country, resids)

unnest_legacy(by_country, cols = c(data, as.list(model), resids))

z <- by_country$model[1]
z[[1]]$coefficients

z[[1]][["coefficients"]]
coef(z[[1]])

extract_model <- function(model){
  coef(model)
}

extract_model(by_country$model[1])

by_country %>% 
  mutate(coef = map(model, extract_model))

by_country %>% 
  mutate(coef = map(model, extract_model)) %>% 
  unnest_wider(coef)
