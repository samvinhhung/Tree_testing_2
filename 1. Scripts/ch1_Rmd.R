
# functions chapter 1 Theory ----------------------------------------------

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



# -------------------------------------------------------------------------
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



# update for min value ----------------------------------------------------

StatMinLine <- ggproto("StatMinLine", Stat,
                        compute_group = function(data, scales) {
                          transform(data, yintercept=min(y))
                        },
                        required_aes = c("x", "y")
)

stat_min_line <- function(mapping = NULL, data = NULL, geom = "hline",
                           position = "identity", na.rm = FALSE, show.legend = NA, 
                           inherit.aes = TRUE,...) {
  layer(
    stat = StatMinLine, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

CrossHVLine <- ggproto("CrossHVLine", Stat,
                          compute_group = function(data, scales) {
                                      # yintercept= 1
                                      # x_intercept = data[which.min(data[,parse(text = y)[[1]]]),
                                                         # parse(text = x)[[1]]]
                                      
                                      # foo[which.min(foo[,"value"]),"split"]
                                      
# 
#                                       data[which.min(data[,parse(text = y)[[1]]]),
#                                            parse(text = y)[[1]]]
                                      
                                      # which_y = which(data[,])
                            (transform(data,
                                       xintercept = data[which.min(data[,parse(text = x)[[1]]]),
                                                         parse(text = x)[[1]]],
                                       yintercept = min(y)
                                       # xintercept = x_intercept

                                       # foo[foo[,"value"] == min(foo[,"value"]), ][,"split"]

                                       #halfway works

                                       # xintercept = data[min(data[,parse(text = x)[[1]]]),
                                       #                   parse(text = x)[[1]]]



                                       )
                              )

                            # data %>%
                            #   group_by(col) %>%
                            #   filter(y == min(y)) %>%

                            
                                       # xintercept = (data[data[,parse(text = y)[[1]]] == min(data[,parse(text = y)[[1]]]), ][,parse(text = x)[[1]]])))
                          },
                       required_aes = c("x", "y")
)

foo[which.min(foo[,"split"]), "split"]

sym("x")

data[min(data[,parse(text = x)[[1]]]),
                        parse(text = x)[[1]]]

stat_cross_h_v_line <- function(mapping = NULL, data = NULL, geom = "hline",
                          position = "identity", na.rm = FALSE, show.legend = NA, 
                          inherit.aes = TRUE,...) {
  list(
    layer(
      stat = CrossHVLine, data = data, mapping = mapping, geom = GeomHline,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    ), 
    layer(
      stat = CrossHVLine, data = data, mapping = mapping, geom = GeomVline,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  )

}

test_kyph %>% 
  gather(key, value, -split) %>% 
  ggplot(aes(x = as.numeric(split), y = value, col = key)) + 
  geom_point() + 
  stat_cross_h_v_line(aes_(x = split, y = value)) +
  facet_wrap(~key)  +
  scale_x_continuous(breaks = 1:20)

test_kyph %>% 
  gather(key, value, -split) %>% 
  ggplot(aes(x = as.numeric(split), y = value, col = key)) + 
  geom_point() + 
  stat_cross_h_v_line(aes(x = split, y = value)) +
  facet_wrap(~key)  +
  scale_x_continuous(breaks = 1:20)

x <- "split"
y <- "value"



as.numeric(foo %>% 
  group_by(key) %>% 
  filter(value == min(value)) %>%  ungroup() %>% 
  select(split) %>% unlist())

eval(y)
substitute("y")

rm(y)
rm(x)
y

foo[which.min(foo$value), "split"]

`$`(foo, parse(text = y)[[1]])

`$`(foo, as.name(y))
get("y")

get(parse(text = y)[[1]])

substitute((parse(text = y)[[1]]))

as.character()

(foo[foo[,parse(text = y)[[1]]] == min(foo[,parse(text = y)[[1]]]), ][,parse(text = x)[[1]]])




test_kyph %>% 
  gather(key, value, -split) %>% 
  ggplot(aes(x = as.numeric(split), y = (value), col = key)) + 
  geom_point() + 
  geom_line() + 
  # geom_hline(aes(yintercept = min(value)), linetype = 3) +
  # stat_min_line(aes(value), linetype = "dotted") + 
  scale_x_continuous("split") +
  theme_classic() + 
  facet_wrap(~key, scales = "free_y", ncol = 1) + 
  stat_cross_h_v_line(aes(x = split, y = value), linetype = 2)

# check transform(data) 
transform(mtcars,
          yintercept = min(mpg),
          xtest = mtcars[mpg == min(mpg), "cyl"])


foo <- test_kyph %>% 
  gather(key, value, -split)

foo[foo[,"value"] == min(foo[,"value"]), ][,"split"]

# geom = GeomHline,  

ggplot(mtcars, aes(mpg, cyl)) +
  stat_min_line(color="red", linetype = 2) +
  geom_point() +
  facet_wrap(~ gear) + 
  geom_hline(yintercept = c(1,2))

test_kyph %>% 
  gather(key, value, -split) %>% 
  ggplot(aes(x = as.numeric(split), y = (value), col = key)) + 
  geom_point() + 
  geom_line() + 
  # geom_hline(aes(yintercept = min(value)), linetype = 3) +
  # stat_min_line(aes(value), linetype = "dotted") + 
  scale_x_continuous("split") +
  theme_classic() + 
  facet_wrap(~key, scales = "free_y", ncol = 1) + 
  stat_cross_h_v_line(aes(split, value))




transform(mtcars, yintercept = min(mpg))


test_kyph %>% 
  gather(key, value, -split) %>% 
  group_by(key) %>% 
  summarise(value = min(value))

test <- test_kyph %>% 
  gather(key, value, -split)

transform(test, yintercept = min(value), 
          xintercept = test[value == min(value), "split"])

test


# plot the splits ---------------------------------------------------------


library(rpart)
library(tidyverse)

# Gini splits

rpart(Kyphosis ~ Age  + Start, data = kyphosis,
      parms = list(split = "gini"), control = rpart.control(cp = 0, maxdepth = 4))

p1 <- kyphosis %>% 
  ggplot(aes(x = Age, y = Start, col = Kyphosis)) + 
  geom_point() + 
  theme_classic() +
  geom_hline(yintercept = c(8.5,14.5), linetype = 2) + 
  geom_vline(xintercept = c(55, 111), linetype = 2) + 
  ggtitle("Splits avec mesure d'impurité de Gini")

install.packages("gridBase")
library(gridBase)
library(grid)
  rpart.plot::rpart.plot(kyph_gini)

plot.new()              ## suggested by @Josh
vps <- baseViewports()
pushViewport(vps$figure) ##   I am in the space of the autocorrelation plot
vp1 <-plotViewport(c(1.8,1,0,1)) ## create new vp with margins, you play with this values 



gridExtra::grid.arrange(p1, rpart.plot::rpart.plot(kyph_gini) )

# information splits

rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,
                   parms = list(split = "information"), control = rpart.control(maxdepth = 2))

rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,
                   parms = list(split = "gini"), control = rpart.control(maxdepth = 4, cp = 0))

rpart(Kyphosis ~ Age + Start, data = kyphosis,
      parms = list(split = "information"), control = rpart.control(maxdepth = 4, cp = 0))

p2 <- kyphosis %>% 
  ggplot(aes(x = Age, y = Start, col = Kyphosis)) + 
  geom_point() + 
  theme_classic() +
  geom_hline(yintercept = c(12.5), linetype = 2) + 
  geom_vline(xintercept = c(34.5,123), linetype = 2) + 
  ggtitle("Splits avec mesure d'entropie")

gridExtra::grid.arrange()

kyph_info

kyphosis %>% 
  ggplot(aes(x = Age, y = Start, col = Kyphosis)) + 
  geom_point() + 
  theme_classic() +
  geom_hline(yintercept = c(12.5,14.5), linetype = 2) + 
  geom_vline(xintercept = 34.5, linetype = 2)

rpart.plot::rpart.plot(kyph_info)



kyphosis %>% 
  mutate()

# start : 8.5 -> 14.5, 
kyph_gini$splits %>% as.data.frame() %>%  rownames_to_column() %>%  
  group_by(count) %>% 
  filter(improve == max(improve))

kyph_info
