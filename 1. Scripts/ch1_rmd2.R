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
                         
                         # transform(data,
                         #            xintercept = data[which.min(data[,parse(text = x)[[1]]]),
                         #                              parse(text = x)[[1]]],
                         #            yintercept = min(y)
                         # )
                         
                         data %>% 
                           mutate(yintercept = min(y)) %>% 
                           filter(yintercept == y) %>% 
                           mutate(xintercept = x)
                                  
                         
                         # data %>%
                         #   group_by(col) %>%
                         #   filter(y == min(y)) %>%
                         
                         
                         # xintercept = (data[data[,parse(text = y)[[1]]] == min(data[,parse(text = y)[[1]]]), ][,parse(text = x)[[1]]])))
                       },
                       required_aes = c("x", "y")
)


# test_kyph %>% 
#   gather(key, value, -split) %>% 
#   ggplot(aes(x = as.numeric(split), y = value, col = key)) + 
#   geom_point() + 
#   stat_cross_h_v_line(aes(x = split, y = value)) +
#   facet_wrap(~key)  +
#   scale_x_continuous(breaks = 1:20)


# Final plot --------------------------------------------------------------

print(
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
)

