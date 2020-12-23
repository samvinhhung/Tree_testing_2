
# Severity ----------------------------------------------------------------

x <- rlnorm(1000, 100, 2)

MASS::fitdistr(x, densfun = "lognormal")

log_test <- df %>% 
  filter(Pay00 > 0) %>% 
  select(Pay00) %>% 
  unlist()

df %>% 
  filter(Pay00 > 0) %>% 
  select(Pay00) %>% 
  unlist() %>% MASS::fitdistr(densfun = "lognormal")

plot(density(log_test))

as_tibble(log_test) %>% 
  ggplot(aes(x = value))  +
  geom_density()  +
  xlim(c(0,1e04)) + 
  stat_function(fun = dlnorm, color='red', args = list(meanlog = 6.272942947, sdlog = 1.310810193 ))


# Use these parameters for each dev year

meanlog_fit <- vector("numeric", length = 11)
sdlog_fit <- vector("numeric", length = 11)

for (i in 1:11) {
  rows <- df[paste0("Pay", pads[i+1])] > 0
  x <- unname(unlist(df[rows, paste0("Pay", pads[i+1])]))
  str_test_fit <- MASS::fitdistr(x, "lognormal")
  meanlog_fit[i] <- str_test_fit$estimate[1]
  sdlog_fit[i] <- str_test_fit$estimate[2]  
}

df_predict_by_dev(2)
df$Pay01

YZXX <- "YZ02"

df[(df$ClNr %in% $ClNr) %>% filter({{YZXX}} %in% c(1,3)),] 

table(df_predict_by_dev(2)$YZ02)

test_pay02 <- "Pay02"

df_predict_by_dev(2) %>% 
  filter( .data[[YZXX]] %in% c(1,3)) %>% 
  mutate({{test_pay02}} := rlnorm(n(), meanlog_fit[2], sdlog_fit[2]))

rlnorm(meanlog_fit[2], )


str_test_fit <- MASS::fitdistr(x, "lognormal")
str_test_fit$estimate[1]

i = 1
df[[df[paste0("Pay", pads[i+1])] > 0 , 2]]
df[[df[paste0("Pay", pads[i+1])] > 0, 
    paste0("Pay", pads[i+1])]]

unlist(df[1:10,1])

rlnorm(n = nrow, meanlog = meanlog_fit[dev], 
       sdlog = sdlog_fit[dev])

df_predict_by_dev(2, df)




# loop trhough each of the dev --------------------------------------------

test_pay02 <- "Pay02"

for (i in 1:11) {
  
  YZXX <- paste0("YZ", pads[i+1])
  test_pay <- paste0("Pay", pads[i+1])
  df[(df$ClNr %in% (df_predict_by_dev(i) %>% 
                      filter( .data[[YZXX]] %in% c(1,3)))$ClNr),] <- df_predict_by_dev(i) %>% 
    filter( .data[[YZXX]] %in% c(1,3)) %>% 
    mutate({{test_pay}} := rlnorm(n(), meanlog_fit[i], sdlog_fit[i]))
  
}

vector_df <- vector("numeric", 11)
vector_validation <- vector("numeric", 11)

for (i in 1:11) {
  YZXX <- paste0("YZ", pads[i+1])
  test_pay <- paste0("Pay", pads[i+1])
  
  vector_df[i] <- sum(df[(df$ClNr %in% (df_predict_by_dev(i) %>% 
                          filter( .data[[YZXX]] %in% c(1,3)))$ClNr),] %>% 
        select(.data[[test_pay]]) %>% unlist() %>% unname())
  
  vector_validation[i] <- sum(validation[(df$ClNr %in% (df_predict_by_dev(i) %>% 
                                  filter( .data[[YZXX]] %in% c(1,3)))$ClNr),] %>% 
        select(.data[[test_pay]]) %>% unlist() %>% unname())
}

test <- tibble(testing = vector_df, 
               validation  = vector_validation)


# Create Histogram --------------------------------------------------------
test_list <- as.list(1:100)
for (l in 1:100) {
  for (i in 1:11) {
    
    YZXX <- paste0("YZ", pads[i+1])
    test_pay <- paste0("Pay", pads[i+1])
    df[(df$ClNr %in% (df_predict_by_dev(i) %>% 
                        filter( .data[[YZXX]] %in% c(1,3)))$ClNr),] <- df_predict_by_dev(i) %>% 
      filter( .data[[YZXX]] %in% c(1,3)) %>% 
      mutate({{test_pay}} := rlnorm(n(), meanlog_fit[i], sdlog_fit[i]))
    
  }
  
  vector_df <- vector("numeric", 11)
  vector_validation <- vector("numeric", 11)
  
  for (i in 1:11) {
    YZXX <- paste0("YZ", pads[i+1])
    test_pay <- paste0("Pay", pads[i+1])
    
    vector_df[i] <- sum(df[(df$ClNr %in% (df_predict_by_dev(i) %>% 
                                            filter( .data[[YZXX]] %in% c(1,3)))$ClNr),] %>% 
                          select(.data[[test_pay]]) %>% unlist() %>% unname())
    
    vector_validation[i] <- sum(validation[(df$ClNr %in% (df_predict_by_dev(i) %>% 
                                                            filter( .data[[YZXX]] %in% c(1,3)))$ClNr),] %>% 
                                  select(.data[[test_pay]]) %>% unlist() %>% unname())
  }
  
  test_list[[l]] <- tibble(testing = vector_df, 
                 validation  = vector_validation)
  print(l)
}

test_list[[1]] <- data.frame(a = 1, b = 2)


test_valid <- bind_rows(test_list, .id = "id")

test_valid %>% 
  gather(key, value, - id) %>% 
  group_by(key, id) %>% 
  mutate(n = 1:n()) %>%
  ungroup() %>% 
  filter(n > 1) %>% 
  ggplot() +
  geom_vline(aes(xintercept = value), data = test_valid %>% 
               gather(key, value, - id) %>% 
               group_by(key, id) %>% 
               mutate(n = 1:n()) %>%
               filter(key == "validation") %>% 
               filter(n > 1)) +
  geom_density(aes(x = value, fill = key), data = test_valid %>% 
                 gather(key, value, - id) %>% 
                 group_by(key, id) %>% 
                 mutate(n = 1:n()) %>%
                 ungroup() %>% 
                 filter(n > 1) %>% 
  filter(key == "testing")) +
  facet_wrap(~n) +
  ggtitle("variability of the aggregated lognormal paid estimates, \n conditionnal to the realization of frequency")

test_valid %>% 
  gather(key, value, - id) %>% 
  group_by(key, id) %>% 
  mutate(n = 1:n()) %>%
  filter(key == "validation") %>% 
  filter(n == 2)

test %>% 
  gather(key, value) %>% 
  group_by(key) %>% 
  mutate(n = 1:n()) %>% 
  filter(n > 1) %>% 
  ggplot() +
  geom_col(aes(x = n, y = value, fill = key), position = "dodge") + 
  ggtitle("Predicted Yearly Aggregated Paid values,\n 1-cross-validated-tree Frequency x Lognormal-Severity")



i=3

df_predict_by_dev(11)
sort(unique(df$AY), decreasing = T)[11]



validation[(df$ClNr %in% (df_predict_by_dev(i) %>% 
                            filter( .data[[YZXX]] %in% c(1,3)))$ClNr),] %>% 
  select(.data[[test_pay]]) %>% unlist() %>% unname()


vector_d <- (df[(df$ClNr %in% (df_predict_by_dev(i) %>% 
                                        filter( .data[[YZXX]] %in% c(1,3)))$ClNr),] %>% 
                      select(.data[[test_pay]]) %>% unlist() %>% unname())

vector_val <- (validation[(df$ClNr %in% (df_predict_by_dev(i) %>% 
                                                        filter( .data[[YZXX]] %in% c(1,3)))$ClNr),] %>% 
                              select(.data[[test_pay]]) %>% unlist() %>% unname())


i  = 10
df_predict_by_dev(12)
validation[(df$ClNr %in% (df_predict_by_dev(i) %>% 
                            filter( .data[[YZXX]] %in% c(1,3)))$ClNr),] %>% 
  select(.data[[test_pay]]) %>% unlist() %>% unname()

tibble(vector_d, vector_val) %>% 
  gather(key, value) %>% 
  ggplot(aes(x = value, col = key)) + 
  geom_density()


test <- tibble(testing = vector_df, 
               validation  = vector_validation)

test %>% 
  gather(key, value) %>% 
  group_by(key) %>% 
  mutate(n = 1:n()) %>% 
  filter(n > 1) %>% 
  ggplot() +
  geom_col(aes(x = n, y = value, fill = key), position = "dodge")


df[(df$ClNr %in% (df_predict_by_dev(i) %>% 
                    filter( .data[[YZXX]] %in% c(1,3)))$ClNr),] %>% 
  select(.data[[test_pay]]) %>% unlist() %>% unname()


sum(df[(df$ClNr %in% (df_predict_by_dev(i) %>% 
                    filter( .data[[YZXX]] %in% c(1,3)))$ClNr),] %>% 
  select(.data[[test_pay]]) %>% unlist() %>% unname())

sum(validation[(df$ClNr %in% (df_predict_by_dev(i) %>% 
                        filter( .data[[YZXX]] %in% c(1,3)))$ClNr),] %>% 
      select(.data[[test_pay]]) %>% unlist() %>% unname())



df_predict_by_dev(2) %>% 
  filter( .data[[YZXX]] %in% c(1,3)) %>% 
  mutate({{test_pay02}} := rlnorm(n(), meanlog_fit[2], sdlog_fit[2]))
