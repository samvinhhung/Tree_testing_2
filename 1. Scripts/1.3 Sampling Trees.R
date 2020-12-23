
# dev 1  ------------------------------------------------------------------

sample_3

sample_3 <- sample_1 %>% 
  select(ClNr, LoB, cc, AY, AQ, age, inj_part, RepDel, contains("00"), contains("01"))

# 22699 claims open in dev1
sample_1 %>% 
  filter(Open00 != 0)

sample_3 <- sample_3 %>% 
  filter(Open00 != 0) %>% 
  mutate(YZ = ifelse((Open01 >= 1) & (Pay01 >= 1), 3, 
                     ifelse(Open01 >= 1 & Pay01 < 1, 2, 
                            ifelse(Open01 <= 0 & Pay01 >= 1, 1, 
                                   ifelse(Open01 <= 0 & Pay01 < 1, 0, NA))))) 
cbind(test, model.matrix(~cc +0 , data = test))


cbind(sample_3, model.matrix(~cc + 0, data = test)) 

# create specfic levels

c(cc14+ cc17+ cc3 + cc33 +cc47 +cc48+ cc7)

c(3,7, 14, 17, 33, 47, 48)


sample_1.3 <- sample_3


# levels(sample_1.3$cc) %in% c(3,7, 14, 17, 33, 47, 48)

# factor(sample_1.3$cc, levels = list(c(3,7, 14, 17, 33, 47, 48), 
#                                     "else" = levels(sample_1.3$cc)[!(levels(sample_1.3$cc) %in% c(3,7, 14, 17, 33, 47, 48))]))

# levels(sample_1.3$cc)[!levels(sample_1.3$cc) %in% c(3,7, 14, 17, 33, 47, 48)]


# sample_1.3$cc %>% fct_collapse(other = levels(sample_1.3$cc)[!levels(sample_1.3$cc) %in% c(3,7, 14, 17, 33, 47, 48)])

sample_1.3 <- sample_1.3 %>% 
  select(-freq) %>% 
  mutate(cc_light = cc) %>% 
  mutate(cc_light = fct_collapse(cc_light, other = levels(sample_1.3$cc)[!levels(sample_1.3$cc) %in% c(3,7, 14, 17, 33, 47, 48)])) 


sample_1.3_wide <- cbind(sample_1.3, model.matrix(~cc_light + 0, data = sample_1.3)) 

sample_1.3_wide <- as_tibble(sample_1.3_wide)

# dev 1 -------------------------------------------------------------------

# do not use last AY
# do not use late Claims arrival

sample_1.3_wide_dev1 <- sample_1.3_wide %>% 
  filter(RepDel < 1) %>% 
  filter(AY < 2005) %>% 
  filter(Open00 >= 1)

rpart(YZ ~ . -YZ - ClNr - cc - AY - inj_part + factor(Pay00 > 0) -Pay00 - Pay01 - Open00 - Open01 -cc_light, 
      data = sample_1.3_wide_dev1)

rpart(YZ ~ . -YZ - ClNr - cc - AY - inj_part + factor(Pay00 > 0), 
      data = sample_1.3_wide_dev1)

# fitted tree
rpart.dev1 <- rpart(YZ ~ cc_light + LoB + AQ + age + factor(Pay00 > 0), 
      data = sample_1.3_wide_dev1, method = "class", 
      control = rpart.control(cp = 8.143322e-04))

printcp(rpart.dev1)

dev1.predict <- predict(rpart.dev1, newdata = sample_1.3_wide %>% 
          filter(AY == 2005, RepDel < 1))

# simulate one path of the predictions for each claim
dev1.predict

dev1.predict.sim <- apply(dev1.predict, 1, function(x) sample(0:3, 1,T, prob = x))

sample_1.3_wide %>% 
  filter(AY == 2005, RepDel < 1) %>% 
  cbind(dev1.predict.sim) %>% 
  count(YZ, dev1.predict.sim) %>% 
  group_by(YZ) %>% 
  mutate(prop = prop.table(n)) %>% 
  ggplot(aes(x = dev1.predict.sim, y = prop)) + 
  geom_col() +
  facet_wrap(~YZ, scales = "free_y")

sample_1.3_wide_pred <- sample_1.3_wide %>% 
  filter(AY == 2005, RepDel < 1) %>% 
  mutate(pred_1 = dev1.predict.sim)

table(sample_1.3_wide_pred$pred_1)
table(sample_1.3_wide_pred$YZ)


plot((rpart.dev1$cptable[,4]) - min((rpart.dev1$cptable[,4])))

rpart.dev1$cptable[,1]

names(sample_1.3_wide_dev1)


rpart(YZ ~ inj_part, data = sample_1.3_wide_dev1[1:100,], control = rpart.control(cp = 0.1, xval = 0), 
      method = "class")


unique(sample_1.3_wide_dev1$AY)

# dev 2 -------------------------------------------------------------------

sample_1.3_wide_dev1



# dev 3 -------------------------------------------------------------------

sample_1 %>% 
  filter(Open01 != 0)

sample_1%>% 
  filter(Open00 != 0)

sample_3 <- sample_3 %>% 
  filter(Open00 != 0) %>% 
  mutate(YZ = ifelse((Open01 >= 1) & (Pay01 >= 1), 3, 
                     ifelse(Open01 >= 1 & Pay01 < 1, 2, 
                            ifelse(Open01 <= 0 & Pay01 >= 1, 1, 
                                   ifelse(Open01 <= 0 & Pay01 < 1, 0, NA))))) 




# mutate all dev ----------------------------------------------------------

df <- sample_1

pads <- str_pad(0:11, 2, side = "left", pad = "0")

pay_var <- grep(paste0("Pay", pads[1]), x = colnames(sample_1))
open_var <- grep(paste0("Open", pads[1]), x = colnames(sample_1))
YZ_var <- paste0("YZ", pads[1])

pay_var <- df[[pay_var]]
open_var <- df[[open_var]]
temp <- vector("numeric", length = nrow(df))

(temp = ifelse((open_var >= 1) & (pay_var >= 1), 3, 
                   ifelse(open_var >= 1 & pay_var < 1, 2, 
                          ifelse(open_var <= 0 & pay_var >= 1, 1, 
                                 ifelse(open_var <= 0 & pay_var < 1, 0, NA))))) 
df$temp <- temp
names(df)[grep("temp", names(df))] <- YZ_var


# loop to get all YZ ------------------------------------------------------
df <- sample_1
for (i in pads) {
  pay_var <- grep(paste0("Pay", i), x = colnames(sample_1))
  open_var <- grep(paste0("Open", i), x = colnames(sample_1))
  YZ_var <- paste0("YZ", i)
  
  pay_var <- df[[pay_var]]
  open_var <- df[[open_var]]
  temp <- vector("numeric", length = nrow(df))
  
  (temp = ifelse((open_var >= 1) & (pay_var >= 1), 3, 
                 ifelse(open_var >= 1 & pay_var < 1, 2, 
                        ifelse(open_var <= 0 & pay_var >= 1, 1, 
                               ifelse(open_var <= 0 & pay_var < 1, 0, NA))))) 
  df$temp <- temp
  names(df)[grep("temp", names(df))] <- YZ_var
}

# resulting twto vectors are the same
sum(sample_1.3$YZ == (df %>% filter(Open00 != 0))$YZ01) - nrow(sample_1.3)

df %>% filter(Open00 != 0)

my_formula <- as.formula(paste0("YZ ~",
                                paste0("factor(Pay", pads,"!=0)" ,collapse =  " +"),
                                "+", 
                                paste0("Open", pads, collapse =  " +")))
df <- df %>% 
  mutate(cc_light = cc) %>% 
  mutate(cc_light = fct_collapse(cc_light, other = levels(df$cc)[!levels(df$cc) %in% c(3,7, 14, 17, 33, 47, 48)])) 


df2 <- cbind(df, model.matrix(~inj_part + 0, data = df)) 

inj_variables <- df2 %>% 
  select(starts_with("inj_part")) %>% 
  select(-inj_part) %>% 
  colnames()

inj_part_formula <- as.formula(paste0("YZ01 ~",
                                      paste0(inj_variables ,collapse =  " +")))

inj_part.rpart <- rpart(inj_part_formula, data = df2 %>%   filter(RepDel < 1) %>% 
                          filter(AY < 2005) %>% 
                          filter(Open00 >= 1), control = rpart.control(cp = 0))


data.frame(imp = inj_part.rpart$variable.importance) %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable)) %>% 
  ggplot2::ggplot() +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw()


inj_part_main <- str_sub(names(inj_part.rpart$variable.importance)[1:15], 9,10)

test <- df2 %>% 
  filter(inj_part %in% inj_part_main)

!(levels(test$inj_part) %in% inj_part_main)

df <- df %>% 
  mutate(inj_part_light = inj_part) %>% 
  mutate(inj_part_light = fct_collapse(inj_part,
                                       other_I = levels(df$inj_part)[!(levels(df$inj_part) %in% inj_part_main)])) 




# after mutate inj_part / cc ----------------------------------------------

dev1_formula <- as.formula(paste0("YZ ~",
                                  paste0("factor(Pay", pads,"!=0)" ,collapse =  " +"),
                                  "+", 
                                  paste0("Open", pads, collapse =  " +"),
                                  "+ RepDel", df))
pads[x]
dev_formula <- function(x){
  as.formula(paste0(paste0("YZ", pads[x+1],"~") ,
                    paste0("factor(Pay", pads[1:x],"!=0)" ,collapse =  " +"),
                    "+", 
                    paste0("Open", pads[1:x], collapse =  " +"),
                    "+ RepDel + inj_part_light + cc_light + LoB"))
}

(dev1_formula <- dev_formula(1))

rpart.dev1 <- rpart(dev1_formula, 
                    data = df %>%  filter(RepDel < 1) %>% 
                      filter(AY < 2005) %>% 
                      filter(Open00 >= 1), method = "class", 
                    control = rpart.control(cp = 8.143322e-04))

rpart.plot(rpart.dev1)

# dev 1 

df[,grep(colnames(df), pattern = "Open")]

df_train_by_dev <- function(x, data = df){
  Open_index <- grep(colnames(data), pattern = "Open")
  df %>%  filter(RepDel < x) %>% 
    filter(AY < sort(unique(data$AY), decreasing = T)[x]) %>% 
    filter(.[[Open_index[x]]] >= 1)
}

df_train_by_dev(1)
df_train_by_dev(2)

rpart.dev1 <- rpart(dev1_formula, 
                    data = df_train_by_dev(1), method = "class", 
                    control = rpart.control(cp = 8.143322e-04))


# wrap in function prediction process -------------------------------------

df_predict_by_dev <- function(x, data = df){
  Open_index <- grep(colnames(data), pattern = "Open")
  data %>% 
    filter(AY >= sort(unique(df$AY), decreasing = T)[x]) %>% 
    filter(RepDel < x) %>% 
    filter(.[[Open_index[x]]] >= 1)
}

nrow(df %>% filter(Open00 == 0))
nrow(df %>% filter(AY == 2005)) - 
nrow(df %>% filter(AY == 2005) %>% filter(RepDel >= 1))

nrow(df_predict_by_dev(1) %>% filter(Open00 >= 1)) +
nrow(df %>% filter(AY == 2005) %>% filter(RepDel<1) %>%  filter(Open00 == 0))

nrow(df_predict_by_dev(1))
nrow(df_train_by_dev(1))

nrow(df %>% 
  filter(RepDel >= 1))

df[df$ClNr %in% df_predict_by_dev(1)$ClNr,]
dev1.predict <- predict(rpart.dev1, newdata = df_predict_by_dev(1))
dev1.predict.sim <- apply(dev1.predict, 1, function(x) sample(0:3, 1,T, prob = x))

df_predict_by_dev(1) %>% 
  mutate(YZ01 = dev1.predict.sim)

df[df$ClNr %in% df_predict_by_dev(1)$ClNr,] <- df[df$ClNr %in% df_predict_by_dev(1)$ClNr,] %>% 
  mutate(YZ01 = dev1.predict.sim)


# hard vvalues ------------------------------------------------------------

table(sample_1.3_wide_pred$YZ)

# simulate one path of the predictions for each claim
dev1.predict

dev1.predict.sim <- apply(dev1.predict, 1, function(x) sample(0:3, 1,T, prob = x))

df_predict_by_dev(1)

sample_1.3_wide_pred <- sample_1.3_wide %>% 
  filter(AY == 2005, RepDel < 1) %>% 
  mutate(pred_1 = dev1.predict.sim)

table(sample_1.3_wide_pred$pred_1)
table(sample_1.3_wide_pred$YZ)



dev_formula(2)
df_train_by_dev(2)


# function ----------------------------------------------------------------

prediction_function <- function(dev){
  rpart.dev1 <- rpart(dev_formula(dev), 
                      data = df_train_by_dev(dev), method = "class", 
                      control = rpart.control(cp = 8.143322e-04))
  
  
  dev1.predict <- predict(rpart.dev1, newdata = df_predict_by_dev(dev))
  dev1.predict.sim <- apply(dev1.predict, 1, function(x) sample(0:3, 1,T, prob = x))
  
  # YZXX location
  
  YZXX <- paste0("YZ", pads[dev+1]) 
  
  
  df[df$ClNr %in% df_predict_by_dev(dev)$ClNr,] <- df[df$ClNr %in% df_predict_by_dev(dev)$ClNr,] %>% 
    mutate({{YZXX}} := dev1.predict.sim)
  
  list(rpart.dev1 = rpart.dev1,df = df)
}

for (i in 1:11) {
  df <- prediction_function(i)$df
  print(i)
}

prediction_function(3)

df[df$ClNr %in% df_predict_by_dev(2)$ClNr,] %>% 
mutate({{YZXX}} := dev2.predict.sim)

df

validation <- sample_1

for (i in pads) {
  pay_var <- grep(paste0("Pay", i), x = colnames(sample_1))
  open_var <- grep(paste0("Open", i), x = colnames(sample_1))
  YZ_var <- paste0("YZ", i)
  
  pay_var <- validation[[pay_var]]
  open_var <- validation[[open_var]]
  temp <- vector("numeric", length = nrow(validation))
  
  (temp = ifelse((open_var >= 1) & (pay_var >= 1), 3, 
                 ifelse(open_var >= 1 & pay_var < 1, 2, 
                        ifelse(open_var <= 0 & pay_var >= 1, 1, 
                               ifelse(open_var <= 0 & pay_var < 1, 0, NA))))) 
  validation$temp <- temp
  names(validation)[grep("temp", names(validation))] <- YZ_var
}

tibble(valid = df_predict_by_dev(1, validation)$YZ01,
test = df_predict_by_dev(1, df)$YZ01) %>% 
  group_by(valid, test) %>% 
  summarise(n = n())


df_predict_by_dev(x = 2, prediction_function(2)$df)$YZ02

table(df_predict_by_dev(2, df)$YZ02)
table(df_predict_by_dev(2, validation)$YZ02)

table(df_predict_by_dev(1, df)$YZ01)
table(df_predict_by_dev(1, validation)$YZ01)






# ok ----------------------------------------------------------------------



rpart.dev2 <- rpart(dev_formula(2), 
                    data = df_train_by_dev(2), method = "class", 
                    control = rpart.control(cp = 8.143322e-04))


dev2.predict <- predict(rpart.dev2, newdata = df_predict_by_dev(2))
dev2.predict.sim <- apply(dev2.predict, 1, function(x) sample(0:3, 1,T, prob = x))

YZXX <- "YZ02"

df[df$ClNr %in% df_predict_by_dev(2)$ClNr,] <- df[df$ClNr %in% df_predict_by_dev(2)$ClNr,] %>% 
  mutate("YZ02" = dev2.predict.sim)

df[df$ClNr %in% df_predict_by_dev(2)$ClNr,] %>% 
  mutate({{YZXX}} := dev2.predict.sim)

list(rpart.dev2 = rpart.dev2,df = df)


for (var in names(mtcars)) {
  mtcars %>% count(.data[[var]]) %>% print()
}

for (var in names(mtcars)) {
  mtcars %>% mutate( {{var}} := 1) %>% print()
}

"petal.{n}" := Petal.Width * n
