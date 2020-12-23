
# Check the tree ----------------------------------------------------------


rpart::plotcp(rpart.ordered)

rpart(Reliability~ ., data=car90) 


paste0("Pay", 1:10)

library(tidyverse)


# Automatically generate formula ------------------------------------------

pads <- str_pad(0:11, 2, side = "left", pad = "0")

as.formula(paste0("YZ ~",
                 paste0("Pay", pads, collapse =  " +"),
                 "+", 
                 paste0("Open", pads, collapse =  " +")))


# Factor the payments

pads <- str_pad(0:10, 2, side = "left", pad = "0")

my_formula <- as.formula(paste0("YZ ~",
                  paste0("factor(Pay", pads,"!=0)" ,collapse =  " +"),
                  "+", 
                  paste0("Open", pads, collapse =  " +")))

as.formula(paste(as.character(my_formula), "+", "test"))

rpart(my_formula, data = sample_1 %>% 
        mutate(YZ = ifelse((Open11 >= 1) & (Pay11 >= 1), 3, 
                           ifelse(Open11 >= 1 & Pay11 < 1, 2, 
                                  ifelse(Open11 <= 0 & Pay11 >= 1, 1, 
                                         ifelse(Open11 <= 0 & Pay11 < 1, 0, NA))))), 
      control = rpart.control(cp = 0))


# Update formula
update.formula(my_formula, ~ . + RepDel)

dev10.rpart <-   rpart(update.formula(my_formula, ~ . + RepDel), data = sample_1 %>% 
                         mutate(YZ = ifelse((Open11 >= 1) & (Pay11 >= 1), 3, 
                                            ifelse(Open11 >= 1 & Pay11 < 1, 2, 
                                                   ifelse(Open11 <= 0 & Pay11 >= 1, 1, 
                                                          ifelse(Open11 <= 0 & Pay11 < 1, 0, NA))))), 
                       control = rpart.control(cp = 0))

printcp(
  dev10.rpart
)

plotcp(
  dev10.rpart
)



names(output_tibble)
glimpse(output_tibble)



# Controlling the fit -----------------------------------------------------


