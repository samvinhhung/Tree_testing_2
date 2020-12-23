library(tree)
library(ISLR)
attach(Carseats)

High = ifelse(Sales <= 8, "No", "Yes")

Carseats = data.frame(Carseats, High)

tree.Carseats <- tree(High~.-Sales, Carseats)

summary(tree.Carseats)
plot(tree.Carseats)
text(tree.Carseats, pretty = 0)


tree.Carseats

# personnal test ----------------------------------------------------------

rpart.Carseats <- rpart(High~ . - Sales, Carseats)

summary(rpart.Carseats)

rpart.plot(rpart.Carseats)

sum(High == "Yes") +
sum(High == "No")

# check the deviance returned : 
tree.Carseats2 <- tree(High~. - Sales, Carseats, control = tree.control(nobs = 400, mincut = 400))

# in hard numbers
-2* (236 * log(0.59) +  164 * log(0.41))

# in formula :

-2* unname(
  # 236
  tree.Carseats2$frame$n * tree.Carseats2$frame$yprob[,"No"] *
    # log (p_m_k) where m is the node
             log(tree.Carseats2$frame$yprob[,"No"]) + 
    # 164
  tree.Carseats2$frame$n * tree.Carseats2$frame$yprob[,"Yes"] * 
    # log (p_m_k) where k is the class of "Yes"
    log(tree.Carseats2$frame$yprob[,"Yes"]))



# End of test -------------------------------------------------------------

set.seed(2)

train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test = High[-train]

tree.Carseats<- tree(High ~. - Sales, data = Carseats, subset = train)
tree.pred <- predict(tree.Carseats, Carseats.test, type = "class")

table(tree.pred, High.test)

# cross-validation

set.seed(3)
cv.Carseats = cv.tree(tree.Carseats, FUN = prune.misclass)
names(cv.Carseats)
cv.Carseats
par(mfrow = c(1,2))

plot(cv.Carseats$size, cv.Carseats$dev, type = "b")
plot(cv.Carseats$k, cv.Carseats$dev, type = "b")

par(mfrow = c(1,1))
prune.Carseats <- prune.misclass(tree.Carseats, best = 21)
plot(prune.Carseats)
text(prune.Carseats, pretty = 0)

tree.pred <- predict(prune.Carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
(table(tree.pred, High.test)[1,1] + table(tree.pred, High.test)[2,2]) / sum(table(tree.pred, High.test))


# personnal test ----------------------------------------------------------
# loop through the different sizes of the tree

# data.frame to compare accuracy w.r. to size
df <- data.frame(size = cv.Carseats$size, accuracy = NA)

for (i in cv.Carseats$size) {
  prune.Carseats <- prune.misclass(tree.Carseats, best = i)
  if (i == 1) {
    tree.Carseats2 <- tree(High~. - Sales, Carseats, subset = train, control = tree.control(nobs = 400, mincut = 400))
    tree.pred <- predict(tree.Carseats2, Carseats.test, type = "class")
  } else {
    tree.pred <- predict(prune.Carseats, Carseats.test, type = "class")
  }

  df[df$size == i, "accuracy"] <- (table(tree.pred, High.test)[1,1] + table(tree.pred, High.test)[2,2]) / sum(table(tree.pred, High.test))
}

# having 21 nodes or 9 nodes give the same results

plot(df, type  = "b")
text(df, labels = df$size, pos = 2)
abline(h = max(df$accuracy))

# End of personnal test ---------------------------------------------------


