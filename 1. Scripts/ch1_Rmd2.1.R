# rpart
# tidyverse
# grid

kyph_gini <- rpart(Kyphosis ~ Age +  Start, data = kyphosis,
                   parms = list(split = "gini"), control = rpart.control(maxdepth = 4, cp = 0))

p1 <- kyphosis %>% 
  ggplot(aes(x = Age, y = Start, col = Kyphosis)) + 
  geom_point() + 
  theme_classic() +
  geom_hline(yintercept = c(8.5,14.5), linetype = 2) + 
  geom_vline(xintercept = c(55, 111), linetype = 2) + 
  ggtitle("Splits avec mesure d'impurité de Gini") + 
  theme(
    legend.position = c(0.95, 0.05),
    legend.justification = c("right", "bottom"), legend.box.background = element_rect(colour = "black")
  )


png(filename = "2. Output/Ch2 Arbres/Gini_split_table_Kyphosis.png", width = 480, height = 480)

#Create figure window and layout
plot.new()


grid.newpage()


pushViewport(viewport(layout = grid.layout(1, 2)))



#Draw ggplot
pushViewport(viewport(layout.pos.col = 1))
print(p1, newpage = FALSE)
popViewport()

#Draw bsae plot
pushViewport(viewport(layout.pos.col = 2))
par(fig = gridFIG(), new = TRUE)
rpart.plot::rpart.plot(kyph_gini)
popViewport()



dev.off()





# plot of p * log(p) ------------------------------------------------------

plot((1:1000)/1000, -(1:1000)/1000 * log((1:1000)/1000) - (1-((1:1000)/1000))* log( 1- (1:1000)/1000), type = "l", col =1, ylab = "value")

plot(z_value, -(z_value) * log(z_value) - (1 - z_value) * (log(1 - z_value)), type = "l")

plot((1:1000)/1000, -(1:1000)/1000 * log((1:1000)/1000) - (1-((1:1000)/1000))* log( 1- (1:1000)/1000), type = "l", col =1, ylab = "value")


z_value <- (1:1000)/1000

lines(z_value, 2 * z_value * (1-z_value) , col ="red")

-2 * 0.5 * log( 0.5)



# compare convex Gini -----------------------------------------------------

ggplot() +   
  stat_function(fun = function(x)  2 * x * (1-x), aes(color = "Gini")) + 
  stat_function(fun = function(x)  (- x * log(x) - 
                  (1-x) * log(1-x)) / -log(0.5) * 0.5,
                n = 1000, 
                aes(color = "Entropie, à l'échelle")) + 
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill = NA)) + 
  ggplot2::scale_color_manual("Mesure", values = c("red", "blue")) + 
  labs(x = "p") +
  ggtitle("Mesures de classification, fonction convexe de l'impurité")


function(x) {}


legend("topright", c("Entropie", "Gini"), col = c(1, 2),
       text.col = "black", lty = c(2, -1), 
       merge = TRUE, bg = "gray90")
