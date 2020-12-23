
# split avec entropie -----------------------------------------------------


kyph_info <- rpart(Kyphosis ~ Age + Start, data = kyphosis,
                   parms = list(split = "information"), control = rpart.control(maxdepth = 4, cp = 0))

p2 <- kyphosis %>% 
  ggplot(aes(x = Age, y = Start, col = Kyphosis)) + 
  geom_point() + 
  theme_classic() +
  geom_hline(yintercept = c(12.5), linetype = 2) + 
  geom_vline(xintercept = c(34.5,123), linetype = 2) + 
  ggtitle("Splits avec mesure d'entropie") + 
  theme(
    legend.position = c(0.95, 0.05),
    legend.justification = c("right", "bottom"), legend.box.background = element_rect(colour = "black")
  )


#Create figure window and layout
plot.new()
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))

#Draw ggplot
pushViewport(viewport(layout.pos.col = 1))
print(p2, newpage = FALSE)
popViewport()

#Draw bsae plot
pushViewport(viewport(layout.pos.col = 2))
par(fig = gridFIG(), new = TRUE)
rpart.plot::rpart.plot(kyph_info)
popViewport()



# save plot

