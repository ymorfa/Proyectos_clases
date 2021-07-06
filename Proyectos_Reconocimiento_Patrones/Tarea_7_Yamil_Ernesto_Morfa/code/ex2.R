library(palmerpenguins)
library(ggplot2)

data = subset(penguins, penguins$species!='Adelie')
data = subset(data, !is.na(data$bill_length_mm))


X = cbind( data$sex, data$bill_length_mm, data$bill_depth_mm, data$flipper_length_mm, data$body_mass_g)
categoria<-factor(data$species)
pairs(X[,2:5], col= categoria, labels = c( "bill_length_mm", "bill_depth_mm", "flipper_length_mm",
                                            "body_mass_g",  "sex", "year"))

ggplot(data, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 2) +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4"))

ggplot(data, aes(x = sex, fill = species)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"),
                    guide = FALSE) +
  theme_minimal() +
  facet_wrap(~species, ncol = 1) +
  coord_flip()

ggplot(data, aes(x = flipper_length_mm,
                     y = body_mass_g)) +
  geom_point(aes(color = sex)) +
  scale_color_manual(values = c("darkorange","cyan4"),
                     na.translate = FALSE) +
  facet_wrap(~species)
