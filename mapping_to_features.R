# Mapping variables to a marker feature

library(ggplot2)
library(viridis)
library(tidyr)

dat = iris

# Transparency by species
plot1 = ggplot(dat, aes(x=Sepal.Length, y=Sepal.Width, alpha=Species)) + 
  geom_point(size=6, color="#69b3a2") +
  theme_minimal()

plot1

# Shape by species
plot2 = ggplot(dat, aes(x=Sepal.Length, y=Sepal.Width, shape=Species)) + 
  geom_point(size=6) +
  theme_minimal()

plot2

# Size by species (not an advisable choice)
plot3 = ggplot(dat, aes(x=Sepal.Length, y=Sepal.Width, size=Species)) + 
  geom_point() +
  theme_minimal()

plot3

# One variable to more than one feature. This violates all the Tufte principles

plot4 = ggplot(dat, aes(x=Sepal.Length, 
                        y=Sepal.Width, 
                        shape=Species, 
                        alpha=Species, 
                        color=Species)) + 
  geom_point() +
  theme_minimal()

plot4

