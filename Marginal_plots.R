## Adding marginal plots.
# These are particularly useful when combining with scatterplots and
# you want to convey what the distribution of the data is for each 
# axis.

library(ggplot2)
library(ggExtra)
library(tidyverse)
library(dplyr)
library(viridis)

data = mtcars
head(data)

plot = ggplot(data, aes(x = wt, y = mpg, color = cyl, size = cyl)) +
  geom_point() +
  theme(legend.position = "none")

plot_hist = ggMarginal(plot, 
                       type = "histogram", 
                       size = 8, 
                       fill = "orange",
                       xparams = list(bins = 10))
plot_hist

plot_density = ggMarginal(plot, type = "density")
plot_density

plot_density2 = ggMarginal(plot, 
                           type = "density", 
                           size = 6, 
                           color = "orange")
plot_density2

plot_density3 = ggMarginal(plot, 
                        margins = 'x', 
                        size = 5, 
                        color = "orange")
plot_density3

plot_boxplot = ggMarginal(plot, type = "boxplot")
plot_boxplot
