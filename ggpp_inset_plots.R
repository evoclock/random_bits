# Making an inset plot (a.k.a "zoomed-in" detail)

library(ggplot2)
library(tibble)
library(dplyr)
library(ggpp)

dat = mtcars
plot_1_inset = ggplot(dat, aes(factor(cyl), mpg, colour = factor(cyl))) +
  stat_boxplot() +
  labs(y = NULL) +
  theme_bw(9) +
  theme(legend.position = "none",
        panel.grid = element_blank())

plot_1 = ggplot(dat, aes(wt, mpg, colour = factor(cyl))) +
  geom_point() +
  theme_bw() +
  annotate("plot_npc", npcx = "left", npcy = "bottom", label = plot_1_inset) +
  expand_limits(y = 0, x = 0)
plot_1

dat2 = mpg

plot2_inset = ggplot(dat2, aes(factor(cyl), hwy, fill = factor(cyl))) +
  stat_summary(geom = "col", fun = mean, width = 2/3) +
  labs(x = "Number of cylinders", y = NULL, title = "Means") +
  scale_fill_discrete(guide = "none") +
  theme_bw()
plot2_inset

data.tb = tibble(x = 7, y = 44,
                 plot = list(plot2_inset + theme_bw(8)))

plot2 = ggplot(dat2, aes(displ, hwy, colour = factor(cyl))) +
  geom_plot(data = data.tb, aes(x, y, label = plot)) +
  geom_point() +
  labs(x = "Engine displacement (l)", y = "Fuel use efficiency (MPG)",
       colour = "Engine cylinder\n(number)") +
  theme_bw()
plot2

# Now using annotate from the ggpp package.
# This bypasses the need to create the tibble containing the plot inset 
# and plot coordinates/margins.

plot3 = ggplot(dat2, aes(displ, hwy, colour = factor(cyl))) +
  annotate("plot", x = 7, y = 44, label = plot2_inset + theme_bw(8)) +
  geom_point() +
  labs(x = "Engine displacement (l)", y = "Fuel use efficiency (MPG)",
       colour = "Engine cylinder\n(number)") +
  theme_bw()
plot3
