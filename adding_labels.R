# Adding text labels to plots.
# You might want to subset your data, for example, by filtering rows and
# plotting only those above a certain threshold.

library(ggplot2)
library(dplyr)
library(tibble)
library(tidylog)

dat = head(mtcars, 20)

plot1 = ggplot(dat, aes(x=wt, y=mpg)) +
  geom_point() +
  geom_text(label=rownames(dat), 
    nudge_x = 0.20, nudge_y = 0.20, 
    check_overlap = T
  )

plot1

# Nudge repels the labels away from the data points to avoid overlapping
# the x and y values can be adjusted to ensure the display is suitable

# geom_label() can also operate in the same way as geom_text(), what changes
# is the aesthetics (box vs floating text)
# Note: the check_overlap argument does not work on geom_label()

plot2 = ggplot(dat, aes(x=wt, y=mpg)) +
  geom_point() +
  geom_label(
    label=rownames(dat), 
    nudge_x = 0.3, nudge_y = 0.3)

plot2


# What if you want to highlight a single data point, much like was done with
# boxplots/categorical variables. It would work best for outliers and things 
# like that.

plot3 = ggplot(dat, aes(x=wt, y=mpg)) +
  geom_point() + # Show dots
  geom_label(
    label="Important", 
    x=4.1,
    y=18,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill="#69b3a2"
  )
plot3

# This is an example where we filter values by some threshold to label
# those cases that are important for some problem-specific reason.

# Change rownames as a real column which we will call 'carName' which will 
# provide the labels.
dat = dat %>%
  rownames_to_column(var="carName")

plot4 = ggplot(dat, aes(x=wt, y=mpg)) +
  geom_point() + 
  geom_label( 
    dat = dat %>% filter(mpg>19 & wt>3), # Filter data first
    aes(label=carName)
  )

plot4

