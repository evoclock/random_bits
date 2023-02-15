#Boxplots with jitter to show the underlying distribution of data points

library(tidyverse)
library(viridis)
library(ggplot2)

data = data.frame(
  name = c(rep("A",500), 
          rep("B",500), 
          rep("B",500), 
          rep("C",20), 
          rep('D', 100)  ),
  value = c( rnorm(500, 10, 5), 
             rnorm(500, 13, 1), 
             rnorm(500, 18, 1), 
             rnorm(20, 25, 4), 
             rnorm(100, 12, 1)))

plot = data %>% 
  ggplot( aes(x=name, y=value, fill=name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.4) +
  geom_jitter(color="black", size=0.2, alpha=0.7) +
  theme_minimal() +
  theme(legend.position="none",
    plot.title = element_text(size=11)) +
  ggtitle("Boxplot with data points") +
  xlab("")

plot

# The geom_jotter function introduces the data points, size of the data points
# and opacity are tweakable of course violin plots give an overall sense of
# the density distribution of the data as well for example, distributions that
# are bimodal are clearly distinguishable as are the tails of a distribution.

plot_2 = data %>%
  ggplot(aes(x=name, y=value, fill=name)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.4, option="E") +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin plot") +
  xlab("")

plot_2

# Specific groups can be highlighted, if for example you want to illustrate
# a group which is significantly different to the others in some post-hoc
# analysis. To allow highlighting, this information needs to be encoded in 
# a separate column in a binary form.

# We need to generate that new column with mutate

library(dplyr)

mpg_2 = mpg %>% mutate(type=ifelse(class=="pickup", "Highlighted", "Normal")) 

plot_3 =  ggplot(mpg_2, aes(x=class, y=hwy, fill=type, alpha=type)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#69b3a2", "grey")) +
  scale_alpha_manual(values=c(1,0.1)) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("")

plot_3

# Categories can also be divided into subgroups. For example, low and high 
# dosage treatments.
  
variety = rep(LETTERS[1:7], each=40)
treatment = rep(c("high","low"), each=20)
note= seq(1:280)+ sample(1:150, 280, replace=T)
data = data.frame(variety, treatment, note)
  

plot_4 = ggplot(data, aes(x=variety, y=note, fill=treatment)) + 
    geom_boxplot()

plot_4
