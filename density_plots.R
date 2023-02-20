library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(tidylog)


dat = diamonds

plot1 = ggplot(dat, aes(x = price, group = cut, fill = cut)) +
  geom_density(adjust = 1.5, alpha = 0.2) +
  theme_minimal()

plot1


data = read.table("https://raw.githubusercontent.com/zonination/perceptions/master/probly.csv", header=TRUE, sep=",")
data = data %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))

# gather: reorganized (Almost.Certainly, Highly.Likely, 
# Very.Good.Chance, Probable, Likely, …) into (text, value) 
# [was 46x17, now 782x2]
# mutate: changed 552 values (71%) of 'text' (0 new NA)
# mutate: changed 5 values (1%) of 'value' (0 new NA)

annotation = data.frame(
  text = c("Almost No Chance", "About Even", "Probable", "Almost Certainly"),
  x = c(5, 53, 65, 79),
  y = c(0.15, 0.4, 0.06, 0.1)
)

plot2 = data %>%
  filter(text %in% c("Almost No Chance", 
                     "About Even", 
                     "Probable", 
                     "Almost Certainly")) %>%
  ggplot( aes(x=value, color=text, fill=text)) +
  geom_density(alpha=0.4) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  geom_text(data=annotation, 
             aes(x=x, y=y, 
                 label=text, 
                 color=text), 
            hjust=0, 
            size=4.5) +
  theme_minimal() +
  theme(
    legend.position="none"
  ) +
  ylab("") +
  xlab("Assigned Probability (%)")

plot2

# When the densities overlap by a great deal you might want to see 
# see each individual density plot like so:

plot3 = ggplot(dat, aes(x=price, group=cut, fill=cut)) +
  geom_density(adjust=1.5, alpha = 0.5) +
  theme_minimal() +
  facet_wrap(~cut) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  )

plot3


# Plotting the distribution of numerical variables as a ridgeline plot.
# Used to be called joyplot after Joy Division's unknwon pleasures album
# art.

library(ggridges)
library(forcats)

plot4 = ggplot(dat, aes(x = price, y = cut, fill = cut)) +
  geom_density_ridges(alpha = 0.5) +
  theme_ridges() + 
  theme(legend.position = "none")

plot4

# With categorical variables we need the forcats library

data2 = read.table("https://raw.githubusercontent.com/zonination/perceptions/master/probly.csv", 
                  header=TRUE, sep=",")
data2 = data2 %>% 
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0)) %>%
  filter(text %in% c("Almost Certainly",
                     "Very Good Chance",
                     "We Believe",
                     "Likely",
                     "About Even", 
                     "Little Chance", 
                     "Chances Are Slight", 
                     "Almost No Chance"))

# gather: reorganized (Almost.Certainly, Highly.Likely, 
# Very.Good.Chance, Probable, Likely, …) into (text, value) 
# [was 46x17, now 782x2]
# mutate: changed 552 values (71%) of 'text' (0 new NA)
# mutate: changed 5 values (1%) of 'value' (0 new NA)
# filter: removed 414 rows (53%), 368 rows remaining
# mutate(text = gsub("\\.", " ",text)) replaced the periods between words to 
# spaces.

plot5 = data %>%
  mutate(text = fct_reorder(text, value)) %>%
  ggplot( aes(y=text, x=value,  fill=text)) +
  geom_density_ridges(alpha=0.4, stat="binline", bins=20) +
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Assigned Probability (%)")

plot5

# mutate: converted 'text' from character to factor (0 new NA) using
# fct_reorder (from forcats)
# If you want the same plot without binning (histogram) remove the 
# stat="binline" argument in geom_density_ridges()

plot6 = data %>%
  mutate(text = fct_reorder(text, value)) %>%
  ggplot( aes(y=text, x=value,  fill=text)) +
  geom_density_ridges(alpha=0.4) +
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Assigned Probability (%)")

plot6

# Setting the colour value relative to a numerical rather than a categorical
# variable. This would be useful when plotting numerical values such as differences
# in number of gene transcripts for example, or temperatures.

data3 = lincoln_weather

plot7 = ggplot(data3, 
               aes(x = `Mean Temperature [F]`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE in 2016') +
  theme_minimal() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

plot7

